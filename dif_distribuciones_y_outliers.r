
# 1. VARIABLES STARTER

n_simulation <- 10
size <- c(10, 50, 100, 500, 1000)
distr <- c('Log_Normal','Cauchy', 'Gamma_asimetria_moderada','Normal_Outliers')

# ==============================================================================
# 2. FUNCIÓN GENERADORA DE GRUPOS

choose_distr <- function(distr_index, n, is_H0){
  x1 <- NULL; x2 <- NULL
  shift <- 0 
  
  if (!is_H0) {
    if (distr_index == 1) shift <- 1.0  # LogNormal
    if (distr_index == 2) shift <- 1.0  # Cauchy
    if (distr_index == 3) shift <- 0.7  # Gamma
    if (distr_index == 4) shift <- 0.5  # Normal Outliers
  }
  
  if (distr_index == 1){
    x1 <- rlnorm(n); x2 <- rlnorm(n) 
  }
  if (distr_index == 2){ 
    x1 <- rcauchy(n); x2 <- rcauchy(n) 
  }
  if (distr_index == 3){
    x1 <- rgamma(n, shape = 2); x2 <- rgamma(n, shape = 2)
  }
  if (distr_index == 4){ 
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    # 10% de los datos outliers:
    idx1 <- sample(1:n, round(n * 0.1))
    idx2 <- sample(1:n, round(n * 0.1))
    x1[idx1] <- x1[idx1] * 5
    x2[idx2] <- x2[idx2] * 5
  }
  # Si no es para calcular el poder, shift será 0
  x2 <- x2 + shift 
  return(list(x1 = x1, x2 = x2))
}

# ==============================================================================
# 3. FUNCIÓN DE SIMULACIÓN COMPARATIVA (3 ESTRATEGIAS)

simulacion_comparativa <- function(total_sim, n_sizes, distributions){
  
  # Plantilla matriz
  plantilla <- matrix(NA, nrow = length(n_sizes), ncol = length(distributions))
  colnames(plantilla) <- distributions
  rownames(plantilla) <- paste0("n=", n_sizes)
  
  # --- MATRICES DE RESULTADOS ---
  # Error Tipo I
  E1_Siempre_T    <- plantilla
  E1_Siempre_W    <- plantilla
  E1_Condicional  <- plantilla
  
  # Poder
  PW_Siempre_T    <- plantilla
  PW_Siempre_W    <- plantilla
  PW_Condicional  <- plantilla
  
  # Test de rechazo de Shapiro-Wilk
  tasa_rechazo_SW <- plantilla
  
  # BUCLE PRINCIPAL
  for (i in 1:length(n_sizes)){ 
    for (j in 1:length(distributions)){ 
      
      n_act <- n_sizes[i]
      
      # Variables contadoras de error cuando:
      
      # H0 es cierta (Error Tipo I)
      h0_sig_T <- 0
      h0_sig_W <- 0
      h0_sig_Cond <- 0
      
      # H1 es cierta (Poder)
      h1_sig_T <- 0; h1_sig_W <- 0
      h1_sig_Cond <- 0
      
      # Shapiro rechaza la H0
      SW_rechazo_contador <- 0
      
      for (k in 1:total_sim) {
        
        # ---------------------------------------------------
        # ESCENARIO 1: ERROR TIPO I (H0 cierta)
        # ---------------------------------------------------
        dat <- choose_distr(j, n_act, is_H0 = TRUE)
        
        # 1. Calculamos p-values de los tests
        pval_t <- t.test(dat$x1, dat$x2)$p.value
        pval_w <- wilcox.test(dat$x1, dat$x2, exact=FALSE)$p.value
        
        # 2. Verificamos Normalidad (DIRECTO)
        p_shapiro1 <- shapiro.test(dat$x1)$p.value
        p_shapiro2 <- shapiro.test(dat$x2)$p.value
        es_normal  <- (p_shapiro1 > 0.05 && p_shapiro2 > 0.05)
        
        # Si se rechazo la normalidad, lo contamos en la variable contador de SW
        if (!es_normal){
          SW_rechazo_contador <- SW_rechazo_contador + 1
        }
          
          # 3. Sumamos éxitos/errores
          if(pval_t < 0.05) h0_sig_T <- h0_sig_T + 1         # Siempre T
          if(pval_w < 0.05) h0_sig_W <- h0_sig_W + 1         # Siempre W
          
          if(es_normal) {
            if(pval_t < 0.05) h0_sig_Cond <- h0_sig_Cond + 1 # Condicional (Usó T)
          } else {
            if(pval_w < 0.05) h0_sig_Cond <- h0_sig_Cond + 1 # Condicional (Usó W)
          }
          
          # ---------------------------------------------------
          # ESCENARIO 2: PODER
          
          dat <- choose_distr(j, n_act, is_H0 = FALSE)
          
          # 1. Calculamos p-values
          pval_t <- t.test(dat$x1, dat$x2)$p.value
          pval_w <- wilcox.test(dat$x1, dat$x2, exact=FALSE)$p.value
          
          # 2. Verificamos Normalidad (DIRECTO)
          p_shapiro1 <- shapiro.test(dat$x1)$p.value
          p_shapiro2 <- shapiro.test(dat$x2)$p.value
          es_normal  <- (p_shapiro1 > 0.05 && p_shapiro2 > 0.05)
          
          # 3. Sumamos éxitos
          if(pval_t < 0.05) h1_sig_T <- h1_sig_T + 1
          if(pval_w < 0.05) h1_sig_W <- h1_sig_W + 1
          
          if(es_normal) {
            if(pval_t < 0.05) h1_sig_Cond <- h1_sig_Cond + 1
          } else {
            if(pval_w < 0.05) h1_sig_Cond <- h1_sig_Cond + 1
          }
        }

        
        # --- RELLENAR MATRICES ---
        # Error Tipo I (Proporción de significativos en H0)
        E1_Siempre_T[i, j]   <- h0_sig_T / total_sim
        E1_Siempre_W[i, j]   <- h0_sig_W / total_sim
        E1_Condicional[i, j] <- h0_sig_Cond / total_sim
        
        # Poder (Proporción de significativos en H1)
        PW_Siempre_T[i, j]   <- h1_sig_T / total_sim
        PW_Siempre_W[i, j]   <- h1_sig_W / total_sim
        PW_Condicional[i, j] <- h1_sig_Cond / total_sim
        
        #Tasa de rechazo de SW
        tasa_rechazo_SW[i,j] <- SW_rechazo_contador / total_sim
      }
    }
    
    return(list(
      ShapiroWilk_rechazo = tasa_rechazo_SW,
      E1_Siempre_T = E1_Siempre_T, 
      E1_Siempre_W = E1_Siempre_W, 
      E1_Condicional = E1_Condicional,
      Power_Siempre_T = PW_Siempre_T, 
      Power_Siempre_W = PW_Siempre_W, 
      Power_Condicional = PW_Condicional
    ))
  }
  
  # ================= EJECUCIÓN =================
  
  resultados <- simulacion_comparativa(n_simulation, size, distr)
  
  print("==== TASA DE RECHAZO SHAPIRO-WILK ====")
  print(resultados$ShapiroWilk_rechazo)
  
  print("===== RESULTADOS ERROR TIPO I  =====")
  print("--- 1. Solo T-Test ---")
  print(resultados$E1_Siempre_T)
  print("--- 2. Solo Wilcoxon ---")
  print(resultados$E1_Siempre_W)
  print("--- 3. Condicional (Shapiro) ---")
  print(resultados$E1_Condicional)
  
  print(" ")
  print("===== RESULTADOS PODER  =====")
  print("--- 1. Solo T-Test ---")
  print(resultados$Power_Siempre_T)
  print("--- 2. Solo Wilcoxon ---")
  print(resultados$Power_Siempre_W)
  print("--- 3. Condicional (Shapiro) ---")
  print(resultados$Power_Condicional)

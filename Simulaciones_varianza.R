
set.seed(1)

tamaño <- 10
media <- 0
desvest <- 1
m_10_sd_1 <- c()
datos <- rnorm(tamaño, media, desvest)
resultado <- shapiro.test(datos)
m_10_sd_1 <- c(m_10_sd_1, resultado$p.value)

?rnorm

sdnormal_typeI <- function(n, simulations, desvest, nmean, desvest_diff) {
  # Función para calcular el error de tipo I (falsos positivos) de los tres test
  # con datos de distribuciones normales con desviaciones estándar dadas.
  # Inputs: n = tamaño muestral; simulations = número de simulaciones; desvest = desviación
  # estándar; nmean = media
  # Output: devuelve la proporcion de errores tipo I de cada procedimiento.
  
  # Creamos 3 varaibles que nos serviran para contar los falsos positivos en
  # cada procedimiento
  normality_testFP <- 0
  t_testFP <- 0
  wilcox_testFP <- 0
  
  # Bucle para realizar el número de simulaciones deseado
  for (i in 1:simulations) {
    
    # Generamos dos distribuciones normales con asimetria
    # Generamos dos distribuciones normales con asimetria
    x1 <- rnorm(n, mean = nmean, sd = desvest)
    x2 <- rnorm(n, mean = nmean, sd = (desvest+desvest_diff))
    
    # Realizamos el test de normalidad (shapiro) y almacenamos el p-valor
    pvalue <- shapiro.test(c(x1,x2))$p.value
    
    # Si el p-valor es alto (no se rechaza la hipotesis nula, es decir, no
    # podemos negar que la distribución sea normal) realizarmos el procedimiento
    # parametrico (test de la t)
    if (pvalue > 0.05) {
      pvalue1 <- t.test(x1, x2)$p.value
      # Contamos los falsos positivos
      if (pvalue1 < 0.05) {
        normality_testFP <- normality_testFP + 1
      }
      
      # Si el p-valor es bajo, realizamos el test no parametrico (wilcoxon)
    } else {
      pvalue2 <- wilcox.test(x1, x2)$p.value
      # Contamos los falsos positivos
      if (pvalue2 < 0.05) {
        normality_testFP <- normality_testFP +1
      }
    }
    
    
    # Realizamos el test de la t
    pvalue3 <- t.test(x1, x2)$p.value
    
    # Contamos los falsos positivos
    if (pvalue3 < 0.05) {
      t_testFP <- t_testFP + 1
    }
    
    
    # Realizamos el test de wilcoxon
    pvalue4 <- wilcox.test(x1, x2)$p.value
    
    # Contamos los falsos positivos
    if (pvalue4 < 0.05) {
      wilcox_testFP <- wilcox_testFP + 1 
    }
  }
  
  # Calculamos la propocion de falsos positivos para cada procedimiento respecto
  # al total de simulaciomes
  normality_testTypeI <- normality_testFP / simulations
  t_testTypeI <- t_testFP / simulations
  wilcox_testTypeI <- wilcox_testFP / simulations
  
  
  
  results <- paste0("Test de normalidad + t/wilcox: ", normality_testTypeI, "  |  ",
                    "Test de la t: ", t_testTypeI, "  |  ",
                    "Test de wilcoxon: ", wilcox_testTypeI)
  return(results)
}

# El test de normalidad no se ve afectado bajo ninguna condición (ni con varianzas grandes
# ni con heterocesdasticidad).
# El test t y el test de wilcoxon sólo se ven afectados por la heterocedasticidad


sdnormal_typeI(n = 3, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 3, simulations = 1000, desvest = 3, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 3, simulations = 1000, desvest = 5, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 3, simulations = 1000, desvest = 10, nmean = 0, desvest_diff = 0)


sdnormal_typeI(n = 5, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 5, simulations = 1000, desvest = 3, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 5, simulations = 1000, desvest = 5, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 5, simulations = 1000, desvest = 10, nmean = 0, desvest_diff = 0)


sdnormal_typeI(n = 10, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 10, simulations = 1000, desvest = 3, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 10, simulations = 1000, desvest = 5, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 10, simulations = 1000, desvest = 10, nmean = 0, desvest_diff = 0)


sdnormal_typeI(n = 50, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 50, simulations = 1000, desvest = 3, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 50, simulations = 1000, desvest = 5, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 50, simulations = 1000, desvest = 10, nmean = 0, desvest_diff = 0)


sdnormal_typeI(n = 500, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 500, simulations = 1000, desvest = 3, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 500, simulations = 1000, desvest = 5, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 500, simulations = 1000, desvest = 10, nmean = 0, desvest_diff = 0)
sdnormal_typeI(n = 500, simulations = 1000, desvest = 20, nmean = 0, desvest_diff = 0)


sdnormal_typeI(n = 5, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 1)
sdnormal_typeI(n = 5, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 2)
sdnormal_typeI(n = 5, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 3)
sdnormal_typeI(n = 5, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 4)


sdnormal_typeI(n = 50, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 1)
sdnormal_typeI(n = 50, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 2)
sdnormal_typeI(n = 50, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 3)
sdnormal_typeI(n = 50, simulations = 1000, desvest = 1, nmean = 0, desvest_diff = 4)




sdnormal_power <- function(n, simulations, effect = 3, desvest, desvest_diff) {
  # Función para calcular el poder estadístico de los tres test con distribucio-
  # -nes asimetricas.
  # Inputs: n = tamaño muestral; 
  #         simulations = número de simulaciones;
  #         effect = tamño del efecto;
  # Output: devuelve el poder estadístico de cada procedimiento.
  
  # Creamos 3 varaibles que nos serviran para contar los falsos negativos en
  # cada procedimiento
  normality_testFN <- 0
  t_testFN <- 0
  wilcox_testFN <- 0
  
  # Bucle para realizar el número de simulaciones deseado
  for (i in 1:simulations) {
    
    # Generamos dos distribuciones normales asimetricas con diferente media
    x1 <- rnorm(n, sd = desvest)
    x2 <- rnorm(n, mean = effect, sd = (desvest+desvest_diff))
    
    
    # Realizamos el test de normalidad (shapiro) y almacenamos el p-valor
    p1 <- shapiro.test(x1)$p.value
    p2 <- shapiro.test(x2)$p.value
    # Si el p-valor es alto (no se rechaza la hipotesis nula, es decir, no
    # podemos negar que la distribución sea normal) realizarmos el procedimiento
    # parametrico (test de la t)
    if (p1 > 0.05 && p2 > 0.05) {
      pvalue1 <- t.test(x1, x2)$p.value
      # Contamos el número de falsos negativos
      if (pvalue1 > 0.05) {
        normality_testFN <- normality_testFN + 1
      }
      
      # Si el p-valor es bajo, realizamos el test no parametrico (wilcoxon)
    } else {
      pvalue2 <- wilcox.test(x1, x2)$p.value
      # Contamos el número de falsos negativos
      if (pvalue2 > 0.05) {
        normality_testFN <- normality_testFN +1
      }
    }
    
    
    # Realizamos el test de la t
    pvalue3 <- t.test(x1, x2)$p.value
    
    # Contamos los falsos negativos
    if (pvalue3 > 0.05) {
      t_testFN <- t_testFN + 1
    }
    
    
    # Realizamos el test de wilcoxon
    pvalue4 <- wilcox.test(x1, x2)$p.value
    
    # Contamos los falsos negativos
    if (pvalue4 > 0.05) {
      wilcox_testFN <- wilcox_testFN + 1 
    }
  }
  
  # Calculamos la propocion de falsos negativos para cada procedimiento respecto
  # al total de simulaciomes
  normality_testTypeII <- normality_testFN / simulations
  t_testTypeII <- t_testFN / simulations
  wilcox_testTypeII <- wilcox_testFN / simulations
  
  # Calculamos el poder (1 - falsos negativos)
  normality_testPower <- 1 -  normality_testTypeII
  t_testPower <- 1 - t_testTypeII
  wilcox_testPower <- 1 - wilcox_testTypeII
  
  
  results <- paste0("Test de normalidad + t/wilcox: ", normality_testPower, "  |  ",
                    "Test de la t: ", t_testPower, "  |  ",
                    "Test de wilcoxon: ", wilcox_testPower)
  return(results)
}

# El poder del test de normalidad no se ve afectado por diferencias ni modificaciones
# la desviación estándar. En el test t y Wilcoxon si se ve un gran efecto.


sdnormal_power(n = 3, simulations = 1000, effect = 3, desvest = 1, desvest_diff = 0)
sdnormal_power(n = 3, simulations = 1000, effect = 3, desvest = 3, desvest_diff = 0)
sdnormal_power(n = 3, simulations = 1000, effect = 3, desvest = 5, desvest_diff = 0)
sdnormal_power(n = 3, simulations = 1000, effect = 3, desvest = 10, desvest_diff = 0)
sdnormal_power(n = 3, simulations = 1000, effect = 3, desvest = 20, desvest_diff = 0)


sdnormal_power(n = 300, simulations = 1000, effect = 3, desvest = 1, desvest_diff = 5)
sdnormal_power(n = 300, simulations = 1000, effect = 3, desvest = 3, desvest_diff = 10)
sdnormal_power(n = 300, simulations = 1000, effect = 3, desvest = 5, desvest_diff = 20)
sdnormal_power(n = 300, simulations = 1000, effect = 3, desvest = 10, desvest_diff = 30)
sdnormal_power(n = 300, simulations = 1000, effect = 3, desvest = 20, desvest_diff = 40)


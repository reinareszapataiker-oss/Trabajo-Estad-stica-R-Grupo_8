# DE MOMENTO SOLO HE INCLUIDO EL TEST DE SHAPIRO, NO HE PROBADO OTROS TEST DE 
# NORMALIDAD. Cabe destaca que Shapiro en R esta limitado a una n de 5000, en
# algunos sitios he leido que precisamente con n muy altas (inclso más que eso)
# es cuando el shapiro test rechaza la normalidad aun habiendola.

# Establecemos una semilla para asegurar reporducibilidad
set.seed(123)


# Definimos funciones para calcular el error de tipo I y el poder para una
# distribución normal con los tres procedimientos.
normal_typeI <- function(n, simulations) {
  # Función para calcular el error de tipo I (falsos positivos) de los tres test.
  # Inputs: n = tamaño muestral; simulations = número de simulaciones
  # Output: devuelve la proporcion de errores tipo I de cada procedimiento.
  
  # Creamos 3 varaibles que nos serviran para contar los falsos positivos en
  # cada procedimiento
  normality_testFP <- 0
  t_testFP <- 0
  wilcox_testFP <- 0
  
  # Bucle para realizar el número de simulaciones deseado
  for (i in 1:simulations) {
    
    # Generamos dos distribuciones normales (en este caso sin nada especial)
    x1 <- rnorm(n = n)
    x2 <- rnorm(n = n)
    
    
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
  
  
  
  results <- c("Test de normalidad + t/wilcox: " = normality_testTypeI,
                    "Test de la t: " = t_testTypeI,
                    "Test de wilcoxon: " = wilcox_testTypeI)
  return(results)
}
normal_power <- function(n, simulations, effect = 1) {
  # Función para calcular el poder estadístico de los tres test.
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
    
    # Generamos dos distribuciones normales con diferente media (en este caso 
    # sin nada especial)
    x1 <- rnorm(n = n, mean = 0)
    x2 <- rnorm(n = n, mean = effect)
    
    
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
  
  
  results <- c("Test de normalidad + t/wilcox: " = normality_testPower, 
                    "Test de la t: " = t_testPower, 
                    "Test de wilcoxon: " = wilcox_testPower)
  return(results)
}

normal_typeI(n = 3, simulations = 10000)
normal_typeI(n = 5, simulations = 10000)
normal_typeI(n = 10, simulations = 10000)
normal_typeI(n = 50, simulations = 10000)
normal_typeI(n = 100, simulations = 10000)

normal_power(n = 3, simulations = 10000)
normal_power(n = 5, simulations = 10000)
normal_power(n = 10, simulations = 10000)
normal_power(n = 50, simulations = 10000)
normal_power(n = 100, simulations = 10000)

# Con esto vemos que con n bajas, el poder del t test parace ligeramente mayor
# que el procedimiento con el test de normalidad (muy poco). Esto se debe a que
# en estas codiciones el test de normalidad no suele rechazar la hipotesis nula,
# por lo que al final se esta realizando casi siempre el test de la t. 

# Con respecto al test de wilcoxon, este tiene menor poder a bajas n que el test
# de la t. Es, de hecho, un procedimiento más conservador, tienes menos falsos 
# positivos pero su poder es menor. 

# Los errores tipo I de todos los procedimiento rondan el 0,05 como cabría
# esperar. 

KSnormal_typeI <- function(n, simulations) {
  # Función para calcular el error de tipo I (falsos positivos) de los tres test.
  # Inputs: n = tamaño muestral; simulations = número de simulaciones
  # Output: devuelve la proporcion de errores tipo I de cada procedimiento.
  
  # Creamos 3 varaibles que nos serviran para contar los falsos positivos en
  # cada procedimiento
  normality_testFP <- 0
  t_testFP <- 0
  wilcox_testFP <- 0
  
  # Bucle para realizar el número de simulaciones deseado
  for (i in 1:simulations) {
    
    # Generamos dos distribuciones normales (en este caso sin nada especial)
    x1 <- rnorm(n = n)
    x2 <- rnorm(n = n)
    
    
    # Realizamos el test de normalidad (shapiro) y almacenamos el p-valor
    x_all <- c(x1, x2)
    
    # Kolmogorov–Smirnov vs normal estimada
    pvalue <- ks.test(x_all, "pnorm",
                      mean(x_all), sd(x_all))$p.value
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
  
  
  
  results <- c("Test de normalidad + t/wilcox: " = normality_testTypeI,
               "Test de la t: " = t_testTypeI,
               "Test de wilcoxon: " = wilcox_testTypeI)
  return(results)
}
KSnormal_power <- function(n, simulations, effect = 1) {
  # Función para calcular el poder estadístico de los tres test.
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
    
    # Generamos dos distribuciones normales con diferente media (en este caso 
    # sin nada especial)
    x1 <- rnorm(n = n, mean = 0)
    x2 <- rnorm(n = n, mean = effect)
    
    
    # Realizamos el test de normalidad (shapiro) y almacenamos el p-valor
    p1 <- ks.test(x1, "pnorm", mean(x1), sd(x1))$p.value
    p2 <- ks.test(x2, "pnorm", mean(x2), sd(x2))$p.value
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
  
  
  results <- c("Test de normalidad + t/wilcox: " = normality_testPower, 
               "Test de la t: " = t_testPower, 
               "Test de wilcoxon: " = wilcox_testPower)
  return(results)
}

KSnormal_typeI(n = 10000, simulations = 5000)
KSnormal_power(n = 10000, simulations = 5000, effect = 0.035)


# Definimos funciones que realizan lo mismo que la anterior pero para 
# distribuciones con colas pesadas utilizando distribución de la t. 
HeavyTails_typeI <- function(n, simulations, df = 3) {
  
  normality_testFP <- 0
  t_testFP <- 0
  wilcox_testFP <- 0
  
  for (i in 1:simulations) {
    
    x1 <- rt(n, df = df)
    x2 <- rt(n, df = df)
    
    pvalue <- shapiro.test(c(x1,x2))$p.value
    
    if (pvalue > 0.05) {
      pvalue1 <- t.test(x1, x2)$p.value
      if (pvalue1 < 0.05) {
        normality_testFP <- normality_testFP + 1
      }
    } else {
      pvalue2 <- wilcox.test(x1, x2)$p.value
      if (pvalue2 < 0.05) {
        normality_testFP <- normality_testFP +1
      }
    }
    
    pvalue3 <- t.test(x1, x2)$p.value
    if (pvalue3 < 0.05) {
      t_testFP <- t_testFP + 1
    }
    
    pvalue4 <- wilcox.test(x1, x2)$p.value
    if (pvalue4 < 0.05) {
      wilcox_testFP <- wilcox_testFP + 1 
    }
  }
  
  normality_testTypeI <- normality_testFP / simulations
  t_testTypeI <- t_testFP / simulations
  wilcox_testType1 <- wilcox_testFP / simulations
  
  results <- c("Test de normalidad + t/wilcox: " = normality_testTypeI,
                    "Test de la t: " = t_testTypeI,
                    "Test de wilcoxon: " = wilcox_testType1)
  return(results)
}
HeavyTails_power <- function(n, simulations, effect = 1, df = 3) {
  # Función para calcular el poder estadístico de los tres test.
  # Inputs: n = tamaño muestral; 
  #         simulations = número de simulaciones;
  #         effect = tamño del efecto;
  #         df = grados de libertad de t-student (menor df = colas más pesadas)
  # Output: devuelve el poder estadístico de cada procedimiento.
  
  # Creamos 3 varaibles que nos serviran para contar los falsos negativos en
  # cada procedimiento
  normality_testFN <- 0
  totalshapirottest <- 0
  t_testFN <- 0
  wilcox_testFN <- 0
  
  # Bucle para realizar el número de simulaciones deseado
  for (i in 1:simulations) {
    
    # Generamos dos distribuciones normales con diferente media y colas pesadas
    x1 <- rt(n, df = df) + 0  
    x2 <- rt(n, df = df) + effect 
    
    
    # Realizamos el test de normalidad (shapiro) y almacenamos el p-valor
    p1 <- shapiro.test(x1)$p.value
    p2 <- shapiro.test(x2)$p.value
    # Si el p-valor es alto (no se rechaza la hipotesis nula, es decir, no
    # podemos negar que la distribución sea normal) realizarmos el procedimiento
    # parametrico (test de la t)
    if (p1 > 0.05 && p2 > 0.05) {
      pvalue1 <- t.test(x1, x2)$p.value
      totalshapirottest <- totalshapirottest +1
      # Contamos el número de falsos negativos
      if (pvalue1 > 0.05) {
        normality_testFN <- normality_testFN + 1
      }
      
      # Si el p-valor es bajo, realizamos el test no parametrico (wilcoxon)
    } else {
      pvalue2 <- wilcox.test(x1, x2)$p.value
      # Contamos el número de falsos negativos
      if (pvalue2 > 0.05) {
        normality_testFN <- normality_testFN + 1 
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

  
  
  results <- c("Test de normalidad + t/wilcox: " = normality_testPower,
                    "Test de la t: " = t_testPower,
                    "Test de wilcoxon: " = wilcox_testPower)
  return(results)
}

HeavyTails_typeI(n = 100, simulations = 10000)

# Con un n pequeño el test de normalidad no va ha rechazar la hipotesis nula
# aun cuando las colas sean pesadas y sería preferible hacer un wilcoxon

HeavyTails_typeI(n = 5, simulations = 10000, df = 5)
HeavyTails_typeI(n = 5, simulations = 10000, df = 3)
HeavyTails_typeI(n = 5, simulations = 10000, df = 2)
HeavyTails_typeI(n = 5, simulations = 10000, df = 1)
HeavyTails_typeI(n = 5, simulations = 10000, df = 0.5)

HeavyTails_power(n = 5, simulations = 10000, df = 2, effect = 10)
HeavyTails_power(n = 5, simulations = 10000, df = 2, effect = 10)


# Definimos funciones que realizan lo mismo que la anterior pero para 
# distribuciones con asimetría utilizando distribución de lognormal.
asimetricnormal_typeI <- function(n, simulations) {
  # Función para calcular el error de tipo I (falsos positivos) de los tres test
  # con distribuciones asimetricas.
  # Inputs: n = tamaño muestral; simulations = número de simulaciones
  # Output: devuelve la proporcion de errores tipo I de cada procedimiento.
  
  # Creamos 3 varaibles que nos serviran para contar los falsos positivos en
  # cada procedimiento
  normality_testFP <- 0
  t_testFP <- 0
  wilcox_testFP <- 0
  
  # Bucle para realizar el número de simulaciones deseado
  for (i in 1:simulations) {
    
    # Generamos dos distribuciones normales con asimetria
    x1 <- rlnorm(n)
    x2 <- rlnorm(n)
    
    
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
asimetricnormal_power <- function(n, simulations, effect = 3) {
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
    x1 <- rlnorm(n)
    x2 <- rlnorm(n, meanlog = effect)
    
    
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

# Con un distribucion muy asimetrica y n pequeña, un escenario típico de wilcox,
# el test de normalidad tiene menor poder que el test no parametrico ya que por
# el bajo tamaño muestral tiende a no rechazar normalidad y hacer un t-test, que 
# no funciona bien con estas distribuciones.

asimetricnormal_typeI(n = 100, simulations = 10000)
asimetricnormal_typeI(n = 10, simulations = 10000)
asimetricnormal_typeI(n = 5, simulations = 10000)
asimetricnormal_typeI(n = 3, simulations = 10000)




# Vamos a probar lo de que con n pequeña el test de normalidad nunca rechaza
# aunque no sea normal y con n grande siempre rechaza aunque sean muy pequeñas
# desviaciones de la normalidad. Esto es una manera alternativa de probar lo
# de las n grandes, ya que me estaba costando como lo hacia antes, sobreto
# porque con n 5000 no me salen tantas diferencias entre t test y wilcoxon

# Definimos una funcion para calcula la proporcion de veces que se rechaza la 
# hipotesis nula

normalitytest_proportions <- function(n, simulations, df = 2) {
  # Función que devuelve el porcentaje de veces que se rechaza la hipotésis
  # nula en un test de normalidad shapiro wilk para distribuciones normal,
  # colas pesadas y asimetrica
  # Inputs: 
  #         n = tamaño muestral
  #         simulations = número de simulaciones
  #         df = peso de las colas (cuantos mayor numero menos pesadas)
  # Outputs: porcentaje de veces que se rechaza la nula para cada distribución
  
  # Bucle para realizar el número deseado de simulaciones
  
  # Creamos variables para contar cuantas veces se rechaza la nula
  h0norm <- 0
  h0tails <- 0
  h0asim <- 0
  
  
  for (i in 1:simulations) {

    # Generamos dos distribuciones normales, colas pesadas y asimetricas
    normal <- rnorm(n)
    colas <- rt(n, df = df)
    asimetrica <- rlnorm(n)
    
    # Realizamos los test de normalidad
    pnorm <- shapiro.test(normal)$p.value
    ptails <- shapiro.test(colas)$p.value
    pasim <- shapiro.test(asimetrica)$p.value
    
    # Contamos las veces que se rechaza la nula
    if (pnorm < 0.05) {
      h0norm <- h0norm + 1 
    }
    if (ptails < 0.05) {
      h0tails <- h0tails + 1 
    }
    if (pasim < 0.05) {
      h0asim <- h0asim + 1 
    }
  }

  # Calculamos el porcentaje  
  percentnorm <- h0norm / simulations * 100
  percenttails <- h0tails / simulations * 100
  percentasim <- h0asim / simulations * 100
 
 return(c("Normal" = percentnorm, 
          "Colas pesadas" = percenttails,
          "Asimetrica" = percentasim))
}

normalitytest_proportions(3, 10000)
normalitytest_proportions(5, 10000)
normalitytest_proportions(50, 10000)
normalitytest_proportions(100, 10000)
normalitytest_proportions(500, 5000)
normalitytest_proportions(1000, 5000)
normalitytest_proportions(5000, 5000)

# Vemos que con n peuqeñas (del rango 3-5 aprox.) aun con notables desviaciones
# de la normalidad el test de shapiro no rechaza la hipotesis nula. Como cabria
# esperar el test de normalidad rechaza antes (menos n es necesaria) las 
# asimetrias que las colas pesadas


normalitytest_proportions(3, 10000, 30)
normalitytest_proportions(5, 10000, 30)
normalitytest_proportions(50, 10000, 30)
normalitytest_proportions(100, 10000, 30)
normalitytest_proportions(500, 5000, 30)
normalitytest_proportions(1000, 5000, 30)
normalitytest_proportions(5000, 5000, 30)


# Para df >20 las distribuciones de la t se aproximan tanto a una normal que
# es preferible hacer un t test antes que un wilcoxon (según bibliografía).
# Sin embargo, con una n grande el test de shapiro suele rechazar la normalidad 
# lo que haria que según el procedimiento perverso hiciesemos wilcoxon

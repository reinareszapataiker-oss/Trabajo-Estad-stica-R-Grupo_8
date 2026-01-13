# Cargado de librerias
library(ggplot2)

# Establecemos seed para asegurar reproductivilidad
set.seed(123)


# Generamos los datos que utilizaremos para graficar las distribuciones tipo de
# ejemplo
x_gamma <- rgamma(250, shape = 2, scale = 1)
x_t_pesada <- rt(250, df = 3)
x_t_df30 <- rt(250, df = 30)
x_lognormal <- rlnorm(200, meanlog = 0, sdlog = 1)
x_outliers <- c(rnorm(200 - 10, mean = 0, sd = 1), rnorm(50, mean = 0, sd = 10))
x_cauchy <- rcauchy(250, location = 0, scale = 1)
x1 <- rnorm(250, 0, 1)
x2 <- rnorm(250, 0, 4)

# Creamos las funciones para las gráficas de las distribuciones y para los
# resultados (representaremos unicamente las condiciones donde las diferencias
# son mayores, mostrando los casos extremos donde el procecidimiento falla )


grafico_densidad_vs_normal <- function(x, titulo = "Distribución empírica vs normal teórica") {
  x <- x[!is.na(x)]
  
  mu <- mean(x)
  sigma <- sd(x)
  
  x_min <- mu - 4 * sigma
  x_max <- mu + 4 * sigma
  
  xx <- seq(x_min, x_max, length.out = 800)
  normal_df <- data.frame(
    x = xx,
    y = dnorm(xx, mu, sigma)
  )
  
  p <- ggplot() +
    # Densidad empírica
    geom_density(
      data = data.frame(x = x),
      aes(x = x, y = after_stat(density), fill = "Empírica"),
      alpha = 0.45,
      linewidth = 1.1,
      color = "#3A7F7E"
    ) +
    # Área normal
    geom_area(
      data = normal_df,
      aes(x = x, y = y, fill = "Normal teórica"),
      alpha = 0.35
    ) +
    # Línea normal
    geom_line(
      data = normal_df,
      aes(x = x, y = y),
      color = "grey30",
      linetype = "dashed",
      linewidth = 1.2
    ) +
    scale_fill_manual(
      name = NULL,
      values = c(
        "Empírica" = "#5DA5A4",
        "Normal teórica" = "grey70"
      )
    ) +
    coord_cartesian(xlim = c(x_min, x_max)) +
    labs(
      title = titulo,
      x = "x",
      y = "Densidad"
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      legend.background = element_blank(),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  return(p)
}
grafico_dos_densidades_vs_normal <- function(x1, x2, titulo = "Dos distribuciones vs normal teórica", label_x1 = "Grupo 1", label_x2 = "Grupo 2") {
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  
  x_all <- c(x1, x2)
  
  mu <- mean(x_all)
  sigma <- sd(x_all)
  
  x_min <- mu - 4 * sigma
  x_max <- mu + 4 * sigma
  
  xx <- seq(x_min, x_max, length.out = 800)
  normal_df <- data.frame(
    x = xx,
    y = dnorm(xx, mu, sigma)
  )
  
  df <- data.frame(
    x = c(x1, x2),
    grupo = factor(
      c(rep(label_x1, length(x1)), rep(label_x2, length(x2)))
    )
  )
  
  ggplot(df, aes(x = x, fill = grupo)) +
    geom_density(
      aes(y = after_stat(density)),
      alpha = 0.45,
      linewidth = 1
    ) +
    geom_area(
      data = normal_df,
      aes(x = x, y = y),
      inherit.aes = FALSE,
      fill = "grey70",
      alpha = 0.35
    ) +
    geom_line(
      data = normal_df,
      aes(x = x, y = y),
      inherit.aes = FALSE,
      color = "grey30",
      linetype = "dashed",
      linewidth = 1.2
    ) +
    coord_cartesian(xlim = c(x_min, x_max)) +
    scale_fill_manual(
      values = c("#5DA5A4", "#B07AA1")
    ) +
    labs(
      title = titulo,
      x = "x",
      y = "Densidad",
      fill = NULL
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c("right", "top"),
      legend.background = element_blank(),
      axis.ticks.length = unit(0.25, "cm"),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}
guardar_figura <- function(plot, nombre_archivo, width = 18, height = 16, units = "cm", dpi = 300) { 
  ggsave( filename = nombre_archivo, plot = plot, width = width, height = height, units = units, dpi = dpi ) 
  }

grafico_barras <- function(valores, etiquetas = c("A", "B", "C"), titulo = "") {
  # Asegurarse de que siempre haya 3 valores
  if(length(valores) != 3) stop("El vector 'valores' debe tener exactamente 3 elementos.")
  
  df <- data.frame(
    grupo = factor(etiquetas, levels = etiquetas),
    y = valores
  )
  
  ggplot(df, aes(x = grupo, y = y)) +
    geom_col(fill = "#5DA5A4", color = "#3A7F7E", linewidth = 1.1) +
    labs(title = titulo, x = NULL, y = "y") +
    theme_classic(base_size = 14) +
    theme(
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "none"
    )
}
guardar_figura_barras <- function(plot, nombre_archivo, width = 10, height = 7, units = "cm", dpi = 300) {
  ggsave(
    filename = nombre_archivo,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
}




# Gamma
p_gamma <- grafico_densidad_vs_normal(
  x_gamma,
  titulo = "Distribución Gamma"
)

guardar_figura(p_gamma, "gamma.png")


# t de Student con colas muy pesadas (df = 3)
p_t_pesada <- grafico_densidad_vs_normal(
  x_t_pesada,
  titulo = "t de Student (df = 3)"
)

guardar_figura(p_t_pesada, "t_df3.png")


# t de Student con df = 30
p_t_df30 <- grafico_densidad_vs_normal(
  x_t_df30,
  titulo = "t de Student (df = 30)"
)

guardar_figura(p_t_df30, "t_df30.png")


# Lognormal
p_lognormal <- grafico_densidad_vs_normal(
  x_lognormal,
  titulo = "Distribución Lognormal"
)

guardar_figura(p_lognormal, "lognormal.png")


# Normal con outliers
p_outliers <- grafico_densidad_vs_normal(
  x_outliers,
  titulo = "Normal con valores atípicos"
)

guardar_figura(p_outliers, "outliers.png")


# Cauchy
p_cauchy <- grafico_densidad_vs_normal(
  x_cauchy,
  titulo = "Distribución de Cauchy"
)

guardar_figura(p_cauchy, "cauchy.png")


# Heterocedasticidad

p_heterocedasticidad <- grafico_dos_densidades_vs_normal(
  x1,
  x2,
  titulo = "Heterocedasticidad: dos varianzas",
  label_x1 = expression(sigma == 1),
  label_x2 = expression(sigma == 4)
)

guardar_figura(p_heterocedasticidad, "heterocedasticidad.png")

# Por ultimo, creamos los vectores con los resultados de las simulaciones. 
# Para demostrar las flaquezas del procedimiento perverso representamos solo
# los resultados en un grafico de barra en las peores situaciones que hayamos 
# encontrado, para así reflejar cuanto efecto en el poder y el error podria 
# llegar a tener el procedimiento perverso.

errorlognormal <- c(0.0332, 0.0207, 0.0292)
poderlognonmal <- c(0.7111, 0.4243, 0.9621)

errorpesadas <- c(0.0271, 0.0057, 0.0293)
poderpesadas <- c(0.5100, 0.3386, 0.5250)

erroroutliers <- c(0.0415, 0.0150, 0.0401)
poderoutliers <- c(0.9775, 0.5950, 0.9968)

errorhetero <- c(0.0757, 0.0485, 0.0782)
poderhetero <- c(0.7760, 0.7842, 0.7400)

errodesv <- c(0.0457, 0.0454, 0.0448)
poderdesv <- c(0.6665, 0.6810, 0.6568)


# =========================
# LOGNORMAL
# =========================

p_error_lognormal <- grafico_barras(
  errorlognormal,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Error tipo I – Lognormal"
)

guardar_figura_barras(p_error_lognormal, "error_lognormal.png")


p_poder_lognormal <- grafico_barras(
  poderlognonmal,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Poder estadístico – Lognormal"
)

guardar_figura_barras(p_poder_lognormal, "poder_lognormal.png")


# =========================
# COLAS PESADAS
# =========================

p_error_pesadas <- grafico_barras(
  errorpesadas,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Error tipo I – Colas pesadas"
)

guardar_figura_barras(p_error_pesadas, "error_pesadas.png")


p_poder_pesadas <- grafico_barras(
  poderpesadas,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Poder estadístico – Colas pesadas"
)

guardar_figura_barras(p_poder_pesadas, "poder_pesadas.png")


# =========================
# OUTLIERS
# =========================

p_error_outliers <- grafico_barras(
  erroroutliers,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Error tipo I – Valores atípicos"
)

guardar_figura_barras(p_error_outliers, "error_outliers.png")


p_poder_outliers <- grafico_barras(
  poderoutliers,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Poder estadístico – Valores atípicos"
)

guardar_figura_barras(p_poder_outliers, "poder_outliers.png")


# =========================
# HETEROCEDASTICIDAD
# =========================

p_error_hetero <- grafico_barras(
  errorhetero,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Error tipo I – Heterocedasticidad"
)

guardar_figura_barras(p_error_hetero, "error_heterocedasticidad.png")


p_poder_hetero <- grafico_barras(
  poderhetero,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Poder estadístico – Heterocedasticidad"
)

guardar_figura_barras(p_poder_hetero, "poder_heterocedasticidad.png")


# =========================
# DESVIACIÓN DE LA NORMAL
# =========================

p_error_desv <- grafico_barras(
  errodesv,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Error tipo I – Desviación leve de la normalidad"
)

guardar_figura_barras(p_error_desv, "error_desviacion.png")


p_poder_desv <- grafico_barras(
  poderdesv,
  etiquetas = c("Normalidad", "t-test", "Wilcoxon"),
  titulo = "Poder estadístico – Desviación leve de la normalidad"
)

guardar_figura_barras(p_poder_desv, "poder_desviacion.png")





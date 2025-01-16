# ===========================
# Tarea / Ejercicio Aplicado
# ===========================

# Limpieza de memoria
rm(list = ls())
options(digits=4)

# Ruta global del archivo de datos
input <- "G:/Académico/FormacionContinua/Ciencia de datos Cs Sociales/BigData/Cenagro2012_short"

library(dplyr)
library(ggplot2)
library(haven)
library(survey)

data_1 <- read_dta(file.path(input, "Cenagro2012_short.dta"))

# Generar variables relevantes
data_1 <- data_1 |>
  mutate(
    mujer = ifelse(sexo == 0, 1, 0),
    rango_edad = cut(edad, 
                     breaks = c(12, 24, 64, Inf), 
                     labels = c("A", "B", "C"), 
                     right = TRUE),
  )
# Variables categóricas
# Grupo A entre 12 hasta 24 
# Grupo B entre 25 hasta 64
# Grupo C de 65 a más

# Establecer semilla para reproducibilidad
set.seed(12345)

# Aplicando un MAS

# Extraer muestras
mas_1 <- data_1[sample(1:nrow(data_1), size = 11234),]

# Intervalo de confianza para variable "sup_total"
confianza <- 0.95
n <- nrow(mas_1)
media <- mean(mas_1$sup_total)
desviacion <- sd(mas_1$sup_total)
t_critico <- qt(1 - (1 - confianza) / 2, df = n - 1)
margen_error <- t_critico * (desviacion / sqrt(n))
limite_inferior <- media - margen_error
limite_superior <- media + margen_error
cat("Intervalo de confianza del 95% para la media de 'sup_total': [", limite_inferior, ",", limite_superior, "]\n")

# Factor de expansión de cada observación, en MAS
N <- nrow(data_1)  # Número total de observaciones
n <- nrow(mas_1)  # Número de observaciones en mas_1
factor_expansion <- N / n
cat("El factor de expansión es:", factor_expansion, "\n")
mas_1$factor_expansion <- factor_expansion
head(mas_1)

# . ¿A qué corresponde la sumatoria de esta variable? ¿Por qué?
# Cada observación en la muestra está representando a un número de observaciones de la población (es decir, cada observación en la muestra "expande" su representación de la población total por un factor). Al sumar todos los factores de expansión de la muestra, estaríamos sumando las "unidades" u "observaciones" representadas por esas unidades de la muestra, lo que nos da el total de la población.

# Mejorando eficiencia con MAE

data_1 %>%
  count(rango_edad)
data_1_clean <- data_1[!is.na(data_1$rango_edad), ]

mae_1 <- data_1_clean %>% slice_sample(n = 11234, by = rango_edad)

mae_2 <- data_1_clean %>% group_by(rango_edad) %>% slice_sample(n = 11234)

# Define un diseño muestral estratificado (rango_edad)
dme_1 <- svydesign(id = ~1, data = mae_1, strata = ~rango_edad)

# Promedio de variable mujer
promedio_mujer <- svymean(~mujer, dme_1)
promedio_mujer






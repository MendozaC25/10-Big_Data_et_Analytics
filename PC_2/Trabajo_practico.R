# ===========================
# Parte II / Censo Nacional de Población Penitenciaria 2016
# ===========================

## INSTRUCCIONES GENERALES
# Limpieza de memoria
rm(list = ls())
options(digits=4)

# Ruta delarchivo de datos
# input <- "G:/Académico/FormacionContinua/Ciencia de datos Cs Sociales/BigData/TP_01"

library(dplyr)
library(ggplot2)
library(haven)
library(survey)

data_1 <- read_sav("G:/Académico/FormacionContinua/Ciencia de datos Cs Sociales/BigData/TP_01/01_PENALES_CARATULA.sav")
data_2 <- read_sav("G:/Académico/FormacionContinua/Ciencia de datos Cs Sociales/BigData/TP_01/01_PENALES_CAP100.sav")
##

n_registros <- nrow(data_1)

# Semilla 12345
set.seed(12345)

# Variable dicotomica "pp_dcsp" 
# 1 == “Delitos contra la seguridad pública”,
# 0 == Otros
# data_1_0 es la base de datos que contiene la variable "pp_dcsp"
data_1_0 <- data_1 |>
  mutate(
    pp_dcsp = ifelse(DELITO_GENERICO == "DELITOS CONTRA LA SEGURIDAD PUBLICA", 1, 0)
  )

# Extraer muestra estratificada y por conglomerados que contenga entre el 3% y 5% de la población
# Justificación de elección de las variables para los estratos y conglomerados.

# 1 variable a usar para estratos edades, en atención a la población penitenciaria
# data_1_e es una variable que se 
data_1_0$data_1_e <- cut(data_1_0$EDAD, breaks = c(0, 30, 45, 60, Inf), 
                          labels = c("18-30", "31-45", "46-60", "60+"), 
                          right = FALSE)

# Conglomerados

base_clusters <- data_1_0 %>%
  count(OF_REGIONAL)

# Muestra aleatoria de conglomerados
mac_1_clusters <- base_clusters %>%
  slice_sample(prop = 0.03)

mac_1_final <- merge(x = data_1_0 , y = mac_1_clusters , by = c("OF_REGIONAL"))

# Declarar el diseño muestral completo

# Calcular el porcentaje de personas que cometió “Delitos contra la seguridad pública”

# Calcular el total de personas que cometió “Delitos contra la seguridad pública”

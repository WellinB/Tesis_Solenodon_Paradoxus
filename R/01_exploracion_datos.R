library(sf)
library(tidyverse)
library(janitor)
library(skimr)
# 1. Dimensiones de la base
dim(segmentos)

# 2. Nombres de las variables
names(segmentos)

# 3. Estructura de las variables
glimpse(segmentos)

# 4. Número de detecciones y no detecciones
table(segmentos$deteccion, useNA = "ifany")

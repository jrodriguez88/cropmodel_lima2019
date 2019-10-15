# Script Practice 1
# Source of Weather data: NASA Prediction Of Worldwide Energy Resources https://power.larc.nasa.gov/
# Source of soil data: https://www.soilgrids.org/  
# Author: Rodriguez-Espinoza J.
# Repository: https://github.com/jrodriguez88/cropmodel_lima2019
# 2019

### Objetivo: 
### Generar ambientes de cultivo en modelos de cultivo mediante la creación de archivos de clima y suelo.


### 1 Cargar Paquetes

library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)
library(naniar)
library(jsonlite)
library(sirad)
library(soiltexture)


source("get_data_nasapower.R")
source("get_data_soilgrids.R")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_soil_aquacrop.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_weather_aquacrop.R", encoding = "UTF-8")

### 2 Definir directorio de trabajo, y zona de estudio
directorio <- getwd()  
#https://power.larc.nasa.gov/docs/v1/
variables_clima <- c("PRECTOT", 
                     "ALLSKY_SFC_SW_DWN", 
                     "RH2M",
                     "T2M_MAX",
                     "T2M_MIN",
                     "WS2M")
#https://www.isric.org/explore/soilgrids/faq-soilgrids
variables_suelo <- c("BLDFIE","CLYPPT","SNDPPT","CRFVOL","ORCDRC","WWP","AWCh1","AWCtS")
profundidades <- c("sl1", "sl2", "sl3", "sl4", "sl5")  # 60cm
  
#Periodo (Año-mes-dia yyyymmdd)
fecha_inicial <- 19880101
fecha_final <- 20181231

# Motupe
localidad <- "Motupe"
latitud <- -6.1139
longitud <- -79.6692

####################

### Descargar datos de clima y suelo
datos_clima_crudos <- get_data_nasapower(variables_clima, fecha_inicial, fecha_final, latitud, longitud)

datos_suelo_crudos <- get_data_soilgrids(variables_suelo, latitud, longitud, profundidades)


### Explorar y Organizar data
skim(datos_clima_crudos)
skim(datos_suelo_crudos)


### Cambiar identificador NA
datos_clima_crudos <- datos_clima_crudos %>% replace_with_na_all(condition = ~.x == -99)

### Cambiemos esos  nombres
names(datos_clima_crudos)

names(datos_clima_crudos) <- c("date", "srad", "rain", "rhum", "tmax", "tmin", "wvel")
skim(datos_clima_crudos)

### Algunas funciones de manipulacion de datos
#select
#slice
#filter
#mutate
#summarise

### Ejemplo usando el paquete sirad para calcular radiacion 
datos_clima <- datos_clima_crudos %>%
  mutate(extraT = extrat(lubridate::yday(date), radians(latitud))$ExtraTerrestrialSolarRadiationDaily,
         srad = if_else(is.na(srad), 0.175*sqrt(tmax - tmin)*extraT, srad)) %>%
  dplyr::select(-extraT) %>% basic_qc_nasa
  




datos_suelo <- from_soilgrids_to_aquacrop(localidad, datos_suelo_crudos)


#pmap(datos_suelo, make_soil_aquacrop)








#problem set 2
#Diana Cano Sánchez-202110997
# R version 4.3.1 (2023-06-16 ucrt)

#Se cargan las librerias necesarias para procesar las bases de datos
library(pacman)
library(rio)
library(data.table)
library(tidyverse)

#Se define la ruta donde se encuentran las bases de datos
setwd("C:/Users/PERSONAL/OneDrive - Universidad de los andes/Universidad de Los Andes/Sexto Semestre/Taller R/Problem sets/#2")

#2.1 Importar

#Con la función read.csv() se importan los datos teniendo en cuenta que tienen encabezados(títulos) y están separados por ;
#Se incluye la codificación UTF-8 para que si hay palabras con tilde se manejen adecuadamente

identification <- read.csv("input/Modulo de identificacion.csv", header = TRUE, sep = ";", encoding = "UTF-8")
location <- read.csv("input/Modulo de sitio o ubicacion.csv", header = TRUE, sep = ";", encoding = "UTF-8")

#2.2 Exportar

# Se usa la función de saveRDS que recibe el nombre del objeto a exportar -creados en el punto 2.1-,
# el nombre de la carpeta donde se guardará el objeto exportar y con su respectivo nombre, con la extensión.rds

saveRDS(identification, file = file.path("output","identification.rds"))
saveRDS(location, file = file.path("output", "location.rds"))

#3  Generar variables

#3.1

# Se crea y agrega (sobrescribir) a la base de datos "identification" la variable "bussines_type", 
# según la columna GRUPOS4 (buscado en el DANE), que tiene la rama de actividad agrupada en los 4 grupos de interés
# mutate es para crear una nueva columna en el dataframe

# case_when es una función que es segunda opción que reemplaza hacer uno por uno los condicionales (if-else), 
# se pueden hacer múltiples condiciones con sus específicos valores

identification <- identification %>%
  mutate(bussiness_type = case_when(
    GRUPOS4 == 1 ~ "Agricultura", 
    GRUPOS4 == 2 ~ "Industria manufacturera",
    GRUPOS4 == 3 ~ "comercio",
    GRUPOS4 == 4 ~ "servicios"
  ))

#3.2
# Se crea y agrega (sobrescribir) a la base de datos "location" la variable "local", 
# según las condiciones dadas
# mutate es para crear una nueva columna en el dataframe

# case_when es una función que es segunda opción que reemplaza hacer uno por uno los condicionales (if-else), 
# se pueden hacer múltiples condiciones con sus específicos valores

location <- location %>%
  mutate(local = case_when(
    P3053 == 6 ~ 1,
    P3053 == 7 ~ 1
  ))

#4 Eliminar filas/columnas de un conjunto de datos


#4.1 
# Creación del objeto "identification_sub" de acuerdo con las condiciones dadas
# La función filter te filtra las filas según una condición

identification_sub <- identification %>%
  filter(bussiness_type == "Industria manufacturera")

#4.2
# Creación del objeto "location_sub" de acuerdo con la selección de variables de interés (columnas),
# del objeto "location"

#select es una función que selecciona las columnas a interés con todas sus filas

location_sub <- location %>%
  select(DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, P3054, P469, COD_DEPTO, F_EXP)

#4.3
# Sobrescribir en el objeto "identification_sub" según la selección de variables de interés (columnas),
# del objeto "identification_sub".

#select es una función que selecciona las columnas a interés con todas sus filas

identification_sub <- identification_sub %>%
  select(DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, P35, P241, P3032_1, P3032_2, P3032_3, P3033, P3034)


#5 Combinar bases de datos

# se hicieron cuatro maneras de hacerlo

# full_join 
# Genera nueva base de datos (dataframe), la cual une 
# todas las filas de ambas bases de datos originales. Cuando no hay una coincidencia 
# en ambas bases, los valores faltantes tienen NA


base_datos_final <- full_join(location_sub, identification_sub, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))

# inner_join:
# Crea nueva base de datos (dataframe), la cual une 
# sólo las filas que tienen coincidencias en ambas bases de datos originales

base_datos_final_1 <- inner_join(location_sub, identification_sub, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))

# left_join:
# Genera nueva base de datos (dataframe), la cual une 
# todas las filas del primer dataframe con los valores que coinciden del segundo dataframe

base_datos_final_2 <- left_join(location_sub, identification_sub, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))

# right_join
# Genera nueva base de datos (dataframe) que resulta de unir 
# todas las filas del segundo dataframe con los valores en común del primer dataframe.
base_datos_final_3 <- right_join(location_sub, identification_sub, by = c("DIRECTORIO","SECUENCIA_P","SECUENCIA_ENCUESTA"))

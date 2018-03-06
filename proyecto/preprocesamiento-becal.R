# Cargar libraries
library(stringi)
library(dplyr)
library(stringr)
source('utils.R')

# Leer datasets
becal17 = read.csv('becal2017.csv', header = T, stringsAsFactors = F, fileEncoding = "UTF-8")
becal_cobertura = read.csv('becal-cobertura.csv', header = T, stringsAsFactors = F, fileEncoding = "UTF-8")

##########  PARTE 1 - DATASET: becal2017.csv  ###################
# A
# Colocar aquí el código que realice la siguiente tarea:
# Renombrar las columnas al siguiente formato: nombres en minúscula sin espacios vacíos y 
# conteniendo solo caracteres a-z sin artículos (no acentos, no ñs, no paréntesis, no /, etc.)
# Sugerencia: Utilizar funciones utilitarias de utilis.R
##

names(becal17) = sapply(names(becal17), limpiar_nombres)
names(becal17) = sapply(names(becal17), normalizar_texto)

##
# B
# Colocar aquí el código que realice la siguiente tarea:
# Renombrar las columnas llamdas 'n' de la siguiente manera: columna 1 = 'nroreg', 
# columna 3 = 'nroconv', columna 20 = 'nrorankuni', columna 22 = 'nrorankcur'
##

names(becal17)[1]<- 'nroreg'
names(becal17)[3]<- 'nroconv'
names(becal17)[20]<- 'nrorankuni'
names(becal17)[22]<- 'nrorankcur'

##
# C
# Colocar aquí el código que realice la siguiente tarea:
# Eliminar los espacios al final de la cadena que indica la condición del becario y eliminar los registros 
# cuya condición sea 'No becario' o 'Pendiente'
##

becal17[, "condicion"] <- str_trim(becal17[ ,'condicion'])
becal17 = filter(becal17, condicion== "Becario" | condicion== "Becario retornado")


##
# D
# Colocar aquí el código que realice la siguiente tarea:
# Convertir la fecha de adjudicación al formato dd/mm/yyyy
##


becal17 = filter(becal17, fechadeadjudicacion!= "pendiente")
becal17$fechadeadjudicacion <- gsub("l","", becal17$fechadeadjudicacion)
becal17[, "fechadeadjudicacion"] <- gsub("de", "/", becal17[, "fechadeadjudicacion"])
becal17[, "fechadeadjudicacion"] <- gsub(" ", "", becal17[, "fechadeadjudicacion"])
becal17[, "fechadeadjudicacion"] <- gsub("febrero", "2", becal17[, "fechadeadjudicacion"])
becal17[, "fechadeadjudicacion"] <- gsub("junio", "6", becal17[, "fechadeadjudicacion"])
becal17[, "fechadeadjudicacion"] <- gsub("agosto", "8", becal17[, "fechadeadjudicacion"])
becal17[, "fechadeadjudicacion"] <- gsub("diciembre", "12", becal17[, "fechadeadjudicacion"])


##
# E
# Colocar aquí el código que realice la siguiente tarea:
# Convertir la columna cédula de identidad al tipo numérico removiendo primeramente el 
# separador de miles (coma o punto)
##

becal17$ci <-gsub(",", "", becal17$ci) 
becal17$ci <-as.numeric(becal17$ci) 
class(becal17$ci)

##
# F
# Colocar aquí el código que realice la siguiente tarea:
# Eliminar registros sin datos en la cédula de identidad
##

becal17<- becal17[complete.cases(becal17$ci), ]

##
# G
# Colocar aquí el código que realice la siguiente tarea:
# Convertir los registros cuyo contenido de la columna **maestriadoctorado** 
# sea de más de una línea (p.ej., 554) a contenido de una sola línea
# Sugerencia: Eliminar el caracter '\n'
##

becal17$maestriadoctorado <- gsub("\n","", becal17$maestriadoctorado)
becal17$maestriadoctorado <- gsub("2016-2017:","", becal17$maestriadoctorado) 
becal17$maestriadoctorado <- gsub("2016/2017","", becal17$maestriadoctorado) 
becal17$maestriadoctorado <- gsub(":","", becal17$maestriadoctorado) 
becal17$maestriadoctorado <- gsub("2016-2017","", becal17$maestriadoctorado) 
becal17$maestriadoctorado <- gsub("2017-2018","", becal17$maestriadoctorado) 
becal17$maestriadoctorado <- gsub("2017/2018","", becal17$maestriadoctorado) 
becal17$maestriadoctorado <- gsub("2016 - 2017","", becal17$maestriadoctorado) 
becal17$maestriadoctorado <- gsub("-","", becal17$maestriadoctorado) 


##
# H
# Colocar aquí el código que realice la siguiente tarea:
# Agregar la columna **categoriauni** para contener la categoría de la 
# universidad de acuerdo a su ranking. Las categorías a considerar son: top_10, 
# top_50, top_100, top_150, top_200, mas_200
##


table(becal17$nrorankuni)
becal17$nrorankuni = as.numeric(becal17$nrorankuni)
class(becal17$nrorankuni)

becal17$categoriauni <- NA
becal17$categoriauni[is.na(becal17$nrorankuni)]<-"sin datos"
becal17$categoriauni[becal17$nrorankuni>=201]<-"mas_200"
becal17$categoriauni[becal17$nrorankuni<=200]<-"top_200"
becal17$categoriauni[becal17$nrorankuni<=151]<-"top_150"
becal17$categoriauni[becal17$nrorankuni<=100]<-"top_100"
becal17$categoriauni[becal17$nrorankuni<=50]<-"top_50"
becal17$categoriauni[becal17$nrorankuni<=10]<-"top_10"

as.factor(becal17$categoriauni)


##
# I
# Colocar aquí el código que realice la siguiente tarea:
# Agregar la columna **tipoestudio** que puede tomar uno de tres valores, maestria_profesional, 
# maestria_academica, o doctorado, dependiendo del tipo de beca
##

table(becal17clean$tipodebecasegunprogramaaprobado)

becal17$tipoestudio <- NA
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Maestría para Profesionales en CyT"]<-"maestria_profesional"
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Maestría para Profesionales en Educación"]<-"maestria_profesional"
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Maestría para Profesionales en otras áreas relevantes "]<-"maestria_profesional"
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Maestría"]<-"maestria_profesional"
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Programa 1"]<-"maestria_profesional"
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Programa 2 "]<-"maestria_profesional"
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Programa 3"]<-"maestria_profesional"
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Maestría para Investigadores en CyT"]<-"maestria_academica"
becal17$tipoestudio[becal17$tipodebecasegunprogramaaprobado=="Doctorado para Investigadores en CyT"]<-"doctorado"

write.csv(becal17)



##########  PARTE 2 - DATASET: becal-cobertura.csv  ###################

##
# A
# Colocar aquí el código que realice la siguiente tarea:
# Renombrar las columnas Total General y C.I. siguiendo las reglas mencionadas anteriormente
##

head(becal_cobertura)
names(becal_cobertura) = sapply(names(becal_cobertura), limpiar_nombres)
names(becal_cobertura) = sapply(names(becal_cobertura), normalizar_texto)


##
# B
# Colocar aquí el código que realice la siguiente tarea:
# Convertir la columna **ci** al tipo numérico removiendo primeramente el separador 
# de miles (coma o punto)
##

becal_cobertura$ci = gsub('\\.', '', becal_cobertura$ci)
becal_cobertura$ci <- as.numeric(as.character(becal_cobertura$ci))
class(becal_cobertura$ci)

##
# C
# Colocar aquí el código que realice la siguiente tarea:
# Eliminar registros sin datos en la cédula de identidad
##

becal_cobertura <- becal_cobertura[-which(is.na(becal_cobertura$ci)), ]

##
# D
# Colocar aquí el código que realice la siguiente tarea:
# Convertir la columna **Total General** al tipo entero después de convertir los montos en euros a dolares 
# (utilizar el cambio 1USD->0.86E), eliminar los signos de dolar y euro también los puntos y espacios vacios, 
# redondear los montos decimales incrementando el parte entera si el primer digito después de la coma es mayor a 5
#

becal_cobertura$totalgeneral = sapply(becal_cobertura$totalgeneral, convertir_totalgeneral)

##########  PARTE 3 ###################

# Colocar aquí el código que realice la siguiente tarea:
# Agregar al dataset _becal2017.csv_ la columna **totalgralusd** del dataset _becal-cobertura.csv_ utilizando la 
# columna **ci** (disponible en ambos datasets) como referencia

becal_cobertura = select(becal_cobertura, "ci", "totalgeneral")

ambos_becal = merge(becal17, becal_cobertura, by.x="ci", by.y="ci", all.y=TRUE)

ambos_becal$nroreg <- NULL
ambos_becal$nroconv <- NULL
ambos_becal <- ambos_becal [complete.cases (ambos_becal),]
becal_completo <-ambos_becal

##########  PARTE 4 ###################

# Escribir nuevo dataset
write.csv(becal_completo, 'becal_completo.csv', row.names = F)

# Seleccionar google_api.R en su sistema de archivos
#install.packages('xlsx')
#install.packages('curl')
#install.packages('stringr')
#install.packages('xlsx')
#install.packages('stringi')
require('stringi')
library('xlsx')
library('jsonlite')
library('stringr')
library('curl')
source('google_api.R')

data <- read.xlsx("C:/Users/Yanelly/Documents/asignacion_2/hogares.xlsx", sheetIndex = 1, startRow = 1, endRow = 104, header = T, encoding = 'UTF-8')

destino <- c("Piazzale Aldo Moro") 
api_key <- "AIzaSyCf1UmIBm0KKHcA0gmtRixYjX00-bAC77g"
data$Duracion <- 0

for(i in 1:nrow(data)){

  origen = c(data$Dirección[i]," ", data$Distrito[i])
  origen <- strsplit(as.character(origen), "\n")
  api_url <- get_url(origen, destino, api_key)
  
  datos <- get_data(api_url)
  
  json <- parse_data(datos) 
  json = as.data.frame(json)
  
  d = as.character(json$duration$text)
  duracion = na.omit(as.numeric(unlist(strsplit(unlist(d), "[^0-9]+"))))
  
  if(length(duracion>1))
    data$Duracion[i] <- duracion[1] * 60 + duracion[2]
  else
    data$Duracion[i] <- duracion[1]

}

#Tipo de inmueble
data$Tipo.de.Inmueble <- as.character(data$Tipo.de.Inmueble) 
data$Tipo.de.Inmueble[grepl("Ap", data$Tipo.de.Inmueble)] <- 1 
data$Tipo.de.Inmueble[grepl("Mini", data$Tipo.de.Inmueble)] <- 0
data$Tipo.de.Inmueble[grepl("Monolocale", data$Tipo.de.Inmueble)] <- 2
data$Tipo.de.Inmueble <- as.numeric(data$Tipo.de.Inmueble) 

#precios y habitaciones disponibles
data$calefaccion <- ifelse(grepl("riscaldamento",data$Precio.Mensual),1,0)
data$condominio <- ifelse(grepl("condominio",data$Precio.Mensual),1,0)
data$servicios.No.Inc <- ifelse(grepl("escluse",data$Precio.Mensual),1,0)
data$Todo.Incluido <- ifelse(grepl("(TUTTO)|(tutto)",data$Precio.Mensual),1,0)

data$Habitaciones.Disp <- as.character(data$Habitaciones.Disp)
data$Precio.Mensual <- as.character(data$Precio.Mensual)
data$Habitaciones.Disponibles <- as.character(data$Habitaciones.Disponibles)

size <- nrow(data)
for(i in 1:size){
  precios <- data$Precio.Mensual[i]
  precios <-  na.omit(as.numeric(unlist(strsplit(unlist(precios), "[^0-9]+"))))
  if(length(precios) == 1)
    data$Habitaciones.Disp[i] <- habitacion(data$Habitaciones.Disponibles[i])
}

for(i in 1:size){
  precios <- data$Precio.Mensual[i]
  precios <-  na.omit(as.numeric(unlist(strsplit(unlist(precios), "[^0-9]+"))))
  data$Precio.Mensual[i] = as.character(precios[1])
  row <- data[i,]
  j <- 2
  f = as.character(precios[3])
  if(length(precios)>2 && ((as.character(precios[3])) == 55 ||  (as.character(precios[3])) == 50))
    x <- 2  
  else
    x <- length(precios)
  while(x > 1){
    if(j == 2){
      data$Habitaciones.Disp[i] <- habitacion(data$Habitaciones.Disponibles[i])  
    }
    row$Precio.Mensual <- as.character(precios[j])
    row$Habitaciones.Disp <- habitacion2(row$Habitaciones.Disponibles)
    data <- rbind(data, row)
    j <- j + 1
    x <- x - 1
  }
}



##funciones para habitaciones
habitacion <- function(x){
  for(i in 1:stri_length(x)){
    if (substr(x, i ,i+3) == "sing" || substr(x,i,i+3) == "Sing" )
      return(1)
    else 
      if (substr(x,i ,i+3) == "Dopp" || substr(x,i ,i+3) == "dopp" )
        return(2)
      else
        if (substr(x,i ,i+3) == "appa" || substr(x,i ,i+3) == "Appa" )
          return(3)
        else
          if (substr(x,i ,i+3) == "post" || substr(x,i ,i+3) == "Post" )
            return(4)
  }
  return(5)
}

habitacion2 <- function(x){
  for(i in stri_length(x):1){
    if (substr(x, i-3 ,i) == "sing" || substr(x,i-3,i) == "Sing" )
      return(1)
    else 
      if (substr(x,i-3 ,i) == "Dopp" || substr(x,i-3 ,i) == "dopp" )
        return(2)
      else
        if (substr(x,i ,i+3) == "post" || substr(x,i ,i+3) == "Post" )
          return(4)
  }
  return(5)
}

#notas

data$Sexo[grepl("(ragazzi)", data$Notas)] <-  0
data$Sexo[grepl("(ragazze)", data$Notas)] <-  1
data$Sexo[grepl("(ragazze/i)|(ragazzi/e)|(ragazzi/ragazze)|(ragazze/ragazzi)", data$Notas)] <- 2
data$Sexo[39] <- 0

#descripcion
data$Entrada <- ifelse(grepl("(ingresso)|(Ingresso)",data$Descripción),1,0)
data$Cocina <- ifelse(grepl("cucina",data$Descripción),1,0)
data$Salon <- ifelse(grepl("sal",data$Descripción),1,0)
data$Balcon <- ifelse(grepl("balcone",data$Descripción),1,0)
data$Internet <- ifelse(grepl("internet",data$Descripción),1,0)

data$Habitacion <- 0 
data$Habitacion[grepl("(camer)", data$Descripción)] <- 1
data$Habitacion[grepl("(2.?camer)", data$Descripción)] <- 2
data$Habitacion[grepl("(3.?camer)", data$Descripción)] <- 3
data$Habitacion[grepl("(4.?camer)", data$Descripción)] <- 4

data$Baño <- 0 
data$Baño[grepl("(bagno)", data$Descripción)] <- 1
data$Baño[grepl("(2.?bagni)", data$Descripción)] <- 2
data$Baño[grepl("(3.?bagni)", data$Descripción)] <- 3
data$Baño[grepl("(4.?bagni)", data$Descripción)] <- 4

data$Descripción <- NULL
data$Habitaciones.Disponibles <- NULL
data$Notas <- NULL
data$Foto <- NULL
data$Piso <- NULL
data$Habitaciones.Disp <- as.numeric(data$Habitaciones.Disp) 
data$Precio.Mensual <- as.numeric(data$Precio.Mensual) 











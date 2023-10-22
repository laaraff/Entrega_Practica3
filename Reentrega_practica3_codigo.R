#Entrega ejercicio Practica 3 - 10 de septiembre
#Lara Raffetti Ballerini
setwd("~/Lara/Entrega_practica3")
rm(list=ls())

#Cargo los datos de cada estacion.
Azul<-read.table("AZUL.txt")
Catamarca<-read.table("CATAMARCA.txt")
Aeroparque<-read.table("AEROPARQUE.txt")
Chilecito<-read.table("CHILECITO.txt")
Mendoza<-read.table("MENDOZA.txt")
Iguazu<-read.table("IGUAZU.txt")
Informacion<-read.table("estaciones.txt")

#Me di cuenta de que en el archivo estacion.txt faltan datos de Mendoza. Google la info y los agrego manualmente.
infomendoza <- c("MENDOZA", -32.8, -68.8, 746) #nombre de la estacion, latitud, longitud, altura
Informacion <- rbind(Informacion, infomendoza)

# ITEM 1 ------------------------------------------------------------------
#Antes de armar la lista de listas para organizar las estaciones, creo una funcion que reemplace 9999.9 por NA y la corro para cada estacion (para los data.frames que me devuelve el read table)
datos_faltantes <- function(estacion) {
  estacion$V1[which(estacion$V1 == "9999.9")] <- NA
  estacion$V2[which(estacion$V2 == "9999.9")] <- NA
  estacion$V3[which(estacion$V3 == "9999.9")] <- NA
  estacion$V4[which(estacion$V4 == "9999.9")] <- NA
  estacion$V5[which(estacion$V5 == "9999.9")] <- NA
  return(estacion)
}

Azul <- datos_faltantes(Azul)
Catamarca <- datos_faltantes(Catamarca)
Aeroparque <- datos_faltantes(Aeroparque)
Chilecito <- datos_faltantes(Chilecito)
Mendoza <- datos_faltantes(Mendoza)
Iguazu <- datos_faltantes(Iguazu)

#Y antes de armar la lista de listas, defino una funcion para pasar de grados fahrenheit a celsius y la corro para cada estacion
celsius <- function(fahrenheit) {
  return((fahrenheit-32)*(5/9))
}
for (i in 1:nrow(Azul)) {
  Azul$V3[i] <- celsius(Azul$V3[i])
  Azul$V4[i] <- celsius(Azul$V4[i])
}
for (i in 1:nrow(Aeroparque)) {
  Aeroparque$V3[i] <- celsius(Aeroparque$V3[i])
  Aeroparque$V4[i] <- celsius(Aeroparque$V4[i])
}
for (i in 1:nrow(Catamarca)) {
  Catamarca$V3[i] <- celsius(Catamarca$V3[i])
  Catamarca$V4[i] <- celsius(Catamarca$V4[i])
}
for (i in 1:nrow(Chilecito)) {
  Chilecito$V3[i] <- celsius(Chilecito$V3[i])
  Chilecito$V4[i] <- celsius(Chilecito$V4[i])
}
for (i in 1:nrow(Iguazu)) {
  Iguazu$V3[i] <- celsius(Iguazu$V3[i])
  Iguazu$V4[i] <- celsius(Iguazu$V4[i])
}

for (i in 1:nrow(Mendoza)) {
  Mendoza$V3[i] <- celsius(Mendoza$V3[i])
  Mendoza$V4[i] <- celsius(Mendoza$V4[i])
}

#Ahora armo una lista por estacion con los datos correspondientes:
Azul <- list("Nombre" = "AZUL", "Latitud" = Informacion$V2[1], "Longitud" = Informacion$V3[1], "Altura"=Informacion$V4[1], "Codigo"= Azul$V1, "Fecha"= Azul$V2, "Temp" = Azul$V3, "Tderocio" = Azul$V4, "Presion"= Azul$V5) 

#Hago la misma idea para el resto de las estaciones
Aeroparque <- list("Nombre" = "AEROPARQUE", "Latitud" = Informacion$V2[2], "Longitud" = Informacion$V3[2], "Altura"=Informacion$V4[2], "Codigo"= Aeroparque$V1, "Fecha"= Aeroparque$V2, "Temp" = Aeroparque$V3, "Tderocio" = Aeroparque$V4, "Presion"= Aeroparque$V5)
Catamarca <- list("Nombre" = "CATAMARCA", "Latitud" = Informacion$V2[3], "Longitud" = Informacion$V3[3], "Altura"=Informacion$V4[3],"Codigo"= Catamarca$V1, "Fecha"= Catamarca$V2, "Temp" = Catamarca$V3, "Tderocio" = Catamarca$V4, "Presion"= Catamarca$V5)
Chilecito <- list("Nombre" = "CHILECITO", "Latitud" = Informacion$V2[4], "Longitud" = Informacion$V3[4], "Altura"=Informacion$V4[4], "Codigo"= Chilecito$V1, "Fecha"= Chilecito$V2, "Temp" = Chilecito$V3, "Tderocio" = Chilecito$V4, "Presion"= Chilecito$V5)
Iguazu <- list("Nombre" = "IGUAZU", "Latitud" = Informacion$V2[5], "Longitud" = Informacion$V3[5], "Altura"=Informacion$V4[5],"Codigo"= Iguazu$V1, "Fecha"= Iguazu$V2, "Temp" = Iguazu$V3, "Tderocio" = Iguazu$V4, "Presion"= Iguazu$V5)
Mendoza <- list("Nombre" = "MENDOZA", "Latitud" = Informacion$V2[6], "Longitud" = Informacion$V3[6], "Altura"=Informacion$V4[6], "Codigo"= Mendoza$V1, "Fecha"= Mendoza$V2, "Temp" = Mendoza$V3, "Tderocio" = Mendoza$V4, "Presion"= Mendoza$V5)

#Finalmente armo la lista de listas con todas las estaciones
estaciones <- list("Azul"= Azul, "Aeroparque" = Aeroparque, "Catamarca" = Catamarca, "Chilecito" = Chilecito, "Iguazu" = Iguazu, "Mendoza" = Mendoza)


# ITEM 2 I) ---------------------------------------------------------------
resumen <- function(lista) {
  resumen_total <- array(list(), dim= c(6,16)) #creo un array vacio para llenar con la info a calcular de cada estacion
  colnames(resumen_total) <- c("Estacion", "Cantidad de datos", "Media temperatura", "Desvio temperatura", "Valor max temperatura", "valor min temperatura", "Media T de rocio", "Desvio T de rocio", "Valor max T de rocio","Valor min T de rocio", "Media presion" , "Desvio presion", "Valor max presion", "Valor min presion", "Fecha inicial", "Fecha final")
  for (i in 1:length(estaciones)) { #cantidad de filas, hay una fila por estacion
  nombre <- lista[[i]][1]
  cant <- length(lista[[i]]$Fecha) #cuento los datos en la columna fechas porque son datos diarios
  mediat <- mean(lista[[i]]$Temp,na.rm=T)  #media de la temperatura
  d <- sd(lista[[i]]$Temp, na.rm=T)
  valor_max <- max(lista[[i]]$Temp, na.rm=T)
  valor_min <- min(lista[[i]]$Temp, na.rm=T)
  mediatro <- mean(lista[[i]]$Tderocio,na.rm=T)  #media de la temperatura de rocio
  dro <- sd(lista[[i]]$Tderocio, na.rm=T)
  valor_max_ro <- max(lista[[i]]$Tderocio, na.rm=T)
  valor_min_ro <- min(lista[[i]]$Tderocio, na.rm=T)
  mediap <-  mean(lista[[i]]$Presion,na.rm=T)
  dpre <- sd(lista[[i]]$Presion, na.rm=T)
  valor_max_pre <- max(lista[[i]]$Presion, na.rm=T)
  valor_min_pre <- min(lista[[i]]$Presion, na.rm=T)
  fecha_inicial <-lista[[i]]$Fecha[1] 
  a<- length(lista[[i]]$Fecha)
  fecha_final <- lista[[i]]$Fecha[a]
  resumen_total[i,] <- c(nombre, cant, mediat, d, valor_max, valor_min, mediatro, dro, valor_max_ro, valor_min_ro, mediap, dpre, valor_max_pre, valor_min_pre, fecha_inicial, fecha_final)

  }
  return(resumen_total)
}

resumen_estaciones <- resumen(estaciones) #Funciona pero con warnings porque Chilecito tiene todos los datos de presion faltantes.

# ITEM 2 II) --------------------------------------------------------------

cercania <- function(l, latmin, latmax, lonmin, lonmax) {
  lista <- list()
  j<- 1
  for(i in 1:length(l)) { #que recorra cada elemento de la lista (osea cada estacion) y se fije si cumple los requisitos (se deben cumplir todos a la vez)
    if (as.numeric(l[[i]]$Latitud) > latmin & as.numeric(l[[i]]$Latitud)<latmax & as.numeric(l[[i]]$Longitud)>lonmin & as.numeric(l[[i]]$Longitud)<lonmax) { #sin el as.numeric leia los datos de l como caracter.
    lista[[j]] <- l[[i]]$Nombre #que guarde en la lista vacia el nombre de la estacion que cumple eso
    j <- j+1
    }
  }
  if (length(lista) == 0 ) {  #si no cumple los requisitos, la lista no se llena, no hay estaciones cercanas.
    warning("No hay estaciones cercanas")
  }
  return(lista)
}

cercania(estaciones,-30,0,-70,0) #devuelve Catamarca, Chilecito, Iguazu
cercania(estaciones,30,0,70,0) #devuelve una lista vacia y el warning de que no hay estaciones cercanas.


# ITEM 2 III) -------------------------------------------------------------
#Guardar la lista generada en un archivo con formato Rdata.

guardar <- function(lista) {
  archivo <- save(lista, file= "Datos_Estaciones.RData")
  return(lista)
}

guardar(estaciones)

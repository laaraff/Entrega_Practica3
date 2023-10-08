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

estaciones <-array(list(),dim=c(6,9)) #Creo un array compuesto por listas vacias, de dimension 6 (estaciones) por 9 (variables)
colnames(estaciones)<-c("Nombre", "Latitud", "Longitud", "Altura", "Codigo", "Fechas","Temperatura", "Tderocio", "Presion") #Nombro las columnas con mis variables de interes.

#Creo una funcion que reemplace 9999.9 por NA y la corro para cada estacion
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

#Relleno las primeras 4 columnas del array con la info que tenia de las estaciones

for(i in 1:4){
  estaciones[,i]=Informacion[,i]
}

#Guardo en un vector los codigos de cada estacion y los asigno a la columna 5 de mi array
codigos<-c(Azul[[1]][1],Aeroparque[[1]][1],Catamarca[[1]][1],Chilecito[[1]][1],Iguazu[[1]][1], Mendoza[[1]][1])
estaciones[,5]<-codigos

#Lo mismo con las fechas, temperaturas y presion (que son todas listas)
fechas <- list(Azul[[2]], Aeroparque[[2]], Catamarca[[2]], Chilecito[[2]], Iguazu[[2]], Mendoza[[2]])
estaciones[,6] <- fechas
temp <- list(Azul[[3]], Aeroparque[[3]], Catamarca[[3]], Chilecito[[3]], Iguazu[[3]], Mendoza[[3]])
estaciones[,7] <- temp
trocio <-  list(Azul[[4]], Aeroparque[[4]], Catamarca[[4]], Chilecito[[4]], Iguazu[[4]], Mendoza[[4]])
estaciones[,8] <- trocio
presion <-  list(Azul[[5]], Aeroparque[[5]], Catamarca[[5]], Chilecito[[5]], Iguazu[[5]], Mendoza[[5]])
estaciones[,9] <- presion


#Defino una funcion para pasar de grados fahrenheit a celsius
celsius <- function(fahrenheit) {
  return((fahrenheit-32)*(5/9))
}

#Recorro el array para cambiarle los valores a las columnas de temperatura y temperatura de rocio a grados celsius
for (i in 1:6) {
  estaciones[[i,7]] <-celsius(estaciones[[i,7]])
  estaciones[[i,8]] <- celsius(estaciones[[i,8]])
}



# ITEM 2 I) ---------------------------------------------------------------
resumen <- function(lista) {
  resumen_total <- array(list(), dim= c(6,16)) #creo un array vacio para llenar con la info a calcular de cada estacion
  colnames(resumen_total) <- c("Estacion", "Cantidad de datos", "Media temperatura", "Desvio temperatura", "Valor max temperatura", "valor min temperatura", "Media T de rocio", "Desvio T de rocio", "Valor max T de rocio","Valor min T de rocio", "Media presion" , "Desvio presion", "Valor max presion", "Valor min presion", "Fecha inicial", "Fecha final")
  for (i in 1:6) { #cantidad de filas, hay una fila por estacion
  nombre <- lista[[i,1]]
  cant <- length(estaciones[[i,"Fechas"]]) #cuento los datos en la columna fechas porque son datos diarios
  mediat <- mean(lista[[i,"Temperatura"]],na.rm=T)  #media de la temperatura
  d <- sd(lista[[i, "Temperatura"]], na.rm=T)
  valor_max <- max(lista[[i, "Temperatura"]], na.rm=T)
  valor_min <- min(lista[[i, "Temperatura"]], na.rm=T)
  mediatro <- mean(lista[[i,"Tderocio"]],na.rm=T)  #media de la temperatura de rocio
  dro <- sd(lista[[i, "Tderocio"]], na.rm=T)
  valor_max_ro <- max(lista[[i, "Tderocio"]], na.rm=T)
  valor_min_ro <- min(lista[[i, "Tderocio"]], na.rm=T)
  mediap <-  mean(lista[[i,"Presion"]],na.rm=T)
  dpre <- sd(lista[[i, "Presion"]], na.rm=T)
  valor_max_pre <- max(lista[[i, "Presion"]], na.rm=T)
  valor_min_pre <- min(lista[[i, "Presion"]], na.rm=T)
  a= lista[[i, "Fechas"[1]]] 
  fecha_inicial <- a[1]
  fecha_final <- a[length(a)]
  resumen_total[i,] <- c(nombre, cant, mediat, d, valor_max, valor_min, mediatro, dro, valor_max_ro, valor_min_ro, mediap, dpre, valor_max_pre, valor_min_pre, fecha_inicial, fecha_final)

  }
  return(resumen_total)
}

resumen_estaciones <- resumen(estaciones) #Funciona pero con warnings porque Chilecito tiene todos los datos de presion faltantes.

# ITEM 2 II) --------------------------------------------------------------

cercania <- function(l, latmin, latmax, lonmin, lonmax) {
  lista <- list()
  j<- 1
  for(i in 1:nrow(l)) { #que recorra cada fila (osea cada estacion) y se fije si cumple los requisitos (se deben cumplir todos a la vez)
    if (as.numeric(l[[i,2]]) > latmin & as.numeric(l[[i,2]])<latmax & as.numeric(l[[i,3]])>lonmin & as.numeric(l[[i,3]])<lonmax) { #sin el as.numeric leia los datos de l como caracter.
    lista[[j]] <- l[[i,1]] #que guarde en la lista vacia el nombre de la estacion que cumple eso
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

guardar <- function(array) {
  archivo <- save(array, file= "Datos_Estaciones.RData")
  return(array)
}

guardar(estaciones)

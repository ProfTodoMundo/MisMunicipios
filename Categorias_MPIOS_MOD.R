#---- SE LLAMAN LAS LIBRERIAS A UTILIZAR ----
library(readr)
library(readxl)
library(dplyr)
library(openxlsx)
#---- SE DEFINE EL DIRECTORIO DE TRABAJO ----
setwd("~/Documents/MacroReportes")
#---- SE LEEN LOS ARCHIVOS A UNIFICAR ----
#dbToWork <- read_excel("dbToWork.xlsx"); View(dbToWork)
#MpiosLoc <- read_excel("Mpios_Localidades.xlsx"); View(MpiosLoc)
#---- SE UNEN LAS BASE DE DATOS ----
#merged_data <- dbToWork %>%
#  left_join(MpiosLoc, by = c("Llave", "Entidad", "Municipio", "Cve_municipio"))
#View(merged_data)
#---- SE GUARDA LA NUEVA BDD ----
#write.xlsx(merged_data, "dbToWork_actualizado.xlsx")
#---- DEFINICION DE LA VARIABLE MISDATOS PARA INICIAR ----
#setwd("~/Documents/MacroReportes")
misdatos <- read_excel("Repositorios/dbToWork_actualizado.xlsx"); View(misdatos); 
misdatos$Tasa_Neta <- misdatos$Tasa_Neta*100
misdatos$Cobertura_Total <- misdatos$Cobertura_Total*100
misdatos$Atencion_Demanda_Potencial <- misdatos$Atencion_Demanda_Potencial*100
#---- ELIMINANDO CARACTERES ESPECIALES ----
##--- ENTIDADES ----
MisMaterias <- misdatos$Entidad; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Entidad <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Entidad <- factor(misdatos$Entidad, levels = niveles, labels = ListaMaterias)
#---- PROCESANDO NOMBRE MUNICIPIO ----
MisMaterias <- misdatos$Municipio; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Municipio <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Municipio <- factor(misdatos$Municipio, levels = niveles, labels = ListaMaterias)
#---- RESPALDO ----
write.csv(misdatos,"Repositorioscorregido/bbToworkUpdated.csv")
View(misdatos)
#---- INPUTACION DE VALORES NUMERICOS A LAS CATEGORIAS DE JENKS ----
##---- RUPTURAS DE JENKS PARA TRES CLASES ----
MisMaterias <- misdatos$Rupturas_Naturales_Jenks; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Rupturas_Naturales_Jenks <- TodasMaterias; 
LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Rupturas_Naturales_Jenks <- factor(misdatos$Rupturas_Naturales_Jenks, levels = niveles, labels = ListaMaterias)
summary(misdatos$Rupturas_Naturales_Jenks)
#---- PROCESANDO NOMBRE GRADO MARGINACION MUNICIPIO ----
MisMaterias <- misdatos$Grado_Marginacion_20_Municipio; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("á","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE))
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Grado_Marginacion_20_Municipio <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Grado_Marginacion_20_Municipio <- factor(misdatos$Grado_Marginacion_20_Municipio, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Marginacion_20_Municipio)
#---- PROCESANDO NOMBRE GRADO MARGINACION ENTIDAD ----
MisMaterias <- misdatos$Grado_Marginacion_20_Entidad; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("á","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE))
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Grado_Marginacion_20_Entidad <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Grado_Marginacion_20_Entidad <- factor(misdatos$Grado_Marginacion_20_Entidad, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Marginacion_20_Entidad)
#---- PROCESANDO NOMBRE GRADO ACCESABILIDAD CTROS URBANOS ----
MisMaterias <- misdatos$Grado_Accesibilidad_Centros_Urbanos_20; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("á","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE))
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Grado_Accesibilidad_Centros_Urbanos_20 <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Grado_Accesibilidad_Centros_Urbanos_20 <- factor(misdatos$Grado_Accesibilidad_Centros_Urbanos_20, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Accesibilidad_Centros_Urbanos_20); summary(misdatos$Grado_Accesibilidad_Centros_Urbanos_20)
#---- PROCESANDO NOMBRE GRADO EQUIPAMIENTO ----
MisMaterias <- misdatos$Grado_Equipamiento_20; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("á","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE))
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Grado_Equipamiento_20 <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Grado_Equipamiento_20 <- factor(misdatos$Grado_Equipamiento_20, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Equipamiento_20);summary(misdatos$Grado_Equipamiento_20)
#---- PROCESANDO NOMBRE GRADO CALIDAD ENTORNO ----
MisMaterias <- misdatos$Grado_Calidad_Entorno_20; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("á","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE))
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Grado_Calidad_Entorno_20 <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Grado_Calidad_Entorno_20 <- factor(misdatos$Grado_Calidad_Entorno_20, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Calidad_Entorno_20)
#---- PROCESANDO NOMBRE GRADO INTENSIDAD MIGRATORIA ----
MisMaterias <- misdatos$Grado_Intensidad_Migratoria; 
n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
MateriasCorrected <- c(); MateriasTemporal <- TodasMaterias
for(i in 1:n){
  MateriasCorrected <- toupper(TodasMaterias[i])
  MateriasCorrected <- gsub("á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("á","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE))
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
misdatos$Grado_Intensidad_Migratoria <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias #head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){
    materia <- Materia2
  }else{
    ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1
  }
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
}
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Grado_Intensidad_Migratoria <- factor(misdatos$Grado_Intensidad_Migratoria, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Intensidad_Migratoria)
#---- RESPALDO ----
write.csv(misdatos,"Repositorioscorregido/bbToworkUpdated.csv")
View(misdatos)
#---- CREACION DE UNA NUEVA VARIABLE QUE INDICA LAS CATEGORIAS A QUE PERTENCE CONSIDERANDO LOS TRES METODOS ----
#misdatos$resultadosCategorias <- paste(misdatos$RNJ_3clases,misdatos$RNJ_4clases,misdatos$RNJ_5clases)
miBDD <- misdatos
View(miBDD)
colnames(miBDD)
#---- RESPALDO ----
write.csv(miBDD,"Repositorioscorregido/bb_indicadores_aumentada.csv")
#---- RESUMIENDO LOS CAMBIOS REALIZADOS EN LA BDD misdatos ----
summary(miBDD)
View(miBDD)
New_miBDD <- miBDD[,1:53];
View(New_miBDD)
colnames(New_miBDD)
nombres_col <- c("Entidad",
                 "Municipio",
                 "Secundarias_sin_TM",
                 "Tasa_Neta",
                 "Cobertura_Total",
                 "Atencion_Demanda_Potencial",
                 "Poblacion_Total",
                 "Poblacion_CONAPO",
                 "Egresados_Secundaria_22-23",
                 "Nuevo_Ingreso_1EMS_23-24",
                 "Lugares_Necesarios_Tasa_Neta",
                 "Localidades_Rurales_Municipio",
                 "Localidades_Urbanas_Municipio",
                 "Ttl_Matricula_Escolarizada_23-24",
                 "Matricula_Ttl_23-24",
                 "Matricula_Escolarizada_22-23",
                 "Egresados_EMS_22-23",
                 "Egresos_secundaria_22-23",
                 "Grado_Marginacion_20_Entidad",
                 "Grado_Marginacion_20_Municipio",
                 "Grado_Accesibilidad_Centros_Urbanos_20",
                 "Poblacion_Tipica_con 3secundaria_no_continuo_20",
                 "Grado_Equipamiento_20",
                 "Grado_Calidad_Entorno_20",
                 "Grado_Intensidad_Migratoria",
                 "Rupturas_Naturales_Jenks",
                 "DEGETAyCM",
                 "DGETI",
                 "AUTÓNOMAS",
                 "BACHILLERATO ESTATAL",
                 "BACHILLERATO GENERAL",
                 "BACHILLERATO INTERCULTURAL BILNGÜE",
                 "PARTICULAR",
                 "DGBETD",
                 "CBDC",
                 "CETI",
                 "COL BACHILLERES",
                 "COL BACHILLERES ESTATALES",
                 "CECYTE",
                 "CONALEP",
                 "EMSAD",
                 "ESC NAL CIEGOS",
                 "IEMS",
                 "INBAL",
                 "IPN",
                 "IEBAS",
                 "OTRAS SECRETARIAS",
                 "OTROS PROGRAMAS ESTATALES",
                 "PREECO",
                 "PREFECO",
                 "SECTEI",
                 "TELEBACHILLERATO",
                 "TBC")
colnames(New_miBDD) <- nombres_col
View(New_miBDD)
write.csv(New_miBDD, "Repositorios/bdd_corregida.csv")
miBDD <- New_miBDD
#---- ORDENANDO POR CRITERIO DE TASA NETA DE MENOR A A MAYOR ---
miBDD <- read_csv("Repositorios/bdd_corregida.csv")
mpios_criterios <- miBDD %>% arrange(Tasa_Neta)
View(mpios_criterios)
write.csv(mpios_criterios, "Repositorioscorregido/mpios_criterios.csv")
#---- CATEGORIA 1: MAYORES A 99,999 ----
mpios_categoria1 <- mpios_criterios %>%
  filter(Poblacion_Total >= 99999) %>% arrange(Tasa_Neta)
View(mpios_categoria1)
write.csv(mpios_categoria1,"Repositorioscorregido/Municipios_Categoria1.csv")
#---- CATEGORIA 2: MAYORES A 14,999 Y MENORES A 99,999 ----
mpios_categoria2 <- mpios_criterios %>%
  filter(Poblacion_Total >= 14999 & Poblacion_Total< 99999) %>%
  arrange(Tasa_Neta)
View(mpios_categoria2)
write.csv(mpios_categoria2,"Repositorioscorregido/Municipios_Categoria2.csv")
#---- CATEGORIA 3: MENORES A 14,999 ----
mpios_categoria3 <- mpios_criterios %>%
  filter(Poblacion_Total < 14999 ) %>%
  arrange(Tasa_Neta)
View(mpios_categoria3)
write.csv(mpios_categoria3,"Repositorioscorregido/Municipios_Categoria3.csv")
#---- AHORA GENERAREMOS LO MISMO PARA CADA UNO DE LOS ESTADOS ----
nombres_estados <- unique(miBDD$Entidad)
subbases <- list()
for (estado in nombres_estados) {
  subbases[[estado]] <- subset(miBDD, Entidad == estado)
}
for (estado in nombres_estados) {
  write.csv(subbases[[estado]], paste0("Repositorioscorregido/Estados/Subsistemas_municipales_", estado, ".csv"), row.names = FALSE)
}
cat("Se generaron las subbases de datos por cada estado.")
#---- AHORA GENERAREMOS LAS GRAFICAS PARA LOS MUNICIPIOS ----
colnames(mpios_categoria1)
indices <- c(1,2,27:53)
mpios_cat1_subsistemas <- mpios_categoria1[,indices]
write.csv(mpios_cat1_subsistemas,"Repositorioscorregido/Municipios_Categoria1_Subsistemas.csv")
View(mpios_cat1_subsistemas)
#---- PLANTILLA PARA GRAFICAR SUBSISTEMAS PARA CATEGORIAS ----
##---- SE DEFINE LA PALETA DE COLORES ----
codigo_colores <- c( "#9b2247", "#a57f2c", "#7e664a",
                    "#611232", "#e6d194", "#c39326", "#806b4a")
calcular_luminosidad <- function(color) {
  rgb <- col2rgb(color)
  return(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
luminosidades <- calcular_luminosidad(codigo_colores)
colores_ordenados <- codigo_colores[order(luminosidades)]
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)
paleta_extendida <- mi_paleta_ordenada(20)
##---- SE CARGAN LOS DATOS DE LA CATEGORIA 1 ----
# Seleccionar las columnas relevantes
library(readr)
library(readxl)
library(dplyr)
library(openxlsx)
setwd("~/Documents/MacroReportes")
mpios_categoria1 <- read_csv("Repositorioscorregido/Municipios_Categoria1.csv")
colnames(mpios_categoria1)
indices <- c(2, 3, 28:54)

mpios_cat1_subsistemas <- mpios_categoria1[, indices]
mpios_cat1_subsistemas$Entidad <- as.character(mpios_cat1_subsistemas$Entidad)
mpios_cat1_subsistemas$Municipio <- as.character(mpios_cat1_subsistemas$Municipio)

# Exportar el archivo corregido
write.csv(mpios_cat1_subsistemas, "Repositorioscorregido/Municipios_Categoria1_Subsistemascorr.csv", row.names = FALSE)

# Crear directorio de salida si no existe
output_dir <- "~/Documents/MacroReportes/Repositorioscorregido/Graficos_Cat1"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

codigo_colores <- c( "#9b2247", "#a57f2c", "#7e664a",
                     "#611232", "#e6d194", "#c39326", "#806b4a")
calcular_luminosidad <- function(color) {
  rgb <- col2rgb(color)
  return(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
luminosidades <- calcular_luminosidad(codigo_colores)
colores_ordenados <- codigo_colores[order(luminosidades)]
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)
paleta_extendida <- mi_paleta_ordenada(20)

# Iterar sobre las filas para crear gráficos
for (i in 1:nrow(mpios_cat1_subsistemas)) {
  entidad <- mpios_cat1_subsistemas[i, "Entidad"]
  municipio <- mpios_cat1_subsistemas[i, "Municipio"]
  
  # Seleccionar las columnas de subsistemas
  valores <- mpios_cat1_subsistemas[i, 4:ncol(mpios_cat1_subsistemas)]
  valores_non_zero <- valores[valores != 0]
  
  # Filtrar categorías vacías
  if (length(valores_non_zero) == 0) {
    cat("Fila", i, "omitida: no hay valores no nulos para Entidad", entidad, "y Municipio", municipio, "\n")
    next
  }
  
  # Crear data frame para graficar
  df_plot <- data.frame(
    Subsistema = colnames(mpios_cat1_subsistemas)[4:ncol(mpios_cat1_subsistemas)][valores != 0],
    Cantidad = as.numeric(valores_non_zero)
  )
  
  # Ordenar subsistemas por cantidad para mejorar el gráfico
  df_plot <- df_plot[order(-df_plot$Cantidad), ]
  
  # Definir colores
  num_colores <- length(df_plot$Subsistema)
  colores_grafico <- paleta_extendida[1:num_colores]
  
  # Crear gráfico
  p <- ggplot(df_plot, aes(x = reorder(Subsistema, -Cantidad), y = Cantidad)) +
    geom_bar(stat = "identity", fill = colores_grafico) +
    labs(title = paste("Entidad:", entidad),
         subtitle = paste("Municipio:", municipio),
         x = "Subsistema",
         y = "Cantidad") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Guardar gráfico en PDF
  pdf_filename <- paste0(output_dir, "/Grafico_subsistema_", gsub(" ", "_", entidad), "_", gsub(" ", "_", municipio), ".pdf")
  ggsave(filename = pdf_filename, plot = p, device = "pdf", width = 12, height = 6)
  
  cat("Archivo", i, "procesado: Gráfico guardado en", pdf_filename, "\n")
}
##---- SE CARGAN LOS DATOS DE LA CATEGORIA 2 ----
mpios_categoria2 <- read_csv("Repositorioscorregido/Municipios_Categoria2.csv")
colnames(mpios_categoria2)
indices <- c(2, 3, 28:54)

mpios_cat2_subsistemas <- mpios_categoria2[, indices]
mpios_cat2_subsistemas$Entidad <- as.character(mpios_cat2_subsistemas$Entidad)
mpios_cat2_subsistemas$Municipio <- as.character(mpios_cat2_subsistemas$Municipio)

# Exportar el archivo corregido
write.csv(mpios_cat2_subsistemas, "Repositorioscorregido/Municipios_Categoria2_Subsistemascorr.csv", row.names = FALSE)

# Crear directorio de salida si no existe
output_dir <- "~/Documents/MacroReportes/Repositorioscorregido/Graficos_Cat2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

codigo_colores <- c( "#9b2247", "#a57f2c", "#7e664a",
                     "#611232", "#e6d194", "#c39326", "#806b4a")
calcular_luminosidad <- function(color) {
  rgb <- col2rgb(color)
  return(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
luminosidades <- calcular_luminosidad(codigo_colores)
colores_ordenados <- codigo_colores[order(luminosidades)]
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)
paleta_extendida <- mi_paleta_ordenada(20)

# Iterar sobre las filas para crear gráficos
for (i in 1:nrow(mpios_cat2_subsistemas)) {
  entidad <- mpios_cat2_subsistemas[i, "Entidad"]
  municipio <- mpios_cat2_subsistemas[i, "Municipio"]
  
  # Seleccionar las columnas de subsistemas
  valores <- mpios_cat2_subsistemas[i, 4:ncol(mpios_cat2_subsistemas)]
  valores_non_zero <- valores[valores != 0]
  
  # Filtrar categorías vacías
  if (length(valores_non_zero) == 0) {
    cat("Fila", i, "omitida: no hay valores no nulos para Entidad", entidad, "y Municipio", municipio, "\n")
    next
  }
  
  # Crear data frame para graficar
  df_plot <- data.frame(
    Subsistema = colnames(mpios_cat2_subsistemas)[4:ncol(mpios_cat2_subsistemas)][valores != 0],
    Cantidad = as.numeric(valores_non_zero)
  )
  
  # Ordenar subsistemas por cantidad para mejorar el gráfico
  df_plot <- df_plot[order(-df_plot$Cantidad), ]
  
  # Definir colores
  num_colores <- length(df_plot$Subsistema)
  colores_grafico <- paleta_extendida[1:num_colores]
  
  # Crear gráfico
  p <- ggplot(df_plot, aes(x = reorder(Subsistema, -Cantidad), y = Cantidad)) +
    geom_bar(stat = "identity", fill = colores_grafico) +
    labs(title = paste("Entidad:", entidad),
         subtitle = paste("Municipio:", municipio),
         x = "Subsistema",
         y = "Cantidad") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Guardar gráfico en PDF
  pdf_filename <- paste0(output_dir, "/Grafico_subsistema_", gsub(" ", "_", entidad), "_", gsub(" ", "_", municipio), ".pdf")
  ggsave(filename = pdf_filename, plot = p, device = "pdf", width = 12, height = 6)
  
  cat("Archivo", i, "procesado: Gráfico guardado en", pdf_filename, "\n")
}
##---- SE CARGAN LOS DATOS DE LA CATEGORIA 3 ----
mpios_categoria3 <- read_csv("Repositorioscorregido/Municipios_Categoria3.csv")
colnames(mpios_categoria3)
indices <- c(2, 3, 28:54)

mpios_cat3_subsistemas <- mpios_categoria3[, indices]
mpios_cat3_subsistemas$Entidad <- as.character(mpios_cat3_subsistemas$Entidad)
mpios_cat3_subsistemas$Municipio <- as.character(mpios_cat3_subsistemas$Municipio)

# Exportar el archivo corregido
write.csv(mpios_cat3_subsistemas, "Repositorioscorregido/Municipios_Categoria3_Subsistemascorr.csv", row.names = FALSE)

# Crear directorio de salida si no existe
output_dir <- "~/Documents/MacroReportes/Repositorioscorregido/Graficos_Cat3"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

codigo_colores <- c( "#9b2247", "#a57f2c", "#7e664a",
                     "#611232", "#e6d194", "#c39326", "#806b4a")
calcular_luminosidad <- function(color) {
  rgb <- col2rgb(color)
  return(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
luminosidades <- calcular_luminosidad(codigo_colores)
colores_ordenados <- codigo_colores[order(luminosidades)]
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)
paleta_extendida <- mi_paleta_ordenada(20)

# Iterar sobre las filas para crear gráficos
for (i in 1:nrow(mpios_cat3_subsistemas)) {
  entidad <- mpios_cat3_subsistemas[i, "Entidad"]
  municipio <- mpios_cat3_subsistemas[i, "Municipio"]
  
  # Seleccionar las columnas de subsistemas
  valores <- mpios_cat3_subsistemas[i, 4:ncol(mpios_cat3_subsistemas)]
  valores_non_zero <- valores[valores != 0]
  
  # Filtrar categorías vacías
  if (length(valores_non_zero) == 0) {
    cat("Fila", i, "omitida: no hay valores no nulos para Entidad", entidad, "y Municipio", municipio, "\n")
    next
  }
  
  # Crear data frame para graficar
  df_plot <- data.frame(
    Subsistema = colnames(mpios_cat3_subsistemas)[4:ncol(mpios_cat3_subsistemas)][valores != 0],
    Cantidad = as.numeric(valores_non_zero)
  )
  
  # Ordenar subsistemas por cantidad para mejorar el gráfico
  df_plot <- df_plot[order(-df_plot$Cantidad), ]
  
  # Definir colores
  num_colores <- length(df_plot$Subsistema)
  colores_grafico <- paleta_extendida[1:num_colores]
  
  # Crear gráfico
  p <- ggplot(df_plot, aes(x = reorder(Subsistema, -Cantidad), y = Cantidad)) +
    geom_bar(stat = "identity", fill = colores_grafico) +
    labs(title = paste("Entidad:", entidad),
         subtitle = paste("Municipio:", municipio),
         x = "Subsistema",
         y = "Cantidad") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Guardar gráfico en PDF
  pdf_filename <- paste0(output_dir, "/Grafico_subsistema_", gsub(" ", "_", entidad), "_", gsub(" ", "_", municipio), ".pdf")
  ggsave(filename = pdf_filename, plot = p, device = "pdf", width = 12, height = 6)
  
  cat("Archivo", i, "procesado: Gráfico guardado en", pdf_filename, "\n")
}
##---- SE CARGAN LOS DATOS DE TODOS LOS MUNICIPIOS ----
library(readr)
library(ggplot2)

# Leer datos
mpios_categoria <- read_csv("Repositorioscorregido/mpios_criterios.csv")
colnames(mpios_categoria)

# Seleccionar las columnas relevantes
indices <- c(3, 4, 29:55)
mpios_cat_subsistemas <- mpios_categoria[, indices]
mpios_cat_subsistemas$Entidad <- as.character(mpios_cat_subsistemas$Entidad)
mpios_cat_subsistemas$Municipio <- as.character(mpios_cat_subsistemas$Municipio)

# Exportar el archivo corregido
write.csv(mpios_cat_subsistemas, "Repositorioscorregido/Municipios_Categoria3_Subsistemascorr.csv", row.names = FALSE)

# Crear directorio de salida si no existe
output_dir <- "~/Documents/MacroReportes/Repositorioscorregido/Graficos_Cat"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Definir paleta de colores
codigo_colores <- c("#9b2247", "#a57f2c", "#7e664a", "#611232", "#e6d194", "#c39326", "#806b4a")
calcular_luminosidad <- function(color) {
  rgb <- col2rgb(color)
  return(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
luminosidades <- calcular_luminosidad(codigo_colores)
colores_ordenados <- codigo_colores[order(luminosidades)]
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)
paleta_extendida <- mi_paleta_ordenada(20)

# Iterar sobre las filas para crear gráficos
for (i in 1:nrow(mpios_cat_subsistemas)) {
  entidad <- mpios_cat_subsistemas[i, "Entidad"]
  municipio <- mpios_cat_subsistemas[i, "Municipio"]
  
  # Seleccionar las columnas de subsistemas
  valores <- mpios_cat_subsistemas[i, 4:ncol(mpios_cat_subsistemas)]
  valores_non_zero <- valores[valores != 0]
  
  # Filtrar categorías vacías
  if (length(valores_non_zero) == 0) {
    cat("Fila", i, "omitida: no hay valores no nulos para Entidad", entidad, "y Municipio", municipio, "\n")
    next
  }
  
  # Crear data frame para graficar
  df_plot <- data.frame(
    Subsistema = colnames(mpios_cat_subsistemas)[4:ncol(mpios_cat_subsistemas)][valores != 0],
    Cantidad = as.numeric(valores_non_zero)
  )
  
  # Ordenar subsistemas por cantidad para mejorar el gráfico
  df_plot <- df_plot[order(-df_plot$Cantidad), ]
  
  # Definir colores
  num_colores <- length(df_plot$Subsistema)
  colores_grafico <- paleta_extendida[1:num_colores]
  
  # Crear gráfico
  p <- ggplot(df_plot, aes(x = reorder(Subsistema, -Cantidad), y = Cantidad)) +
    geom_bar(stat = "identity", fill = colores_grafico) +
    labs(title = paste("Entidad:", entidad),
         subtitle = paste("Municipio:", municipio),
         x = "Subsistema",
         y = "Cantidad") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 8), # Reducir tamaño de etiquetas
      axis.title.x = element_text(size = 10),  # Tamaño de título del eje X
      axis.title.y = element_text(size = 10)   # Tamaño de título del eje Y
    )
  
  # Guardar gráfico en PDF
  pdf_filename <- paste0(output_dir, "/Grafico_subsistema_", gsub(" ", "_", entidad), "_", gsub(" ", "_", municipio), ".pdf")
  ggsave(filename = pdf_filename, plot = p, device = "pdf", width = 12, height = 6)
  
  cat("Archivo", i, "procesado: Gráfico guardado en", pdf_filename, "\n")
}
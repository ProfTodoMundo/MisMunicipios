#---- SE LLAMAN LAS LIBRERIAS A UTILIZAR ----
library(readr)
library(readxl)
library(dplyr)
library(openxlsx)
#---- SE DEFINE EL DIRECTORIO DE TRABAJO ----
setwd("~/Documents/MacroReportes")
#---- SE LEEN LOS ARCHIVOS A UNIFICAR ----
dbToWork <- read_excel("dbToWork.xlsx"); View(dbToWork)
MpiosLoc <- read_excel("Mpios_Localidades.xlsx"); View(MpiosLoc)
#---- SE UNEN LAS BASE DE DATOS ----
merged_data <- dbToWork %>%
  left_join(MpiosLoc, by = c("Llave", "Entidad", "Municipio", "Cve_municipio"))
View(merged_data)
#---- SE GUARDA LA NUEVA BDD ----
write.xlsx(merged_data, "dbToWork_actualizado.xlsx")
#---- DEFINICION DE LA VARIABLE MISDATOS PARA INICIAR ----
#setwd("~/Documents/MacroReportes")
misdatos <- read_excel("dbToWork_actualizado.xlsx"); View(misdatos); 
misdatos$Tasa_Neta <- round((misdatos$Tasa_Neta*100),2)
misdatos$Cobertura_Total <- round((misdatos$Cobertura_Total*100),2)
misdatos$Atencion_Demanda_Potencial <- round((misdatos$Atencion_Demanda_Potencial*100),2)
#---- ELIMINANDO CARACTERES ESPECIALES ----
##--- ENTIDADES ----
MisMaterias <- misdatos$Entidad; 
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
write.csv(misdatos,"Repositorios/bbToworkUpdated.csv")
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
write.csv(misdatos,"Repositorios/bbToworkUpdated.csv")
View(misdatos)
misdatos <- bbToworkUpdated
indices <- c(2,3,5,6,8,9,48:83)
miBDD <- misdatos[,indices]; View(miBDD)
#---- SE CARGA LA ULTIMA BDD TRABAJADA ----
library(readr)
setwd("~/Documents/MacroReportes")
miBDD <- read_csv("Repositorios/bbToworkUpdated.csv")
View(miBDD)
#---- RESPALDO PARCIAL DEL ESPACIO DE TRABAJO ----
#---- GENERACION DE LA PALETA DE COLORES ----
codigo_colores <- c("#161a1d", "#9b2247", "#a57f2c", "#7e664a",
                    "#611232", "#e6d194", "#c39326", "#806b4a")
calcular_luminosidad <- function(color) {
  rgb <- col2rgb(color)
  return(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
luminosidades <- calcular_luminosidad(codigo_colores)
colores_ordenados <- codigo_colores[order(luminosidades)]
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)
paleta_extendida <- mi_paleta_ordenada(20)
print(paleta_extendida)
barplot(rep(1, length(paleta_extendida)), 
        col = paleta_extendida, border = NA,
        main = "Paleta extendida ordenada por luminosidad")
#---- AHORA GENERAREMOS LAS GRAFICAS DE LAS PROYECCIONES PARA LOS ESTADOS  ----
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

misdatos <- read_csv("Documents/MacroReportes/Repositorios/bbToworkUpdated.csv")
indices <- c(2,3,5,6,8,9,48:83)
miBDD <- misdatos[,indices]; View(miBDD)
View(miBDD)
# Leer los datos
nuevos_nombres <- c("Entidad",
                    "Municipio","Tasa_Neta","Cobertura_Total",
                    "Poblacion_Total","Poblacion_Tipica_23",            
                    "Est_Pob_H_15a19_2024", 
                    "Est_Pob_M_15a19_2024",
                    "TtEst_Pob_15a19_2024",
                    "Est_Pob_H_15a19_2025", 
                    "Est_Pob_M_15a19_2025",
                    "TtEst_Pob_15a19_2025",
                    "Est_Pob_H_15a19_2026", 
                    "Est_Pob_M_15a19_2026",
                    "TtEst_Pob_15a19_2026",
                    "Est_Pob_H_15a19_2027", 
                    "Est_Pob_M_15a19_2027",
                    "TtEst_Pob_15a19_2027",
                    "Est_Pob_H_15a19_2028", 
                    "Est_Pob_M_15a19_2028",
                    "TtEst_Pob_15a19_2028",
                    "Est_Pob_H_15a19_2029", 
                    "Est_Pob_M_15a19_2029",
                    "TtEst_Pob_15a19_2029",
                    "Est_Pob_H_15a19_2030", 
                    "Est_Pob_M_15a19_2030",
                    "TtEst_Pob_15a19_2030",
                    "Est_Pob_H_15a19_2031", 
                    "Est_Pob_M_15a19_2031",
                    "TtEst_Pob_15a19_2031",
                    "Est_Pob_H_15a19_2032", 
                    "Est_Pob_M_15a19_2032",
                    "TtEst_Pob_15a19_2032",
                    "Est_Pob_H_15a19_2033", 
                    "Est_Pob_M_15a19_2033",
                    "TtEst_Pob_15a19_2033",
                    "Est_Pob_H_15a19_2034", 
                    "Est_Pob_M_15a19_2034",
                    "TtEst_Pob_15a19_2034",
                    "Est_Pob_H_15a19_2035", 
                    "Est_Pob_M_15a19_2035",
                    "TtEst_Pob_15a19_2035")
colnames(miBDD)<- nuevos_nombres
View(miBDD)
# Filtrar el primer municipio
bdd_Graficar <- miBDD[,c("Entidad","Municipio","Tasa_Neta",
                         "Cobertura_Total","Poblacion_Total",
                         "Poblacion_Tipica_23",            
                         "Est_Pob_H_15a19_2024", 
                         "Est_Pob_H_15a19_2025", 
                         "Est_Pob_H_15a19_2026", 
                         "Est_Pob_H_15a19_2027",
                         "Est_Pob_H_15a19_2028",
                         "Est_Pob_H_15a19_2029", 
                         "Est_Pob_H_15a19_2030", 
                         "Est_Pob_H_15a19_2031", 
                         "Est_Pob_H_15a19_2032", 
                         "Est_Pob_H_15a19_2033", 
                         "Est_Pob_H_15a19_2034", 
                         "Est_Pob_H_15a19_2035", 
                         "Est_Pob_M_15a19_2024",
                         "Est_Pob_M_15a19_2025",
                         "Est_Pob_M_15a19_2026",
                         "Est_Pob_M_15a19_2027",
                         "Est_Pob_M_15a19_2028",
                         "Est_Pob_M_15a19_2029",
                         "Est_Pob_M_15a19_2030",
                         "Est_Pob_M_15a19_2031",
                         "Est_Pob_M_15a19_2032",
                         "Est_Pob_M_15a19_2033",
                         "Est_Pob_M_15a19_2034",
                         "Est_Pob_M_15a19_2035",
                         "TtEst_Pob_15a19_2024",
                         "TtEst_Pob_15a19_2025",
                         "TtEst_Pob_15a19_2026",
                         "TtEst_Pob_15a19_2027",
                         "TtEst_Pob_15a19_2028",
                         "TtEst_Pob_15a19_2029",
                         "TtEst_Pob_15a19_2030",
                         "TtEst_Pob_15a19_2031",
                         "TtEst_Pob_15a19_2032",
                         "TtEst_Pob_15a19_2033",
                         "TtEst_Pob_15a19_2034",
                         "TtEst_Pob_15a19_2035")]
View(bdd_Graficar)
indicesH <- c(1:6,7:18); indicesM <- c(1:6,19:30); indicesTtl<- c(1:6,31:42)
graficandoH <- bdd_Graficar[,indicesH]; View(graficandoH)
graficandoM <- bdd_Graficar[,indicesM]; View(graficandoM)
GraficandoTtl <- bdd_Graficar[,indicesTtl]; View(GraficandoTtl)

##---- SE GRAFICAN LAS PROYECCIONES POBLACIONALES PARA EL MUNICIPIO PARA LOS HOMBRES ----
indicesH <- c(1:6,7:18);graficandoMpioH <- graficandoH[1,]; print(graficandoMpioH)
# Definir los índices
indicesH <- c(1:6, 7:18)
# Asumiendo que `graficandoH` ya está definido y es una matriz o data frame
# Extraer la primera fila
graficandoMpioH <- graficandoH[1,]
# Mostrar los valores de las entradas 1:6
valores_1_6 <- as.numeric(graficandoMpioH[1:6])
# Graficar los elementos de las entradas 7:18
valores_7_18 <- as.numeric(graficandoMpioH[7:18])
etiquetas_x <- 2024:2035
# Crear el gráfico con la paleta extendida
plot(
  valores_7_18,
  type = "b",               # Puntos y líneas
  col = paleta_extendida,   # Aplicar la paleta personalizada
  pch = 19,                 # Forma de los puntos
  lty = 1,                  # Tipo de línea sólida
  xlab = "Años",  # Etiqueta del eje x
  ylab = "Población de Hombres",         # Etiqueta del eje y
  ylim = c(min(valores_7_18)-500,max(valores_7_18)+1000),
  xaxt = 'n',
  main = "",                # Título vacío (se agrega con title())
  cex.lab = 0.8,            # Reducir tamaño de las etiquetas de los ejes
  cex.axis = 0.7  
)
axis(
  side = 1,                      # Eje X
  at = seq_along(valores_7_18),  # Posiciones de las etiquetas
  labels = etiquetas_x,          # Etiquetas (2024 a 2035)
  las = 2,                       # Rotar etiquetas en vertical
  cex.axis = 0.6                # Reducir tamaño del texto
)
# Agregar título en azul marino
title(
  main = "Proyección de la Población de 17 a 19 años (CONAPO)",
  col.main = "navy"  # Color del título
)
# Agregar valores como etiquetas
text(
  x = seq_along(valores_7_18),
  y = valores_7_18,
  labels = round(valores_7_18, 2),
  pos = 3, # Arriba de los puntos
  las=2,
  cex = 0.6,
  col = "black"
)
# Crear la leyenda con los valores de graficandoMpioH[1:6]
valores_1_2 <- as.character(graficandoMpioH[1:2])  # Convertir a caracteres para nombres de entidad y municipio
valores_3_4 <- as.numeric(graficandoMpioH[3:4])   # Asegurar valores numéricos para tasas
valores_5_6 <- as.numeric(graficandoMpioH[5:6])   # Asegurar valores numéricos para población
# Formatear los valores 5:6 con separadores de miles
valores_5_6_formateados <- format(valores_5_6, big.mark = ",", scientific = FALSE)
# Texto descriptivo para los valores
texto_1_2 <- c("Entidad", "Municipio")
texto_3_6 <- c("Tasa Neta", "Cobertura Total", "Población Total", "Población Típica")
# Generar las leyendas
texto_leyenda_1_2 <- paste0(texto_1_2, ": ", valores_1_2)  # Leyenda para los nombres
texto_leyenda_3_4 <- paste0(texto_3_6[1:2], ": ", round(valores_3_4, 2))  # Leyenda para las tasas
texto_leyenda_5_6 <- paste0(texto_3_6[3:4], ": ", valores_5_6_formateados)  # Leyenda para la población
# Dividir las leyendas en dos columnas
texto_izquierda <- c(texto_leyenda_1_2[1], texto_leyenda_3_4[1], texto_leyenda_3_4[2])  # Primera columna
texto_derecha <- c(texto_leyenda_1_2[2], texto_leyenda_5_6[1], texto_leyenda_5_6[2])    # Segunda columna
# Colores correspondientes
colores_izquierda <- c("#161a1d", "#9b2247", "#a57f2c")
colores_derecha <- c("#7e664a", "#611232", "#e6d194")
# Agregar la primera columna de la leyenda
legend(
  "topleft",                 # Ubicación en la esquina superior izquierda
  legend = texto_izquierda,  # Texto de la primera columna
  fill = colores_izquierda,  # Colores de los cuadros
  border = "black",          # Bordes de los cuadros
  bty = "n",                 # Sin borde en la caja de la leyenda
  cex = 0.6                  # Reducir tamaño del texto
)
# Agregar la segunda columna de la leyenda
legend(
  "topright",                # Ubicación en la esquina superior derecha
  legend = texto_derecha,    # Texto de la segunda columna
  fill = colores_derecha,    # Colores de los cuadros
  border = "black",          # Bordes de los cuadros
  bty = "n",                 # Sin borde en la caja de la leyenda
  cex = 0.6                  # Reducir tamaño del texto
)
##---- TERMINA LA GENERACION DE LA GRAFICA ----
##---- SE GRAFICAN LAS PROYECCIONES POBLACIONALES PARA EL MUNICIPIO PARA LAS MUJERES ----
indicesM <- c(1:6,19:30);graficandoMpioM <- graficandoM[1,]; print(graficandoMpioM)
# Definir los índices
indicesM <- c(1:6, 7:18)
# Extraer la primera fila
graficandoMpioM <- graficandoM[1,]
# Mostrar los valores de las entradas 1:6
valores_1_6 <- as.numeric(graficandoMpioM[1:6])
# Graficar los elementos de las entradas 7:18
valores_7_18 <- as.numeric(graficandoMpioM[7:18])
etiquetas_x <- 2024:2035
# Crear el gráfico con la paleta extendida
plot(
  valores_7_18,
  type = "b",               # Puntos y líneas
  col = paleta_extendida,   # Aplicar la paleta personalizada
  pch = 19,                 # Forma de los puntos
  lty = 1,                  # Tipo de línea sólida
  xlab = "Años",  # Etiqueta del eje x
  ylab = "Población de Mujeres",         # Etiqueta del eje y
  ylim = c(min(valores_7_18)-500,max(valores_7_18)+1000),
  xaxt = 'n',
  main = "",                # Título vacío (se agrega con title())
  cex.lab = 0.8,            # Reducir tamaño de las etiquetas de los ejes
  cex.axis = 0.7  
)
axis(
  side = 1,                      # Eje X
  at = seq_along(valores_7_18),  # Posiciones de las etiquetas
  labels = etiquetas_x,          # Etiquetas (2024 a 2035)
  las = 2,                       # Rotar etiquetas en vertical
  cex.axis = 0.6                # Reducir tamaño del texto
)
# Agregar título en azul marino
title(
  main = "Proyección de la Población de 17 a 19 años (CONAPO)",
  col.main = "navy"  # Color del título
)
# Agregar valores como etiquetas
text(
  x = seq_along(valores_7_18),
  y = valores_7_18,
  labels = round(valores_7_18, 2),
  pos = 3, # Arriba de los puntos
  las=2,
  cex = 0.6,
  col = "black"
)
# Crear la leyenda con los valores de graficandoMpioH[1:6]
valores_1_2 <- as.character(graficandoMpioM[1:2])  # Convertir a caracteres para nombres de entidad y municipio
valores_3_4 <- as.numeric(graficandoMpioM[3:4])   # Asegurar valores numéricos para tasas
valores_5_6 <- as.numeric(graficandoMpioM[5:6])   # Asegurar valores numéricos para población
# Formatear los valores 5:6 con separadores de miles
valores_5_6_formateados <- format(valores_5_6, big.mark = ",", scientific = FALSE)
# Texto descriptivo para los valores
texto_1_2 <- c("Entidad", "Municipio")
texto_3_6 <- c("Tasa Neta", "Cobertura Total", "Población Total", "Población Típica")
# Generar las leyendas
texto_leyenda_1_2 <- paste0(texto_1_2, ": ", valores_1_2)  # Leyenda para los nombres
texto_leyenda_3_4 <- paste0(texto_3_6[1:2], ": ", round(valores_3_4, 2))  # Leyenda para las tasas
texto_leyenda_5_6 <- paste0(texto_3_6[3:4], ": ", valores_5_6_formateados)  # Leyenda para la población
# Dividir las leyendas en dos columnas
texto_izquierda <- c(texto_leyenda_1_2[1], texto_leyenda_3_4[1], texto_leyenda_3_4[2])  # Primera columna
texto_derecha <- c(texto_leyenda_1_2[2], texto_leyenda_5_6[1], texto_leyenda_5_6[2])    # Segunda columna
# Colores correspondientes
  colores_izquierda <- c("#161a1d", "#9b2247", "#a57f2c")
colores_derecha <- c("#7e664a", "#611232", "#e6d194")
# Agregar la primera columna de la leyenda
legend(
  "topleft",                 # Ubicación en la esquina superior izquierda
  legend = texto_izquierda,  # Texto de la primera columna
  fill = colores_izquierda,  # Colores de los cuadros
  border = "black",          # Bordes de los cuadros
  bty = "n",                 # Sin borde en la caja de la leyenda
  cex = 0.6                  # Reducir tamaño del texto
)
# Agregar la segunda columna de la leyenda
legend(
  "topright",                # Ubicación en la esquina superior derecha
  legend = texto_derecha,    # Texto de la segunda columna
  fill = colores_derecha,    # Colores de los cuadros
  border = "black",          # Bordes de los cuadros
  bty = "n",                 # Sin borde en la caja de la leyenda
  cex = 0.6                  # Reducir tamaño del texto
)
##---- TERMINA LA GENERACION DE LA GRAFICA ----
##---- SE GRAFICAN LAS PROYECCIONES POBLACIONALES PARA EL MUNICIPIO PARA LA POBLACION TOTAL ----
indicesTtl <- c(1:6,31:42);graficandoMpioTtl <- GraficandoTtl[1,]; print(graficandoMpioTtl)
# Definir los índices
indicesTtl <- c(1:6, 31:42)
# Extraer la primera fila
graficandoMpioTtl <- GraficandoTtl[1,]
# Mostrar los valores de las entradas 1:6
valores_1_6 <- as.numeric(graficandoMpioTtl[1:6])
# Graficar los elementos de las entradas 7:18
valores_7_18 <- as.numeric(graficandoMpioTtl[7:18])
etiquetas_x <- 2024:2035
# Crear el gráfico con la paleta extendida
plot(
  valores_7_18,
  type = "b",               # Puntos y líneas
  col = paleta_extendida,   # Aplicar la paleta personalizada
  pch = 19,                 # Forma de los puntos
  lty = 1,                  # Tipo de línea sólida
  xlab = "Años",  # Etiqueta del eje x
  ylab = "Población Total",         # Etiqueta del eje y
  ylim = c(min(valores_7_18)-500,max(valores_7_18)+1000),
  xaxt = 'n',
  main = "",                # Título vacío (se agrega con title())
  cex.lab = 0.8,            # Reducir tamaño de las etiquetas de los ejes
  cex.axis = 0.7  
)
axis(
  side = 1,                      # Eje X
  at = seq_along(valores_7_18),  # Posiciones de las etiquetas
  labels = etiquetas_x,          # Etiquetas (2024 a 2035)
  las = 2,                       # Rotar etiquetas en vertical
  cex.axis = 0.6                # Reducir tamaño del texto
)
# Agregar título en azul marino
title(
  main = "Proyección de la Población de 17 a 19 años (CONAPO)",
  col.main = "navy"  # Color del título
)
# Agregar valores como etiquetas
text(
  x = seq_along(valores_7_18),
  y = valores_7_18,
  labels = round(valores_7_18, 2),
  pos = 3, # Arriba de los puntos
  las=2,
  cex = 0.6,
  col = "black"
)
# Crear la leyenda con los valores de graficandoMpioH[1:6]
valores_1_2 <- as.character(graficandoMpioTtl[1:2])  # Convertir a caracteres para nombres de entidad y municipio
valores_3_4 <- as.numeric(graficandoMpioTtl[3:4])   # Asegurar valores numéricos para tasas
valores_5_6 <- as.numeric(graficandoMpioTtl[5:6])   # Asegurar valores numéricos para población
# Formatear los valores 5:6 con separadores de miles
valores_5_6_formateados <- format(valores_5_6, big.mark = ",", scientific = FALSE)
# Texto descriptivo para los valores
texto_1_2 <- c("Entidad", "Municipio")
texto_3_6 <- c("Tasa Neta", "Cobertura Total", "Población Total", "Población Típica")
# Generar las leyendas
texto_leyenda_1_2 <- paste0(texto_1_2, ": ", valores_1_2)  # Leyenda para los nombres
texto_leyenda_3_4 <- paste0(texto_3_6[1:2], ": ", round(valores_3_4, 2))  # Leyenda para las tasas
texto_leyenda_5_6 <- paste0(texto_3_6[3:4], ": ", valores_5_6_formateados)  # Leyenda para la población
# Dividir las leyendas en dos columnas
texto_izquierda <- c(texto_leyenda_1_2[1], texto_leyenda_3_4[1], texto_leyenda_3_4[2])  # Primera columna
texto_derecha <- c(texto_leyenda_1_2[2], texto_leyenda_5_6[1], texto_leyenda_5_6[2])    # Segunda columna
# Colores correspondientes
colores_izquierda <- c("#161a1d", "#9b2247", "#a57f2c")
colores_derecha <- c("#7e664a", "#611232", "#e6d194")
# Agregar la primera columna de la leyenda
legend(
  "topleft",                 # Ubicación en la esquina superior izquierda
  legend = texto_izquierda,  # Texto de la primera columna
  fill = colores_izquierda,  # Colores de los cuadros
  border = "black",          # Bordes de los cuadros
  bty = "n",                 # Sin borde en la caja de la leyenda
  cex = 0.6                  # Reducir tamaño del texto
)
# Agregar la segunda columna de la leyenda
legend(
  "topright",                # Ubicación en la esquina superior derecha
  legend = texto_derecha,    # Texto de la segunda columna
  fill = colores_derecha,    # Colores de los cuadros
  border = "black",          # Bordes de los cuadros
  bty = "n",                 # Sin borde en la caja de la leyenda
  cex = 0.6                  # Reducir tamaño del texto
)
##---- TERMINA LA GENERACION DE LA GRAFICA ----
#---- GENERACION AUTOMATICA DE LAS GRAFICAS PARA HOMBRES, MUJERES Y TOTAL ----
setwd("~/Documents/MacroReportes/Repositorios")
# Función para generar y guardar gráficos
crear_grafico <- function(datos, indices, etiquetas_x, nombre_archivo, ylab_texto) {
  # Extraer datos
  graficandoMpio <- datos[1, ]
  valores_1_2 <- as.character(graficandoMpio[1:2])  # Entidad y Municipio
  valores_3_4 <- as.numeric(graficandoMpio[3:4])   # Tasa Neta y Cobertura Total
  valores_5_6 <- as.numeric(graficandoMpio[5:6])   # Población Total y Típica
  valores_7_18 <- as.numeric(graficandoMpio[7:18]) # Valores a graficar
  
  # Formatear valores 5:6
  valores_5_6_formateados <- format(valores_5_6, big.mark = ",", scientific = FALSE)
  
  # Texto descriptivo
  texto_1_2 <- c("Entidad", "Municipio")
  texto_3_6 <- c("Tasa Neta", "Cobertura Total", "Población Total", "Población Típica")
  
  # Generar leyendas
  texto_leyenda_1_2 <- paste0(texto_1_2, ": ", valores_1_2)
  texto_leyenda_3_4 <- paste0(texto_3_6[1:2], ": ", round(valores_3_4, 2))
  texto_leyenda_5_6 <- paste0(texto_3_6[3:4], ": ", valores_5_6_formateados)
  
  texto_izquierda <- c(texto_leyenda_1_2[1], texto_leyenda_3_4[1], texto_leyenda_3_4[2])
  texto_derecha <- c(texto_leyenda_1_2[2], texto_leyenda_5_6[1], texto_leyenda_5_6[2])
  
  # Colores
  colores_izquierda <- c("#161a1d", "#9b2247", "#a57f2c")
  colores_derecha <- c("#7e664a", "#611232", "#e6d194")
  
  # Crear PDF
  pdf(file = paste0(nombre_archivo, ".pdf"))
  
  # Crear el gráfico
  plot(
    valores_7_18,
    type = "b",               # Puntos y líneas
    col = paleta_extendida,   # Aplicar la paleta personalizada
    pch = 19,                 # Forma de los puntos
    lty = 1,                  # Tipo de línea sólida
    xlab = "Años",            # Etiqueta del eje X
    ylab = ylab_texto,        # Etiqueta del eje Y
    ylim = c(min(valores_7_18) - 500, max(valores_7_18) + 1000),
    xaxt = 'n',
    cex.lab = 0.8,            # Reducir tamaño de las etiquetas de los ejes
    cex.axis = 0.7  
  )
  
  # Eje X
  axis(
    side = 1,
    at = seq_along(valores_7_18),
    labels = etiquetas_x,
    las = 2,
    cex.axis = 0.6
  )
  
  # Título
  title(
    main = paste("Proyección de la Población de 17 a 19 años (CONAPO)\n", valores_1_2[2]),
    col.main = "navy"
  )
  
  # Valores en el gráfico
  text(
    x = seq_along(valores_7_18),
    y = valores_7_18,
    labels = round(valores_7_18, 2),
    pos = 3,
    cex = 0.6,
    col = "black"
  )
  
  # Leyenda izquierda
  legend(
    "topleft",
    legend = texto_izquierda,
    fill = colores_izquierda,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  # Leyenda derecha
  legend(
    "topright",
    legend = texto_derecha,
    fill = colores_derecha,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  # Cerrar el archivo PDF
  dev.off()
}

# Crear las gráficas para cada subconjunto de datos
etiquetas_x <- 2024:2035

crear_grafico(
  datos = graficandoH,
  indices = indicesH,
  etiquetas_x = etiquetas_x,
  nombre_archivo = paste0("Proyeccion_Hombres_", graficandoH[1, 2]),
  ylab_texto = "Población de Hombres"
)

crear_grafico(
  datos = graficandoM,
  indices = indicesM,
  etiquetas_x = etiquetas_x,
  nombre_archivo = paste0("Proyeccion_Mujeres_", graficandoM[1, 2]),
  ylab_texto = "Población de Mujeres"
)

crear_grafico(
  datos = GraficandoTtl,
  indices = indicesTtl,
  etiquetas_x = etiquetas_x,
  nombre_archivo = paste0("Proyeccion_Total_", GraficandoTtl[1, 2]),
  ylab_texto = "Población Total"
)
#---- SEGUNDO INTENTO, EL ANTERIO SOLO GENERABA PARA EL PRIMER MUNICIPIO ----
# Función para generar y guardar gráficos
crear_grafico <- function(datos, fila, etiquetas_x, nombre_archivo_base, ylab_texto) {
  # Extraer datos para la fila específica
  graficandoMpio <- datos[fila, ]
  valores_1_2 <- as.character(graficandoMpio[1:2])  # Entidad y Municipio
  valores_3_4 <- as.numeric(graficandoMpio[3:4])   # Tasa Neta y Cobertura Total
  valores_5_6 <- as.numeric(graficandoMpio[5:6])   # Población Total y Típica
  valores_7_18 <- as.numeric(graficandoMpio[7:18]) # Valores a graficar
  
  # Formatear valores 5:6
  valores_5_6_formateados <- format(valores_5_6, big.mark = ",", scientific = FALSE)
  
  # Texto descriptivo
  texto_1_2 <- c("Entidad", "Municipio")
  texto_3_6 <- c("Tasa Neta", "Cobertura Total", "Población Total", "Población Típica")
  
  # Generar leyendas
  texto_leyenda_1_2 <- paste0(texto_1_2, ": ", valores_1_2)
  texto_leyenda_3_4 <- paste0(texto_3_6[1:2], ": ", round(valores_3_4, 2))
  texto_leyenda_5_6 <- paste0(texto_3_6[3:4], ": ", valores_5_6_formateados)
  
  texto_izquierda <- c(texto_leyenda_1_2[1], texto_leyenda_3_4[1], texto_leyenda_3_4[2])
  texto_derecha <- c(texto_leyenda_1_2[2], texto_leyenda_5_6[1], texto_leyenda_5_6[2])
  
  # Colores
  colores_izquierda <- c("#161a1d", "#9b2247", "#a57f2c")
  colores_derecha <- c("#7e664a", "#611232", "#e6d194")
  
  # Crear PDF
  pdf(file = paste0(nombre_archivo_base, "_", valores_1_2[2], ".pdf"))
  
  # Crear el gráfico
  plot(
    valores_7_18,
    type = "b",               # Puntos y líneas
    col = paleta_extendida,   # Aplicar la paleta personalizada
    pch = 19,                 # Forma de los puntos
    lty = 1,                  # Tipo de línea sólida
    xlab = "Años",            # Etiqueta del eje X
    ylab = ylab_texto,        # Etiqueta del eje Y
    ylim = c(min(valores_7_18) - 500, max(valores_7_18) + 1000),
    xaxt = 'n',
    cex.lab = 0.8,            # Reducir tamaño de las etiquetas de los ejes
    cex.axis = 0.7  
  )
  
  # Eje X
  axis(
    side = 1,
    at = seq_along(valores_7_18),
    labels = etiquetas_x,
    las = 2,
    cex.axis = 0.6
  )
  
  # Título
  title(
    main = paste("Proyección de la Población de 17 a 19 años (CONAPO)\n", valores_1_2[2]),
    col.main = "navy"
  )
  
  # Valores en el gráfico
  text(
    x = seq_along(valores_7_18),
    y = valores_7_18,
    labels = round(valores_7_18, 2),
    pos = 3,
    cex = 0.6,
    col = "black"
  )
  
  # Leyenda izquierda
  legend(
    "topleft",
    legend = texto_izquierda,
    fill = colores_izquierda,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  # Leyenda derecha
  legend(
    "topright",
    legend = texto_derecha,
    fill = colores_derecha,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  # Cerrar el archivo PDF
  dev.off()
}

# Iterar sobre cada fila y generar gráficos
etiquetas_x <- 2024:2035

# Hombres
for (i in 1:nrow(graficandoH)) {
  crear_grafico(
    datos = graficandoH,
    fila = i,
    etiquetas_x = etiquetas_x,
    nombre_archivo_base = "Proyeccion_Hombres",
    ylab_texto = "Población de Hombres"
  )
}

# Mujeres
for (i in 1:nrow(graficandoM)) {
  crear_grafico(
    datos = graficandoM,
    fila = i,
    etiquetas_x = etiquetas_x,
    nombre_archivo_base = "Proyeccion_Mujeres",
    ylab_texto = "Población de Mujeres"
  )
}

# Total
for (i in 1:nrow(GraficandoTtl)) {
  crear_grafico(
    datos = GraficandoTtl,
    fila = i,
    etiquetas_x = etiquetas_x,
    nombre_archivo_base = "Proyeccion_Total",
    ylab_texto = "Población Total"
  )
}
#---- TERCER INTENTO: SE GUARDAN EN LA CARPETA GraficaProyeccionesEdad ----
# Crear la carpeta si no existe
output_dir <- "GraficaProyeccionesEdad"
#if (!dir.exists(output_dir)) {
#  dir.create(output_dir)
#}

# Función para generar y guardar gráficos
crear_grafico <- function(datos, fila, etiquetas_x, nombre_archivo_base, ylab_texto, output_dir) {
  # Extraer datos para la fila específica
  graficandoMpio <- datos[fila, ]
  valores_1_2 <- as.character(graficandoMpio[1:2])  # Entidad y Municipio
  valores_3_4 <- as.numeric(graficandoMpio[3:4])   # Tasa Neta y Cobertura Total
  valores_5_6 <- as.numeric(graficandoMpio[5:6])   # Población Total y Típica
  valores_7_18 <- as.numeric(graficandoMpio[7:18]) # Valores a graficar
  
  # Formatear valores 5:6
  valores_5_6_formateados <- format(valores_5_6, big.mark = ",", scientific = FALSE)
  
  # Texto descriptivo
  texto_1_2 <- c("Entidad", "Municipio")
  texto_3_6 <- c("Tasa Neta", "Cobertura Total", "Población Total", "Población Típica")
  
  # Generar leyendas
  texto_leyenda_1_2 <- paste0(texto_1_2, ": ", valores_1_2)
  texto_leyenda_3_4 <- paste0(texto_3_6[1:2], ": ", round(valores_3_4, 2))
  texto_leyenda_5_6 <- paste0(texto_3_6[3:4], ": ", valores_5_6_formateados)
  
  texto_izquierda <- c(texto_leyenda_1_2[1], texto_leyenda_3_4[1], texto_leyenda_3_4[2])
  texto_derecha <- c(texto_leyenda_1_2[2], texto_leyenda_5_6[1], texto_leyenda_5_6[2])
  
  # Colores
  colores_izquierda <- c("#161a1d", "#9b2247", "#a57f2c")
  colores_derecha <- c("#7e664a", "#611232", "#e6d194")
  
  # Crear PDF en la carpeta
  archivo_pdf <- file.path(output_dir, paste0(nombre_archivo_base, "_", valores_1_2[2], ".pdf"))
  pdf(file = archivo_pdf)
  
  # Crear el gráfico
  plot(
    valores_7_18,
    type = "b",               # Puntos y líneas
    col = paleta_extendida,   # Aplicar la paleta personalizada
    pch = 19,                 # Forma de los puntos
    lty = 1,                  # Tipo de línea sólida
    xlab = "Años",            # Etiqueta del eje X
    ylab = ylab_texto,        # Etiqueta del eje Y
    ylim = c(min(valores_7_18) - 500, max(valores_7_18) + 1000),
    xaxt = 'n',
    cex.lab = 0.8,            # Reducir tamaño de las etiquetas de los ejes
    cex.axis = 0.7  
  )
  
  # Eje X
  axis(
    side = 1,
    at = seq_along(valores_7_18),
    labels = etiquetas_x,
    las = 2,
    cex.axis = 0.6
  )
  
  # Título
  title(
    main = paste("Proyección de la Población de 17 a 19 años (CONAPO)\n", valores_1_2[2]),
    col.main = "navy"
  )
  
  # Valores en el gráfico
  text(
    x = seq_along(valores_7_18),
    y = valores_7_18,
    labels = round(valores_7_18, 2),
    pos = 3,
    cex = 0.6,
    col = "black"
  )
  
  # Leyenda izquierda
  legend(
    "topleft",
    legend = texto_izquierda,
    fill = colores_izquierda,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  # Leyenda derecha
  legend(
    "topright",
    legend = texto_derecha,
    fill = colores_derecha,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  # Cerrar el archivo PDF
  dev.off()
}

# Iterar sobre cada fila y generar gráficos
etiquetas_x <- 2024:2035

# Hombres
for (i in 1:nrow(graficandoH)) {
  crear_grafico(
    datos = graficandoH,
    fila = i,
    etiquetas_x = etiquetas_x,
    nombre_archivo_base = "Proyeccion_Hombres",
    ylab_texto = "Población de Hombres",
    output_dir = output_dir
  )
}

# Mujeres
for (i in 1:nrow(graficandoM)) {
  crear_grafico(
    datos = graficandoM,
    fila = i,
    etiquetas_x = etiquetas_x,
    nombre_archivo_base = "Proyeccion_Mujeres",
    ylab_texto = "Población de Mujeres",
    output_dir = output_dir
  )
}

# Total
for (i in 1:nrow(GraficandoTtl)) {
  crear_grafico(
    datos = GraficandoTtl,
    fila = i,
    etiquetas_x = etiquetas_x,
    nombre_archivo_base = "Proyeccion_Total",
    ylab_texto = "Población Total",
    output_dir = output_dir
  )
}
#---- TERMINA DE GRAFICAR ----
#---- CUARTO INTENTO: GRAFICAS UNIFICADAS ----
# Función para generar gráficos unificados con leyendas descriptivas
crear_grafico_unificado <- function(fila, etiquetas_x, output_dir) {
  # Extraer datos de la fila específica para cada conjunto
  graficandoH_fila <- graficandoH[fila, ]
  graficandoM_fila <- graficandoM[fila, ]
  graficandoTtl_fila <- GraficandoTtl[fila, ]
  
  # Valores para hombres, mujeres y total
  valores_H <- as.numeric(graficandoH_fila[7:18])
  valores_M <- as.numeric(graficandoM_fila[7:18])
  valores_Ttl <- as.numeric(graficandoTtl_fila[7:18])
  
  # Entidad y Municipio (asumiendo que son iguales en los tres subconjuntos)
  entidad <- as.character(graficandoH_fila[1])
  municipio <- as.character(graficandoH_fila[2])
  
  # Valores descriptivos
  valores_3_4 <- as.numeric(graficandoH_fila[3:4])   # Tasa Neta y Cobertura Total
  valores_5_6 <- as.numeric(graficandoH_fila[5:6])   # Población Total y Típica
  valores_5_6_formateados <- format(valores_5_6, big.mark = ",", scientific = FALSE)
  
  # Texto descriptivo para las leyendas
  texto_1_2 <- c("Entidad", "Municipio")
  texto_3_6 <- c("Tasa Neta", "Cobertura Total", "Población Total", "Población Típica")
  texto_leyenda_1_2 <- paste0(texto_1_2, ": ", c(entidad, municipio))
  texto_leyenda_3_4 <- paste0(texto_3_6[1:2], ": ", round(valores_3_4, 2))
  texto_leyenda_5_6 <- paste0(texto_3_6[3:4], ": ", valores_5_6_formateados)
  
  texto_izquierda <- c(texto_leyenda_1_2[1], texto_leyenda_3_4[1], texto_leyenda_3_4[2])
  texto_derecha <- c(texto_leyenda_1_2[2], texto_leyenda_5_6[1], texto_leyenda_5_6[2])
  
  # Colores
  colores_izquierda <- c("#161a1d", "#9b2247", "#a57f2c")
  colores_derecha <- c("#7e664a", "#611232", "#e6d194")
  
  # Rango del eje Y
  rango_y <- c(
    min(c(valores_H, valores_M, valores_Ttl)) - 500,
    max(c(valores_H, valores_M, valores_Ttl)) + 1000
  )
  
  # Crear PDF
  archivo_pdf <- file.path(output_dir, paste0("graficas_unificadas_", entidad, "_", municipio, ".pdf"))
  pdf(file = archivo_pdf)
  
  # Crear el gráfico
  plot(
    etiquetas_x, valores_H,
    type = "b",               # Puntos y líneas
    col = "blue",             # Color para hombres
    pch = 19,                 # Forma de los puntos
    lty = 1,                  # Tipo de línea sólida
    xlab = "Años",            # Etiqueta del eje X
    ylab = "Población",       # Etiqueta del eje Y
    ylim = rango_y,           # Rango del eje Y
    cex.lab = 0.8,            # Reducir tamaño de las etiquetas de los ejes
    cex.axis = 0.7,
    main = paste("Proyección de la Población de 17 a 19 años\n", entidad, "-", municipio)
  )
  
  # Añadir datos de mujeres
  lines(etiquetas_x, valores_M, type = "b", col = "red", pch = 17, lty = 2)
  
  # Añadir datos de población total
  lines(etiquetas_x, valores_Ttl, type = "b", col = "green", pch = 15, lty = 3)
  
  # Leyendas de valores descriptivos
  legend(
    "topleft",
    legend = texto_izquierda,
    fill = colores_izquierda,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  legend(
    "topright",
    legend = texto_derecha,
    fill = colores_derecha,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  # Leyenda de líneas y puntos
  legend(
    "bottomright",
    legend = c("Hombres", "Mujeres", "Total"),
    col = c("blue", "red", "green"),
    pch = c(19, 17, 15),
    lty = c(1, 2, 3),
    bty = "n",
    cex = 0.8
  )
  
  # Cerrar el archivo PDF
  dev.off()
}

# Iterar sobre cada fila y generar gráficos unificados
output_dir <- "GraficaProyeccionesEdad"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

etiquetas_x <- 2024:2035

for (i in 1:nrow(graficandoH)) {
  crear_grafico_unificado(
    fila = i,
    etiquetas_x = etiquetas_x,
    output_dir = output_dir
  )
}
#---- TERMINA DE GRAFICAR ----
#---- QUINTO INTENTO: GRAFICAS UNIFICADAS ----
# Función para generar gráficos unificados con leyendas descriptivas
crear_grafico_unificado <- function(fila, etiquetas_x, output_dir) {
  # Extraer datos de la fila específica para cada conjunto
  graficandoH_fila <- graficandoH[fila, ]
  graficandoM_fila <- graficandoM[fila, ]
  graficandoTtl_fila <- GraficandoTtl[fila, ]
  
  # Valores para hombres, mujeres y total
  valores_H <- as.numeric(graficandoH_fila[7:18])
  valores_M <- as.numeric(graficandoM_fila[7:18])
  valores_Ttl <- as.numeric(graficandoTtl_fila[7:18])
  
  # Entidad y Municipio (asumiendo que son iguales en los tres subconjuntos)
  entidad <- as.character(graficandoH_fila[1])
  municipio <- as.character(graficandoH_fila[2])
  
  # Valores descriptivos
  valores_3_4 <- as.numeric(graficandoH_fila[3:4])   # Tasa Neta y Cobertura Total
  valores_5_6 <- as.numeric(graficandoH_fila[5:6])   # Población Total y Típica
  valores_5_6_formateados <- format(valores_5_6, big.mark = ",", scientific = FALSE)
  
  # Texto descriptivo para las leyendas
  texto_1_2 <- c("Entidad", "Municipio")
  texto_3_6 <- c("Tasa Neta", "Cobertura Total", "Población Total", "Población Típica")
  texto_leyenda_1_2 <- paste0(texto_1_2, ": ", c(entidad, municipio))
  texto_leyenda_3_4 <- paste0(texto_3_6[1:2], ": ", round(valores_3_4, 2))
  texto_leyenda_5_6 <- paste0(texto_3_6[3:4], ": ", valores_5_6_formateados)
  
  texto_izquierda <- c(texto_leyenda_1_2[1], texto_leyenda_3_4[1], texto_leyenda_3_4[2])
  texto_derecha <- c(texto_leyenda_1_2[2], texto_leyenda_5_6[1], texto_leyenda_5_6[2])
  
  # Colores
  colores_izquierda <- c("#161a1d", "#9b2247", "#a57f2c")
  colores_derecha <- c("#7e664a", "#611232", "#e6d194")
  
  # Rango del eje Y basado en Población Total
  rango_y <- c(
    min(valores_Ttl,valores_H,valores_M) - 500,
    max(valores_Ttl,valores_H,valores_M)*1.3
  )
  
  # Crear PDF
  archivo_pdf <- file.path(output_dir, paste0("graficas_unificadas_", entidad, "_", municipio, ".pdf"))
  pdf(file = archivo_pdf)
  
  # Crear el gráfico
  plot(
    etiquetas_x, valores_H,
    type = "b",               # Puntos y líneas
    col = "#161a1d",          # Color para hombres
    pch = 19,                 # Forma de los puntos
    lty = 1,                  # Tipo de línea sólida
    xlab = "Años",            # Etiqueta del eje X
    ylab = "Población",       # Etiqueta del eje Y
    ylim = rango_y,           # Rango del eje Y
    cex.lab = 0.8,            # Reducir tamaño de las etiquetas de los ejes
    cex.axis = 0.7,
    main = paste("Proyección de la Población de 17 a 19 años\n", entidad, "-", municipio)
  )
  
  # Añadir datos de mujeres
  lines(etiquetas_x, valores_M, type = "b", col = "#a57f2c", pch = 17, lty = 2)
  
  # Añadir datos de población total
  lines(etiquetas_x, valores_Ttl, type = "b", col = "#9b2247", pch = 15, lty = 3)
  
  # Leyendas de valores descriptivos
  legend(
    "topleft",
    legend = texto_izquierda,
    fill = colores_izquierda,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  legend(
    "topright",
    legend = texto_derecha,
    fill = colores_derecha,
    border = "black",
    bty = "n",
    cex = 0.6
  )
  
  # Leyenda de líneas y puntos
  legend(
    "bottomright",
    legend = c("Hombres", "Mujeres", "Total"),
    col = c("#161a1d", "#a57f2c", "#9b2247"),
    pch = c(19, 17, 15),
    lty = c(1, 2, 3),
    bty = "n",
    cex = 0.8
  )
  
  # Cerrar el archivo PDF
  dev.off()
}

# Iterar sobre cada fila y generar gráficos unificados
output_dir <- "GraficaProyeccionesEdad"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

etiquetas_x <- 2024:2035

for (i in 1:nrow(graficandoH)) {
  crear_grafico_unificado(
    fila = i,
    etiquetas_x = etiquetas_x,
    output_dir = output_dir
  )
}

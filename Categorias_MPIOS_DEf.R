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
#---- CREACION DE UNA NUEVA VARIABLE QUE INDICA LAS CATEGORIAS A QUE PERTENCE CONSIDERANDO LOS TRES METODOS ----
#misdatos$resultadosCategorias <- paste(misdatos$RNJ_3clases,misdatos$RNJ_4clases,misdatos$RNJ_5clases)
miBDD <- misdatos
View(miBDD)
#---- RESPALDO ----
write.csv(miBDD,"Repositorios/bb_indicadores_aumentada.csv")
colnames(miBDD)
miBDD  <- read_csv("Repositorios/bb_indicadores_aumentada.csv")
#---- RESUMIENDO LOS CAMBIOS REALIZADOS EN LA BDD misdatos ----
summary(miBDD)
View(miBDD)
New_miBDD <- miBDD[,2:54]; colnames(New_miBDD)
View(New_miBDD)
summary(New_miBDD)
colnames(New_miBDD)
nombres_col <- c("Entidad",
                 "Municipio",
                 "Secundarias_sin_TV",
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
                 "DEGETAYCM",
                 "DGETIS",
                 "AUTONOMAS",
                 "Bach Estatal",
                 "Bach General",
                 "Bach Interc Bilingüe",
                 "PARTICULAR",
                 "DGBETD",
                 "Ctro Bach Des Comunitario",
                 "CETI",
                 "Col Bachilleres",
                 "Col Bach Estatales",
                 "CECYTE",
                 "CONALEP",
                 "EMSAD",
                 "Esc Nal Ciegos",
                 "IEMS",
                 "INBAL",
                 "IPN",
                 "IEBAS",
                 "OTRAS SECRETARIAS",
                 "OTROS PROGRAMAS ESTATALES",
                 "PREECO",
                 "PREFECO",
                 "SECTEI",
                 "Telebachillerato",
                 "TBC")
colnames(New_miBDD) <- nombres_col
View(New_miBDD); summary(New_miBDD)
write.csv(New_miBDD, "Repositorios/bdd_corregida.csv")
miBDD <- New_miBDD
save.image("~/Documents/MacroReportes/WkspaceWorkingRight.RData")
#---- ORDENANDO POR CRITERIO DE TASA NETA DE MENOR A A MAYOR ---
mpios_criterios <- miBDD %>% arrange(Tasa_Neta)
View(mpios_criterios)
#---- CATEGORIA 1: MAYORES A 99,999 ----
mpios_categoria1 <- mpios_criterios %>%
  filter(Poblacion_Total >= 99999) %>% arrange(Tasa_Neta)
View(mpios_categoria1)
write.csv(mpios_categoria1,"Repositorios/Municipios_Categoria1.csv")
#---- CATEGORIA 2: MAYORES A 14,999 Y MENORES A 99,999 ----
mpios_categoria2 <- mpios_criterios %>%
  filter(Poblacion_Total >= 14999 & Poblacion_Total< 99999) %>%
  arrange(Tasa_Neta)
View(mpios_categoria2)
write.csv(mpios_categoria2,"Repositorios/Municipios_Categoria2.csv")
#---- CATEGORIA 3: MENORES A 14,999 ----
mpios_categoria3 <- mpios_criterios %>%
  filter(Poblacion_Total < 14999 ) %>%
  arrange(Tasa_Neta)
View(mpios_categoria3)
write.csv(mpios_categoria3,"Repositorios/Municipios_Categoria3.csv")
#---- AHORA GENERAREMOS LO MISMO PARA CADA UNO DE LOS ESTADOS ----
nombres_estados <- unique(miBDD$Entidad)
subbases <- list()
for (estado in nombres_estados) {
  subbases[[estado]] <- subset(miBDD, Entidad == estado)
}
for (estado in nombres_estados) {
  write.csv(subbases[[estado]], paste0("Repositorios/Estados/Indicadores_estatales_", estado, ".csv"), row.names = FALSE)
}
cat("Se generaron las subbases de datos por cada estado.")

save.image("WkspaceDefinitivo.RData")
#---- AHORA GENERAREMOS LAS GRAFICAS PARA LOS MUNICIPIOS ----
colnames(mpios_categoria1)
indices <- c(1,2,27:53)
mpios_cat1_subsistemas <- mpios_categoria1[,indices]
write.csv(mpios_cat1_subsistemas,"Repositorios/Municipios_Categoria1_Subsistemas.csv")
View(mpios_cat1_subsistemas)
#---- PLANTILLA PARA GRAFICAR SUBSISTEMAS PARA CATEGORIAS ----
##---- GENERACION DE LA PALETA DE COLORES ----
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
#---- VERSION DEFINITVA ----
##---- SE CARGAN LOS DATOS DE LA CATEGORIA 1 ----
colnames(mpios_categoria1)
indices <- c(1,2,27:53)
mpios_cat1_subsistemas <- mpios_categoria1[,indices]
write.csv(mpios_cat1_subsistemas,"Repositorios/Municipios_Categoria1_Subsistemas.csv")
data <- read.csv("Repositorios/Municipios_Categoria1_Subsistemas.csv")
output_dir <- "~/Documents/MacroReportes/Graficos_Cat1"
for (i in 1:nrow(data)) {
  entidad <- data[i, "Entidad"]
  municipio <- data[i, "Municipio"]
  valores <- data[i, 4:ncol(data)]  # Excluir las primeras tres columnas (Índice, Entidad y Municipio)
  valores_non_zero <- valores[valores != 0]
  names(valores_non_zero) <- names(valores)[valores != 0]  # Asignar los nombres correctamente
  df_plot <- data.frame(
    Subsistema = names(valores_non_zero),
    Cantidad = as.numeric(valores_non_zero)
  )
  num_colores <- length(df_plot$Subsistema)
  #paleta_ordenada_desc <- rev(paleta_extendida)
  #colores_grafico <- paleta_ordenada_desc[1:num_colores]
  colores_grafico <- sample(paleta_extendida, num_colores, replace = TRUE)
  p <- ggplot(df_plot, aes(x = reorder(Subsistema, -Cantidad), y = Cantidad)) +
    geom_bar(stat = "identity", fill = colores_grafico) +
    labs(title = paste("Entidad:", entidad),
         subtitle = paste("Municipio:", municipio),
         x = "Subsistema presente",
         y = "Cantidad") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  pdf_filename <- paste0(output_dir, "/Grafico_", entidad, "_", municipio, ".pdf")
  ggsave(filename = pdf_filename, plot = p, device = "pdf", width = 12, height = 6)
  cat("Archivo", i, "procesado: Grafico guardado en", pdf_filename, "\n")
}
##---- SE CARGAN LOS DATOS DE LA CATEGORIA 2 ----
colnames(mpios_categoria2)
indices <- c(1,2,27:53)
mpios_cat2_subsistemas <- mpios_categoria2[,indices]
write.csv(mpios_cat2_subsistemas,"Repositorios/Municipios_Categoria2_Subsistemas.csv")
data <- read.csv("Repositorios/Municipios_Categoria2_Subsistemas.csv")
output_dir <- "~/Documents/MacroReportes/Graficos_Cat2"
for (i in 1:nrow(data)) {
  entidad <- data[i, "Entidad"]
  municipio <- data[i, "Municipio"]
  valores <- data[i, 4:ncol(data)]  # Excluir las primeras tres columnas (Índice, Entidad y Municipio)
  valores_non_zero <- valores[valores != 0]
  names(valores_non_zero) <- names(valores)[valores != 0]  # Asignar los nombres correctamente
  df_plot <- data.frame(
    Subsistema = names(valores_non_zero),
    Cantidad = as.numeric(valores_non_zero)
  )
  num_colores <- length(df_plot$Subsistema)
  #paleta_ordenada_desc <- rev(paleta_extendida)
  #colores_grafico <- paleta_ordenada_desc[1:num_colores]
  colores_grafico <- sample(paleta_extendida, num_colores, replace = TRUE)
  p <- ggplot(df_plot, aes(x = reorder(Subsistema, -Cantidad), y = Cantidad)) +
    geom_bar(stat = "identity", fill = colores_grafico) +
    labs(title = paste("Entidad:", entidad),
         subtitle = paste("Municipio:", municipio),
         x = "Subsistema presente",
         y = "Cantidad") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  pdf_filename <- paste0(output_dir, "/Grafico_", entidad, "_", municipio, ".pdf")
  ggsave(filename = pdf_filename, plot = p, device = "pdf", width = 12, height = 6)
  cat("Archivo", i, "procesado: Grafico guardado en", pdf_filename, "\n")
}
##---- SE CARGAN LOS DATOS DE LA CATEGORIA 3 ----
colnames(mpios_categoria3)
indices <- c(1,2,27:53)
mpios_cat3_subsistemas <- mpios_categoria3[,indices]
write.csv(mpios_cat3_subsistemas,"Repositorios/Municipios_Categoria3_Subsistemas.csv")
data <- read.csv("Repositorios/Municipios_Categoria3_Subsistemas.csv")
output_dir <- "~/Documents/MacroReportes/Graficos_Cat3"
for (i in 1:nrow(data)) {
  entidad <- data[i, "Entidad"]
  municipio <- data[i, "Municipio"]
  valores <- data[i, 4:ncol(data)]  # Excluir las primeras tres columnas (Índice, Entidad y Municipio)
  valores_non_zero <- valores[valores != 0]
  names(valores_non_zero) <- names(valores)[valores != 0]  # Asignar los nombres correctamente
  df_plot <- data.frame(
    Subsistema = names(valores_non_zero),
    Cantidad = as.numeric(valores_non_zero)
  )
  num_colores <- length(df_plot$Subsistema)
  #paleta_ordenada_desc <- rev(paleta_extendida)
  #colores_grafico <- paleta_ordenada_desc[1:num_colores]
  colores_grafico <- sample(paleta_extendida, num_colores, replace = TRUE)
  p <- ggplot(df_plot, aes(x = reorder(Subsistema, -Cantidad), y = Cantidad)) +
    geom_bar(stat = "identity", fill = colores_grafico) +
    labs(title = paste("Entidad:", entidad),
         subtitle = paste("Municipio:", municipio),
         x = "Subsistema presente",
         y = "Cantidad") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  pdf_filename <- paste0(output_dir, "/Grafico_", entidad, "_", municipio, ".pdf")
  ggsave(filename = pdf_filename, plot = p, device = "pdf", width = 12, height = 6)
  cat("Archivo", i, "procesado: Grafico guardado en", pdf_filename, "\n")
}
##---- GENERACION DE UNA ANIMACION CON LOS ARCHIVOS EN CADA CATEGORIA ----
library(magick)
#install.packages("pdftools")
library(pdftools)
# Definir el directorio con los archivos
##---- CATEGORIA 1 ----
input_dir <- "~/Documents/MacroReportes/Graficos_Cat1"
output_gif <- "~/Documents/MacroReportes/Graficos_Cat1/animacion_graficos.gif"
archivos <- list.files(path = input_dir, pattern = "\\.pdf$", full.names = TRUE)
imagenes <- lapply(archivos, image_read_pdf)  # Usar image_read() si son imágenes como PNG/JPG
animacion <- image_animate(image_join(imagenes), fps = 5)  # Cambia 'fps' según la velocidad deseada
image_write(animacion, path = output_gif)
cat("Animación GIF guardada en:", output_gif, "\n")
##---- CATEGORIA 2 ----
input_dir <- "~/Documents/MacroReportes/Graficos_Cat2"
output_gif <- "~/Documents/MacroReportes/Graficos_Cat2/animacion_graficos.gif"
archivos <- list.files(path = input_dir, pattern = "\\.pdf$", full.names = TRUE)
imagenes <- lapply(archivos, image_read_pdf)  # Usar image_read() si son imágenes como PNG/JPG
animacion <- image_animate(image_join(imagenes), fps = 5)  # Cambia 'fps' según la velocidad deseada
image_write(animacion, path = output_gif)
cat("Animación GIF guardada en:", output_gif, "\n")
##---- CATEGORIA 3 ----
input_dir <- "~/Documents/MacroReportes/Graficos_Cat3"
output_gif <- "~/Documents/MacroReportes/Graficos_Cat3/animacion_graficos.gif"
archivos <- list.files(path = input_dir, pattern = "\\.pdf$", full.names = TRUE)
imagenes <- lapply(archivos, image_read_pdf)  # Usar image_read() si son imágenes como PNG/JPG
animacion <- image_animate(image_join(imagenes), fps = 5)  # Cambia 'fps' según la velocidad deseada
image_write(animacion, path = output_gif)
cat("Animación GIF guardada en:", output_gif, "\n")

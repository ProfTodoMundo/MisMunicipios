#---- CARGAMOS LAS LIBRERIAS A UTILIZAR ----
library(readxl)
library(dplyr)
#---- DEFINIMOS EL DIRECTORIO DE TRABAJO ----
setwd("~/Desktop/GraficasAuxiliares")
#---- LECTURA DE LOS DATOS ----
data1 <- read_excel("Indicadores _mun_corregida_verificada.xlsx")
colnames(data1)
View(data1)
colnames(data1) <- c("CV_Entidad",
                     "Entidad",
                     "CV_Mpio",
                     "Municipio",
                     "Llave",
                     "Tasa_Neta",
                     "Cobertura_Ttl",
                     "ADP",
                     "Pob_Ttl",
                     "Pob_Tipica",
                     "Pob_Tipica_3oSec_NoTermino",
                     "Egresados_Secundaria",
                     "Matricula_23-24",
                     "Nuevo_Ingreso_Primero",
                     "Demanda_Potencial",
                     "Numero_Demanda_Potencial",
                     "Lugares_Tasa_Neta",
                     "Grado_Marginacion_Entidad_20",
                     "Grado_Marginacion_Mpio_20",
                     "Grado_Accesibilidad_Ctos_Urbanos",
                     "Grado_Equipamiento",
                     "Grado_Calidad_Entorno",
                     "Grado_Intensidad_Migratoria",
                     "RNJ_clases",
                     "Est_Pob_15a19_H_24","Est_Pob_15a19_M_24","Ttl_Est_Pob_15a19_H_24",
                     "Est_Pob_15a19_H_25","Est_Pob_15a19_M_25","Ttl_Est_Pob_15a19_H_25",
                     "Est_Pob_15a19_H_26","Est_Pob_15a19_M_26","Ttl_Est_Pob_15a19_H_26",
                     "Est_Pob_15a19_H_27","Est_Pob_15a19_M_27","Ttl_Est_Pob_15a19_H_27",
                     "Est_Pob_15a19_H_28","Est_Pob_15a19_M_28","Ttl_Est_Pob_15a19_H_28",
                     "Est_Pob_15a19_H_29","Est_Pob_15a19_M_29","Ttl_Est_Pob_15a19_H_29",
                     "Est_Pob_15a19_H_30","Est_Pob_15a19_M_30","Ttl_Est_Pob_15a19_H_30",
                     "Est_Pob_15a19_H_31","Est_Pob_15a19_M_31","Ttl_Est_Pob_15a19_H_31",
                     "Est_Pob_15a19_H_32","Est_Pob_15a19_M_32","Ttl_Est_Pob_15a19_H_32",
                     "Est_Pob_15a19_H_33","Est_Pob_15a19_M_33","Ttl_Est_Pob_15a19_H_33",
                     "Est_Pob_15a19_H_34","Est_Pob_15a19_M_34","Ttl_Est_Pob_15a19_H_34",
                     "Est_Pob_15a19_H_35","Est_Pob_15a19_M_35","Ttl_Est_Pob_15a19_H_35",
                     "Tasa_Crecimiento")
View(data1)
#---- SELECCION DE LAS VARIABLES A CONSIDERAR ----
misdatos <- data1[,c("Entidad",
                     "Municipio",
                     "Tasa_Neta",
                     "Cobertura_Ttl",
                     "ADP",
                     "Pob_Ttl",
                     "Pob_Tipica",
                     "Pob_Tipica_3oSec_NoTermino",
                     "Egresados_Secundaria",
                     "Matricula_23-24",
                     "Nuevo_Ingreso_Primero",
                     "Demanda_Potencial",
                     "Numero_Demanda_Potencial",
                     "Lugares_Tasa_Neta",
                     "Grado_Marginacion_Entidad_20",
                     "Grado_Marginacion_Mpio_20",
                     "Grado_Accesibilidad_Ctos_Urbanos",
                     "Grado_Equipamiento",
                     "Grado_Calidad_Entorno",
                     "Grado_Intensidad_Migratoria",
                     "RNJ_clases",
                     "Est_Pob_15a19_H_24","Est_Pob_15a19_M_24","Ttl_Est_Pob_15a19_H_24",
                     "Est_Pob_15a19_H_25","Est_Pob_15a19_M_25","Ttl_Est_Pob_15a19_H_25",
                     "Est_Pob_15a19_H_26","Est_Pob_15a19_M_26","Ttl_Est_Pob_15a19_H_26",
                     "Est_Pob_15a19_H_27","Est_Pob_15a19_M_27","Ttl_Est_Pob_15a19_H_27",
                     "Est_Pob_15a19_H_28","Est_Pob_15a19_M_28","Ttl_Est_Pob_15a19_H_28",
                     "Est_Pob_15a19_H_29","Est_Pob_15a19_M_29","Ttl_Est_Pob_15a19_H_29",
                     "Est_Pob_15a19_H_30","Est_Pob_15a19_M_30","Ttl_Est_Pob_15a19_H_30",
                     "Est_Pob_15a19_H_31","Est_Pob_15a19_M_31","Ttl_Est_Pob_15a19_H_31",
                     "Est_Pob_15a19_H_32","Est_Pob_15a19_M_32","Ttl_Est_Pob_15a19_H_32",
                     "Est_Pob_15a19_H_33","Est_Pob_15a19_M_33","Ttl_Est_Pob_15a19_H_33",
                     "Est_Pob_15a19_H_34","Est_Pob_15a19_M_34","Ttl_Est_Pob_15a19_H_34",
                     "Est_Pob_15a19_H_35","Est_Pob_15a19_M_35","Ttl_Est_Pob_15a19_H_35",
                     "Tasa_Crecimiento")]
View(misdatos)
misdatos$Tasa_Neta <- misdatos$Tasa_Neta*100
misdatos$Cobertura_Ttl <- misdatos$Cobertura_Ttl*100
misdatos$ADP <- misdatos$ADP*100
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
write.csv(misdatos,"Repositorios/bb_indicadores.csv")
View(misdatos)
#---- INCORPORACION DE LA COLUMNA DE DEMANDA POTENCIAL (SIGUE PENDIENTE)----
#DemandaPotencial = 
#DemandaPotencial = matricula del ciclo pasado - egresados EMS ciclo anterior+ egresados de secundaria
#Numero de lugares segun la ADP =DemandaPotencial - DemandaPotencial*ATP
#multiplicar la tasa de ADP*DemandaPotencial
#---- INPUTACION DE VALORES NUMERICOS A LAS CATEGORIAS DE JENKS ----
##---- RUPTURAS DE JENKS PARA TRES CLASES ----
MisMaterias <- misdatos$RNJ_clases; 
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
misdatos$RNJ_clases <- TodasMaterias; 
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
misdatos$RNJ_clases <- factor(misdatos$RNJ_clases, levels = niveles, labels = ListaMaterias)
summary(misdatos$RNJ_clases)
#---- PROCESANDO NOMBRE GRADO MARGINACION MUNICIPIO ----
MisMaterias <- misdatos$Grado_Marginacion_Mpio_20; 
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
misdatos$Grado_Marginacion_Mpio_20 <- TodasMaterias; LasMaterias <- TodasMaterias
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
misdatos$Grado_Marginacion_Mpio_20 <- factor(misdatos$Grado_Marginacion_Mpio_20, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Marginacion_Mpio_20)
#---- PROCESANDO NOMBRE GRADO MARGINACION ENTIDAD ----
MisMaterias <- misdatos$Grado_Marginacion_Entidad_20; 
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
misdatos$Grado_Marginacion_Entidad_20 <- TodasMaterias; LasMaterias <- TodasMaterias
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
misdatos$Grado_Marginacion_Entidad_20 <- factor(misdatos$Grado_Marginacion_Entidad_20, levels = niveles, labels = ListaMaterias)
#---- PROCESANDO NOMBRE GRADO ACCESABILIDAD CTROS URBANOS ----
MisMaterias <- misdatos$Grado_Accesibilidad_Ctos_Urbanos; 
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
misdatos$Grado_Accesibilidad_Ctos_Urbanos <- TodasMaterias; LasMaterias <- TodasMaterias
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
misdatos$Grado_Accesibilidad_Ctos_Urbanos <- factor(misdatos$Grado_Accesibilidad_Ctos_Urbanos, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Accesibilidad_Ctos_Urbanos)
#---- PROCESANDO NOMBRE GRADO EQUIPAMIENTO ----
MisMaterias <- misdatos$Grado_Equipamiento; 
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
misdatos$Grado_Equipamiento <- TodasMaterias; LasMaterias <- TodasMaterias
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
misdatos$Grado_Equipamiento <- factor(misdatos$Grado_Equipamiento, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Equipamiento)
#---- PROCESANDO NOMBRE GRADO CALIDAD ENTORNO ----
MisMaterias <- misdatos$Grado_Calidad_Entorno; 
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
misdatos$Grado_Calidad_Entorno <- TodasMaterias; LasMaterias <- TodasMaterias
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
misdatos$Grado_Calidad_Entorno <- factor(misdatos$Grado_Calidad_Entorno, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Calidad_Entorno)
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
}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
factorizando <- factor(ListaMaterias, levels = niveles, labels=ListaMaterias)
misdatos$Grado_Intensidad_Migratoria <- factor(misdatos$Grado_Intensidad_Migratoria, levels = niveles, labels = ListaMaterias)
summary(misdatos$Grado_Intensidad_Migratoria)
#---- RESPALDO ----
write.csv(misdatos,"Repositorios/bb_indicadores.csv")
View(misdatos)
#---- CREACION DE UNA NUEVA VARIABLE QUE INDICA LAS CATEGORIAS A QUE PERTENCE CONSIDERANDO LOS TRES METODOS ----
#misdatos$resultadosCategorias <- paste(misdatos$RNJ_3clases,misdatos$RNJ_4clases,misdatos$RNJ_5clases)
miBDD <- misdatos[,c("Entidad",
                     "Municipio",
                     "Tasa_Neta",
                     "Cobertura_Ttl",
                     "ADP",
                     "Pob_Ttl",
                     "Pob_Tipica",
                     "Pob_Tipica_3oSec_NoTermino",
                     "Egresados_Secundaria",
                     "Matricula_23-24",
                     "Nuevo_Ingreso_Primero",
                     "Demanda_Potencial",
                     "Numero_Demanda_Potencial",
                     "Lugares_Tasa_Neta",
                     "RNJ_clases",
                     "Grado_Marginacion_Entidad_20",
                     "Grado_Marginacion_Mpio_20",
                     "Grado_Accesibilidad_Ctos_Urbanos",
                     "Grado_Equipamiento",
                     "Grado_Calidad_Entorno",
                     "Grado_Intensidad_Migratoria")]
colnames(miBDD) <- c("Entidad","Municipio","Tasa Neta",
                     "Cobertura Total","Atencion Demanda Potencial",
                     "Poblacion Total","Poblacion Tipica",
                     "Poblacion Tipica 3oSec No Termino",
                     "Egresados Secundaria","Matricula 23-24",
                     "Nuevo Ingreso Primero EMS","Demanda Potencial",
                     "Numero Lugares  Demanda Potencial","Numero Lugares Tasa Neta",
                     "Ruptura Numero Jenks","Grado Marginacion Entidad 2020",
                     "Grado Marginacion Municipio 2020","Grado Accesibilidad Centros Urbanos",
                     "Grado Equipamiento","Grado Calidad Entorno",
                     "Grado Intensidad Migratoria")
View(miBDD)
#---- RESPALDO ----
write.csv(miBDD,"Repositorios/bb_indicadores_mod.csv")
#---- RESUMIENDO LOS CAMBIOS REALIZADOS EN LA BDD misdatos ----
summary(miBDD)
View(miBDD)
#---- ORDENANDO POR CRITERIO DE TASA NETA DE MENOR A A MAYOR ---
mpios_criterios <- miBDD %>% arrange(`Tasa Neta`)
View(mpios_criterios)
#---- CATEGORIA 1: MAYORES A 99,999 ----
mpios_categoria1 <- mpios_criterios %>%
  filter(`Poblacion Total` >= 99999) %>% arrange(`Tasa Neta`)
View(mpios_categoria1)
write.csv(mpios_categoria1,"Repositorios/Municipios_Categoria1.csv")
#---- CATEGORIA 2: MAYORES A 14,999 Y MENORES A 99,999 ----
mpios_categoria2 <- mpios_criterios %>%
  filter(`Poblacion Total` >= 14999 & `Poblacion Total` < 99999) %>%
  arrange(`Tasa Neta`)
View(mpios_categoria2)
write.csv(mpios_categoria2,"Repositorios/Municipios_Categoria2.csv")
#---- CATEGORIA 3: MENORES A 14,999 ----
mpios_categoria3 <- mpios_criterios %>%
  filter(`Poblacion Total` < 14999 ) %>%
  arrange(`Tasa Neta`)
View(mpios_categoria3)
write.csv(mpios_categoria3,"Repositorios/Municipios_Categoria3.csv")
#---- AHORA GENERAREMOS LO MISMO PARA CADA UNO DE LOS ESTADOS ----
nombres_estados <- unique(miBDD$Entidad)
subbases <- list()
for (estado in nombres_estados) {
  subbases[[estado]] <- subset(miBDD, Entidad == estado)
}
for (estado in nombres_estados) {
  write.csv(subbases[[estado]], paste0("Repositorios/Indicadores_estatales_", estado, ".csv"), row.names = FALSE)
}
cat("Se generaron las subbases de datos por cada estado.")
#---- DATOS DE POBLACION TOTAL Y POR GENERO ----
summary(data1)
View(data1)
datosPob <- misdatos[,c("Entidad","Municipio","Tasa_Neta","Cobertura_Ttl","Pob_Ttl","Pob_Tipica",
                        "Est_Pob_15a19_H_24","Est_Pob_15a19_M_24","Ttl_Est_Pob_15a19_H_24",
                        "Est_Pob_15a19_H_25","Est_Pob_15a19_M_25","Ttl_Est_Pob_15a19_H_25",
                        "Est_Pob_15a19_H_26","Est_Pob_15a19_M_26","Ttl_Est_Pob_15a19_H_26",
                        "Est_Pob_15a19_H_27","Est_Pob_15a19_M_27","Ttl_Est_Pob_15a19_H_27",
                        "Est_Pob_15a19_H_28","Est_Pob_15a19_M_28","Ttl_Est_Pob_15a19_H_28",
                        "Est_Pob_15a19_H_29","Est_Pob_15a19_M_29","Ttl_Est_Pob_15a19_H_29",
                        "Est_Pob_15a19_H_30","Est_Pob_15a19_M_30","Ttl_Est_Pob_15a19_H_30",
                        "Est_Pob_15a19_H_31","Est_Pob_15a19_M_31","Ttl_Est_Pob_15a19_H_31",
                        "Est_Pob_15a19_H_32","Est_Pob_15a19_M_32","Ttl_Est_Pob_15a19_H_32",
                        "Est_Pob_15a19_H_33","Est_Pob_15a19_M_33","Ttl_Est_Pob_15a19_H_33",
                        "Est_Pob_15a19_H_34","Est_Pob_15a19_M_34","Ttl_Est_Pob_15a19_H_34",
                        "Est_Pob_15a19_H_35","Est_Pob_15a19_M_35","Ttl_Est_Pob_15a19_H_35")]
View(datosPob)
Estados <- datosPob[1,c("Entidad","Municipio",
                       "Est_Pob_15a19_H_24","Est_Pob_15a19_M_24","Ttl_Est_Pob_15a19_H_24",
                       "Est_Pob_15a19_H_25","Est_Pob_15a19_M_25","Ttl_Est_Pob_15a19_H_25",
                       "Est_Pob_15a19_H_26","Est_Pob_15a19_M_26","Ttl_Est_Pob_15a19_H_26",
                       "Est_Pob_15a19_H_27","Est_Pob_15a19_M_27","Ttl_Est_Pob_15a19_H_27",
                       "Est_Pob_15a19_H_28","Est_Pob_15a19_M_28","Ttl_Est_Pob_15a19_H_28",
                       "Est_Pob_15a19_H_29","Est_Pob_15a19_M_29","Ttl_Est_Pob_15a19_H_29",
                       "Est_Pob_15a19_H_30","Est_Pob_15a19_M_30","Ttl_Est_Pob_15a19_H_30",
                       "Est_Pob_15a19_H_31","Est_Pob_15a19_M_31","Ttl_Est_Pob_15a19_H_31",
                       "Est_Pob_15a19_H_32","Est_Pob_15a19_M_32","Ttl_Est_Pob_15a19_H_32",
                       "Est_Pob_15a19_H_33","Est_Pob_15a19_M_33","Ttl_Est_Pob_15a19_H_33",
                       "Est_Pob_15a19_H_34","Est_Pob_15a19_M_34","Ttl_Est_Pob_15a19_H_34",
                       "Est_Pob_15a19_H_35","Est_Pob_15a19_M_35","Ttl_Est_Pob_15a19_H_35")]


# Definimos los colores y sus valores
colores <- c("AMARILLO" = "yellow", "AZUL" = "blue", "CELESTE" = "lightblue", 
             "NARANJA" = "orange", "ROJO" = "red", 
             "VERDE CLARO" = "lightgreen", "VERDE FUERTE" = "green")

valores <- c(559, 158, 540, 320, 439, 237)

# Nombres de los colores para las etiquetas
nombres <- names(colores)
pdf("Graficas/Colores.pdf")
# Configuración de la ventana de gráficos
par(mfrow = c(1, 6), mar = c(1, 1, 2, 1))

# Crear los recuadros con colores y valores
for (i in 1:length(valores)) {
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  rect(0.3, 0.3, 0.7, 0.7, col = colores[i], border = "black") # Tamaño del recuadro ajustado
  text(0.5, 0.5, labels = valores[i], cex = 0.8, col = "black") # Tamaño de la letra reducido
  title(nombres[i], line = -1, cex.main = 0.8) # Posición y tamaño de la etiqueta ajustado
}
dev.off()

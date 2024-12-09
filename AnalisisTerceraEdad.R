##---- RESPALDO ----
setwd("~/Documents/GitHub/LicCienciaDatosbis")
library(readxl)
library(dplyr)
library(xtable)
# ----  Lectura de la BDD de la plantilla ----
##---- Preprocesamiento de datos ----
Dataset <- read_excel("00-NA-20240308_PLANTILLA_1ERA_MARZO_2024.xlsx")
View(Dataset)
head(Dataset)
##---- Preprocesamiento de datos ----
nombres <- colnames(Dataset); print(nombres)
nomcol <- c('Num', 'Folio','Plaza','Nombre','Nivel','Puesto','Nombramiento','Tiempo','Coordinacion',
            'Direccion','Departamento','Sede','Academia','Estatus','Situacion','CCT',
            'Regimen','Ingreso','FechaNacimiento','Edad','InicioUM','FinalUM')
colnames(Dataset)<- nomcol
write.csv(Dataset,"CSVFiles/BDDPlantilla.csv")
##---- Creacion subbase de datos ----
Data1 <- Dataset[,c('Nivel','Puesto','Nombramiento','Tiempo','Coordinacion',
                    'Direccion','Departamento','Sede','Academia','Estatus','Situacion','CCT',
                    'Regimen','Ingreso','FechaNacimiento','Edad')]; 
View(Data1)
write.csv(Data1,"CSVFiles/SBDDPlantilla.csv")
##--- Codificacion de variables ----
Data1$Nivel <- factor(Data1$Nivel,
                      levels = c('16','18','20','20.5','29.5','30','34.5','45.5','85.6','85.7'),
                      labels = c('16','18','20','20.5','29.5','30','34.5','45.5','85.6','85.7'))
summary(Data1)
Data1$Tiempo <- factor(Data1$Tiempo,
                       levels = c('CUARTO DE TIEMPO','MEDIO TIEMPO','NO APLICA','TIEMPO COMPLETO'),
                       labels = c('CT','MT','NA','TC'))
summary(Data1$Tiempo)                       
Data1$Sede <- factor(Data1$Sede,
                     levels = c('CENTRO HISTORICO','GARCIA DIEGO','DEL VALLE','CASA LIBERTAD',
                                'SAN LORENZO TEZONCO','CUAUTEPEC','CASA TALAVERA','CENTRO VLADY'),
                     labels = c('CH','GADI','DV','CL','SLT','CUAU','TALAVERA','VLADY'))
summary(Data1$Sede)
Data1$Estatus <- factor(Data1$Estatus,levels = c('ACTIVO','LICENCIA','VACANTE'),labels = c('ACTIVO','LICENCIA','VACANTE'))
summary(Data1$Estatus)
Data1$Regimen <- factor(Data1$Regimen,levels = c('ASIMILADOS','LABORAL'),labels = c('ASIMILADOS','LABORAL'))
summary(Data1$Regimen)
Data1$Situacion <- factor(Data1$Situacion,
                          levels = c('NORMAL','AÑO SABATICO','COMISION - DESTINO','SEMESTRE SABATICO','INTERINATO - DESTINO'),
                          labels = c('NORMAL','SABATICO','COMDEST','SEMSABATICO','INTDEST'))
summary(Data1$Situacion)
Data1$CCT <- factor(Data1$CCT,levels = c('BASE','CONFIANZA','NO APLICA'),labels = c('BASE','CONFIANZA','NA'))
summary(Data1$CCT)
##---- Codificando variable Puesto ----
MisMaterias <- Data1$Puesto; n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
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
  MateriasCorrected <- gsub(":","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
Data1$Puesto <- TodasMaterias; LasMaterias <- TodasMaterias; todasmismaterias <- sort(TodasMaterias); 
TodasMaterias <- todasmismaterias; head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){materia <- Materia2;}else{ListaMaterias[k]<- Materia2;materia <- Materia2; k <- k+1}}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp);
Data1$Puesto <- factor(Data1$Puesto,levels = niveles, labels = ListaMaterias);
tempo <- as.data.frame(table(Data1$Puesto)); #View(tempo)
##---- Codificando variable Nombramiento ----
MisMaterias <- Data1$Nombramiento; 
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
  MateriasCorrected <- gsub(":","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
Data1$Nombramiento <- TodasMaterias;LasMaterias <- TodasMaterias;todasmismaterias <- sort(TodasMaterias); 
TodasMaterias <- todasmismaterias;head(TodasMaterias,15); tail(TodasMaterias,15)
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){materia <- Materia2;}else{ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1}}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
Data1$Nombramiento <- factor(Data1$Nombramiento, levels = niveles, labels = ListaMaterias)
tempo <- as.data.frame(table(Data1$Nombramiento)); #View(tempo)
##---- Codificando variable Coordinacion ----
MisMaterias <- Data1$Coordinacion; n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
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
  MateriasCorrected <- gsub(":","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
Data1$Coordinacion <- TodasMaterias; LasMaterias <- TodasMaterias;
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias; 
head(TodasMaterias,15); tail(TodasMaterias,15);materia <- TodasMaterias[1]; 
n <- length(TodasMaterias); k <- 2;ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){materia <- Materia2;}else{ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1}}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
Data1$Coordinacion <- factor(Data1$Coordinacion, levels = niveles, labels = ListaMaterias)
tempo <- as.data.frame(table(Data1$Coordinacion)); #View(tempo)
##---- Codificando variable Direccion ----
MisMaterias <- Data1$Direccion;n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
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
  MateriasCorrected <- gsub(":","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
Data1$Direccion <- TodasMaterias; LasMaterias <- TodasMaterias;
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias; 
head(TodasMaterias,15); tail(TodasMaterias,15);
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i];
  if(materia==Materia2){materia <- Materia2;}else{ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1}}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
Data1$Direccion <- factor(Data1$Direccion, levels = niveles, labels = ListaMaterias)
tempo <- as.data.frame(table(Data1$Direccion)); #View(tempo)
##---- Codificando variable Departamento ----
MisMaterias <- Data1$Departamento; n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
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
  MateriasCorrected <- gsub(":","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
Data1$Departamento <- TodasMaterias; LasMaterias <- TodasMaterias
todasmismaterias <- sort(TodasMaterias); TodasMaterias <- todasmismaterias; 
head(TodasMaterias,15); tail(TodasMaterias,15);materia <- TodasMaterias[1]; 
n <- length(TodasMaterias); k <- 2;ListaMaterias <- c(); ListaMaterias[1] <- materia
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){materia <- Materia2;}else{ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1}}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
Data1$Departamento <- factor(Data1$Departamento,levels = niveles, labels = ListaMaterias)
tempo <- as.data.frame(table(Data1$Departamento)); #View(tempo)
##---- Codificando variable Academias ----
MisMaterias <- Data1$Academia; n <- length(MisMaterias); TodasMaterias <- as.character(MisMaterias)
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
  MateriasCorrected <- gsub(":","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ARTE Y PATRIMONIO CULTURAL","AyPC",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CENTRO DE ESTUDIOS SOBRE LA CIUDAD","CESC",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CIENCIA POLITICA Y ADMINISTRACION URBANA","CPyAU",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CIENCIAS AMBIENTALES","CiAmbientales",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("BIOLOGIA HUMANA","BioHumana",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CIENCIAS DE LA COMPLEJIDAD","CComplejidad",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CIENCIAS GENOMICAS","CGenomicas",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CIENCIAS SOCIALES","CiSoc",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("COMUNICACION Y CULTURA","ComyCult",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CREACION LITERARIA","CREACION",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CULTURA CIENTIFICO HUMANISTICA","CCYH",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("CULTURA CIENTIFICO-HUMANISTICA","CCYH",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("DERECHOS HUMANOS","DDHH",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("DINAMICA NO LINEAL Y SISTEMAS COMPLEJOS","DNLySC",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("ESTUDIOS SOCIALES E HISTORICOS","ESeH",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("EXPRESION ORAL Y ESCRITA","EOyE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("FILOSOFIA E HISTORIA DE LAS IDEAS","FeHI",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("HISTORIA Y SOCIEDAD CONTEMPORANEA","HySC",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("IDENTIDAD CONOCIMIENTO Y APRENDIZAJE","ICA",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("INGENIERIA EN SISTEMAS DE TRANSPORTE URBANO","ISTU",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("INGENIERIA EN SISTEMAS ELECTRONICOS INDUSTRIALES","ISEI",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("INGENIERIA EN SISTEMAS ELECTRONICOS Y DE TELECOMUNICACIONES","ISET",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("LENGUAJE Y PENSAMIENTO","LyP",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("MAESTRIA EN EDUCACION AMBIENTAL","MsEdAmb",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("NUTRICION Y SALUD","Nutricion",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("POSGRADO","Posg",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("HUMANIDADES","Hum",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("EN","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("PROGRAMA AMBIENTAL","ProgAmbiental",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("PROGRAMA","Prog",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("PROMOCION DE LA SALUD","PS",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("PROTECCION CIVIL Y GESTION DE RIESGOS","PCyGR",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("PSICOLOGIA","PSIC",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("SALUD COMUNITARIA","SaludCom",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("  ","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(" Y ","y",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(" DE ERGIA","Energia",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(" DE ESTUDIOS DE LA CIUDAD","EstCd",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("AMBITAL","Ambiental",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
Data1$Academia <- TodasMaterias; LasMaterias <- TodasMaterias;todasmismaterias <- sort(TodasMaterias); 
TodasMaterias <- todasmismaterias; head(TodasMaterias,15); tail(TodasMaterias,15);
materia <- TodasMaterias[1]; n <- length(TodasMaterias); k <- 2;
ListaMaterias <- c(); ListaMaterias[1] <- materia;
for(i in 2:n){
  Materia2 <- TodasMaterias[i]
  if(materia==Materia2){materia <- Materia2;}else{ListaMaterias[k]<- Materia2; materia <- Materia2; k <- k+1}}
n <- length(ListaMaterias); temp <- factor(ListaMaterias); niveles <- levels(temp)
Data1$Academia <- factor(Data1$Academia, levels = niveles, labels = ListaMaterias)
tempo <- as.data.frame(table(Data1$Academia)); #View(tempo)
##---- Codificando fecha de ingreso ----
temp <- Data1$Ingreso; n <- length(temp); tail(temp); anhoIng <- c(); 
messIng <- c(); LocMenos  <- unlist(gregexpr("/", temp[1]));Checking <- c();
for(i in 1:n){
  anhoIng[i] <- as.numeric(substring(temp[i], LocMenos[2]+1, 10)); 
  messIng[i] <- as.numeric(substring(temp[i], LocMenos[1]+1, 5));
  if(is.na(anhoIng[i])==TRUE){anhoIng[i] <- 0;}
  if(is.na(messIng[i])==TRUE){messIng[i] <- 0;}}
head(anhoIng,15);print(anhoIng); tail(anhoIng); head(messIng,15);print(messIng)
Checking <- cbind(messIng,anhoIng);Data1  <- cbind(Data1,Checking);
##---- Codificando fecha de nacimiento ----
temp <- Data1$FechaNacimiento; n <- length(temp); tail(temp)
anhoNac <- c(); messNac <- c(); LocMenos  <- unlist(gregexpr("/", temp[1]));Checking <- c();
for(i in 1:n){
  anhoNac[i] <- as.numeric(substring(temp[i], LocMenos[2]+1, 10)); 
  messNac[i] <- as.numeric(substring(temp[i], LocMenos[1]+1, 5));
  if(is.na(anhoNac[i])==TRUE){anhoNac[i] <- 0;}
  if(is.na(messNac[i])==TRUE){messNac[i] <- 0;}}
head(anhoNac,15);print(anhoNac); tail(anhoNac); head(messNac,15);print(messNac)
Checking <- cbind(messNac,anhoNac); Data1  <- cbind(Data1,Checking);
write.csv(Data1,"CSVFiles/SBDDPlantilla.csv")
##---- Codificando fecha edad ----
temp <- Data1$Edad; n <- length(temp); tail(temp)
for(i in 1:n){if(is.na(temp[i])==TRUE){temp[i] <- 0;}}
head(temp,15);print(temp); tail(temp);Data1$Edad <- temp; summary(Data1); View(Data1)
##---- Codificando Antiguedad laboral----
temp <- Data1$anhoIng; AnhoActual <- 2024;
AntiguedadLaboral <- AnhoActual -temp
Data1  <- cbind(Data1,AntiguedadLaboral);
View(Data1)
##---- Procesamiento general de datos: Incorporacion Costo ----
NivelSalarial <-  Data1$Nivel; n<- length(NivelSalarial); CS <- 0;
# Crear la matriz con las entradas proporcionadas
matrizCosto<- matrix(c(16, 246125.08,
                       18, 387382.86,
                       20, 531683.40,
                       20.5, 591428.99,
                       85.6, 746499.77,
                       85.7, 771827.51,
                       29.5, 1069033.61), 
                     ncol = 2, byrow = TRUE)
print(matrizCosto); CostoSal <- c()
for(i in 1:n){
  if(NivelSalarial[i] == matrizCosto[1,1]){CostoSal[i] <- matrizCosto[1,2]}
  if(NivelSalarial[i] == matrizCosto[2,1]){CostoSal[i] <- matrizCosto[2,2]}
  if(NivelSalarial[i] == matrizCosto[3,1]){CostoSal[i] <- matrizCosto[3,2]}
  if(NivelSalarial[i] == matrizCosto[4,1]){CostoSal[i] <- matrizCosto[4,2]}
  if(NivelSalarial[i] == matrizCosto[5,1]){CostoSal[i] <- matrizCosto[5,2]}  
  if(NivelSalarial[i] == matrizCosto[6,1]){CostoSal[i] <- matrizCosto[6,2]}
  if(NivelSalarial[i] == matrizCosto[7,1]){CostoSal[i] <- matrizCosto[7,2]}
}
SBDD1<- cbind(Data1,CostoSal); View(SBDD1)
write.csv(SBDD1,"CSVFiles/SBDDPlantilla.csv")
View(SBDD1)


##---- RESPALDO ----
save.image("Wkspaces/Wkspcplantilla2.RData")
#setwd("~/Documents/GitHub/LicCienciaDatosbis")
#load('Wkspaces/Wkspc2.RData')
#library(readxl)
#library(dplyr)
#library(xtable)
##---- Subbase de datos: Activo y Profesor Investigador ----
PlantillaActiva <- SBDD1  %>% filter(SBDD1$Estatus=='ACTIVO'& SBDD1$Puesto =='PROFESOR INVESTIGADOR')
write.csv(PlantillaActiva,"CSVFiles/PLANTILLACTIVA.csv")
View(PlantillaActiva)
PlantillaProf <- PlantillaActiva[,c("Direccion","Academia","Sede","Edad","AntiguedadLaboral")];
View(PlantillaProf)
write.csv(PlantillaProf,"CSVFiles/SBDDProf.csv")
##---- Contando Personal academico por edad ----
PlantillaMayorEdad <- PlantillaProf%>%filter(PlantillaProf$Edad>=65); View(PlantillaMayorEdad)
contando <- as.data.frame(table(PlantillaMayorEdad$Sede,PlantillaMayorEdad$Edad))
FilteredCont <- contando[contando$Freq!=0,]; View(FilteredCont)
colnames(FilteredCont) <- c('Plantel','Edad','Activos')
FilteredCont <- FilteredCont %>% arrange(desc(Edad)); View(FilteredCont)

print(xtable(FilteredCont, caption = "Tabla de distribución por Edad y Sede"), caption.placement = "top")
FilteredCont_table <- print(xtable(FilteredCont, caption = "Tabla de distribución por Edad y Sede"), caption.placement = "top")
writeLines(as.character(FilteredCont_table), "Tablas/LicenciaturaSedeEdad.tex")
write.csv(FilteredCont,"CSVFiles/soloTerceraEdad.csv")
write.csv(PlantillaMayorEdad,"CSVFiles/PLANTILL3aEdad.csv")
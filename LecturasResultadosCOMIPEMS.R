#---- 1. LECTURA DE LAS LIBRERIAS QUE SE VAN A UTILIZAR ----
library(readxl)
library(lubridate)
#---- 1.1 DETERMINACION DEL DIRECTORIO DE TRABAJO ----
setwd("~/Documents/BDD_CENEVAL")
#---- 1.2 LECTURA DE LAS BASE DE RESULTADOS COMIPEMS 22, 23 Y 24 ----
data22 <- read_excel("Base_Comipems_2022/METRO_2022_FINAL_SEMS.xlsx")
data23 <- read_excel("Base_Comipems_2023/METRO_2023.xlsx")
data24 <- read_excel("Base_Comipems_2024/RESULTADOS_FINAL_2024_SEMS.xlsx")
#---- 2. RESUMEN Y VISUALIZACION DE LAS BDD ----
summary(data22);View(data22)
summary(data23);View(data23)
summary(data24);View(data24)
#---- 3. RESPALDO 1 ----
write.csv(data22,"Repositorios/Resultados_COMIPEMS_22.csv")
saveRDS(data22, file = "Repositorios/vResultadosCOMIPEMS_22.rds")
write.csv(data23,"Repositorios/Resultados_COMIPEMS_23.csv")
saveRDS(data23, file = "Repositorios/ResultadosCOMIPEMS_23.rds")
write.csv(data24,"Repositorios/Resultados_COMIPEMS_24.csv")
saveRDS(data24, file = "Repositorios/ResultadosCOMIPEMS_24.rds")
save.image("Wkspaces/Wkspace1.RData")
#---- 4. PREPROCESAMIENTO DE LAS BDD's ---- 
cols2check <- c("SEXO","COLONIA","CP","CVE_ALCMUN","CATEGO_ASP",
                "STAT_CERT","PROMEDIO","FECHA_CERT","FOL_CERT","NUM_OPC",
                "OPC_ED01", "OPC_ED02","OPC_ED03","OPC_ED04","OPC_ED05",
                "OPC_ED06","OPC_ED07","OPC_ED08","OPC_ED09","OPC_ED10",
                "OPC_ED11","OPC_ED12","OPC_ED13","OPC_ED14","OPC_ED15",
                "OPC_ED16","OPC_ED17","OPC_ED18","OPC_ED19","OPC_ED20",
                "PRE_EXA","NGLOBAL","PNGLOBAL","NO_PRES","NOPC_ASI",
                "COPC_ASI","CVEINS_ASI","CVESUB_ASI","INST_ASI","SUBS_ASI")
subBDD <- data24[,cols2check]; View(subBDD)
subBDD10 <- subBDD[subBDD$NUM_OPC >= 10, ]; View(subBDD10)
year_vector <- as.numeric(format(as.Date(subBDD10$FECHA_CERT, format = "%d-%m-%Y"), "%Y"))
print(head(year_vector, 10))  # Muestra los primeros 10 años extraídos
subBDD10$AnhoCert <- year_vector
#---- 5. GENERACION DE CATEGORIAS PARA LAS OPCIONES EDUCATIVAS ---
##---- 5.1 OPCION EDUCATIVA 1----
niveles <- unique(sort(subBDD10$OPC_ED01));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED01 <- factor(subBDD10$OPC_ED01, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.2 OPCION EDUCATIVA 2----
niveles <- unique(sort(subBDD10$OPC_ED02));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED02 <- factor(subBDD10$OPC_ED02, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.3 OPCION EDUCATIVA 3----
niveles <- unique(sort(subBDD10$OPC_ED03));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED03 <- factor(subBDD10$OPC_ED03, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.4 OPCION EDUCATIVA 4----
niveles <- unique(sort(subBDD10$OPC_ED04));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED04 <- factor(subBDD10$OPC_ED04, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.5 OPCION EDUCATIVA 5----
niveles <- unique(sort(subBDD10$OPC_ED05));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED05 <- factor(subBDD10$OPC_ED05, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.6 OPCION EDUCATIVA 6----
niveles <- unique(sort(subBDD10$OPC_ED06));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED06 <- factor(subBDD10$OPC_ED06, levels = niveles, labels = ListaMaterias)
summary(subBDD10$OPC_ED06)
##---- 5.7 OPCION EDUCATIVA 7----
niveles <- unique(sort(subBDD10$OPC_ED07));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED07 <- factor(subBDD10$OPC_ED07, levels = niveles, labels = ListaMaterias)
summary(subBDD10$OPC_ED07)
##---- 5.8 OPCION EDUCATIVA 8----
niveles <- unique(sort(subBDD10$OPC_ED08));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED08 <- factor(subBDD10$OPC_ED08, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.9 OPCION EDUCATIVA 9----
niveles <- unique(sort(subBDD10$OPC_ED09));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED09 <- factor(subBDD10$OPC_ED09, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.10 OPCION EDUCATIVA 10----
niveles <- unique(sort(subBDD10$OPC_ED10));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED10 <- factor(subBDD10$OPC_ED10, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.11 OPCION EDUCATIVA 11----
niveles <- unique(sort(subBDD10$OPC_ED11));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED11 <- factor(subBDD10$OPC_ED11, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.12 OPCION EDUCATIVA 12----
niveles <- unique(sort(subBDD10$OPC_ED12));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED12 <- factor(subBDD10$OPC_ED12, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.13 OPCION EDUCATIVA 13----
niveles <- unique(sort(subBDD10$OPC_ED13));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED13 <- factor(subBDD10$OPC_ED13, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.14 OPCION EDUCATIVA 14----
niveles <- unique(sort(subBDD10$OPC_ED14));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED14 <- factor(subBDD10$OPC_ED14, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.15 OPCION EDUCATIVA 15----
niveles <- unique(sort(subBDD10$OPC_ED15));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED15 <- factor(subBDD10$OPC_ED15, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.16 OPCION EDUCATIVA 16----
niveles <- unique(sort(subBDD10$OPC_ED16));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED16 <- factor(subBDD10$OPC_ED16, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.17 OPCION EDUCATIVA 17----
niveles <- unique(sort(subBDD10$OPC_ED17));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED17 <- factor(subBDD10$OPC_ED17, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.18 OPCION EDUCATIVA 18----
niveles <- unique(sort(subBDD10$OPC_ED18));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED18 <- factor(subBDD10$OPC_ED18, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.19 OPCION EDUCATIVA 19----
niveles <- unique(sort(subBDD10$OPC_ED19));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED19 <- factor(subBDD10$OPC_ED19, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
##---- 5.20 OPCION EDUCATIVA 20----
niveles <- unique(sort(subBDD10$OPC_ED20));    ListaMaterias <- as.character(niveles);  
subBDD10$OPC_ED20 <- factor(subBDD10$OPC_ED20, levels = niveles, labels = ListaMaterias)
summary(subBDD10)
#---- 6. RESPALDO 2----
write.csv(subBDD10,"Repositorios/SubBDDComipems24_10.csv")
saveRDS(subBDD10, file = "Repositorios/SubBDDComipems24_10.rds")
save.image("Wkspaces/Wkspace2.RData")
#---- 7. CLAVE CATEGORIA DEL ASPIRANTE ----
subBDD10$CATEGO_ASP <- factor(subBDD10$CATEGO_ASP, levels = c("E","F","I","Z"), 
                            labels = c("Egresado","Foraneo","INEA","Local"))
subBDD10$STAT_CERT <- factor(subBDD10$STAT_CERT,level = c("I","R"),
                             labels = c("Irregular","Regular"))
subBDD10$PRE_EXA <- factor(subBDD10$PRE_EXA, levels = c("N","S"), labels = c("No","Si"))
subBDD10$NO_PRES <- factor(subBDD10$NO_PRES, levels = c("NP","SP"), labels = c("No Presento","Si Presento"))
niveles <- unique(sort(subBDD10$NOPC_ASI));    ListaMaterias <- as.character(niveles);  
subBDD10$NOPC_ASI <- factor(subBDD10$NOPC_ASI, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$COPC_ASI));    ListaMaterias <- as.character(niveles);  
subBDD10$COPC_ASI <- factor(subBDD10$COPC_ASI, levels = niveles, labels = ListaMaterias)
#---- 8. RESPALDO 3----
write.csv(subBDD10,"Repositorios/SubBDDComipems24_10.csv")
saveRDS(subBDD10, file = "Repositorios/SubBDDComipems24_10.rds")
save.image("Wkspaces/Wkspace3.RData")
#---- 9. SEGUIMOS CODIFICANDO ----
niveles <- unique(sort(subBDD10$CVEINS_ASI));    ListaMaterias <- as.character(niveles);  
subBDD10$CVEINS_ASI <- factor(subBDD10$CVEINS_ASI, levels = niveles, labels = ListaMaterias)
subBDD10$CVEINS_ASI <- factor(subBDD10$CVEINS_ASI, 
                              levels = c("A3","B0","C1","D4","G2","I5","M9",
                                         "S0","S1","S2","S4","S5","S7","S8",
                                         "U6"), 
                              labels = c("DGETAyCM","COLBACH","CONALEP","DGETI",
                                         "DGB","IPN","UAEM","SECTI_COBAEM",
                                         "SECTI_CONALEP","SECTI_TELEBACHILLERATO",
                                         "SECTI CBT","SECTI CECYTEM",
                                         "SECTI_PO","SECTI_PO","UNAM"))
niveles <- unique(sort(subBDD10$CVESUB_ASI));    ListaMaterias <- as.character(niveles);  
subBDD10$CVESUB_ASI <- factor(subBDD10$CVESUB_ASI, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$INST_ASI));    ListaMaterias <- as.character(niveles);  
subBDD10$INST_ASI <- factor(subBDD10$INST_ASI, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$SUBS_ASI));    ListaMaterias <- as.character(niveles);  
subBDD10$SUBS_ASI <- factor(subBDD10$SUBS_ASI, levels = niveles, labels = ListaMaterias)
#---- 10. RESPALDO 4 ---
write.csv(subBDD10,"Repositorios/SubBDDComipems24_10.csv")
saveRDS(subBDD10, file = "Repositorios/SubBDDComipems24_10.rds")
save.image("Wkspaces/Wkspace4.RData")
#---- 11. CONTINUAMOS PROCESANDO ----
niveles <- unique(sort(subBDD10$SEXO));    ListaMaterias <- as.character(niveles);  
subBDD10$SEXO <- factor(subBDD10$SEXO, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$COLONIA));    ListaMaterias <- as.character(niveles);  
subBDD10$COLONIA <- factor(subBDD10$COLONIA, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$CP));    ListaMaterias <- as.character(niveles);  
subBDD10$CP <- factor(subBDD10$CP, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$CVE_ALCMUN));    ListaMaterias <- as.character(niveles);  
subBDD10$CVE_ALCMUN <- factor(subBDD10$CVE_ALCMUN, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$CATEGO_ASP));    ListaMaterias <- as.character(niveles);  
subBDD10$CATEGO_ASP <- factor(subBDD10$CATEGO_ASP, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$CP));    ListaMaterias <- as.character(niveles);  
subBDD10$CP <- factor(subBDD10$CP, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$PROMEDIO));    ListaMaterias <- as.character(niveles);  
subBDD10$PROMEDIO <- factor(subBDD10$PROMEDIO, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$NUM_OPC));    ListaMaterias <- as.character(niveles);  
subBDD10$NUM_OPC <- factor(subBDD10$NUM_OPC, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD10$AnhoCert));    ListaMaterias <- as.character(niveles);  
subBDD10$AnhoCert <- factor(subBDD10$AnhoCert, levels = niveles, labels = ListaMaterias)
#---- 12. RESPALDO 5 ---
write.csv(subBDD10,"Repositorios/SubBDDComipems24_10.csv")
saveRDS(subBDD10, file = "Repositorios/SubBDDComipems24_10.rds")
save.image("Wkspaces/Wkspace5.RData")
#---- 13. PROCESAMIENTO DE LOS DATOS ----
##---- 13. 1 PROCESANDO PREFERENCIAS ----
pesos <- c(); N<- 20; denominador <- N*(N+1)/2
for(i in 1:N){
  numerador <- N-i+1;
  pesos[i] <- round((numerador/denominador)*100,digits = 3)
}
indices <- 11:30; OpcionesEstudio <- subBDD10[,11:30]; View(OpcionesEstudio)
entradas <- dim(OpcionesEstudio);  
num_filas <- entradas[1]; num_columnas <- entradas[2]
OpcionesEstudio <- as.data.frame(OpcionesEstudio)  # Convertir tibble a data.frame
OpcionesEstudio[] <- lapply(OpcionesEstudio, as.character)
OpcionesConPesos <- matrix(NA, nrow = num_filas, ncol = num_columnas)
for (i in 1:num_filas) {
  OpcionesConPesos[i, ] <- paste0(pesos, "-", OpcionesEstudio[i, ])
}
colnames(OpcionesConPesos) <- colnames(OpcionesEstudio)
View(OpcionesConPesos)
OpcionesConPesos_df <- as.data.frame(OpcionesConPesos)
View(OpcionesConPesos_df)
##---- 13.2 PROCESANDO CERCANIAS DE LA CDMX----
library(readxl)
setwd("~/Documents/DOCUMENTOS DOLORES/CODIGOS_POSTALES")
cp_CDMX <- read_excel("CDMX.xls")
data1 <- cp_CDMX
###---- 13.2.1 PROCESANDO ASENTAMIENTO ----
MisMaterias <- data1$d_asenta;
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
data1$d_asenta <- TodasMaterias; LasMaterias <- TodasMaterias
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
data1$d_asenta <- factor(data1$d_asenta, levels = niveles, labels = ListaMaterias)
summary(data1$d_asenta)
###---- 13.2.2 PROCESANDO TIPO ASENTAMIENTO ----
MisMaterias <- data1$d_tipo_asenta;
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
data1$d_tipo_asenta <- TodasMaterias; LasMaterias <- TodasMaterias
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
data1$d_tipo_asenta <- factor(data1$d_tipo_asenta, levels = niveles, labels = ListaMaterias)
summary(data1$d_tipo_asenta)
###---- 13.2.3 PROCESANDO MUNICIPIO ----
MisMaterias <- data1$D_mnpio;
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
data1$D_mnpio <- TodasMaterias; LasMaterias <- TodasMaterias
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
data1$D_mnpio <- factor(data1$D_mnpio, levels = niveles, labels = ListaMaterias)
summary(data1$D_mnpio)
###---- 13.2.4 PROCESANDO ESTADO ----
MisMaterias <- data1$d_estado;
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
data1$d_estado <- TodasMaterias; LasMaterias <- TodasMaterias
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
data1$d_estado <- factor(data1$d_estado, levels = niveles, labels = ListaMaterias)
summary(data1$d_estado)
###---- 13.2.5 PROCESANDO CIUDAD ----
MisMaterias <- data1$d_ciudad
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
data1$d_ciudad <- TodasMaterias; LasMaterias <- TodasMaterias
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
data1$d_ciudad <- factor(data1$d_ciudad, levels = niveles, labels = ListaMaterias)
summary(data1$d_ciudad)
###---- 13.2.6 PROCESANDO ZONA ----
MisMaterias <- data1$d_zona
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
data1$d_zona <- TodasMaterias; LasMaterias <- TodasMaterias
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
data1$d_zona <- factor(data1$d_zona, levels = niveles, labels = ListaMaterias)
summary(data1$d_zona)
##---- 13.3 RESUMEN DE RESULTADOS DEL PROCESAMIENTO ----
View(data1)
write.csv(data1,"cp_CiudadMexico.csv")
data_cdmx <- data1
##---- 13.4 PROCESANDO CERCANIAS DEL EDOMEX----
cp_EDOMX <- read_excel("EDOMEX.xls")
data2 <- cp_EDOMX
View(cp_EDOMX)
###---- 13.4.1 PROCESANDO ASENTAMIENTO ----
MisMaterias <- data2$d_asenta;
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
data2$d_asenta <- TodasMaterias; LasMaterias <- TodasMaterias
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
data2$d_asenta <- factor(data2$d_asenta, levels = niveles, labels = ListaMaterias)
summary(data2$d_asenta)
###---- 13.4.2 PROCESANDO TIPO ASENTAMIENTO ----
MisMaterias <- data2$d_tipo_asenta;
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
data2$d_tipo_asenta <- TodasMaterias; LasMaterias <- TodasMaterias
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
data2$d_tipo_asenta <- factor(data2$d_tipo_asenta, levels = niveles, labels = ListaMaterias)
summary(data2$d_tipo_asenta)
###---- 13.4.3 PROCESANDO MUNICIPIO ----
MisMaterias <- data2$D_mnpio;
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
data2$D_mnpio <- TodasMaterias; LasMaterias <- TodasMaterias
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
data2$D_mnpio <- factor(data2$D_mnpio, levels = niveles, labels = ListaMaterias)
summary(data2$D_mnpio)
###---- 13.4.4 PROCESANDO ESTADO ----
MisMaterias <- data2$d_estado;
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
data2$d_estado <- TodasMaterias; LasMaterias <- TodasMaterias
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
data2$d_estado <- factor(data2$d_estado, levels = niveles, labels = ListaMaterias)
summary(data2$d_estado)
###---- 13.4.5 PROCESANDO CIUDAD ----
MisMaterias <- data2$d_ciudad
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
data2$d_ciudad <- TodasMaterias; LasMaterias <- TodasMaterias
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
data2$d_ciudad <- factor(data2$d_ciudad, levels = niveles, labels = ListaMaterias)
summary(data2$d_ciudad)
###---- 13.4.6 PROCESANDO ZONA ----
MisMaterias <- data2$d_zona
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
data2$d_zona <- TodasMaterias; LasMaterias <- TodasMaterias
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
data2$d_zona <- factor(data2$d_zona, levels = niveles, labels = ListaMaterias)
summary(data2$d_zona)
##---- 13.5 RESUMEN DE RESULTADOS DEL PROCESAMIENTO ----
View(data2)
summary(data2)
##---- 13.6 SELECCION DE MUNICIPIOS DEL EDOMEX QUE PARTICIPAN EN COMIPEMS ----
# Crear un vector con los nombres de los municipios
library(dplyr)
municipios_comipems <- c("ACOLMAN","ATIZAPAN DE ZARAGOZA","CHALCO",
                         "CHICOLOAPAN","CHIMALHUACAN","COACALCO DE BERRIOZABAL",
                         "CUAUTITLAN","CUAUTITLAN IZCALLI","ECATEPEC DE MORELOS",
                         "HUIXQUILUCAN","IXTAPALUCA","NAUCALPAN DE JUAREZ",
                         "NEZAHUALCOYOTL","NICOLAS ROMERO","LA PAZ",
                         "TECAMAC","TEPOTZOTLAN","TEXCOCO",
                         "TLALNEPANTLA DE BAZ","TULTEPEC","TULTITLAN",
                         "VALLE DE CHALCO SOLIDARIDAD")
data3 <- data2 %>% filter(D_mnpio %in% municipios_comipems)
data_edomex <- data3
View(data3)
write.csv(data3,"cp_EdoMex.csv")
##---- 13.6 SE UNIFICAN AMBAS BASES DE DATOS PARA TENER TODO COMIPEMS ----
data_COMIPEMS <- rbind(data1,data3)
write.csv(data_COMIPEMS,"data_COMIPEMS.csv")
summary(data_COMIPEMS)
#---- 13.7 RESPALDO 4 ----
write.csv(data1,"cp_CDMX_mod.csv")
saveRDS(data1, file = "cp_CDMX_mod.rds")
write.csv(data3,"cp_EDOMX_mod.csv")
saveRDS(data3, file = "cp_EDOMX_mod.rds")
write.csv(data_COMIPEMS,"cp_COMIPEMS.csv")
saveRDS(data_COMIPEMS, file = "cp_COMIPEMS.rds")
cp_COMIPEMS <- droplevels(cp_COMIPEMS)
write.csv(cp_COMIPEMS, "cp_COMIPEMS.csv", row.names = FALSE)
saveRDS(cp_COMIPEMS, file = "cp_COMIPEMS.rds")
save.image("Wkspace4.RData")
##---- 13.8 LEYENDO LA NUEVA BDD COMIPEMS ----
setwd("~/Documents/DOCUMENTOS DOLORES/CODIGOS_POSTALES")
cp_COMIPEMS <- readRDS("cp_COMIPEMS.rds")
summary(cp_COMIPEMS$D_mnpio)
summary(cp_COMIPEMS)










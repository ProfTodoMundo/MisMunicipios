setwd("~/Documents/BDD_CENEVAL")
load("Wkspaces/Wkspace1.RData")
View(data24)
print(cols2check)
cols2check <- c("SEXO","COLONIA","CP","CVE_ALCMUN","CATEGO_ASP",
                "STAT_CERT","PROMEDIO","FECHA_CERT","FOL_CERT","NUM_OPC",
                "OPC_ED01", "OPC_ED02","OPC_ED03","OPC_ED04","OPC_ED05",
                "OPC_ED06","OPC_ED07","OPC_ED08","OPC_ED09","OPC_ED10",
                "OPC_ED11","OPC_ED12","OPC_ED13","OPC_ED14","OPC_ED15",
                "OPC_ED16","OPC_ED17","OPC_ED18","OPC_ED19","OPC_ED20",
                "PRE_EXA","NGLOBAL","PNGLOBAL","NO_PRES","NOPC_ASI",
                "COPC_ASI","CVEINS_ASI","CVESUB_ASI","INST_ASI","SUBS_ASI","NOM_ESC")
subBDD <- data24[,cols2check]; View(subBDD)
#---- CODIFICACION DEL VECTOR FECHA ----
year_vector <- as.numeric(format(as.Date(subBDD$FECHA_CERT, format = "%d-%m-%Y"), "%Y"))
print(head(year_vector, 10))  # Muestra los primeros 10 años extraídos
subBDD$AnhoCert <- year_vector
#---- 5. GENERACION DE CATEGORIAS PARA LAS OPCIONES EDUCATIVAS ---
##---- 5.1 OPCION EDUCATIVA 1----
niveles <- unique(sort(subBDD$OPC_ED01));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED01 <- factor(subBDD$OPC_ED01, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.2 OPCION EDUCATIVA 2----
niveles <- unique(sort(subBDD$OPC_ED02));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED02 <- factor(subBDD$OPC_ED02, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.3 OPCION EDUCATIVA 3----
niveles <- unique(sort(subBDD$OPC_ED03));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED03 <- factor(subBDD$OPC_ED03, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.4 OPCION EDUCATIVA 4----
niveles <- unique(sort(subBDD$OPC_ED04));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED04 <- factor(subBDD$OPC_ED04, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.5 OPCION EDUCATIVA 5----
niveles <- unique(sort(subBDD$OPC_ED05));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED05 <- factor(subBDD$OPC_ED05, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.6 OPCION EDUCATIVA 6----
niveles <- unique(sort(subBDD$OPC_ED06));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED06 <- factor(subBDD$OPC_ED06, levels = niveles, labels = ListaMaterias)
summary(subBDD$OPC_ED06)
##---- 5.7 OPCION EDUCATIVA 7----
niveles <- unique(sort(subBDD$OPC_ED07));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED07 <- factor(subBDD$OPC_ED07, levels = niveles, labels = ListaMaterias)
summary(subBDD$OPC_ED07)
##---- 5.8 OPCION EDUCATIVA 8----
niveles <- unique(sort(subBDD$OPC_ED08));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED08 <- factor(subBDD$OPC_ED08, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.9 OPCION EDUCATIVA 9----
niveles <- unique(sort(subBDD$OPC_ED09));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED09 <- factor(subBDD$OPC_ED09, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.10 OPCION EDUCATIVA 10----
niveles <- unique(sort(subBDD$OPC_ED10));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED10 <- factor(subBDD$OPC_ED10, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.11 OPCION EDUCATIVA 11----
niveles <- unique(sort(subBDD$OPC_ED11));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED11 <- factor(subBDD$OPC_ED11, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.12 OPCION EDUCATIVA 12----
niveles <- unique(sort(subBDD$OPC_ED12));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED12 <- factor(subBDD$OPC_ED12, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.13 OPCION EDUCATIVA 13----
niveles <- unique(sort(subBDD$OPC_ED13));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED13 <- factor(subBDD$OPC_ED13, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.14 OPCION EDUCATIVA 14----
niveles <- unique(sort(subBDD$OPC_ED14));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED14 <- factor(subBDD$OPC_ED14, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.15 OPCION EDUCATIVA 15----
niveles <- unique(sort(subBDD$OPC_ED15));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED15 <- factor(subBDD$OPC_ED15, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.16 OPCION EDUCATIVA 16----
niveles <- unique(sort(subBDD$OPC_ED16));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED16 <- factor(subBDD$OPC_ED16, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.17 OPCION EDUCATIVA 17----
niveles <- unique(sort(subBDD$OPC_ED17));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED17 <- factor(subBDD$OPC_ED17, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.18 OPCION EDUCATIVA 18----
niveles <- unique(sort(subBDD$OPC_ED18));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED18 <- factor(subBDD$OPC_ED18, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.19 OPCION EDUCATIVA 19----
niveles <- unique(sort(subBDD$OPC_ED19));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED19 <- factor(subBDD$OPC_ED19, levels = niveles, labels = ListaMaterias)
summary(subBDD)
##---- 5.20 OPCION EDUCATIVA 20----
niveles <- unique(sort(subBDD$OPC_ED20));    ListaMaterias <- as.character(niveles);  
subBDD$OPC_ED20 <- factor(subBDD$OPC_ED20, levels = niveles, labels = ListaMaterias)
summary(subBDD)
subBDD$CATEGO_ASP <- factor(subBDD$CATEGO_ASP, levels = c("E","F","I","Z"), 
                              labels = c("Egresado","Foraneo","INEA","Local"))
subBDD$STAT_CERT <- factor(subBDD$STAT_CERT,level = c("I","R"),
                             labels = c("Irregular","Regular"))
subBDD$PRE_EXA <- factor(subBDD$PRE_EXA, levels = c("N","S"), labels = c("No","Si"))
subBDD$NO_PRES <- factor(subBDD$NO_PRES, levels = c("NP","SP"), labels = c("No Presento","Si Presento"))
niveles <- unique(sort(subBDD$NOPC_ASI));    ListaMaterias <- as.character(niveles);  
subBDD$NOPC_ASI <- factor(subBDD$NOPC_ASI, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$COPC_ASI));    ListaMaterias <- as.character(niveles);  
subBDD$COPC_ASI <- factor(subBDD$COPC_ASI, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$CVEINS_ASI));    ListaMaterias <- as.character(niveles);  
subBDD$CVEINS_ASI <- factor(subBDD$CVEINS_ASI, levels = niveles, labels = ListaMaterias)
subBDD$CVEINS_ASI <- factor(subBDD$CVEINS_ASI, 
                              levels = c("A3","B0","C1","D4","G2","I5","M9",
                                         "S0","S1","S2","S4","S5","S7","S8",
                                         "U6"), 
                              labels = c("DGETAyCM","COLBACH","CONALEP","DGETI",
                                         "DGB","IPN","UAEM","SECTI_COBAEM",
                                         "SECTI_CONALEP","SECTI_TELEBACHILLERATO",
                                         "SECTI CBT","SECTI CECYTEM",
                                         "SECTI_PO","SECTI_PO","UNAM"))
niveles <- unique(sort(subBDD$CVESUB_ASI));    ListaMaterias <- as.character(niveles);  
subBDD$CVESUB_ASI <- factor(subBDD$CVESUB_ASI, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$INST_ASI));    ListaMaterias <- as.character(niveles);  
subBDD$INST_ASI <- factor(subBDD$INST_ASI, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$SUBS_ASI));    ListaMaterias <- as.character(niveles);  
subBDD$SUBS_ASI <- factor(subBDD$SUBS_ASI, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$SEXO));    ListaMaterias <- as.character(niveles);  
subBDD$SEXO <- factor(subBDD$SEXO, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$COLONIA));    ListaMaterias <- as.character(niveles);  
subBDD$COLONIA <- factor(subBDD$COLONIA, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$CP));    ListaMaterias <- as.character(niveles);  
subBDD$CP <- factor(subBDD$CP, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$CVE_ALCMUN));    ListaMaterias <- as.character(niveles);  
subBDD$CVE_ALCMUN <- factor(subBDD$CVE_ALCMUN, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$CATEGO_ASP));    ListaMaterias <- as.character(niveles);  
subBDD$CATEGO_ASP <- factor(subBDD$CATEGO_ASP, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$CP));    ListaMaterias <- as.character(niveles);  
subBDD$CP <- factor(subBDD$CP, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$PROMEDIO));    ListaMaterias <- as.character(niveles);  
subBDD$PROMEDIO <- factor(subBDD$PROMEDIO, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$NUM_OPC));    ListaMaterias <- as.character(niveles);  
subBDD$NUM_OPC <- factor(subBDD$NUM_OPC, levels = niveles, labels = ListaMaterias)
niveles <- unique(sort(subBDD$AnhoCert));    ListaMaterias <- as.character(niveles);  
subBDD$AnhoCert <- factor(subBDD$AnhoCert, levels = niveles, labels = ListaMaterias)

MisMaterias <- subBDD$NOM_ESC;
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
  MateriasCorrected <- gsub("\\.","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
subBDD$NOM_ESC <- TodasMaterias; LasMaterias <- TodasMaterias
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
subBDD$NOM_ESC <- factor(subBDD$NOM_ESC, levels = niveles, labels = ListaMaterias)
summary(subBDD$NOM_ESC)

MisMaterias <- subBDD$FOL_CERT;
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
  MateriasCorrected <- gsub("\\.","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  TodasMaterias[i] <- MateriasCorrected
}
subBDD$FOL_CERT <- TodasMaterias; LasMaterias <- TodasMaterias
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
subBDD$FOL_CERT <- factor(subBDD$FOL_CERT, levels = niveles, labels = ListaMaterias)
summary(subBDD$FOL_CERT)
colnames(subBDD)
#---- RESPALDO ----
write.csv(subBDD,"Repositorios/basedatosCOMIPEMS24.csv")
save.image("Wkspaces/Wkspace9.RData")
##---- 5.21 PROCESANDO PREFERENCIAS ----
setwd("~/Documents/BDD_CENEVAL")
load("Wkspaces/Wkspace9.RData")

colnames(subBDD)
indices <- c(1,3,7,8,9,10,11:30,41:42); misdatos <- subBDD[,indices]; View(misdatos)
colnames(misdatos)
##---- SELECCIONAMOS AQUELLOS QUE NO ELIGIERON A LA UNAM COMO PRIMERA OPCION ----
indices_PrimOpcion <- c(1:6,26:27)
misdatos_PrimeraOpc <-misdatos[,indices_PrimOpcion]; colnames(misdatos_PrimeraOpc)
write.csv(misdatos_PrimeraOpc,"Repositorios/bddPrimeraOpcion.csv")




View(misdatos_No_UNAM)
misdatos_No_UNAM_PrimOpc <- misdatos_No_UNAM %>%
  filter(!grepl("^U6", OPC_ED01))
colnames(misdatos_No_UNAM_PrimOpc)
indices_PrimOpcion <- c(4,3,1,7,8,2,6,5)
misdatos_No_UNAM_PrimOpc <- misdatos_No_UNAM_PrimOpc[,indices_PrimOpcion];
View(misdatos_No_UNAM_PrimOpc)


conteo <- as.data.frame(table(top_cal_Opc1$OPC_ED01,top_cal_Opc1$PROMEDIO));
colnames(conteo) <- c("Opcion1", "PROMEDIO","Frecuencia")

conteo_ordenado <- conteo %>% filter(Frecuencia > 0)%>%  arrange(desc(PROMEDIO))
View(conteo_ordenado)

opciones_frecuencia_cero <- conteo %>% filter(Frecuencia == 0)
View(opciones_frecuencia_cero)

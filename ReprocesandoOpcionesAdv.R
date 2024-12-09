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
#---- RESPALDO ----
write.csv(subBDD,"Repositorios/basedatosCOMIPEMS24.csv")
save.image("Wkspaces/Wkspace9.RData")
##---- 5.21 PROCESANDO PREFERENCIAS ----
indices <- c(11:30,41:42); misdatos <- subBDD[,indices]; View(misdatos)
library(dplyr)

# Crear subbases como una lista
subbases <- misdatos %>%
  group_by(OPC_ED01) %>%
  group_split()

# Verificar el número de subbases generadas
length(subbases)

# Ver los nombres de las categorías
categorias <- misdatos %>%
  distinct(OPC_ED01) %>%
  pull(OPC_ED01)  # Extraer nombres únicos

print(categorias)



colnames(OpcionesConPesos) <- c(colnames(OpcionesEstudio),"PROMEDIO")
OpcionesConPesos_df <- as.data.frame(OpcionesConPesos)
View(OpcionesConPesos_df)
#---- RESPALDO 8 ----
write_csv(OpcionesConPesos_df,"Repositorios/bdd_codificaca.csv")
setwd("~/Documents/BDD_CENEVAL/")
save.image("Wkspaces/Wkspace8.RData")
#---- se agrega la columna promedio ----
UNAM_PRIMERA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^9\\.524-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_SEGUNDA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^9\\.048-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_TERCERA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^8\\.571-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_CUARTA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^8\\.095-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_QUINTA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^7\\.619-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_SEXTA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^7\\.143-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_SEPTIMA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^6\\.667-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_OCTAVA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^6\\.190-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_NOVENA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^5\\.714-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_DECIMA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^5\\.238-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_ONCE_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^4\\.762-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_DOCE_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^4\\.286-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_TRECE_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^3\\.810-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_CATORCE_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^3\\.333-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_QUINCE_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^2\\.857-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_DICEISIES_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^2\\.381-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_DIECISIETE_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^1\\.905-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_DIECIOCHO_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^1\\.429-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_DIECINUEVE_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^0\\.952-U6", OpcionesConPesos_df$OPC_ED01)]
UNAM_VEINTE_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^0\\.476-U6", OpcionesConPesos_df$OPC_ED01)]

Sin_UNAM_PRIMERA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^9\\.524-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_SEGUNDA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^9\\.048-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_TERCERA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^8\\.571-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_CUARTA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^8\\.095-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_QUINTA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^7\\.619-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_SEXTA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^7\\.143-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_SEPTIMA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^6\\.667-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_OCTAVA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^6\\.190-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_NOVENA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^5\\.714-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_DECIMA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^5\\.238-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_ONCE_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^4\\.762-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_DOCE_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^4\\.286-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_TRECE_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^3\\.810-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_CATORCE_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^3\\.333-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_QUINCE_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^2\\.857-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_DIECISEIS_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^2\\.381-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_DIECISIETE_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^1\\.905-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_DIECIOCHO_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^1\\.429-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_DIECINUEVE_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^0\\.952-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_VEINTE_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^0\\.476-U6", OpcionesConPesos_df$OPC_ED01)]


misdatos <- OpcionesConPesos_df; summary(misdatos); View(misdatos)







##---- 13.4 EXTRAEMOS LAS ENTRADAS RELACIONADAS CON LA UNAM ----


# Crear listas para las opciones de UNAM y Sin UNAM
unam_opciones <- list(
  PRIMERA_OPC = OpcionesConPesos_df$OPC_ED01[grepl("^9\\.524-U6", OpcionesConPesos_df$OPC_ED01)],
  SEGUNDA_OPC = OpcionesConPesos_df$OPC_ED02[grepl("^9\\.048-U6", OpcionesConPesos_df$OPC_ED02)],
  TERCERA_OPC = OpcionesConPesos_df$OPC_ED03[grepl("^8\\.571-U6", OpcionesConPesos_df$OPC_ED03)],
  CUARTA_OPC = OpcionesConPesos_df$OPC_ED04[grepl("^8\\.095-U6", OpcionesConPesos_df$OPC_ED04)],
  QUINTA_OPC = OpcionesConPesos_df$OPC_ED05[grepl("^7\\.619-U6", OpcionesConPesos_df$OPC_ED05)],
  SEXTA_OPC = OpcionesConPesos_df$OPC_ED06[grepl("^7\\.143-U6", OpcionesConPesos_df$OPC_ED06)],
  SEPTIMA_OPC = OpcionesConPesos_df$OPC_ED07[grepl("^6\\.667-U6", OpcionesConPesos_df$OPC_ED07)],
  OCTAVA_OPC = OpcionesConPesos_df$OPC_ED08[grepl("^6\\.190-U6", OpcionesConPesos_df$OPC_ED08)],
  NOVENA_OPC = OpcionesConPesos_df$OPC_ED09[grepl("^5\\.714-U6", OpcionesConPesos_df$OPC_ED09)],
  DECIMA_OPC = OpcionesConPesos_df$OPC_ED10[grepl("^5\\.238-U6", OpcionesConPesos_df$OPC_ED10)],
  ONCE_OPC = OpcionesConPesos_df$OPC_ED11[grepl("^4\\.762-U6", OpcionesConPesos_df$OPC_ED11)],
  DOCE_OPC = OpcionesConPesos_df$OPC_ED12[grepl("^4\\.286-U6", OpcionesConPesos_df$OPC_ED12)],
  TRECE_OPC = OpcionesConPesos_df$OPC_ED13[grepl("^3\\.810-U6", OpcionesConPesos_df$OPC_ED13)],
  CATORCE_OPC = OpcionesConPesos_df$OPC_ED14[grepl("^3\\.333-U6", OpcionesConPesos_df$OPC_ED14)],
  QUINCE_OPC = OpcionesConPesos_df$OPC_ED15[grepl("^2\\.857-U6", OpcionesConPesos_df$OPC_ED15)],
  DIECISEIS_OPC = OpcionesConPesos_df$OPC_ED16[grepl("^2\\.381-U6", OpcionesConPesos_df$OPC_ED16)],
  DIECISIETE_OPC = OpcionesConPesos_df$OPC_ED17[grepl("^1\\.905-U6", OpcionesConPesos_df$OPC_ED17)],
  DIECIOCHO_OPC = OpcionesConPesos_df$OPC_ED18[grepl("^1\\.429-U6", OpcionesConPesos_df$OPC_ED18)],
  DIECINUEVE_OPC = OpcionesConPesos_df$OPC_ED19[grepl("^0\\.952-U6", OpcionesConPesos_df$OPC_ED19)],
  VEINTE_OPC = OpcionesConPesos_df$OPC_ED20[grepl("^0\\.476-U6", OpcionesConPesos_df$OPC_ED20)]
)

sin_unam_opciones <- list(
  PRIMERA_OPC = OpcionesConPesos_df$OPC_ED01[!grepl("^9\\.524-U6", OpcionesConPesos_df$OPC_ED01)],
  SEGUNDA_OPC = OpcionesConPesos_df$OPC_ED02[!grepl("^9\\.048-U6", OpcionesConPesos_df$OPC_ED02)],
  TERCERA_OPC = OpcionesConPesos_df$OPC_ED03[!grepl("^8\\.571-U6", OpcionesConPesos_df$OPC_ED03)],
  CUARTA_OPC = OpcionesConPesos_df$OPC_ED04[!grepl("^8\\.095-U6", OpcionesConPesos_df$OPC_ED04)],
  QUINTA_OPC = OpcionesConPesos_df$OPC_ED05[!grepl("^7\\.619-U6", OpcionesConPesos_df$OPC_ED05)],
  SEXTA_OPC = OpcionesConPesos_df$OPC_ED06[!grepl("^7\\.143-U6", OpcionesConPesos_df$OPC_ED06)],
  SEPTIMA_OPC = OpcionesConPesos_df$OPC_ED07[!grepl("^6\\.667-U6", OpcionesConPesos_df$OPC_ED07)],
  OCTAVA_OPC = OpcionesConPesos_df$OPC_ED08[!grepl("^6\\.190-U6", OpcionesConPesos_df$OPC_ED08)],
  NOVENA_OPC = OpcionesConPesos_df$OPC_ED09[!grepl("^5\\.714-U6", OpcionesConPesos_df$OPC_ED09)],
  DECIMA_OPC = OpcionesConPesos_df$OPC_ED10[!grepl("^5\\.238-U6", OpcionesConPesos_df$OPC_ED10)],
  ONCE_OPC = OpcionesConPesos_df$OPC_ED11[!grepl("^4\\.762-U6", OpcionesConPesos_df$OPC_ED11)],
  DOCE_OPC = OpcionesConPesos_df$OPC_ED12[!grepl("^4\\.286-U6", OpcionesConPesos_df$OPC_ED12)],
  TRECE_OPC = OpcionesConPesos_df$OPC_ED13[!grepl("^3\\.810-U6", OpcionesConPesos_df$OPC_ED13)],
  CATORCE_OPC = OpcionesConPesos_df$OPC_ED14[!grepl("^3\\.333-U6", OpcionesConPesos_df$OPC_ED14)],
  QUINCE_OPC = OpcionesConPesos_df$OPC_ED15[!grepl("^2\\.857-U6", OpcionesConPesos_df$OPC_ED15)],
  DIECISEIS_OPC = OpcionesConPesos_df$OPC_ED16[!grepl("^2\\.381-U6", OpcionesConPesos_df$OPC_ED16)],
  DIECISIETE_OPC = OpcionesConPesos_df$OPC_ED17[!grepl("^1\\.905-U6", OpcionesConPesos_df$OPC_ED17)],
  DIECIOCHO_OPC = OpcionesConPesos_df$OPC_ED18[!grepl("^1\\.429-U6", OpcionesConPesos_df$OPC_ED18)],
  DIECINUEVE_OPC = OpcionesConPesos_df$OPC_ED19[!grepl("^0\\.952-U6", OpcionesConPesos_df$OPC_ED19)],
  VEINTE_OPC = OpcionesConPesos_df$OPC_ED20[!grepl("^0\\.476-U6", OpcionesConPesos_df$OPC_ED20)]
)
unam_df <- as.data.frame(do.call(cbind, unam_opciones))
sin_unam_df <- as.data.frame(do.call(cbind, sin_unam_opciones))
print(unam_df); View(unam_df)
print(sin_unam_df); View(sin_unam_df)
#--- PROCESAMIENTO DE LAS COLUMNAS PARA GENERAR LAS CATEGORIAS Y CONTAR ----
sin_unam_df$VEINTE_OPC <- gsub("^0\\.476-", "", sin_unam_df$VEINTE_OPC)
niveles <- unique(sort(sin_unam_df$VEINTE_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$VEINTE_OPC <- factor(sin_unam_df$VEINTE_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$VEINTE_OPC)

sin_unam_df$DIECINUEVE_OPC <- gsub("^0\\.952-", "", sin_unam_df$DIECINUEVE_OPC)
niveles <- unique(sort(sin_unam_df$DIECINUEVE_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$DIECINUEVE_OPC <- factor(sin_unam_df$DIECINUEVE_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$DIECINUEVE_OPC)


sin_unam_df$DIECIOCHO_OPC <- gsub("^1\\.429-", "", sin_unam_df$DIECIOCHO_OPC)
niveles <- unique(sort(sin_unam_df$DIECIOCHO_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$DIECIOCHO_OPC <- factor(sin_unam_df$DIECIOCHO_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$DIECIOCHO_OPC)

sin_unam_df$DIECISIETE_OPC <- gsub("^1\\.905-", "", sin_unam_df$DIECISIETE_OPC)
niveles <- unique(sort(sin_unam_df$DIECISIETE_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$DIECISIETE_OPC <- factor(sin_unam_df$DIECISIETE_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$DIECISIETE_OPC)

sin_unam_df$DIECIS <- gsub("^2\\.381-", "", sin_unam_df$DIECISEIS_OPC)
niveles <- unique(sort(sin_unam_df$DIECISEIS_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$DIECISEIS_OPC <- factor(sin_unam_df$DIECISEIS_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$DIECISEIS_OPC)

sin_unam_df$QUINCE_OPC <- gsub("^2\\.857-", "", sin_unam_df$QUINCE_OPC)
niveles <- unique(sort(sin_unam_df$QUINCE_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$QUINCE_OPC <- factor(sin_unam_df$QUINCE_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$QUINCE_OPC)


sin_unam_df$CATORCE_OPC <- gsub("^3\\.333-", "", sin_unam_df$CATORCE_OPC)
niveles <- unique(sort(sin_unam_df$CATORCE_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$CATORCE_OPC <- factor(sin_unam_df$CATORCE_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$CATORCE_OPC)


sin_unam_df$TRECE_OPC <- gsub("^3\\.810-", "", sin_unam_df$TRECE_OPC)
niveles <- unique(sort(sin_unam_df$TRECE_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$TRECE_OPC <- factor(sin_unam_df$TRECE_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$TRECE_OPC)



sin_unam_df$DOCE_OPC <- gsub("^4\\.286-", "", sin_unam_df$DOCE_OPC)
niveles <- unique(sort(sin_unam_df$DOCE_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$DOCE_OPC <- factor(sin_unam_df$DOCE_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$DOCE_OPC)


sin_unam_df$ONCE_OPC <- gsub("^4\\.762-", "", sin_unam_df$ONCE_OPC)
niveles <- unique(sort(sin_unam_df$ONCE_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$ONCE_OPC <- factor(sin_unam_df$ONCE_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$ONCE_OPC)


sin_unam_df$DECIMA_OPC <- gsub("^5\\.238-", "", sin_unam_df$DECIMA_OPC)
niveles <- unique(sort(sin_unam_df$DECIMA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$DECIMA_OPC <- factor(sin_unam_df$DECIMA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$DECIMA_OPC)



sin_unam_df$NOVENA_OPC <- gsub("^5\\.714-", "", sin_unam_df$NOVENA_OPC)
niveles <- unique(sort(sin_unam_df$NOVENA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$NOVENA_OPC <- factor(sin_unam_df$NOVENA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$NOVENA_OPC)



sin_unam_df$OCTAVA_OPC <- gsub("^6\\.190-", "", sin_unam_df$OCTAVA_OPC)
niveles <- unique(sort(sin_unam_df$OCTAVA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$OCTAVA_OPC <- factor(sin_unam_df$OCTAVA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$OCTAVA_OPC)



sin_unam_df$SEPTIMA_OPC <- gsub("^6\\.667-", "", sin_unam_df$SEPTIMA_OPC)
niveles <- unique(sort(sin_unam_df$SEPTIMA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$SEPTIMA_OPC <- factor(sin_unam_df$SEPTIMA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$SEPTIMA_OPC)


sin_unam_df$SEXTA_OPC <- gsub("^7\\.143-", "", sin_unam_df$SEXTA_OPC)
niveles <- unique(sort(sin_unam_df$SEXTA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$SEXTA_OPC <- factor(sin_unam_df$SEXTA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$SEXTA_OPC)



sin_unam_df$QUINTA_OPC <- gsub("^7\\.619-", "", sin_unam_df$QUINTA_OPC)
niveles <- unique(sort(sin_unam_df$QUINTA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$QUINTA_OPC <- factor(sin_unam_df$QUINTA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$QUINTA_OPC)

sin_unam_df$PRIMERA_OPC <- gsub("^9\\.524-", "", sin_unam_df$PRIMERA_OPC)
niveles <- unique(sort(sin_unam_df$PRIMERA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$PRIMERA_OPC <- factor(sin_unam_df$PRIMERA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$PRIMERA_OPC)

sin_unam_df$SEGUNDA_OPC <- gsub("^9\\.048-", "", sin_unam_df$SEGUNDA_OPC)
niveles <- unique(sort(sin_unam_df$SEGUNDA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$SEGUNDA_OPC <- factor(sin_unam_df$SEGUNDA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$SEGUNDA_OPC)

sin_unam_df$TERCERA_OPC <- gsub("^8\\.571-", "", sin_unam_df$TERCERA_OPC)
niveles <- unique(sort(sin_unam_df$TERCERA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$TERCERA_OPC <- factor(sin_unam_df$TERCERA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$TERCERA_OPC)

sin_unam_df$CUARTA_OPC <- gsub("^8\\.095-", "", sin_unam_df$CUARTA_OPC)
niveles <- unique(sort(sin_unam_df$CUARTA_OPC));    ListaMaterias <- as.character(niveles);  
sin_unam_df$CUARTA_OPC <- factor(sin_unam_df$CUARTA_OPC, levels = niveles, labels = ListaMaterias)
summary(sin_unam_df$CUARTA_OPC)
#---- RESPALDO 9 ----
write.csv(sin_unam_df,"Repositorios/SinUNAM.csv")
save.image("Wkspaces/Wkspace9.RData")
#---- CREACION DE GRUPOS POR OPCION EDUCATIVA ELEGIDA COMO PRIMERA OPCION ----
View(sin_unam_df)
View(misdatos)
misdatos <- sin_unam_df
primera_seleccion <- misdatos %>% filter(PROMEDIO >=9.5)

#---- se agrega la columna promedio ----
# Crear listas para las opciones Sin UNAM y UNAM, incluyendo los promedios
sin_unam_opciones <- list(
  PRIMERA_OPC = OpcionesConPesos_df[!grepl("^9\\.524-U6", OpcionesConPesos_df$OPC_ED01), c("OPC_ED01", "PROMEDIO")],
  SEGUNDA_OPC = OpcionesConPesos_df[!grepl("^9\\.048-U6", OpcionesConPesos_df$OPC_ED02), c("OPC_ED02", "PROMEDIO")],
  TERCERA_OPC = OpcionesConPesos_df[!grepl("^8\\.571-U6", OpcionesConPesos_df$OPC_ED03), c("OPC_ED03", "PROMEDIO")],
  CUARTA_OPC = OpcionesConPesos_df[!grepl("^8\\.095-U6", OpcionesConPesos_df$OPC_ED04), c("OPC_ED04", "PROMEDIO")],
  QUINTA_OPC = OpcionesConPesos_df[!grepl("^7\\.619-U6", OpcionesConPesos_df$OPC_ED05), c("OPC_ED05", "PROMEDIO")],
  SEXTA_OPC = OpcionesConPesos_df[!grepl("^7\\.143-U6", OpcionesConPesos_df$OPC_ED06), c("OPC_ED06", "PROMEDIO")],
  SEPTIMA_OPC = OpcionesConPesos_df[!grepl("^6\\.667-U6", OpcionesConPesos_df$OPC_ED07), c("OPC_ED07", "PROMEDIO")],
  OCTAVA_OPC = OpcionesConPesos_df[!grepl("^6\\.190-U6", OpcionesConPesos_df$OPC_ED08), c("OPC_ED08", "PROMEDIO")],
  NOVENA_OPC = OpcionesConPesos_df[!grepl("^5\\.714-U6", OpcionesConPesos_df$OPC_ED09), c("OPC_ED09", "PROMEDIO")],
  DECIMA_OPC = OpcionesConPesos_df[!grepl("^5\\.238-U6", OpcionesConPesos_df$OPC_ED10), c("OPC_ED10", "PROMEDIO")],
  ONCE_OPC = OpcionesConPesos_df[!grepl("^4\\.762-U6", OpcionesConPesos_df$OPC_ED11), c("OPC_ED11", "PROMEDIO")],
  DOCE_OPC = OpcionesConPesos_df[!grepl("^4\\.286-U6", OpcionesConPesos_df$OPC_ED12), c("OPC_ED12", "PROMEDIO")],
  TRECE_OPC = OpcionesConPesos_df[!grepl("^3\\.810-U6", OpcionesConPesos_df$OPC_ED13), c("OPC_ED13", "PROMEDIO")],
  CATORCE_OPC = OpcionesConPesos_df[!grepl("^3\\.333-U6", OpcionesConPesos_df$OPC_ED14), c("OPC_ED14", "PROMEDIO")],
  QUINCE_OPC = OpcionesConPesos_df[!grepl("^2\\.857-U6", OpcionesConPesos_df$OPC_ED15), c("OPC_ED15", "PROMEDIO")],
  DIECISEIS_OPC = OpcionesConPesos_df[!grepl("^2\\.381-U6", OpcionesConPesos_df$OPC_ED16), c("OPC_ED16", "PROMEDIO")],
  DIECISIETE_OPC = OpcionesConPesos_df[!grepl("^1\\.905-U6", OpcionesConPesos_df$OPC_ED17), c("OPC_ED17", "PROMEDIO")],
  DIECIOCHO_OPC = OpcionesConPesos_df[!grepl("^1\\.429-U6", OpcionesConPesos_df$OPC_ED18), c("OPC_ED18", "PROMEDIO")],
  DIECINUEVE_OPC = OpcionesConPesos_df[!grepl("^0\\.952-U6", OpcionesConPesos_df$OPC_ED19), c("OPC_ED19", "PROMEDIO")],
  VEINTE_OPC = OpcionesConPesos_df[!grepl("^0\\.476-U6", OpcionesConPesos_df$OPC_ED20), c("OPC_ED20", "PROMEDIO")]
)

# Convertir las listas en data frames
sin_unam_df <- do.call(rbind, lapply(names(sin_unam_opciones), function(opc) {
  df <- sin_unam_opciones[[opc]]
  df$OPCION <- opc
  return(df)
}))

# Visualizar
print(sin_unam_df)
View(sin_unam_df)






pesos <- c(); N<- 20; denominador <- N*(N+1)/2
for(i in 1:N){
  numerador <- N-i+1;
  pesos[i] <- round((numerador/denominador)*100,digits = 3)
}
indices <- 11:30; OpcionesEstudio <- subBDD[,11:30]; View(OpcionesEstudio)
entradas <- dim(OpcionesEstudio);  
num_filas <- entradas[1]; num_columnas <- entradas[2]
OpcionesEstudio <- as.data.frame(OpcionesEstudio)  # Convertir tibble a data.frame
OpcionesEstudio[] <- lapply(OpcionesEstudio, as.character)
OpcionesConPesos <- matrix(NA, nrow = num_filas, ncol = num_columnas)
for (i in 1:num_filas) {
  OpcionesConPesos[i, ] <- paste0(pesos, "-", OpcionesEstudio[i, ])
}
OpcionesConPesos <- cbind(OpcionesConPesos_df,subBDD$PROMEDIO)


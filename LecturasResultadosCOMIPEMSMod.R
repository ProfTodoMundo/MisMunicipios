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
                "COPC_ASI","CVEINS_ASI","CVESUB_ASI","INST_ASI","SUBS_ASI","NOM_ESC")
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
#---- 11. RESPALDO 5 ---
write.csv(subBDD10,"Repositorios/SubBDDComipems24_10.csv")
saveRDS(subBDD10, file = "Repositorios/SubBDDComipems24_10.rds")
save.image("Wkspaces/Wkspace5.RData")
#---- 11.bis Se sigue configurando la base de datos ----
MisMaterias <- subBDD10$NOM_ESC;
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
subBDD10$NOM_ESC <- TodasMaterias; LasMaterias <- TodasMaterias
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
subBDD10$NOM_ESC <- factor(subBDD10$NOM_ESC, levels = niveles, labels = ListaMaterias)
summary(subBDD10$NOM_ESC)
#---- 12. RESPALDO 6 ---
write.csv(subBDD10,"Repositorios/SubBDDComipems24_10.csv")
saveRDS(subBDD10, file = "Repositorios/SubBDDComipems24_10.rds")
save.image("Wkspaces/Wkspace6.RData")
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
summary(OpcionesConPesos)

#niveles <- unique(sort(OpcionesConPesos_df$OPC_ED01));    ListaMaterias <- as.character(niveles);  
#OpcionesConPesos_df$OPC_ED01 <- factor(OpcionesConPesos_df$OPC_ED01, levels = niveles, labels = ListaMaterias)
#summary(OpcionesConPesos_df$OPC_ED01)
##---- 13.2. RESPALDO 7 ---
write.csv(OpcionesConPesos_df,"Repositorios/OpcionesPonderadas.csv")
saveRDS(OpcionesConPesos_df, file = "Repositorios/OpcionesPonderadas.rds")
save.image("Wkspaces/Wkspace7.RData")
##---- 13.3 SE GENERA UNA PALETA DE COLORES ----
codigo_colores <- c("#161a1d", "#9b2247", "#a57f2c", "#7e664a",
                    "#611232", "#e6d194", "#c39326", "#806b4a")
mi_paleta <- colorRampPalette(codigo_colores)
##---- 13.4 EXTRAEMOS LAS ENTRADAS RELACIONADAS CON LA UNAM ----
UNAM_PRIMERA_OPC <- OpcionesConPesos_df$OPC_ED01[grepl("^9\\.524-U6", OpcionesConPesos_df$OPC_ED01)]
Sin_UNAM_PRIMERA_OPC <- OpcionesConPesos_df$OPC_ED01[!grepl("^9\\.524-U6", OpcionesConPesos_df$OPC_ED01)]

freq_sin_unam <- table(Sin_UNAM_PRIMERA_OPC)
freq_sin_unam_sorted <- sort(freq_sin_unam, decreasing = TRUE)

total_pref_unam <- sum(freq_unam_sorted)
total_pref_sin_unam <- sum(freq_sin_unam)
max_freq_sin_unam <- max(freq_sin_unam_sorted) + 5000
max_freq_unam <- max(freq_unam_sorted) + 5000

percent_unam <- round(100 * freq_unam_sorted / sum(freq_unam_sorted), 1) # Porcentajes redondeados
percent_sin_unam <- round(100 * freq_sin_unam_sorted / sum(freq_sin_unam_sorted), 1) # Porcentajes redondeados
labels_unam <- paste(freq_unam_sorted, " (", percent_unam, "%)", sep = "") # Crear etiquetas con frecuencia y porcentaje
labels_sin_unam <- paste(freq_sin_unam_sorted, " (", percent_sin_unam, "%)", sep = "") # Crear etiquetas con frecuencia y porcentaje

porcentaje_pref_unam <- round(100 *total_pref_unam/(total_pref_unam+total_pref_sin_unam))
porcentaje_pref_sin_unam <- round(100 *total_pref_sin_unam/(total_pref_unam+total_pref_sin_unam))

UNAM_PRIMERA_OPC <- gsub("^9\\.524-", "", UNAM_PRIMERA_OPC)
freq_unam <- table(UNAM_PRIMERA_OPC)
freq_unam_sorted <- sort(freq_unam, decreasing = TRUE)

colores_ordenados_unam <- colores_interpolados_unam[order(-freq_unam)]
colores_ordenados_sin_unam <- colores_interpolados_sin_unam[order(-freq_sin_unam)]
##---- GRAFICANDO PREFERENCIAS UNAM ----
bar_coords <- barplot(
  freq_unam_sorted, # Frecuencias ordenadas
  main = paste("PREFERENCIAS SOBRE LA UNAM COMO PRIMER OPCION (", porcentaje_pref_unam, "%)", sep = ""),
  xlab = "",
  ylab = "Frecuencia",
  col = colores_ordenados_unam[1:length(freq_unam)], # Colores ordenados por intensidad
  las = 2, # Rotar etiquetas del eje X
  cex.main = 0.7,  # Título más pequeño
  cex.lab = 0.8,   # Tamaño de las etiquetas de los ejes (X e Y)
  cex.axis = 0.7,  # Tamaño de los números en los ejes (incluye Y)
  cex.names = 0.6, # Tamaño de las etiquetas del eje X
  ylim = c(0, max_freq_unam) # Ajustar escala del eje Y
)
text(
  x = bar_coords,  # Coordenadas X de las barras
  y = freq_unam_sorted+1500,  # Altura de las barras
  labels = labels_unam,  # Valores a mostrar
  srt = 90,  # Rotar el texto 45 grados
  #adj = c(1, 1), # Alinear texto a la esquina superior derecha
  pos = 3,  # Posición del texto (3 = encima de la barra)
  cex = 0.5, # Tamaño del texto
  col = "black" # Color del texto
)
##---- GRAFICANDO PREFERENCIAS SIN UNAM ----
library(dplyr)
# Eliminar el prefijo "9.524-" de Sin_UNAM_PRIMERA_OPC
Sin_UNAM_PRIMERA_OPC <- gsub("^9\\.524-", "", Sin_UNAM_PRIMERA_OPC)
freq_sin_unam_sorted <- sort(freq_sin_unam, decreasing = TRUE)

# Convertir freq_sin_unam_sorted en un data.frame
freq_sin_unam_df <- as.data.frame(freq_sin_unam_sorted)
colnames(freq_sin_unam_df) <- c("Categoria", "Frecuencia")

# Filtrar categorías con frecuencia >= 50
Freq_sunam <- freq_sin_unam_df %>% filter(Frecuencia >= 50)

# Seleccionar las primeras 100 categorías
Freq_sunam_top100 <- Freq_sunam %>% slice(1:100)

# Dividir en paquetes de 20 categorías
paquetes <- split(Freq_sunam_top100, (seq_len(nrow(Freq_sunam_top100)) - 1) %/% 20)

# Calcular frecuencias para UNAM_PRIMERA_OPC


# Función para graficar cada paquete
graficar_paquete <- function(paquete, index) {
  # Ordenar las categorías dentro del paquete
  paquete <- paquete[order(-paquete$Frecuencia), ]
  
  # Crear el gráfico
  bar_coords <- barplot(
    paquete$Frecuencia,
    names.arg = paquete$Categoria,
    main = paste("Preferencias Sin UNAM - grupo ", index, " (", porcentaje_pref_sin_unam, "%)", sep = ""),
    xlab = "",
    ylab = "Frecuencia",
    col = mi_paleta(nrow(paquete)), # Paleta de colores
    las = 2, # Rotar etiquetas del eje X
    cex.main = 0.8,  # Título más pequeño
    cex.lab = 0.8,   # Tamaño de las etiquetas de los ejes (X e Y)
    cex.axis = 0.7,  # Tamaño de los números en los ejes (incluye Y)
    cex.names = 0.6, # Tamaño de las etiquetas del eje X
    ylim = c(0, max(paquete$Frecuencia) + 500) # Ajustar escala del eje Y
  )
  
  # Agregar los valores de las barras
  text(
    x = bar_coords,
    y = paquete$Frecuencia + 350, # Ajuste hacia arriba
    labels = paquete$Frecuencia,
    srt = 90, # Rotar las etiquetas 90 grados
    adj = c(1, 0), # Alineación superior derecha
    cex = 0.7,
    col = "black"
  )
}

# Graficar todos los paquetes
for (i in seq_along(paquetes)) {
  graficar_paquete(paquetes[[i]], i)
}
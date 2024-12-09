# Configura el entorno de trabajo y carga los datos
library(readxl)
library(readxl)
#setwd("~/Dropbox/BDD_MPIOS")
setwd("~/Desktop/GraficasAuxiliares")
data1 <- read_excel("datos_historico.xlsx")
#---- uno ----
# Renombra las columnas
colnames(data1) <- c("Ciclo_Escolar","Matricula","Matricula_15a17","Tasa_Neta_15a17",
                     "Cobertura_Total","Absorcion","Poblacion_15a17_CONAPO",
                     "Egresados_Secundaria","Nuevo_Ingreso_Primero","Egresados",
                     "Reprobacion","Reprobacion_Total","Abandono_Escolar",
                     "Eficiencia_Terminal","Cobertura","Atencion_Demanda",
                     "Matricula_Total","Primero","Segundo","Tercero","Cuarto")

# Crea la gráfica
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula")

# Personaliza el eje X con las etiquetas de Ciclo_Escolar
axis(1, at = 1:length(data1$Ciclo_Escolar), labels = data1$Ciclo_Escolar)

# Añade otras series de datos
lines(data1$Matricula_15a17, type = "b", col = "green", lty = 2, pch = 17)
lines(data1$Cobertura_Total, type = "b", col = "orange", lty = 3, pch = 15)
lines(data1$Absorcion, type = "b", col = "red", lty = 4, pch = 18)

# Añade una leyenda
legend("topright", legend = c("Matrícula", "Matrícula 15-17", "Cobertura Total", "Absorción"),
       col = c("blue", "green", "orange", "red"), lty = c(1, 2, 3, 4), pch = c(16, 17, 15, 18))

#---- dos ----
# Configura el entorno de trabajo y carga los datos
data1 <- read_excel("Auxiliares/datos_historico.xlsx")

# Renombra las columnas
colnames(data1) <- c("Ciclo_Escolar","Matricula","Matricula_15a17","Tasa_Neta_15a17",
                     "Cobertura_Total","Absorcion","Poblacion_15a17_CONAPO",
                     "Egresados_Secundaria","Nuevo_Ingreso_Primero","Egresados",
                     "Reprobacion","Reprobacion_Total","Abandono_Escolar",
                     "Eficiencia_Terminal","Cobertura","Atencion_Demanda",
                     "Matricula_Total","Primero","Segundo","Tercero","Cuarto")

# Crea la gráfica con etiquetas del eje Y en vertical
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 2)

# Personaliza el eje X con las etiquetas de Ciclo_Escolar
axis(1, at = 1:length(data1$Ciclo_Escolar), labels = data1$Ciclo_Escolar)

# Añade otras series de datos
lines(data1$Matricula_15a17, type = "b", col = "green", lty = 2, pch = 17)
lines(data1$Cobertura_Total, type = "b", col = "orange", lty = 3, pch = 15)
lines(data1$Absorcion, type = "b", col = "red", lty = 4, pch = 18)

# Añade una leyenda
legend("topright", legend = c("Matrícula", "Matrícula 15-17", "Cobertura Total", "Absorción"),
       col = c("blue", "green", "orange", "red"), lty = c(1, 2, 3, 4), pch = c(16, 17, 15, 18))

#---- tres ----
# Configura el entorno de trabajo y carga los datos
#library(readxl)
#setwd("~/Dropbox/BDD_MPIOS")
#data1 <- read_excel("Auxiliares/datos_historico.xlsx")

# Renombra las columnas
colnames(data1) <- c("Ciclo_Escolar","Matricula","Matricula_15a17","Tasa_Neta_15a17",
                     "Cobertura_Total","Absorcion","Poblacion_15a17_CONAPO",
                     "Egresados_Secundaria","Nuevo_Ingreso_Primero","Egresados",
                     "Reprobacion","Reprobacion_Total","Abandono_Escolar",
                     "Eficiencia_Terminal","Cobertura","Atencion_Demanda",
                     "Matricula_Total","Primero","Segundo","Tercero","Cuarto")

# Crea la gráfica
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1)

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:length(data1$Ciclo_Escolar), labels = FALSE)
text(x = 1:length(data1$Ciclo_Escolar), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.8)

# Añade otras series de datos
lines(data1$Matricula_15a17, type = "b", col = "green", lty = 2, pch = 17)
lines(data1$Cobertura_Total, type = "b", col = "orange", lty = 3, pch = 15)
lines(data1$Absorcion, type = "b", col = "red", lty = 4, pch = 18)

# Añade una leyenda
legend("topright", legend = c("Matrícula", "Matrícula 15-17", "Cobertura Total", "Absorción"),
       col = c("blue", "green", "orange", "red"), lty = c(1, 2, 3, 4), pch = c(16, 17, 15, 18))



#---- cuatro ----
# Configura el entorno de trabajo y carga los datos
# Crea la gráfica
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1)

# Añade etiquetas del eje X con rotación vertical y disminución de tamaño
axis(1, at = 1:length(data1$Ciclo_Escolar), labels = FALSE)
text(x = 1:length(data1$Ciclo_Escolar), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.7) # Ajusta cex para cambiar el tamaño

# Añade otras series de datos
lines(data1$Matricula_15a17, type = "b", col = "green", lty = 2, pch = 17)
lines(data1$Cobertura_Total, type = "b", col = "orange", lty = 3, pch = 15)
lines(data1$Absorcion, type = "b", col = "red", lty = 4, pch = 18)

# Añade una leyenda
legend("topright", legend = c("Matrícula", "Matrícula 15-17", "Cobertura Total", "Absorción"),
       col = c("blue", "green", "orange", "red"), lty = c(1, 2, 3, 4), pch = c(16, 17, 15, 18))

#---- cinco ----

# Agrega las líneas sin el argumento 'type'
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

lines(data1$Tasa_Neta_15a17, col = "purple", lty = 5)
points(data1$Tasa_Neta_15a17, col = "purple", pch = 2)

lines(data1$Cobertura_Total, col = "orange", lty = 3)
points(data1$Cobertura_Total, col = "orange", pch = 15)

lines(data1$Absorcion, col = "red", lty = 4)
points(data1$Absorcion, col = "red", pch = 18)


#---- seis ----
# Configura el entorno de trabajo y carga los datos

# Verifica si hay NA en las columnas a graficar y elimínalos
data1 <- na.omit(data1[, c("Matricula", "Matricula_15a17", "Tasa_Neta_15a17", 
                           "Cobertura_Total", "Absorcion")])

# Crea la gráfica principal
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1)

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Añade otras series de datos con líneas y puntos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

lines(data1$Tasa_Neta_15a17, col = "purple", lty = 5)
points(data1$Tasa_Neta_15a17, col = "purple", pch = 2)

lines(data1$Cobertura_Total, col = "orange", lty = 3)
points(data1$Cobertura_Total, col = "orange", pch = 15)

lines(data1$Absorcion, col = "red", lty = 4)
points(data1$Absorcion, col = "red", pch = 18)

# Añade una leyenda
legend("topright", legend = c("Matrícula", "Matrícula 15-17", "Tasa Neta 15-17", "Cobertura Total", "Absorción"),
       col = c("blue", "green", "purple", "orange", "red"), lty = c(1, 2, 5, 3, 4), pch = c(16, 17, 2, 15, 18))

#---- siete ----

# Gráfico base
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16)

# Agregar más series de datos al mismo gráfico
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

lines(data1$Tasa_Neta_15a17, col = "purple", lty = 5)
points(data1$Tasa_Neta_15a17, col = "purple", pch = 2)


#---- ocho ----
# Divide la ventana gráfica en una matriz de 2 filas y 2 columnas
par(mfrow = c(2, 2))

# Crea el primer gráfico
plot(data1$Matricula, type = "b", col = "blue", main = "Matrícula")

# Crea el segundo gráfico
plot(data1$Matricula_15a17, type = "b", col = "green", main = "Matrícula 15-17 años")

# Crea el tercer gráfico
plot(data1$Tasa_Neta_15a17, type = "b", col = "purple", main = "Tasa Neta 15-17 años")

# Crea el cuarto gráfico
plot(data1$Cobertura_Total, type = "b", col = "orange", main = "Cobertura Total")

#---- NUEVE ----

# Configura el entorno de trabajo y carga los datos
library(readxl)
setwd("~/Dropbox/BDD_MPIOS")
data1 <- read_excel("Auxiliares/datos_historico.xlsx")

# Renombra las columnas
colnames(data1) <- c("Ciclo_Escolar","Matricula","Matricula_15a17","Tasa_Neta_15a17",
                     "Cobertura_Total","Absorcion","Poblacion_15a17_CONAPO",
                     "Egresados_Secundaria","Nuevo_Ingreso_Primero","Egresados",
                     "Reprobacion","Reprobacion_Total","Abandono_Escolar",
                     "Eficiencia_Terminal","Cobertura","Atencion_Demanda",
                     "Matricula_Total","Primero","Segundo","Tercero","Cuarto")

# Verifica si hay NA en las columnas a graficar y elimínalos
data1 <- na.omit(data1[, c("Ciclo_Escolar", "Matricula", "Matricula_15a17", "Tasa_Neta_15a17", 
                           "Cobertura_Total", "Absorcion")])

# Crea la gráfica principal
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Valores", las = 1)

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.7)

# Añade otras series de datos con líneas y puntos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

lines(data1$Tasa_Neta_15a17, col = "purple", lty = 5)
points(data1$Tasa_Neta_15a17, col = "purple", pch = 2)

lines(data1$Cobertura_Total, col = "orange", lty = 3)
points(data1$Cobertura_Total, col = "orange", pch = 15)

lines(data1$Absorcion, col = "red", lty = 4)
points(data1$Absorcion, col = "red", pch = 18)

# Añade una leyenda
legend("topright", legend = c("Matrícula", "Matrícula 15-17", "Tasa Neta 15-17", "Cobertura Total", "Absorción"),
       col = c("blue", "green", "purple", "orange", "red"), lty = c(1, 2, 5, 3, 4), pch = c(16, 17, 2, 15, 18))


#---- DIEZ ----
# Cargar las bibliotecas necesarias
library(ggplot2)
library(readxl)

# Configurar el entorno de trabajo y cargar los datos
setwd("~/Dropbox/BDD_MPIOS")
data1 <- read_excel("Auxiliares/datos_historico.xlsx")

# Renombrar las columnas
colnames(data1) <- c("Ciclo_Escolar","Matricula","Matricula_15a17","Tasa_Neta_15a17",
                     "Cobertura_Total","Absorcion","Poblacion_15a17_CONAPO",
                     "Egresados_Secundaria","Nuevo_Ingreso_Primero","Egresados",
                     "Reprobacion","Reprobacion_Total","Abandono_Escolar",
                     "Eficiencia_Terminal","Cobertura","Atencion_Demanda",
                     "Matricula_Total","Primero","Segundo","Tercero","Cuarto")

# Convertir el ciclo escolar a factor para el eje X
data1$Ciclo_Escolar <- as.factor(data1$Ciclo_Escolar)

# Crear el gráfico con ggplot2
ggplot(data1, aes(x = Ciclo_Escolar)) +
  geom_line(aes(y = Matricula, color = "Matrícula"), size = 1) +
  geom_point(aes(y = Matricula, color = "Matrícula"), size = 2) +
  geom_line(aes(y = Matricula_15a17, color = "Matrícula 15-17"), size = 1, linetype = "dashed") +
  geom_point(aes(y = Matricula_15a17, color = "Matrícula 15-17"), size = 2) +
  geom_line(aes(y = Tasa_Neta_15a17, color = "Tasa Neta 15-17"), size = 1, linetype = "dotdash") +
  geom_point(aes(y = Tasa_Neta_15a17, color = "Tasa Neta 15-17"), size = 2) +
  geom_line(aes(y = Cobertura_Total, color = "Cobertura Total"), size = 1, linetype = "twodash") +
  geom_point(aes(y = Cobertura_Total, color = "Cobertura Total"), size = 2) +
  geom_line(aes(y = Absorcion, color = "Absorción"), size = 1, linetype = "longdash") +
  geom_point(aes(y = Absorcion, color = "Absorción"), size = 2) +
  labs(title = "Indicadores de Matrícula por Ciclo Escolar",
       x = "Ciclo Escolar",
       y = "Valores",
       color = "Indicadores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#---- ONCE ----

ggplot(data1, aes(x = Ciclo_Escolar)) +
  geom_line(aes(y = Matricula, color = "Matrícula"), size = 1) +
  geom_point(aes(y = Matricula, color = "Matrícula"), size = 2) +
  geom_line(aes(y = Matricula_15a17, color = "Matrícula 15-17"), size = 1, linetype = "dashed") +
  geom_point(aes(y = Matricula_15a17, color = "Matrícula 15-17"), size = 2) +
  labs(title = "Matrícula por Ciclo Escolar",
       x = "Ciclo Escolar",
       y = "Matricula Escolar",
       color = "Indicadores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8))  # Usa 'size' para ajustar el tamaño


#---- DOCE ----
# Crea la gráfica principal
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1)

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Añade la línea y los puntos de la segunda serie de datos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

#---- TRECE ----
# Calcular el valor máximo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)

# Crea la gráfica principal con el ajuste de ylim
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(0, max_y * 1.1))

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Añade la línea y los puntos de la segunda serie de datos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

# Añade una leyenda
legend("topleft", legend = c("Matrícula", "Matrícula 15-17"),
       col = c("blue", "green"), lty = c(1, 2), pch = c(16, 17))



#---- CATORCE ----
# Desactivar la notación científica en los ejes
options(scipen = 10)

# Calcular el valor máximo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)

# Crea la gráfica principal con el ajuste de ylim
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(0, max_y * 1.1))

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Añade la línea y los puntos de la segunda serie de datos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

# Añade una leyenda
legend("topleft", legend = c("Matrícula", "Matrícula 15-17"),
       col = c("blue", "green"), lty = c(1, 2), pch = c(16, 17))

#---- QUINCE ----


# Desactivar la notación científica en los ejes
options(scipen = 10)

# Ajustar el tamaño de la letra de las etiquetas de los ejes
par(cex.axis = 0.7)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)

# Crea la gráfica principal con el ajuste de ylim
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(0, max_y * 1.1))

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Añade la línea y los puntos de la segunda serie de datos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

# Añade una leyenda
legend("topleft", legend = c("Matrícula", "Matrícula 15-17"),
       col = c("blue", "green"), lty = c(1, 2), pch = c(16, 17))

#---- 16 ----
# Desactivar la notación científica en los ejes
options(scipen = 10)

# Ajustar el tamaño de la letra de las etiquetas de los ejes
par(cex.axis = 0.7)  # Ajusta el tamaño de la fuente de las etiquetas

# Calcular el valor máximo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)

# Crea la gráfica principal con el ajuste de ylim
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(0, max_y * 1.1))

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Añade la línea y los puntos de la segunda serie de datos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

# Añade un eje Y secundario en el lado derecho
axis(4, at = pretty(range(data1$Matricula_15a17, na.rm = TRUE)), las = 1, cex.axis = 0.7)
mtext("Matrícula 15-17", side = 4, line = 3)

# Añade una leyenda
legend("topleft", legend = c("Matrícula", "Matrícula 15-17"),
       col = c("blue", "green"), lty = c(1, 2), pch = c(16, 17))

#---- 17 ----

# Desactivar la notación científica en los ejes
options(scipen = 10)

# Ajustar el tamaño de la letra de las etiquetas de los ejes
par(cex.axis = 0.7)  # Ajusta el tamaño de la fuente de las etiquetas

# Calcular el valor máximo y mínimo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)
min_y <- min(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)

# Crea la gráfica principal con el ajuste de ylim
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y * 1.1))

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Escala los valores de la serie secundaria para que se ajuste al rango del eje Y
scaled_values <- (data1$Matricula_15a17 - min(data1$Matricula_15a17, na.rm = TRUE)) /
  (max(data1$Matricula_15a17, na.rm = TRUE) - min(data1$Matricula_15a17, na.rm = TRUE)) *
  (max_y - min_y) + min_y

# Añade la línea y los puntos de la segunda serie de datos escalados
lines(scaled_values, col = "green", lty = 2)
points(scaled_values, col = "green", pch = 17)

# Añade una leyenda
legend("topleft", legend = c("Matrícula", "Matrícula 15-17 (escalada)"),
       col = c("blue", "green"), lty = c(1, 2), pch = c(16, 17))

#---- 18 ----
# Desactivar la notación científica en los ejes
options(scipen = 10)

# Ajustar el tamaño de la letra de las etiquetas de los ejes
par(cex.axis = 0.7)  # Ajusta el tamaño de la fuente de las etiquetas

# Calcular el valor máximo y mínimo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)
min_y <- min(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)

# Crea la gráfica principal con el ajuste de ylim
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y * 1.1))

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Personaliza las etiquetas del eje Y
eje_y <- seq(min_y, max_y, by = (max_y - min_y) / 10)  # Divide el rango en 10 pasos (ajusta según lo necesites)
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añade la línea y los puntos de la segunda serie de datos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

# Añade una leyenda
legend("topleft", legend = c("Matrícula", "Matrícula 15-17"),
       col = c("blue", "green"), lty = c(1, 2), pch = c(16, 17))

#---- 19 ----
# Desactivar la notación científica en los ejes
options(scipen = 10)

# Ajustar el tamaño de la letra de las etiquetas de los ejes
par(cex.axis = 0.7)  # Ajusta el tamaño de la fuente de las etiquetas

# Calcular el valor máximo y mínimo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)
min_y <- min(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)

# Crea la gráfica principal con el ajuste de ylim y omitiendo la escala automática del eje Y
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",  # yaxt = "n" omite la escala del eje Y
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y * 1.1))

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Personaliza las etiquetas del eje Y
eje_y <- seq(min_y, max_y, by = (max_y - min_y) / 10)  # Divide el rango en 10 pasos (ajusta según lo necesites)
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añade la línea y los puntos de la segunda serie de datos
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)

# Añade una leyenda
legend("topleft", legend = c("Matrícula", "Matrícula 15-17"),
       col = c("blue", "green"), lty = c(1, 2), pch = c(16, 17))

#---- 20 ----
# Redondea min_y y max_y al múltiplo de 10 más cercano
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crea la secuencia con valores múltiplos de 10
eje_y <- seq(min_y_rounded, max_y_rounded, by = 50000)

#---- 21 ----
# Desactivar la notación científica en los ejes
options(scipen = 10)

# Ajustar el tamaño de la letra de las etiquetas de los ejes
par(cex.axis = 0.65)

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)

# Redondear min_y y max_y al múltiplo de 10 más cercano o a un valor que se ajuste mejor
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crea la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y_rounded, max_y_rounded))

# Añade etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Crear una secuencia de valores en el eje Y con incrementos de 10 o valores más grandes si es necesario
eje_y <- seq(min_y_rounded, max_y_rounded, by = 10)  # Ajusta `by` según la escala que quieras

# Añadir la escala personalizada en el eje Y
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añade la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añade una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total"),
       col = c("blue", "red"), lty = c(1, 2), pch = c(16, 17))

#---- 22 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y_rounded, max_y_rounded))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 10)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añadir la línea y los puntos de la serie de Absorción
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)

# Añadir una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))

#---- 23 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
mtext(data1$Ciclo_Escolar, side = 1, at = 1:nrow(data1), line = 1, srt = 90, adj = 1, cex = 0.5, xpd = TRUE)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añadir la línea y los puntos de la serie de Absorción
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)

# Añadir una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))

#---- 24 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añadir la línea y los puntos de la serie de Absorción
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)

# Añadir una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))

#---- 25 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añadir la línea y los puntos de la serie de Absorción
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)

# Añadir una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))

#---- 26 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))

# Añadir etiquetas del eje X con rotación vertical usando mtext
axis(1, at = 1:nrow(data1), labels = FALSE)
mtext(side = 1, at = 1:nrow(data1), text = data1$Ciclo_Escolar, line = 1, srt = 90, adj = 1, cex = 0.5, xpd = TRUE)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añadir la línea y los puntos de la serie de Absorción
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)

# Añadir una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))

#---- 27 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añadir la línea y los puntos de la serie de Absorción
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)

# Añadir una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))

#---- 28 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.1 * (max_y - min_y),  # Ajusta la posición 'y' para que sea visible
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añadir la línea y los puntos de la serie de Absorción
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)

# Añadir una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))

#---- 29 ----
# Crear un archivo PDF para guardar el gráfico
pdf("grafico_matricula.pdf")

# Configuraciones previas al gráfico
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las tres series
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal con la escala ajustada
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.1 * (max_y - min_y), 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea y los puntos de la serie de Cobertura Total
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)

# Añadir la línea y los puntos de la serie de Absorción
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)

# Añadir una leyenda
legend("bottomright", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))

# Finalizar la salida al archivo PDF
dev.off()

#---- 30 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE) + 100000
min_y <- min(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE) - 100000

# Crear la gráfica principal con la escala ajustada
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Matrícula total y por edad de 15 a 17 a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y - 1000, max_y * 1.1))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(floor(min_y / 10) * 10, floor(max_y / 10) * 10 + 1000000, by = 500000)
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea punteada en el año 2024
index_2024 <- which(data1$Ciclo_Escolar == "2024")
if (length(index_2024) > 0) {
  abline(v = index_2024, col = "black", lty = 3)  # Línea punteada vertical en el año 2024
}

# Añadir la línea y los puntos de la serie de matrícula 15-17
lines(data1$Matricula_15a17, col = "red", lty = 2)
points(data1$Matricula_15a17, col = "red", pch = 16)

# Añadir una leyenda
legend("bottomright", legend = c("Matrícula", "Matrícula 15-17"),
       col = c("blue", "red"), lty = c(1, 2), pch = c(16, 17))


#---- 31 ----
options(scipen = 10)
par(cex.axis = 0.65)  # Ajusta este valor para cambiar el tamaño de la fuente

# Calcular el valor máximo y mínimo entre las dos series
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE) + 100000
min_y <- min(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE) - 100000

# Crear la gráfica principal con la escala ajustada
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Matrícula total y por edad de 15 a 17 a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y - 1000, max_y * 1.1))

# Añadir etiquetas del eje X con rotación vertical
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.5, 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Crear una secuencia de valores en el eje Y
eje_y <- seq(floor(min_y / 10) * 10, floor(max_y / 10) * 10 + 1000000, by = 500000)
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la línea punteada en el año "2023-24"
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)  # Línea punteada vertical en "2023-24"
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}

# Añadir la línea y los puntos de la serie de matrícula 15-17
lines(data1$Matricula_15a17, col = "red", lty = 2)
points(data1$Matricula_15a17, col = "red", pch = 16)

# Añadir una leyenda
legend("bottomright", legend = c("Matrícula", "Matrícula 15-17"),
       col = c("blue", "red"), lty = c(1, 2), pch = c(16, 17))

#---- 32 ----
# Cargar biblioteca necesaria
library(ggplot2)

# Transformar los datos para ggplot
data_piramide_long <- reshape2::melt(data_piramide, id.vars = "Grupo_Edad")

# Cambiar el signo de la población masculina para que se visualice hacia la izquierda
data_piramide_long$value[data_piramide_long$variable == "Hombres"] <- 
  -data_piramide_long$value[data_piramide_long$variable == "Hombres"]

# Crear la pirámide poblacional
ggplot(data_piramide_long, aes(x = Grupo_Edad, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +  # Mostrar valores absolutos en el eje Y
  labs(title = "Pirámide Poblacional",
       x = "Grupo de Edad",
       y = "Población",
       fill = "Sexo") +
  theme_minimal()

#---- 33 ----
# Instalar y cargar las bibliotecas necesarias
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Cargar los datos (ajusta la ruta al archivo de ser necesario)
data <- readxl::read_excel("PiramidesPoblacionales.xlsx", sheet = "Hoja1")

# Seleccionar los datos para el año 2024
data_2024 <- data.frame(
  Grupo_Edad = data$Edad,
  Hombres = -data$`Hombres 2024`,  # Negativo para que las barras se orienten a la izquierda
  Mujeres = data$`Mujeres 2024`
)

# Transformar los datos a formato largo
data_2024_long <- melt(data_2024, id.vars = "Grupo_Edad")

# Crear la pirámide poblacional
ggplot(data_2024_long, aes(x = Grupo_Edad, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +  # Mostrar valores absolutos en el eje Y
  labs(title = "Pirámide Poblacional 2024",
       x = "Grupo de Edad",
       y = "Población",
       fill = "Sexo") +
  theme_minimal()

#---- 34 ----
# Cargar la biblioteca necesaria
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(reshape2)
library(readxl)

# Cargar los datos desde el archivo Excel (ajusta la ruta según corresponda)
datosPP <- read_excel("PiramidesPoblacionales.xlsx", sheet = "Hoja1")

# Seleccionar los datos para el año 2024
datosPP_2024 <- data.frame(
  Grupo_Edad = datosPP$Edad,
  Hombres = -datosPP$`Hombres 2024`,  # Negativo para que las barras se orienten a la izquierda
  Mujeres = datosPP$`Mujeres 2024`
)

# Transformar los datos a formato largo
datosPP_2024_long <- melt(datosPP_2024, id.vars = "Grupo_Edad")

# Crear la pirámide poblacional
ggplot(datosPP_2024_long, aes(x = Grupo_Edad, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +  # Mostrar valores absolutos en el eje Y
  labs(title = "Pirámide Poblacional 2024",
       x = "Grupo de Edad",
       y = "Población",
       fill = "Sexo") +
  theme_minimal()

#---- 35 ----
# Cargar la biblioteca necesaria
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(reshape2)
library(readxl)

# Cargar los datos desde el archivo Excel (ajusta la ruta según corresponda)
datosPP <- read_excel("PiramidesPoblacionales.xlsx", sheet = "Hoja1")

# Seleccionar los datos para el año 2024
datosPP_2024 <- data.frame(
  Grupo_Edad = datosPP$Edad,
  Hombres = -datosPP$`Hombres 2024`,  # Negativo para que las barras se orienten a la izquierda
  Mujeres = datosPP$`Mujeres 2024`
)

# Transformar los datos a formato largo
datosPP_2024_long <- melt(datosPP_2024, id.vars = "Grupo_Edad")

# Crear la pirámide poblacional centrada
ggplot(datosPP_2024_long, aes(x = Grupo_Edad, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-max(abs(datosPP_2024_long$value)), max(abs(datosPP_2024_long$value)))) +  # Escala simétrica en el eje Y
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +  # Línea vertical central
  labs(title = "Pirámide Poblacional 2024",
       x = "Grupo de Edad",
       y = "Población",
       fill = "Sexo") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),  # Ocultar líneas de la cuadrícula horizontal
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"))  # Opcional: líneas verticales en el eje Y

#---- 36 ----
# Cargar la biblioteca necesaria
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(reshape2)
library(readxl)

# Cargar los datos desde el archivo Excel (ajusta la ruta según corresponda)
datosPP <- read_excel("PiramidesPoblacionales.xlsx", sheet = "Hoja1")

# Seleccionar los datos para el año 2024
datosPP_2024 <- data.frame(
  Grupo_Edad = datosPP$Edad,
  Hombres = -datosPP$`Hombres 2024`,  # Negativo para que las barras de hombres vayan a la izquierda
  Mujeres = datosPP$`Mujeres 2024`    # Positivo para que las barras de mujeres vayan a la derecha
)

# Transformar los datos a formato largo
datosPP_2024_long <- melt(datosPP_2024, id.vars = "Grupo_Edad")

# Crear la pirámide poblacional centrada
ggplot(datosPP_2024_long, aes(x = Grupo_Edad, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-max(abs(datosPP_2024_long$value)), max(abs(datosPP_2024_long$value)))) +  # Escala simétrica en el eje Y
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +  # Línea vertical central
  labs(title = "Pirámide Poblacional 2024",
       x = "Grupo de Edad",
       y = "Población",
       fill = "Sexo") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),  # Ocultar líneas de la cuadrícula horizontal
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"))  # Opcional: líneas verticales en el eje Y

#---- 37 ----
# Remover filas con valores NA
datosPP_2024_long <- datosPP_2024_long[!is.na(datosPP_2024_long$value), ]

# Crear la pirámide poblacional centrada
ggplot(datosPP_2024_long, aes(x = Grupo_Edad, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = c(-max(abs(datosPP_2024_long$value)), max(abs(datosPP_2024_long$value)))) +  # Escala simétrica en el eje Y
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +  # Línea vertical central
  labs(title = "Pirámide Poblacional 2024",
       x = "Grupo de Edad",
       y = "Población",
       fill = "Sexo") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),  # Ocultar líneas de la cuadrícula horizontal
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"))  # Opcional: líneas verticales en el eje Y

#---- 38 ----
# Crear la pirámide poblacional centrada sin límites en el eje Y
ggplot(datosPP_2024_long, aes(x = Grupo_Edad, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +  # Mostrar valores absolutos sin limitar el rango
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +  # Línea vertical central
  labs(title = "Pirámide Poblacional 2024",
       x = "Grupo de Edad",
       y = "Población",
       fill = "Sexo") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),  # Ocultar líneas de la cuadrícula horizontal
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted"))  # Opcional: líneas verticales en el eje Y

#---- 39 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame
H1 <- round(Hombres20 / 1000, 0)
M1 <- round(Mujeres20 / 1000, 0)
datos <- data.frame(H1, M1, Edad)

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024",  # Corrige el título para el año correcto
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5)

#---- 40 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame
H1 <- round(Hombres20 / 1000, 0)
M1 <- round(Mujeres20 / 1000, 0)
datos <- data.frame(H1, M1, Edad)

# Ordenar los datos por edad si es necesario
datos <- datos[order(datos$Edad), ]

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024", 
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5)


#---- 41 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame
H1 <- round(Hombres20 / 1000, 0)
M1 <- round(Mujeres20 / 1000, 0)
datos <- data.frame(H1, M1, Edad)

# Ordenar los datos por edad si es necesario
datos <- datos[order(datos$Edad), ]

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024", 
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5)

#---- 42 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame
H1 <- round(Hombres20 / 1000, 0)  # Dividir por 1000 para mostrar en miles
M1 <- round(Mujeres20 / 1000, 0)
datos <- data.frame(H1, M1, Edad)

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Título similar al ejemplo
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5,
        Laxis = seq(0, max(H1), by = 500),  # Eje izquierdo con intervalos
        Raxis = seq(0, max(M1), by = 500))  # Eje derecho con intervalos

#---- 43 ----
# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Título similar al ejemplo
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5,
        Laxis = seq(0, max(abs(H1)), by = 500),  # Eje izquierdo con intervalos corregidos
        Raxis = seq(0, max(M1), by = 500))  # Eje derecho con intervalos

#---- 44 ----
# Crear la pirámide poblacional con las barras de los hombres hacia la izquierda
datos$H1 <- -abs(datos$H1)  # Asegurarse de que los valores sean negativos para graficar a la izquierda

pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Título similar al ejemplo
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5,
        Laxis = seq(0, max(abs(H1)), by = 500),  # Eje izquierdo con intervalos
        Raxis = seq(0, max(M1), by = 500))  # Eje derecho con intervalos

#---- 45 ----
# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame con las barras de los hombres orientadas a la izquierda
H1 <- -round(Hombres20 / 1000, 0)  # Negativo para graficar hacia la izquierda
M1 <- round(Mujeres20 / 1000, 0)

datos <- data.frame(H1, M1, Edad)

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Año corregido para 2024
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 2.75)

#---- 46 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame con las barras de los hombres orientadas a la izquierda
H1 <- -round(Hombres20 / 1000, 0)  # Negativo para graficar hacia la izquierda
M1 <- round(Mujeres20 / 1000, 0)

datos <- data.frame(H1, M1, Edad)

# Configurar las etiquetas del eje X y rotarlas 90 grados
par(las = 1)  # Hace que las etiquetas del eje estén rotadas 90 grados

# Crear la pirámide poblacional con etiquetas centrales personalizadas
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Año corregido para 2024
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 2.75,
        AxisFM = list(x = list(las = 2)),  # Rotar las etiquetas del eje X
        labels = datos$Edad)  # Etiquetas de la edad en el centro

#---- 47 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame con las barras de los hombres orientadas a la izquierda
H1 <- -round(Hombres20 / 1000, 0)  # Negativo para graficar hacia la izquierda
M1 <- round(Mujeres20 / 1000, 0)

datos <- data.frame(H1, M1, Edad)

# Configurar las etiquetas del eje X y rotarlas 90 grados
par(las = 1)  # Configura las etiquetas de los ejes para que estén rotadas 90 grados

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Año corregido para 2024
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5)

# Agregar las etiquetas de edad como texto en el centro
mtext(side = 3, at = 1:length(datos$Edad), text = datos$Edad, line = 0.5, cex = 0.8)

#---- 48 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame con las barras de los hombres orientadas a la izquierda
H1 <- -round(Hombres20 / 1000, 0)  # Negativo para graficar hacia la izquierda
M1 <- round(Mujeres20 / 1000, 0)

datos <- data.frame(H1, M1, Edad)

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "",  # Se elimina Clab para agregar etiquetas manualmente
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Año corregido para 2024
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5)

# Agregar las etiquetas de edad de manera vertical en el centro de la pirámide
text(x = 0, y = 1:nrow(datos), labels = datos$Edad, srt = 90, adj = 1, xpd = TRUE, cex = 0.8)

#---- 49 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame con las barras de los hombres orientadas a la izquierda
H1 <- -round(Hombres20 / 1000, 0)  # Negativo para graficar hacia la izquierda
M1 <- round(Mujeres20 / 1000, 0)

datos <- data.frame(H1, M1, Edad)

# Configurar las etiquetas de los ejes X con los valores de población
Laxis <- seq(-max(abs(H1)), 0, by = 500)  # Valores negativos para el eje de los hombres
Raxis <- seq(0, max(M1), by = 500)  # Valores positivos para el eje de las mujeres

# Crear la pirámide poblacional con los valores de población en el eje X
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "",  # Se elimina Clab para agregar etiquetas manualmente
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Año corregido para 2024
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5,
        Laxis = Laxis,  # Eje X para los hombres
        Raxis = Raxis)  # Eje X para las mujeres

# Agregar las etiquetas de edad de manera vertical en el centro de la pirámide
text(x = 0, y = 1:nrow(datos), labels = datos$Edad, srt = 90, adj = 1, xpd = TRUE, cex = 0.8)

#---- 50 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame con las barras de los hombres orientadas a la izquierda
H1 <- -round(Hombres20 / 1000, 0)  # Negativo para graficar hacia la izquierda
M1 <- round(Mujeres20 / 1000, 0)

datos <- data.frame(H1, M1, Edad)

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Año corregido para 2024
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5)

# Agregar los valores de Hombres20 y Mujeres20 como etiquetas en el eje X
axis(1, at = seq(-max(abs(H1)), max(M1), by = 500), labels = FALSE)

# Etiquetas de los valores en el eje X para hombres y mujeres
text(x = seq(-max(abs(H1)), 0, by = 500), y = par("usr")[3] - 1, 
     labels = seq(0, max(abs(H1)), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.8)
text(x = seq(0, max(M1), by = 500), y = par("usr")[3] - 1, 
     labels = seq(0, max(M1), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.8)

#---- 41 ----
# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame con las barras de los hombres orientadas a la izquierda
H1 <- -round(Hombres20 / 1000, 0)  # Negativo para graficar hacia la izquierda
M1 <- round(Mujeres20 / 1000, 0)

datos <- data.frame(H1, M1, Edad)

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Año corregido para 2024
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5)

# Agregar el eje X y las etiquetas de manera vertical
axis(1, at = seq(-max(abs(H1)), max(M1), by = 500), labels = FALSE)

# Etiquetas de los valores en el eje X para hombres y mujeres
text(x = seq(-max(abs(H1)), 0, by = 500), y = par("usr")[3] - 2, 
     labels = seq(0, max(abs(H1)), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.5)
text(x = seq(0, max(M1), by = 500), y = par("usr")[3] - 2, 
     labels = seq(0, max(M1), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.5)


#---- 42 ----
# Desactivar la notación científica
options(scipen = 10)

# Instalar y cargar la biblioteca necesaria
if (!require(plotrix)) install.packages("plotrix")
library(plotrix)

# Definir los datos
Edad <- datosPP$Edad
Hombres20 <- datosPP$`Hombres 2024`
Mujeres20 <- datosPP$`Mujeres 2024`

# Redondear los datos y crear el data frame con las barras de los hombres orientadas a la izquierda
H1 <- -round(Hombres20 / 1000, 0)  # Negativo para graficar hacia la izquierda
M1 <- round(Mujeres20 / 1000, 0)

datos <- data.frame(H1, M1, Edad)

# Crear la pirámide poblacional
pyramid(datos, 
        Llab = "Hombres", 
        Rlab = "Mujeres", 
        Clab = "Edad", 
        main = "Población Hombres/Mujeres en 2024 (en miles)",  # Año corregido para 2024
        Lcol = "green", 
        Rcol = "cyan", 
        Cgap = 0.5)

# Agregar el eje X y las etiquetas de manera vertical
axis(1, at = seq(-max(abs(H1)), max(M1), by = 500), labels = FALSE)

# Etiquetas de los valores en el eje X para hombres y mujeres
text(x = seq(-max(abs(H1)), 0, by = 500), y = par("usr")[3] - 2, 
     labels = seq(0, max(abs(H1)), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.5)
text(x = seq(0, max(M1), by = 500), y = par("usr")[3] - 2, 
     labels = seq(0, max(M1), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

#---- 43 ----
# Configurar la ventana gráfica para mostrar 2 filas y 3 columnas
par(mfrow = c(2, 3))

# Desactivar la notación científica
options(scipen = 10)

# Función para crear una pirámide poblacional para un año específico
crear_piramide <- function(Hombres, Mujeres, Edad, titulo) {
  H1 <- -round(Hombres / 1000, 0)  # Negativo para graficar hacia la izquierda
  M1 <- round(Mujeres / 1000, 0)
  datos <- data.frame(H1, M1, Edad)
  
  pyramid(datos, 
          Llab = "Hombres", 
          Rlab = "Mujeres", 
          Clab = "Edad", 
          main = titulo,  # Título para el año correspondiente
          Lcol = "green", 
          Rcol = "cyan", 
          Cgap = 0.5)
  
  # Agregar el eje X y las etiquetas de manera vertical
  axis(1, at = seq(-max(abs(H1)), max(M1), by = 500), labels = FALSE)
  text(x = seq(-max(abs(H1)), 0, by = 500), y = par("usr")[3] - 2, 
       labels = seq(0, max(abs(H1)), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.5)
  text(x = seq(0, max(M1), by = 500), y = par("usr")[3] - 2, 
       labels = seq(0, max(M1), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.5)
}

# Crear las pirámides para los años 2024 a 2030
crear_piramide(datosPP$Hombres_2024, datosPP$Mujeres_2024, datosPP$Edad, "Población Hombres/Mujeres en 2024 (en miles)")
crear_piramide(datosPP$Hombres_2025, datosPP$Mujeres_2025, datosPP$Edad, "Población Hombres/Mujeres en 2025 (en miles)")
crear_piramide(datosPP$Hombres_2026, datosPP$Mujeres_2026, datosPP$Edad, "Población Hombres/Mujeres en 2026 (en miles)")
crear_piramide(datosPP$Hombres_2027, datosPP$Mujeres_2027, datosPP$Edad, "Población Hombres/Mujeres en 2027 (en miles)")
crear_piramide(datosPP$Hombres_2028, datosPP$Mujeres_2028, datosPP$Edad, "Población Hombres/Mujeres en 2028 (en miles)")
crear_piramide(datosPP$Hombres_2029, datosPP$Mujeres_2029, datosPP$Edad, "Población Hombres/Mujeres en 2029 (en miles)")

#---- 44 ----
# Configurar la ventana gráfica para mostrar 2 filas y 3 columnas
par(mfrow = c(2, 3), cex.axis = 0.6)  # Ajustar cex.axis para reducir el tamaño de la letra en los ejes

# Desactivar la notación científica
options(scipen = 10)

# Función para crear una pirámide poblacional para un año específico
crear_piramide <- function(Hombres, Mujeres, Edad, titulo) {
  H1 <- -round(Hombres / 1000, 0)  # Negativo para graficar hacia la izquierda
  M1 <- round(Mujeres / 1000, 0)
  datos <- data.frame(H1, M1, Edad)
  
  pyramid(datos, 
          Llab = "Hombres", 
          Rlab = "Mujeres", 
          Clab = "Edad", 
          main = titulo,  # Título para el año correspondiente
          Lcol = "green", 
          Rcol = "cyan", 
          Cgap = 0.5)
  
  # Agregar el eje X y las etiquetas de manera vertical
  axis(1, at = seq(-max(abs(H1)), max(M1), by = 500), labels = FALSE, cex.axis = 0.6)
  text(x = seq(-max(abs(H1)), 0, by = 500), y = par("usr")[3] - 2, 
       labels = seq(0, max(abs(H1)), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.4)  # Ajustar cex para reducir el tamaño
  text(x = seq(0, max(M1), by = 500), y = par("usr")[3] - 2, 
       labels = seq(0, max(M1), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.4)  # Ajustar cex para reducir el tamaño
}

# Crear las pirámides para los años 2024 a 2029
crear_piramide(datosPP$Hombres_2024, datosPP$Mujeres_2024, datosPP$Edad, "Población Hombres/Mujeres en 2024 (en miles)")
crear_piramide(datosPP$Hombres_2025, datosPP$Mujeres_2025, datosPP$Edad, "Población Hombres/Mujeres en 2025 (en miles)")
crear_piramide(datosPP$Hombres_2026, datosPP$Mujeres_2026, datosPP$Edad, "Población Hombres/Mujeres en 2026 (en miles)")
crear_piramide(datosPP$Hombres_2027, datosPP$Mujeres_2027, datosPP$Edad, "Población Hombres/Mujeres en 2027 (en miles)")
crear_piramide(datosPP$Hombres_2028, datosPP$Mujeres_2028, datosPP$Edad, "Población Hombres/Mujeres en 2028 (en miles)")
crear_piramide(datosPP$Hombres_2029, datosPP$Mujeres_2029, datosPP$Edad, "Población Hombres/Mujeres en 2029 (en miles)")

#---- 45 ----
# Configurar la ventana gráfica para mostrar 2 filas y 3 columnas y sin bordes
par(mfrow = c(2, 3), cex.axis = 0.6, bty = "n")  # 'bty = "n"' elimina los bordes alrededor de la gráfica

# Desactivar la notación científica
options(scipen = 10)

# Función para crear una pirámide poblacional para un año específico
crear_piramide <- function(Hombres, Mujeres, Edad, titulo) {
  H1 <- -round(Hombres / 1000, 0)  # Negativo para graficar hacia la izquierda
  M1 <- round(Mujeres / 1000, 0)
  datos <- data.frame(H1, M1, Edad)
  
  pyramid(datos, 
          Llab = "Hombres", 
          Rlab = "Mujeres", 
          Clab = "Edad", 
          main = titulo,  # Título para el año correspondiente
          Lcol = "green", 
          Rcol = "cyan", 
          Cgap = 0.5,
          show.axes = FALSE)  # Desactiva los ejes adicionales
  
  # Agregar el eje X y las etiquetas de manera vertical
  axis(1, at = seq(-max(abs(H1)), max(M1), by = 500), labels = FALSE, cex.axis = 0.6)
  text(x = seq(-max(abs(H1)), 0, by = 500), y = par("usr")[3] - 2, 
       labels = seq(0, max(abs(H1)), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.4)
  text(x = seq(0, max(M1), by = 500), y = par("usr")[3] - 2, 
       labels = seq(0, max(M1), by = 500), srt = 90, adj = 1, xpd = TRUE, cex = 0.4)
}

# Crear las pirámides para los años 2024 a 2029
crear_piramide(datosPP$Hombres_2024, datosPP$Mujeres_2024, datosPP$Edad, "Población Hombres/Mujeres en 2024 (en miles)")
crear_piramide(datosPP$Hombres_2025, datosPP$Mujeres_2025, datosPP$Edad, "Población Hombres/Mujeres en 2025 (en miles)")
crear_piramide(datosPP$Hombres_2026, datosPP$Mujeres_2026, datosPP$Edad, "Población Hombres/Mujeres en 2026 (en miles)")
crear_piramide(datosPP$Hombres_2027, datosPP$Mujeres_2027, datosPP$Edad, "Población Hombres/Mujeres en 2027 (en miles)")
crear_piramide(datosPP$Hombres_2028, datosPP$Mujeres_2028, datosPP$Edad, "Población Hombres/Mujeres en 2028 (en miles)")
crear_piramide(datosPP$Hombres_2029, datosPP$Mujeres_2029, datosPP$Edad, "Población Hombres/Mujeres en 2029 (en miles)")

#---- 46 ----

#---- 47 ----

#---- 48 ----

#---- 49 ----

#---- 50 ----
#---- 41 ----

#---- 42 ----

#---- 43 ----

#---- 44 ----

#---- 45 ----

#---- 46 ----

#---- 47 ----

#---- 48 ----

#---- 49 ----

#---- 50 ----
#---- 41 ----

#---- 42 ----

#---- 43 ----

#---- 44 ----

#---- 45 ----

#---- 46 ----

#---- 47 ----

#---- 48 ----

#---- 49 ----

#---- 50 ----
#---- 41 ----

#---- 42 ----

#---- 43 ----

#---- 44 ----

#---- 45 ----

#---- 46 ----

#---- 47 ----

#---- 48 ----

#---- 49 ----

#---- 50 ----
#---- 41 ----

#---- 42 ----

#---- 43 ----

#---- 44 ----

#---- 45 ----

#---- 46 ----

#---- 47 ----

#---- 48 ----

#---- 49 ----

#---- 50 ----

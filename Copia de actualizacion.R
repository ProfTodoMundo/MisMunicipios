#---- SE CARGAN LAS LIBRERIAS QUE SE VAN A UTILIZAR ----
library(readxl)
#---- SE ESTABLECE EL DIRECTORIO DE TRABAJO ----
setwd("~/Desktop/GraficasAuxiliares")
data1 <- read_excel("estimaciones_crecimiento.xlsx")
#---- CAMBIO DE NOMBRES DE LAS COLUMNAS ----
colnames(data1) <- c("Ciclo_Escolar", "Matricula_Estimada",
                     "Matricula_con_Tasa_de_Crecimiento",
                     "Crecimiento_inercia",
                     "Crecimiento_con_Tasa_de_Crecimiento",
                     "Crecimiento_Necesario_Meta")
#---- CREACION DE GRAFICAS CON LOS COLORES DEL PANTONE ----
pdf("Graficas/grafico_MatriculaEstimada_TasaCP.pdf")
options(scipen = 10)
par(cex.axis = 0.45)
# Calcular valores máximos y mínimos para el eje Y
max_y <- max(data1$Matricula_Estimada, data1$Matricula_con_Tasa_de_Crecimiento, na.rm = TRUE) * 1.1
min_y <- min(data1$Matricula_Estimada, data1$Matricula_con_Tasa_de_Crecimiento, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10
# Crear la gráfica principal
plot(data1$Matricula_Estimada, type = "b", col = rgb(115, 35, 72, maxColorValue = 255), lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Comportamiento de la Matricula", xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))
# Añadir etiquetas al eje X
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.03 * (max_y - min_y), 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)

# Personalizar las etiquetas del eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 50000)  # Ajusta `by` según la escala que desees
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la segunda serie de datos
lines(data1$Matricula_con_Tasa_de_Crecimiento, col = rgb(196, 148, 39, maxColorValue = 255), lty = 2)
points(data1$Matricula_con_Tasa_de_Crecimiento, col = rgb(196, 148, 39, maxColorValue = 255), pch = 17)

# Añadir una línea vertical en "2023-24"
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}

# Añadir una leyenda
legend("topleft", legend = c("Matricula Estimada", "Matricula con tasa de crecimiento"),
       col = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255)), lty = c(1, 2), pch = c(16, 17))

# Finalizar la salida al archivo PDF
dev.off()


# Crear un archivo PDF para guardar el gráfico
pdf("Graficas/grafico_Crecimiento2.pdf")

# Configuración de opciones y gráficos
options(scipen = 10)
par(cex.axis = 0.45)

# Calcular valores máximos y mínimos para el eje Y
max_y <- max(data1$Crecimiento_inercia, data1$Crecimiento_con_Tasa_de_Crecimiento, na.rm = TRUE) * 1.1
min_y <- min(data1$Crecimiento_inercia, data1$Crecimiento_con_Tasa_de_Crecimiento, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear la gráfica principal
plot(data1$Crecimiento_inercia, type = "b", col = rgb(115, 35, 72, maxColorValue = 255), lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Aumento de la matrícula", xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))

# Añadir etiquetas al eje X
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.03 * (max_y - min_y), 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)

# Personalizar las etiquetas del eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 50000)
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir la segunda serie de datos
lines(data1$Crecimiento_con_Tasa_de_Crecimiento, col = rgb(196, 148, 39, maxColorValue = 255), lty = 2)
points(data1$Crecimiento_con_Tasa_de_Crecimiento, col = rgb(196, 148, 39, maxColorValue = 255), pch = 17)

# Añadir una línea vertical en "2023-24"
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}

# Añadir una leyenda
legend("bottomright", legend = c("Crecimiento estimado de la Matricula", "Matricula por tasa de crecimiento"),
       col = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255)), lty = c(1, 2), pch = c(16, 17))

# Finalizar la salida al archivo PDF
dev.off()

# Crear el archivo PDF
pdf("Graficas/lugares_proyectados2.pdf")

# Configuración de opciones y gráficos
options(scipen = 10)
par(cex.axis = 0.45)

# Calcular valores máximos y mínimos para el eje Y
max_y <- max(data1$Crecimiento_Necesario_Meta, na.rm = TRUE) * 1.2
min_y <- min(data1$Crecimiento_con_Tasa_de_Crecimiento, na.rm = TRUE) * 0.8

min_y_rounded <- floor(min_y / 10000) * 10000
max_y_rounded <- ceiling(max_y / 10000) * 10000

# Crear la gráfica principal
plot(data1$Crecimiento_Necesario_Meta, type = "b", col = rgb(196, 148, 39, maxColorValue = 255), lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Aumento de la matrícula para llegar a la meta", xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1)

# Añadir etiquetas al eje X
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.03 * (max_y - min_y), 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)

# Personalizar las etiquetas del eje Y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 100000)
axis(2, at = eje_y, labels = eje_y, las = 1)

# Añadir una línea vertical en "2023-24"
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}

# Añadir una leyenda
legend("bottomright", legend = "Crecimiento de la Matricula para la meta",
       col = rgb(196, 148, 39, maxColorValue = 255), lty = 1, pch = 16)

# Finalizar la salida al archivo PDF
dev.off()

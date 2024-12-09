# Configura el entorno de trabajo y carga los datos
library(readxl)
#setwd("~/Dropbox/BDD_MPIOS")
setwd("~/Desktop/GraficasAuxiliares")
data1 <- read_excel("datos_historico.xlsx")
#---- uno ----
colnames(data1) <- c("Ciclo_Escolar","Matricula","Matricula_15a17","Tasa_Neta_15a17",
                     "Cobertura_Total","Absorcion","Poblacion_15a17_CONAPO",
                     "Egresados_Secundaria","Nuevo_Ingreso_Primero","Egresados",
                     "Reprobacion","Reprobacion_Total","Abandono_Escolar",
                     "Eficiencia_Terminal","Cobertura","Atencion_Demanda",
                     "Matricula_Total","Primero","Segundo","Tercero","Cuarto")

#---- 29 ----
# Crear un archivo PDF para guardar el gráfico
pdf("Graficas/grafico_TasaNeta_Cobertura_Absorcion.pdf")
options(scipen = 10)
par(cex.axis = 0.45)  
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad, Absorción y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y))
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.03 * (max_y - min_y), 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)
lines(data1$Absorcion, col = "green", lty = 3)
points(data1$Absorcion, col = "green", pch = 18)
# Añadir la línea punteada en el año "2023-24"
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)  # Línea punteada vertical en "2023-24"
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}
legend("topleft", legend = c("Tasa Neta de Escolaridad", "Cobertura Total", "Absorción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 3), pch = c(16, 17, 18))
# Finalizar la salida al archivo PDF
dev.off()
#---- GRAFICA 31 ----
pdf("Graficas/grafico_TasaNeta_Cobertura.pdf")
options(scipen = 10)
par(cex.axis = 0.45)  
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10
plot(data1$Tasa_Neta_15a17, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Indicadores", las = 1, ylim = c(min_y, max_y))
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.03 * (max_y - min_y), 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)
lines(data1$Cobertura_Total, col = "red", lty = 2)
points(data1$Cobertura_Total, col = "red", pch = 17)
# Añadir la línea punteada en el año "2023-24"
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)  # Línea punteada vertical en "2023-24"
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}
legend("topleft", legend = c("Tasa Neta de Escolaridad", "Cobertura Total"),
       col = c("blue", "red"), lty = c(1, 2, 3), pch = c(16, 17, 18))
# Finalizar la salida al archivo PDF
dev.off()
#---- HASTA AQUI LAS GRAFICAS VAN BIEN ----
pdf("Graficas/grafico_Matricula_total_Matricula_Edad_Tipica.pdf")
options(scipen = 10)
par(cex.axis = 0.5)  # Ajusta el tamaño de la fuente de las etiquetas
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)
min_y <- min(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10
eje_y <- seq(min_y_rounded, max_y_rounded, by = 150000)  # Ajusta el intervalo según sea necesario
plot(data1$Matricula, type = "b", col = "blue", lty = 1, pch = 16, xaxt = "n", yaxt = "n",  # yaxt = "n" omite la escala del eje Y
     main = "Indicadores de Matrícula por Ciclo Escolar",
     xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y * 1.1))
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - (0.02 * diff(par("usr")[3:4])),  # Ajuste mayor en la posición vertical
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)
axis(2, at = eje_y, labels = eje_y, las = 1)
lines(data1$Matricula_15a17, col = "green", lty = 2)
points(data1$Matricula_15a17, col = "green", pch = 17)
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)  # Línea punteada vertical en "2023-24"
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}
legend("topleft", legend = c("Matrícula Total", "Matrícula Edad 15-17"),
       col = c("blue", "green"), lty = c(1, 2), pch = c(16, 17))
dev.off()
#---- 31 ----

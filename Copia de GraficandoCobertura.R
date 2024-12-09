# Crear el archivo PDF para guardar la gráfica
pdf("Graficas/grafico_Matricula_total_Matricula_Edad_Tipica2.pdf")

# Ajustar opciones de visualización
options(scipen = 10)
par(cex.axis = 0.5)  # Ajusta el tamaño de la fuente de las etiquetas

# Calcular los límites de y
max_y <- max(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)
min_y <- min(data1$Matricula, data1$Matricula_15a17, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10
eje_y <- seq(min_y_rounded, max_y_rounded, by = 150000)  # Ajusta el intervalo según sea necesario

# Crear el gráfico
plot(data1$Matricula, type = "b", col = rgb(115, 35, 72, maxColorValue = 255), lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Indicadores de Matrícula por Ciclo Escolar", xlab = "Ciclo Escolar", ylab = "Matrícula", las = 1, ylim = c(min_y, max_y * 1.1))

# Agregar el eje x con las etiquetas rotadas
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - (0.02 * diff(par("usr")[3:4])),
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.5)

# Agregar el eje y
axis(2, at = eje_y, labels = eje_y, las = 1)

# Agregar la línea y puntos de "Matrícula Edad 15-17"
lines(data1$Matricula_15a17, col = rgb(196, 148, 39, maxColorValue = 255), lty = 2)
points(data1$Matricula_15a17, col = rgb(196, 148, 39, maxColorValue = 255), pch = 17)

# Agregar línea vertical en el ciclo "2023-24" si existe
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)  # Línea punteada vertical en "2023-24"
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}

# Agregar la leyenda
legend("topleft", legend = c("Matrícula Total", "Matrícula Edad 15-17"),
       col = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255)), lty = c(1, 2), pch = c(16, 17))

# Cerrar el archivo PDF
dev.off()


# Crear el archivo PDF para guardar la gráfica
pdf("Graficas/grafico_TasaNeta_Cobertura2.pdf")

# Ajustar opciones de visualización
options(scipen = 10)
par(cex.axis = 0.45)  

# Calcular los límites de y
max_y <- max(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE) + 0.1
min_y <- min(data1$Tasa_Neta_15a17, data1$Cobertura_Total, data1$Absorcion, na.rm = TRUE)
min_y_rounded <- floor(min_y / 10) * 10
max_y_rounded <- ceiling(max_y / 10) * 10

# Crear el gráfico
plot(data1$Tasa_Neta_15a17, type = "b", col = rgb(115, 35, 72, maxColorValue = 255), lty = 1, pch = 16, xaxt = "n", yaxt = "n",
     main = "Tasa Neta de Escolaridad y Cobertura Total a nivel nacional",
     xlab = "Ciclo Escolar", ylab = "Indicadores", las = 1, ylim = c(min_y, max_y))

# Agregar el eje x con las etiquetas rotadas
axis(1, at = 1:nrow(data1), labels = FALSE)
text(x = 1:nrow(data1), y = par("usr")[3] - 0.03 * (max_y - min_y), 
     labels = data1$Ciclo_Escolar, srt = 90, adj = 1, xpd = TRUE, cex = 0.65)

# Agregar el eje y
eje_y <- seq(min_y_rounded, max_y_rounded, by = 0.05)  # Ajusta `by` según la escala que quieras
axis(2, at = eje_y, labels = eje_y, las = 1)

# Agregar la línea y puntos de "Cobertura Total"
lines(data1$Cobertura_Total, col = rgb(196, 148, 39, maxColorValue = 255), lty = 2)
points(data1$Cobertura_Total, col = rgb(196, 148, 39, maxColorValue = 255), pch = 17)

# Añadir la línea punteada en el año "2023-24"
index_2023_24 <- which(data1$Ciclo_Escolar == "2023-24")
if (length(index_2023_24) > 0) {
  abline(v = index_2023_24, col = "black", lty = 3)  # Línea punteada vertical en "2023-24"
} else {
  message("El ciclo '2023-24' no se encontró en la columna Ciclo_Escolar.")
}

# Agregar la leyenda
legend("topleft", legend = c("Tasa Neta de Escolaridad", "Cobertura Total"),
       col = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255)), lty = c(1, 2), pch = c(16, 17))

# Finalizar la salida al archivo PDF
dev.off()

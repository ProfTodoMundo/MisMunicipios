# Cargar librerías necesarias
library(readxl)
library(ggplot2)
library(reshape2)

# Cargar los datos desde el archivo Excel
datosPP <- read_excel("PiramidesPoblacionales.xlsx", sheet = "Hoja1")
#---- Piramide poblacional 2024 ----
# Crear una pirámide poblacional para un año específico
pdf("Graficas/PiramidePoblacional2024.pdf")
datosPP_2024 <- data.frame(
  Edad = datosPP$Edad,
  Hombres = datosPP$`Hombres 2024`,
  Mujeres = datosPP$`Mujeres 2024`
)
# Transformar los datos a formato largo
datosPP_2024_long <- melt(datosPP_2024, id.vars = "Edad", variable.name = "Genero", value.name = "Poblacion")
# Crear la pirámide poblacional con ggplot2
ggplot(datosPP_2024_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(labels = abs, name = "Población") +
  coord_flip() +
  labs(title = "Población Hombres/Mujeres en 2024", x = "Edad", y = "Población") +
  scale_fill_manual(values = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255))) +
  theme_minimal()+
  theme(plot.title = element_text(size = 20))
dev.off()
#---- Piramide poblacional 2025 ----
# Crear una pirámide poblacional para un año específico
pdf("Graficas/PiramidePoblacional2025.pdf")
datosPP_2025 <- data.frame(
  Edad = datosPP$Edad,
  Hombres = datosPP$`Hombres 2025`,
  Mujeres = datosPP$`Mujeres 2025`
)
# Transformar los datos a formato largo
datosPP_2025_long <- melt(datosPP_2025, id.vars = "Edad", variable.name = "Genero", value.name = "Poblacion")
# Crear la pirámide poblacional con ggplot2
ggplot(datosPP_2025_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(labels = abs, name = "Población") +
  coord_flip() +
  labs(title = "Población Hombres/Mujeres en 2025", x = "Edad", y = "Población") +
  scale_fill_manual(values = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255))) +
  theme_minimal()+
  theme(plot.title = element_text(size = 20))
dev.off()
#---- Piramide poblacional 2026 ----
# Crear una pirámide poblacional para un año específico
pdf("Graficas/PiramidePoblacional2026.pdf")
datosPP_2026 <- data.frame(
  Edad = datosPP$Edad,
  Hombres = datosPP$`Hombres 2026`,
  Mujeres = datosPP$`Mujeres 2026`
)
# Transformar los datos a formato largo
datosPP_2026_long <- melt(datosPP_2026, id.vars = "Edad", 
                          variable.name = "Genero", value.name = "Poblacion")
# Crear la pirámide poblacional con ggplot2
ggplot(datosPP_2026_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(labels = abs, name = "Población") +
  coord_flip() +
  labs(title = "Población Hombres/Mujeres en 2026", x = "Edad", y = "Población") +
  scale_fill_manual(values = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255))) +
  theme_minimal()+
  theme(plot.title = element_text(size = 20))
dev.off()
#---- Piramide poblacional 2027 ----
pdf("Graficas/PiramidePoblacional2027.pdf")
# Crear una pirámide poblacional para un año específico
datosPP_2027 <- data.frame(
  Edad = datosPP$Edad,
  Hombres = datosPP$`Hombres 2027`,
  Mujeres = datosPP$`Mujeres 2027`
)
# Transformar los datos a formato largo
datosPP_2027_long <- melt(datosPP_2027, id.vars = "Edad", 
                          variable.name = "Genero", value.name = "Poblacion")
# Crear la pirámide poblacional con ggplot2
ggplot(datosPP_2027_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(labels = abs, name = "Población") +
  coord_flip() +
  labs(title = "Población Hombres/Mujeres en 2027", x = "Edad", y = "Población") +
  scale_fill_manual(values = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255))) +
  theme_minimal()+
  theme(plot.title = element_text(size = 20))
dev.off()
#---- Piramide poblacional 2028 ----
# Crear una pirámide poblacional para un año específico
pdf("Graficas/PiramidePoblacional2028.pdf")
datosPP_2028 <- data.frame(
  Edad = datosPP$Edad,
  Hombres = datosPP$`Hombres 2028`,
  Mujeres = datosPP$`Mujeres 2028`
)
# Transformar los datos a formato largo
datosPP_2028_long <- melt(datosPP_2028, id.vars = "Edad", 
                          variable.name = "Genero", value.name = "Poblacion")
# Crear la pirámide poblacional con ggplot2
ggplot(datosPP_2028_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(labels = abs, name = "Población") +
  coord_flip() +
  labs(title = "Población Hombres/Mujeres en 2028", x = "Edad", y = "Población") +
  scale_fill_manual(values = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255))) +
  theme_minimal()+
  theme(plot.title = element_text(size = 20))
dev.off()
#---- Piramide poblacional 2029 ----
# Crear una pirámide poblacional para un año específico
pdf("Graficas/PiramidePoblacional2029.pdf")
datosPP_2029 <- data.frame(
  Edad = datosPP$Edad,
  Hombres = datosPP$`Hombres 2029`,
  Mujeres = datosPP$`Mujeres 2029`
)
# Transformar los datos a formato largo
datosPP_2029_long <- melt(datosPP_2029, id.vars = "Edad", 
                          variable.name = "Genero", value.name = "Poblacion")
# Crear la pirámide poblacional con ggplot2
ggplot(datosPP_2029_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(labels = abs, name = "Población") +
  coord_flip() +
  labs(title = "Población Hombres/Mujeres en 2029", x = "Edad", y = "Población") +
  scale_fill_manual(values = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255))) +
  theme_minimal()+
  theme(plot.title = element_text(size = 20))
dev.off()
#---- Piramide poblacional 2030 ----
# Crear una pirámide poblacional para un año específico
pdf("Graficas/PiramidePoblacional2030.pdf")
datosPP_2030 <- data.frame(
  Edad = datosPP$Edad,
  Hombres = datosPP$`Hombres 2030`,
  Mujeres = datosPP$`Mujeres 2030`
)
# Transformar los datos a formato largo
datosPP_2030_long <- melt(datosPP_2030, id.vars = "Edad", 
                          variable.name = "Genero", value.name = "Poblacion")
# Crear la pirámide poblacional con ggplot2
ggplot(datosPP_2030_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_y_continuous(labels = abs, name = "Población") +
  coord_flip() +
  labs(title = "Población Hombres/Mujeres en 2030", x = "Edad", y = "Población") +
  scale_fill_manual(values = c(rgb(115, 35, 72, maxColorValue = 255), rgb(196, 148, 39, maxColorValue = 255))) +
  theme_minimal()+
  theme(plot.title = element_text(size = 20))
dev.off()
#---- UNIFICANDO GRAFICAS ----
# Cargar las librerías necesarias
library(readxl)
library(ggplot2)
library(reshape2)
library(gridExtra)

# Crear una lista para almacenar los gráficos
graficos <- list()

# Cargar los datos desde el archivo Excel
datosPP <- read_excel("PiramidesPoblacionales.xlsx", sheet = "Hoja1")

# Crear pirámides poblacionales de 2024 a 2030
anhos <- 2027:2030
for (anho in anhos) {
  # Crear el data frame para el año específico
  datosPP_anho <- data.frame(
    Edad = datosPP$Edad,
    Hombres = datosPP[[paste0("Hombres ", anho)]],
    Mujeres = datosPP[[paste0("Mujeres ", anho)]]
  )
  
  # Transformar los datos a formato largo
  datosPP_anho_long <- melt(datosPP_anho, id.vars = "Edad", variable.name = "Genero", value.name = "Poblacion")
  
  # Crear el gráfico de pirámide poblacional
  grafico <- ggplot(datosPP_anho_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_y_continuous(labels = abs, name = "Población") +
    coord_flip() +
    labs(title = paste("Población Hombres/Mujeres en", anho), x = "Edad", y = "Población") +
    scale_fill_manual(values = c("green", "blue")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 6))
  
  # Añadir el gráfico a la lista
  graficos[[as.character(anho)]] <- grafico
}


# Guardar el archivo PDF con la cuadrícula 2x3
pdf("Graficas/PiramidesPoblacionales2027_2030.pdf", width = 14, height = 10)
grid.arrange(grobs = graficos, ncol = 2, nrow = 2)
dev.off()


# Crear pirámides poblacionales de 2024 a 2030
anhos <- 2024:2027
for (anho in anhos) {
  # Crear el data frame para el año específico
  datosPP_anho <- data.frame(
    Edad = datosPP$Edad,
    Hombres = datosPP[[paste0("Hombres ", anho)]],
    Mujeres = datosPP[[paste0("Mujeres ", anho)]]
  )
  
  # Transformar los datos a formato largo
  datosPP_anho_long <- melt(datosPP_anho, id.vars = "Edad", variable.name = "Genero", value.name = "Poblacion")
  
  # Crear el gráfico de pirámide poblacional
  grafico <- ggplot(datosPP_anho_long, aes(x = Edad, y = Poblacion, fill = Genero)) +
    geom_bar(stat = "identity", position = "identity") +
    scale_y_continuous(labels = abs, name = "Población") +
    coord_flip() +
    labs(title = paste("Población Hombres/Mujeres en", anho), x = "Edad", y = "Población") +
    scale_fill_manual(values = c("green", "blue")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 6))
  
  # Añadir el gráfico a la lista
  graficos[[as.character(anho)]] <- grafico
}


# Guardar el archivo PDF con la cuadrícula 2x3
pdf("Graficas/PiramidesPoblacionales2024_2027.pdf", width = 14, height = 10)
grid.arrange(grobs = graficos, ncol = 2, nrow = 2)
dev.off()


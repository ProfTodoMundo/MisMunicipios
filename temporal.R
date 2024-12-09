#---- 32 ----
# Cargar biblioteca necesaria
#---- 33 ----
# Instalar y cargar las bibliotecas necesarias
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(reshape2)
library(readxl)
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

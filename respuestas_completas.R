
# Librerías necesarias
library(sf)
library(leaflet)
library(jsonlite)
library(readr)

# Configuración inicial
setwd("~/Documents/BDD_CENEVAL/GEOLOCALIZACIONES")

# Lectura de datos
opciones_educativas_csv <- read_delim("Informacion_Opciones_Educativas_2024.csv", delim = "|")
View(opciones_educativas_csv)

# Renombrar columnas de Latitud y Longitud
names(opciones_educativas_csv)[17] <- "Latitud"
names(opciones_educativas_csv)[18] <- "Longitud"

# Conversión de coordenadas a numérico
opciones_educativas_csv$Latitud <- gsub(",", ".", opciones_educativas_csv$Latitud)
opciones_educativas_csv$Longitud <- gsub(",", ".", opciones_educativas_csv$Longitud)
opciones_educativas_csv$Latitud <- trimws(opciones_educativas_csv$Latitud)
opciones_educativas_csv$Longitud <- trimws(opciones_educativas_csv$Longitud)
opciones_educativas_csv$Latitud <- as.numeric(opciones_educativas_csv$Latitud)
opciones_educativas_csv$Longitud <- as.numeric(opciones_educativas_csv$Longitud)

# Filtrar filas con coordenadas válidas
opciones_educativas_csv <- opciones_educativas_csv[!is.na(opciones_educativas_csv$Latitud) & !is.na(opciones_educativas_csv$Longitud), ]

# Verificar los datos
summary(opciones_educativas_csv$Latitud)
summary(opciones_educativas_csv$Longitud)

# Crear un mapa con Leaflet
leaflet(misdatos) %>%
  addTiles() %>%
  addMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    popup = ~Institucion
  )



##---- me estan pensado ----
invalid_lat <- opciones_educativas_csv[is.na(opciones_educativas_csv$Latitud), ]
invalid_long <- opciones_educativas_csv[is.na(opciones_educativas_csv$Longitud), ]

# Ver las filas con problemas
print(invalid_lat)
print(invalid_long)
unique(opciones_educativas_csv$Latitud)
unique(opciones_educativas_csv$Longitud)
# Reemplazar comas con puntos
opciones_educativas_csv$Latitud <- gsub(",", ".", opciones_educativas_csv$Latitud)
opciones_educativas_csv$Longitud <- gsub(",", ".", opciones_educativas_csv$Longitud)

# Remover espacios en blanco
opciones_educativas_csv$Latitud <- trimws(opciones_educativas_csv$Latitud)
opciones_educativas_csv$Longitud <- trimws(opciones_educativas_csv$Longitud)

# Convertir nuevamente a numérico
opciones_educativas_csv$Latitud <- as.numeric(opciones_educativas_csv$Latitud)
opciones_educativas_csv$Longitud <- as.numeric(opciones_educativas_csv$Longitud)
opciones_educativas_csv <- opciones_educativas_csv[!is.na(opciones_educativas_csv$Latitud) & !is.na(opciones_educativas_csv$Longitud), ]
summary(opciones_educativas_csv$Latitud)
summary(opciones_educativas_csv$Longitud)
leaflet(opciones_educativas_csv) %>%
  addTiles() %>%
  addMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    popup = ~`CONALEP ÁLVARO OBREGÓN I`
  )





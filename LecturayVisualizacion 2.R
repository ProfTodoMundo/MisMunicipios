#---- LECTURA DE LOS ARCHIVOS ENVIACOS POR CONNECT IT ----
#---- INSTALACION DE LAS LIBRERIAS NECESARIAS Y SE DEFINE EL DIRECTORIO DE TRABAJO ----
#install.packages(c("sf", "jsonlite", "readr", "leaflet"))
setwd("~/Documents/BDD_CENEVAL/GEOLOCALIZACIONES")
#---- SE CARGAN LAS LIBRERIAS ----
library(sf)
library(leaflet)
library(jsonlite)
library(readr)
#---- Leer archivo CSV ----
opciones_educativas_csv <- read_delim("Informacion_Opciones_Educativas_2024.csv", delim = "|")

names(opciones_educativas_csv) <- c(
  "ClavePlantel", "Tipo", "Institucion",
  "NombreInstitucion", "Especialidad1","Institucion_Especialidad",
  "Direccion_Plantel", "ValorExtra1", "Especialidad",
  "DatoExtra1", "ClaveEstado", "ClaveMunicipio", 
  "DatoExtra2", "DatoExtra3", "DatoExtra4",
  "DatoExtra5", "DatoExtra6", "DatoExtra7", 
  "DatoExtra8", "DatoExtra9", "DatoExtra10",
  "Latitud", "Longitud",  "DatoExtra11",
  "DatoExtra12", "DatoExtra13", "DatoExtra14",
  "DatoExtra15", "DatoExtra16", "DatoExtra17",
  "DatoExtra18")
opciones_educativas_csv$Latitud <- as.numeric(opciones_educativas_csv$Latitud)
opciones_educativas_csv$Longitud <- as.numeric(opciones_educativas_csv$Longitud)
View(opciones_educativas_csv)

misdatos <- opciones_educativas_csv[,c(
  "ClavePlantel", "Institucion",
  "NombreInstitucion", "Especialidad1","Especialidad",
  "Institucion_Especialidad","Direccion_Plantel",
  "ClaveEstado", "ClaveMunicipio", "Latitud", 
  "Longitud", 
  "ValorExtra1",  "DatoExtra1",  "DatoExtra2", 
  "DatoExtra3",  "DatoExtra4",  "DatoExtra5", 
  "DatoExtra6",  "DatoExtra7",  "DatoExtra8",
  "DatoExtra9",  "DatoExtra10", "DatoExtra11",
  "DatoExtra12", "DatoExtra13", "DatoExtra14",
  "DatoExtra15", "DatoExtra16", "DatoExtra17",
  "DatoExtra18", "Tipo")]
View(misdatos)
write.csv(misdatos,"OpcionesEducativasPreliminar.csv")
# ---- Personalizar los colores por categoría ----
codigo_colores <- c("#161a1d", "#9b2247", "#a57f2c", "#7e664a",
                    "#611232", "#e6d194", "#c39326", "#806b4a")

# Calcular luminosidad y ordenar colores
calcular_luminosidad <- function(color) {
  rgb <- col2rgb(color)
  return(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}
luminosidades <- calcular_luminosidad(codigo_colores)
colores_ordenados <- codigo_colores[order(luminosidades)]
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)

# Extender la paleta si hay más categorías
categorias <- unique(misdatos$Institucion)
paleta_extendida <- mi_paleta_ordenada(length(categorias))

# Asignar un color a cada categoría
color_categoria <- setNames(paleta_extendida, categorias)

# Crear iconos personalizados
iconos <- awesomeIcons(
  icon = 'graduation-cap',
  iconColor = 'white', 
  markerColor = sapply(misdatos$Institucion, function(x) color_categoria[x])
)

# ---- Crear un mapa con Leaflet ----
leaflet(misdatos) %>%
  addTiles() %>%
  addAwesomeMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    icon = iconos,
    popup = ~Institucion_Especialidad
  )



#---- correcciones ----
# Crear un data frame con colores asignados
color_categoria_df <- data.frame(
  Institucion = categorias,
  Color = paleta_extendida
)

# Unir colores a los datos
misdatos <- merge(misdatos, color_categoria_df, 
                  by.x = "Institucion", 
                  by.y = "Institucion", all.x = TRUE)

# Crear iconos personalizados
iconos <- awesomeIcons(
  icon = 'graduation-cap',
  iconColor = 'white',
  markerColor = misdatos$Color
)


leaflet(misdatos) %>%
  addTiles() %>%
  addAwesomeMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    icon = iconos,
    popup = ~Institucion_Especialidad
  )

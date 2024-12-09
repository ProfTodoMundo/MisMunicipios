#---- EJEMPLO QUE FUNCIONA ----

if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("diegovalle/mxmaps")
library("mxmaps")

setwd("~/Documents/IMAGENES_PRESENTACIONES/MisMunicipios")
codigo_colores <- c("#161a1d", "#9b2247", "#a57f2c", "#7e664a",
                    "#611232", "#e6d194", "#c39326", "#806b4a")

#---- CORRECCION FINAL ----
library(ggplot2)
library(mxmaps)

# Establecer el directorio de trabajo
setwd("~/Documents/IMAGENES_PRESENTACIONES/MisMunicipios")

# Municipio de interés y su población
municipio_interes <- "Xalapa"
estado_interes <- "Veracruz"
poblacion <- df_mxmunicipio_2020$pop[df_mxmunicipio_2020$municipio_name == municipio_interes]
poblacion_formateada <- format(poblacion, big.mark = ",", scientific = FALSE)

# Crear una nueva columna para destacar el municipio de interés
df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, poblacion, NA)

# Filtrar los contornos solo para el estado de interés
contornos <- subset(mxmunicipio.map, region %in% subset(df_mxmunicipio_2020, state_name == estado_interes)$region)

# Crear el mapa con bordes visibles de menor intensidad
mapa <- suppressWarnings({
  mxmunicipio_choropleth(df_mxmunicipio_2020, 
                         num_colors = 1,
                         zoom = subset(df_mxmunicipio_2020, state_name %in% c(estado_interes))$region,
                         title = paste(estado_interes, "-", municipio_interes, "Población:", poblacion_formateada),
                         show_states = FALSE,
                         legend = "Población total") +
    scale_fill_gradient(na.value = "white", low = "#9b2247", high = "#9b2247") +
    geom_path(data = contornos, 
              aes(x = long, y = lat, group = group),
              color = "#00000080", size = 0.5) +  # Color negro con 50% de transparencia
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Título centrado y en negrita
      legend.position = "none",               # Deshabilitar la leyenda
      plot.margin = margin(10, 10, 10, 10),  # Márgenes alrededor del gráfico
      panel.border = element_rect(color = "black", fill = NA, size = 1), # Bordes externos
      panel.background = element_blank()     # Fondo blanco
    )
})

# Guardar el mapa como PNG optimizado para PowerPoint
nombre_archivo <- paste0(municipio_interes, "_", estado_interes, ".png")
ggsave(
  filename = nombre_archivo, 
  plot = mapa, 
  width = 12,         # Aumentar ancho de la imagen
  height = 9,         # Aumentar alto de la imagen
  dpi = 300           # Alta resolución para PowerPoint
)

message("Mapa guardado en: ", getwd(), "/", nombre_archivo)
#---- GENERACION DE TODOS LOS MUNICIPIOS ----
library(ggplot2)
library(mxmaps)

# Establecer el directorio de trabajo donde se guardarán las imágenes
output_dir <- "~/Documents/IMAGENES_PRESENTACIONES/MisMunicipios"
dir.create(output_dir, showWarnings = FALSE)  # Crear la carpeta si no existe
setwd(output_dir)

# Filtrar los municipios únicos del archivo fuente
municipios <- unique(df_mxmunicipio_2020$municipio_name)
estados <- unique(df_mxmunicipio_2020$state_name)

# Generar gráficos para cada municipio
for (i in 1:length(municipios)) {
  # Municipio y estado de interés
  municipio_interes <- municipios[i]
  estado_interes <- df_mxmunicipio_2020$state_name[df_mxmunicipio_2020$municipio_name == municipio_interes][1]
  poblacion <- df_mxmunicipio_2020$pop[df_mxmunicipio_2020$municipio_name == municipio_interes]
  poblacion_formateada <- format(poblacion, big.mark = ",", scientific = FALSE)
  
  # Crear una nueva columna para destacar el municipio de interés
  df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, poblacion, NA)
  
  # Filtrar los contornos solo para el estado de interés
  contornos <- subset(mxmunicipio.map, region %in% subset(df_mxmunicipio_2020, state_name == estado_interes)$region)
  
  # Crear el mapa con bordes visibles
  mapa <- suppressWarnings({
    mxmunicipio_choropleth(df_mxmunicipio_2020, 
                           num_colors = 1,
                           zoom = subset(df_mxmunicipio_2020, state_name %in% c(estado_interes))$region,
                           title = paste(estado_interes, "-", municipio_interes, "Población:", poblacion_formateada),
                           show_states = FALSE,
                           legend = "Población total") +
      scale_fill_gradient(na.value = "white", low = "#7a001f", high = "#7a001f") +
      geom_path(data = contornos, 
                aes(x = long, y = lat, group = group),
                color = "#00000080", size = 0.5) +  # Color negro con 50% de transparencia
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Título centrado y en negrita
        legend.position = "none",               # Deshabilitar la leyenda
        plot.margin = margin(10, 10, 10, 10),  # Márgenes alrededor del gráfico
        panel.border = element_rect(color = "black", fill = NA, size = 1), # Bordes externos
        panel.background = element_blank()     # Fondo blanco
      )
  })
  
  # Guardar el mapa como PNG optimizado para PowerPoint
  nombre_archivo <- paste0(estado_interes,  "_", municipio_interes, ".png")
  ggsave(
    filename = nombre_archivo, 
    plot = mapa, 
    width = 12,         # Aumentar ancho de la imagen
    height = 9,         # Aumentar alto de la imagen
    dpi = 300           # Alta resolución para PowerPoint
  )
  
  message("Mapa guardado para: ", municipio_interes, " en ", getwd(), "/", nombre_archivo)
}


#---- GENERACION DE TODOS LOS MUNICIPIOS ----
library(ggplot2)
library(mxmaps)

# Establecer el directorio de trabajo donde se guardarán las imágenes
output_dir <- "~/Documents/IMAGENES_PRESENTACIONES/MisMunicipios"
dir.create(output_dir, showWarnings = FALSE)  # Crear la carpeta si no existe
setwd(output_dir)

# Filtrar los municipios únicos del archivo fuente
municipios <- unique(df_mxmunicipio_2020$municipio_name)
estados <- unique(df_mxmunicipio_2020$state_name)

# Generar gráficos para cada municipio
for (i in 1:length(municipios)) {
  # Municipio y estado de interés
  municipio_interes <- municipios[i]
  estado_interes <- df_mxmunicipio_2020$state_name[df_mxmunicipio_2020$municipio_name == municipio_interes][1]
  poblacion <- df_mxmunicipio_2020$pop[df_mxmunicipio_2020$municipio_name == municipio_interes]
  poblacion_formateada <- format(poblacion, big.mark = ",", scientific = FALSE)
  
  # Crear una nueva columna para destacar el municipio de interés
  df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, poblacion, NA)
  
  # Filtrar los contornos solo para el estado de interés
  contornos <- subset(mxmunicipio.map, region %in% subset(df_mxmunicipio_2020, state_name == estado_interes)$region)
  
  # Crear el mapa con bordes visibles
  mapa <- suppressWarnings({
    mxmunicipio_choropleth(df_mxmunicipio_2020, 
                           num_colors = 1,
                           zoom = subset(df_mxmunicipio_2020, state_name %in% c(estado_interes))$region,
                           title = paste(estado_interes, "-", municipio_interes, "Población:", poblacion_formateada),
                           show_states = FALSE,
                           legend = "Población total") +
      scale_fill_gradient(na.value = "white", low = "#7a001f", high = "#7a001f") +
      geom_path(data = contornos, 
                aes(x = long, y = lat, group = group),
                color = "#00000080", size = 0.5) +  # Color negro con 50% de transparencia
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Título centrado y en negrita
        legend.position = "none",               # Deshabilitar la leyenda
        plot.margin = margin(10, 10, 10, 10),  # Márgenes alrededor del gráfico
        panel.border = element_rect(color = "black", fill = NA, size = 1), # Bordes externos
        panel.background = element_blank()     # Fondo blanco
      )
  })
  
  # Guardar el mapa como PNG optimizado para PowerPoint
  nombre_archivo <- paste0(municipio_interes, "_", estado_interes, ".png")
  ggsave(
    filename = nombre_archivo, 
    plot = mapa, 
    width = 12,         # Aumentar ancho de la imagen
    height = 9,         # Aumentar alto de la imagen
    dpi = 300           # Alta resolución para PowerPoint
  )
  
  message("Mapa guardado para: ", municipio_interes, " en ", getwd(), "/", nombre_archivo)
}




#---- versiones previas ----
# Municipio de interés y su población
municipio_interes <- "Xalapa"
estado_interes <- "Veracruz"
poblacion <- df_mxmunicipio_2020$pop[df_mxmunicipio_2020$municipio_name == municipio_interes]
poblacion_formateada <- format(poblacion, big.mark = ",", scientific = FALSE)

# Crear una nueva columna para destacar el municipio de interés
df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, poblacion, NA)

# Crear el mapa con ggplot2 para personalizar la leyenda
suppressWarnings({
  mxmunicipio_choropleth(df_mxmunicipio_2020, 
                         num_colors = 1,
                         zoom = subset(df_mxmunicipio_2020, state_name %in% c(estado_interes))$region,
                         title = paste(estado_interes, "-", municipio_interes, "Población:", poblacion_formateada),
                         show_states = FALSE,
                         legend = "Población total") +
    scale_fill_gradient(na.value = "white", low = "#7a001f", high = "#7a001f") +
    theme(legend.position = "none")  # Deshabilitar la leyenda
})

#---- TERMINA EL GRAFICO QUE FUNCIONA, AHORA GENEREMOS EL PNG ----

# Establecer el directorio de trabajo
setwd("~/Documents/IMAGENES_PRESENTACIONES/MisMunicipios")

# Paleta de colores personalizada
codigo_colores <- c("#161a1d", "#9b2247", "#a57f2c", "#7e664a",
                    "#611232", "#e6d194", "#c39326", "#806b4a")

# Municipio de interés y su población
municipio_interes <- "Xalapa"
estado_interes <- "Veracruz"
poblacion <- df_mxmunicipio_2020$pop[df_mxmunicipio_2020$municipio_name == municipio_interes]
poblacion_formateada <- format(poblacion, big.mark = ",", scientific = FALSE)

# Crear una nueva columna para destacar el municipio de interés
df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, poblacion, NA)

# Crear el mapa con bordes visibles y márgenes optimizados
mapa <- suppressWarnings({
  mxmunicipio_choropleth(df_mxmunicipio_2020, 
                         num_colors = 1,
                         zoom = subset(df_mxmunicipio_2020, state_name %in% c(estado_interes))$region,
                         title = paste(estado_interes, "-", municipio_interes, "Población:", poblacion_formateada),
                         show_states = FALSE,
                         legend = "Población total") +
    scale_fill_gradient(na.value = "white", low = "#7a001f", high = "#7a001f") +
    theme(
      legend.position = "none",               # Deshabilitar la leyenda
      plot.margin = margin(10, 10, 10, 10),  # Márgenes alrededor del gráfico
      panel.border = element_rect(color = "black", fill = NA, size = 1)  # Bordes negros alrededor del gráfico
    )
})

# Guardar el mapa como PNG optimizado para PowerPoint
nombre_archivo <- paste0(municipio_interes, "_", estado_interes, ".png")
ggsave(
  filename = nombre_archivo, 
  plot = mapa, 
  width = 12,         # Aumentar ancho de la imagen
  height = 9,         # Aumentar alto de la imagen
  dpi = 300           # Alta resolución para PowerPoint
)

message("Mapa guardado en: ", getwd(), "/", nombre_archivo)




#---- CORRECCIONES DE LOS BORDES ----

library(ggplot2)
library(mxmaps)

# Establecer el directorio de trabajo
setwd("~/Documents/IMAGENES_PRESENTACIONES/MisMunicipios")

# Municipio de interés y su población
municipio_interes <- "Xalapa"
estado_interes <- "Veracruz"
poblacion <- df_mxmunicipio_2020$pop[df_mxmunicipio_2020$municipio_name == municipio_interes]
poblacion_formateada <- format(poblacion, big.mark = ",", scientific = FALSE)

# Crear una nueva columna para destacar el municipio de interés
df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, poblacion, NA)

# Filtrar los contornos solo para el estado de interés
contornos <- subset(mxmunicipio.map, region %in% subset(df_mxmunicipio_2020, state_name == estado_interes)$region)

# Crear el mapa con bordes visibles de menor intensidad
mapa <- suppressWarnings({
  mxmunicipio_choropleth(df_mxmunicipio_2020, 
                         num_colors = 1,
                         zoom = subset(df_mxmunicipio_2020, state_name %in% c(estado_interes))$region,
                         title = paste(estado_interes, "-", municipio_interes, "Población:", poblacion_formateada),
                         show_states = FALSE,
                         legend = "Población total") +
    scale_fill_gradient(na.value = "white", low = "#7a001f", high = "#7a001f") +
    geom_path(data = contornos, 
              aes(x = long, y = lat, group = group),
              color = "#00000080", size = 0.35) +  # Color negro con 50% de transparencia
    theme(
      legend.position = "none",               # Deshabilitar la leyenda
      plot.margin = margin(10, 10, 10, 10),  # Márgenes alrededor del gráfico
      panel.border = element_rect(color = "black", fill = NA, size = 1), # Bordes externos
      panel.background = element_blank()     # Fondo blanco
    )
})

# Guardar el mapa como PNG optimizado para PowerPoint
nombre_archivo <- paste0(municipio_interes, "_", estado_interes, ".png")
ggsave(
  filename = nombre_archivo, 
  plot = mapa, 
  width = 12,         # Aumentar ancho de la imagen
  height = 9,         # Aumentar alto de la imagen
  dpi = 300           # Alta resolución para PowerPoint
)

message("Mapa guardado en: ", getwd(), "/", nombre_archivo)




#---- CORRECCIONES DE LOS BORDES 1----
library(ggplot2)
library(mxmaps)

# Establecer el directorio de trabajo
setwd("~/Documents/IMAGENES_PRESENTACIONES/MisMunicipios")

# Municipio de interés y su población
municipio_interes <- "Xalapa"
estado_interes <- "Veracruz"
poblacion <- df_mxmunicipio_2020$pop[df_mxmunicipio_2020$municipio_name == municipio_interes]
poblacion_formateada <- format(poblacion, big.mark = ",", scientific = FALSE)

# Crear una nueva columna para destacar el municipio de interés
df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, poblacion, NA)

# Filtrar los contornos solo para el estado de interés
contornos <- subset(mxmunicipio.map, region %in% subset(df_mxmunicipio_2020, state_name == estado_interes)$region)

# Crear el mapa con bordes visibles solo para el estado y municipios seleccionados
mapa <- suppressWarnings({
  mxmunicipio_choropleth(df_mxmunicipio_2020, 
                         num_colors = 1,
                         zoom = subset(df_mxmunicipio_2020, state_name %in% c(estado_interes))$region,
                         title = paste(estado_interes, "-", municipio_interes, "Población:", poblacion_formateada),
                         show_states = FALSE,
                         legend = "Población total") +
    scale_fill_gradient(na.value = "white", low = "#7a001f", high = "#7a001f") +
    geom_path(data = contornos, 
              aes(x = long, y = lat, group = group),
              color = "black", size = 0.5) +  # Grosor y color de los bordes
    theme(
      legend.position = "none",               # Deshabilitar la leyenda
      plot.margin = margin(10, 10, 10, 10),  # Márgenes alrededor del gráfico
      panel.border = element_rect(color = "black", fill = NA, size = 1), # Bordes externos
      panel.background = element_blank()     # Fondo blanco
    )
})

# Guardar el mapa como PNG optimizado para PowerPoint
nombre_archivo <- paste0(municipio_interes, "_", estado_interes, ".png")
ggsave(
  filename = nombre_archivo, 
  plot = mapa, 
  width = 12,         # Aumentar ancho de la imagen
  height = 9,         # Aumentar alto de la imagen
  dpi = 300           # Alta resolución para PowerPoint
)

message("Mapa guardado en: ", getwd(), "/", nombre_archivo)




#---- OTRA SECCION ----
df_mxstate_2020$value <- df_mxstate_2020$pop
mxstate_choropleth(df_mxstate_2020,
                   title = "Población total por estado") 

data("df_mxmunicipio_2020")





df_mxmunicipio_2020$value <-  df_mxmunicipio_2020$pop
mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,
                       title = "Percentage of the population that speaks\nan indigenous language",
                       legend = "%")



mxmunicipio_choropleth(df_mxmunicipio_2020, num_colors = 5,
                       zoom = subset(df_mxmunicipio_2020, metro_area %in% 
                                       c("Oaxaca"))$region,
                       title = "Percentage of the population that speaks\nan indigenous language",
                       legend = "%") 


mxmunicipio_choropleth(df_mxmunicipio_2020, num_colors = 9,
                       zoom = subset(df_mxmunicipio_2020, state_name %in% 
                                       c("Veracruz"))$region,
                       title = "Percentage of the population that speaks\nan indigenous language in Yucatán and Veracruz",
                       show_states = FALSE,
                       legend = "%")

# Crear una paleta personalizada de tonos guinda
guinda_palette <- c("#ffccd5", "#fda4af", "#fa7298", "#f84f83", 
                    "#d72638", "#b21f2b", "#8c1721", "#7a001f", "#5f0019")

# Crear el mapa con colores personalizados
mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 9,  # Número de colores debe coincidir con la paleta
                       zoom = subset(df_mxmunicipio_2020, state_name %in% c("Veracruz"))$region,
                       title = "Percentage of the population that speaks\nan indigenous language in Veracruz",
                       show_states = FALSE,
                       legend = "%") +
  scale_fill_manual(values = guinda_palette, name = "% Población indígena")

# Municipio de interés
municipio_interes <- "Xalapa"

# Crear una nueva columna para destacar solo el municipio de interés
df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, 1, NA)

# Crear el mapa con un único color para el municipio de interés
mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,  # Solo un color
                       zoom = subset(df_mxmunicipio_2020, state_name %in% c("Veracruz"))$region,
                       title = paste("Municipio destacado:", municipio_interes),
                       show_states = FALSE,
                       legend = "") +
  scale_fill_gradient(na.value = "white", low = "#7a001f", high = "#7a001f", name = municipio_interes)


# Municipio de interés y su población
municipio_interes <- "Xalapa"
estado_interes <- "Veracruz"
poblacion <- df_mxmunicipio_2020$pop[df_mxmunicipio_2020$municipio_name == municipio_interes]

# Crear una nueva columna para destacar el municipio de interés
df_mxmunicipio_2020$value <- ifelse(df_mxmunicipio_2020$municipio_name == municipio_interes, poblacion, NA)

# Crear el mapa con ggplot2 para personalizar la leyenda
mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,
                       zoom = subset(df_mxmunicipio_2020, state_name %in% c(estado_interes))$region,
                       title = paste(estado_interes, "-", municipio_interes,"Población:",poblacion),
                       show_states = FALSE,
                       legend = "Población total") +
  scale_fill_gradient(na.value = "white", low = "#7a001f", high = "#7a001f")

setwd("~/Documents/ARCHIVOS_PRESENTACIONES")

# Instalar las librerías necesarias (si no están instaladas)
if (!require(pdftools)) install.packages("pdftools")
if (!require(magick)) install.packages("magick")

library(pdftools)
library(magick)

# Directorio donde están los archivos PDF
directorio_pdf <- "GraficaProyeccionesEdad"
# Directorio de salida para los archivos PNG
directorio_salida <- "ImagenesPNG"

# Crear el directorio de salida si no existe
if (!dir.exists(directorio_salida)) {
  dir.create(directorio_salida)
}

# Obtener la lista de archivos PDF en el directorio
archivos_pdf <- list.files(directorio_pdf, pattern = "\\.pdf$", full.names = TRUE)

# Iterar sobre cada archivo PDF
for (archivo_pdf in archivos_pdf) {
  # Nombre base del archivo (sin extensión)
  nombre_base <- tools::file_path_sans_ext(basename(archivo_pdf))
  
  # Leer las páginas del PDF
  num_paginas <- pdf_info(archivo_pdf)$pages
  for (i in 1:num_paginas) {
    # Renderizar la página a una imagen
    imagen <- pdf_render_page(archivo_pdf, page = i, dpi = 300)
    imagen_magick <- image_read(imagen)
    
    # Guardar la imagen como archivo PNG, con el mismo nombre que el archivo PDF
    # Si hay más de una página, añade "_pagina_X" al final del nombre
    if (num_paginas > 1) {
      ruta_salida <- file.path(directorio_salida, paste0(nombre_base, "_pagina_", i, ".png"))
    } else {
      ruta_salida <- file.path(directorio_salida, paste0(nombre_base, ".png"))
    }
    
    # Guardar la imagen en formato PNG
    image_write(imagen_magick, path = ruta_salida, format = "png")
  }
  
  cat("Procesado:", archivo_pdf, "\n")
}

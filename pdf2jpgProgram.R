

library(pdftools)
library(magick)


# Establecer el directorio de trabajo
#setwd("~/Documents/MacroReportes/COAHUILA")
setwd("~/Documents/MacroReportes/NUEVO_LEON")

# Carpeta de salida para las imágenes
output_folder <- "IMAGENES"

# Crear la carpeta de salida si no existe
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Obtener lista de archivos PDF en la carpeta actual
pdf_files <- list.files(pattern = "\\.pdf$", full.names = TRUE)

# Función para convertir PDF a JPG
convert_pdf_to_jpg <- function(pdf_path, output_folder) {
  pdf_name <- tools::file_path_sans_ext(basename(pdf_path))
  pdf_pages <- pdf_info(pdf_path)$pages
  
  for (page in 1:pdf_pages) {
    # Leer la página del PDF como imagen
    image <- image_read_pdf(pdf_path, pages = page)
    
    # Guardar la imagen como JPG
    output_file <- file.path(output_folder, paste0(pdf_name, "_page", page, ".jpg"))
    image_write(image, path = output_file, format = "jpg")
    cat("Página", page, "del archivo", pdf_name, "convertida a JPG\n")
  }
}

# Convertir todos los PDFs en la carpeta
for (pdf_file in pdf_files) {
  convert_pdf_to_jpg(pdf_file, output_folder)
}

cat("Conversión completada. Las imágenes están en:", file.path(getwd(), output_folder), "\n")

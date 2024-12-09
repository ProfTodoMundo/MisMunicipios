# Definir los colores
codigo_colores <- c("#242A34", "#9b2247", "#a57f2c", "#7e664a",
                    "#611232", "#e6d194", "#c39326", "#806b4a")

# Función para calcular luminosidad
calcular_luminosidad <- function(color) {
  # Convertir el color hexadecimal a RGB
  rgb <- col2rgb(color)
  # Calcular la luminosidad según el estándar
  luminosidad <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  return(luminosidad)
}

# Calcular luminosidades para la paleta
luminosidades <- calcular_luminosidad(codigo_colores)

# Crear una paleta ordenada basada en luminosidad
colores_ordenados <- codigo_colores[order(luminosidades)]
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)

# Extender la paleta a un número mayor de colores
paleta_extendida <- mi_paleta_ordenada(20)  # Por ejemplo, generar 20 colores

# Ver los resultados
print(paleta_extendida)

"#611232" "#4A1A32" "#342333" "#302936" "#5C263D"
[6] "#882344" "#943047" "#8A4948" "#7F6249" "#7E674A"
[11] "#7F694A" "#816C48" "#8F733D" "#9D7A32" "#A9822B"
[16] "#B48928" "#BF9026" "#CCA342" "#D9BA6B" "#E6D194"


"#611232" "#4A1A32" "#342333" "#302936" "#5C263D" "#882344" "#943047"
"#8A4948" "#7F6249" "#7E674A" "#7F694A" "#816C48" "#8F733D" "#9D7A32"
"#A9822B" "#B48928" "#BF9026" "#CCA342" "#D9BA6B" "#E6D194"


"#611232" "#372233" "#4F273A" "#942347"
"#3F3452" "#1E3A59" "#283E5C" "#695C4E"
"#7E684A" "#756954" "#456084" "#726F5A"
"#AB832A" "#BF9026" "#D28937" "#E09759"
"#E5CC8F" "#BFCAD1" "#BCD4F8" "#E6F2FF"

# Definir los colores
codigo_colores <- c( "#1E3A5F", "#3B5F8F","#242A34", "#9b2247", "#a57f2c", "#7e664a",
                    "#611232", "#e6d194", "#c39326", "#806b4a","#A9C7F5", "#E6F2FF", "#1F3B56")

# Función para calcular luminosidad
calcular_luminosidad <- function(color) {
  # Convertir el color hexadecimal a RGB
  rgb <- col2rgb(color)
  # Calcular la luminosidad según el estándar
  luminosidad <- 0.2126 * rgb[1, ] + 0.9152 * rgb[2, ] + 0.3722 * rgb[3, ]
  return(luminosidad)
}

# Calcular luminosidades para la paleta
luminosidades <- calcular_luminosidad(codigo_colores)

# Ordenar colores por luminosidad
colores_ordenados <- codigo_colores[order(luminosidades)]

# Crear una paleta ordenada con más tonalidades
mi_paleta_ordenada <- colorRampPalette(colores_ordenados)

# Extender la paleta a un número mayor de colores (ejemplo: 20 tonos)
paleta_extendida <- mi_paleta_ordenada(20)

# Verificar los resultados
print(paleta_extendida)

# Graficar la paleta para visualizar los colores
barplot(rep(1, length(paleta_extendida)), col = paleta_extendida, border = NA, space = 0, main = "Paleta Extendida")


codigo_colores <- c("#242A34", "#1E3A5F", "#3B5F8F", "#4973B9", 
                    "#6A94D4", "#A9C7F5", "#E6F2FF", "#1F3B56")
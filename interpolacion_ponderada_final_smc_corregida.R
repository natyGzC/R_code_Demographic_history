# Cargar las librerías necesarias
library(dplyr)

# Cargar las dos tablas
table1 <- read.csv("/Users/redstudiodurango/Desktop/articulo_smc_mexico/Total_poblaciones_Mexico/analisis_clima/Global_paleo-temperature_Ne.csv")
table2 <- read.csv("/Users/redstudiodurango/Desktop/articulo_smc_mexico/Total_poblaciones_Mexico/analisis_clima/mediana_tiempo_clima.csv")

# Renombrar columnas si es necesario
colnames(table1)[colnames(table1) == 'Age_2'] <- 'Time'
colnames(table2)[colnames(table2) == 'x'] <- 'Time'

# Función para realizar interpolación ponderada
weighted_interpolation <- function(time_val, time_col, ne_col) {
  # Calcular distancias absolutas
  distances <- abs(time_col - time_val)
  
  # Si hay un valor exactamente igual, devolver ese directamente
  if (0 %in% distances) {
    return(ne_col[which.min(distances)])
  }
  
  # Calcular los pesos como inverso de la distancia (ponderación)
  weights <- 1 / distances
  
  # Calcular la interpolación ponderada
  weighted_ne <- sum(weights * ne_col) / sum(weights)
  
  return(weighted_ne)
}

# Aplicar la interpolación ponderada para calcular Ne en función del tiempo
table1$Ne <- sapply(table1$Time, function(t) {
  if (is.na(t) || t == 0) return(NA)  # Evitar NA si no hay un valor de tiempo
  weighted_interpolation(t, table2$Time, table2$Ne_median)  
})

# Verificar los primeros resultados
print(head(table1))

# Guardar el archivo actualizado
write.csv(table1, "Global_paleo-temperature_Ne_with_Interpolated_Ne.csv", row.names = FALSE)

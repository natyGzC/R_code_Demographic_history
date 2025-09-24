######1.- Crear la mediana con spline
#library(readxl)
basis_data <- read_excel("/Users/redstudiodurango/Desktop/articulo_smc_mexico/Total_poblaciones_Mexico/cromosomas_corregidos/median_data_total_mexico.xlsx")  # Read data, please replace the path
# Load libraries for plotting
library(ggplot2)
as.numeric(basis_data$tiempo2)
as.numeric(basis_data$Ne_real)
library(dplyr)
#install.packages("splines2")
library(splines2)  # Library for spline functions
# Perform spline interpolation for all chromosomes
interpolated_values <- lapply(unique(basis_data$Chrom), function(chrom) {
  subset_data <- basis_data[basis_data$Chrom == chrom, ]
  if (length(unique(subset_data$tiempo2)) >= 4) {
    spline_fit <- tryCatch(
      smooth.spline(subset_data$tiempo2, subset_data$Ne_real, df = 5),
      error = function(e) NULL
    )
    if (!is.null(spline_fit)) {
      spline_values <- predict(spline_fit, x = seq(0, 15000000, by = 500))$y
      spline_values <- pmin(spline_values, 1000000)  # Limit values to 20000 for plotting
      spline_values <- pmax(spline_values, -1000000)  # Ensure values are not less than 1 for calculations
      data.frame(tiempo2 = seq(0, 15000000, by = 500), Ne_real_approx = spline_values, Chrom = chrom)
    } else {
      data.frame(tiempo2 = rep(NA, times = 151), Ne_real_approx = rep(NA, times = 151), Chrom = chrom)
    }
    
  } else {
    data.frame(tiempo2 = rep(NA, times = 151), Ne_real_approx = rep(NA, times = 151), Chrom = chrom)
  }
  
})
interpolated_df <- do.call(rbind, interpolated_values)

# Calculate the average of the 19 graphs

average_values <- interpolated_df %>%
  
  group_by(tiempo2) %>%
  
  summarize(Ne_real_avg = median(Ne_real_approx, na.rm = TRUE)) 
# Create and display the line plot for all chromosomes and the average

p_all_with_avg <- ggplot() +
  
  geom_line(data = interpolated_df, aes(x = tiempo2, y = pmax(Ne_real_approx, -10000000), color = Chrom)) +
  
  geom_line(data = average_values, aes(x = tiempo2, y = Ne_real_avg, color = "mean"), linetype = "dashed", show.legend = TRUE) +
  
  labs(title = "Interpolated data for all 19 chromosomes with median",
       
       x = "tiempo2",
       
       y = "Ne_real_approx (min 1, max 20000)") +
  
  scale_color_manual(values = c(rainbow(length(unique(basis_data$Chrom))), "black"), name = "Chrom") +
  
  xlim(0, 150000)+ ylim(0, 20000)

print(p_all_with_avg)

# Data cleaning and removal of missing or invalid values
cleaned_df <- interpolated_df[complete.cases(interpolated_df),]
cleaned_df <- cleaned_df[!is.na(cleaned_df$Ne_real_approx),]

p_all_with_avg <- ggplot(cleaned_df, aes(x = tiempo2, y = Ne_real_approx, color = factor(Chrom))) +
  geom_line() +
  stat_summary(geom = "ribbon", fun.data = "mean_cl_normal", alpha = 0.2, color = "blue", aes(ymin = ..ymin.., ymax = ..ymax..)) +
  stat_summary(geom = "line", fun.y = "median", color = "black", size = 0.5) +
  stat_summary(geom = "line", fun.y = "median", color = "black", linetype = "dashed", size = 0.8) +
  labs(title = "Interpolated data for all 19 chromosomes with median and 95% CI",
       x = "tiempo2",
       y = "Ne_real_approx (min 0, max 20000)") +
  scale_color_manual(values = rainbow(length(unique(cleaned_df$Chrom))), name = "Chrom") +
  xlim(0, 150000) + ylim(0, 20000)
print(p_all_with_avg)
# Creamos el objeto ggplot
p_built <- ggplot_build(p_all_with_avg)
############plot al 90%#############
p_all_with_avg1 <- ggplot(cleaned_df, aes(x = tiempo2, y = Ne_real_approx, color = factor(Chrom))) +
  geom_line() +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, conf.int = 0.90, alpha = 0.2, color = "blue", aes(ymin = ..ymin.., ymax = ..ymax..)) +
  stat_summary(geom = "line", fun.y = "median", color = "black", size = 0.5) +
  stat_summary(geom = "line", fun.y = "median", color = "black", linetype = "dashed", size = 0.8) +
  labs(title = "Interpolated data for all 19 chromosomes with median and 90% CI",
       x = "tiempo2",
       y = "Ne_real_approx (min 0, max 20000)") +
  scale_color_manual(values = rainbow(length(unique(cleaned_df$Chrom))), name = "Chrom") +
  xlim(0, 150000) + ylim(0, 20000)
print(p_all_with_avg1)



# Este índice podría cambiar dependiendo de cuántas capas hay en tu gráfico
median_data <- p_built$data[[3]]  # Este es un ejemplo, el índice debe ser ajustado
library(openxlsx)
#write.xlsx(median_data, "/Users/redstudiodurango/Desktop/articulo_smc_mexico/Eje_neovolcanico/interpolaciones/median_data_ENV.xlsx")


####2.- Crear la media con aproxx

library(readxl)
basisdaten<- read_excel("/Users/redstudiodurango/Desktop/articulo_smc_mexico/Eje_neovolcanico/interpolaciones/data_frame_ENV_1.xlsx")  # Read data, please replace the path


# Perform interpolation for all chromosomes
interpolated_values2 <- lapply(unique(basisdaten$Chrom), function(chrom) {
  subset_data <- basisdaten[basisdaten$Chrom == chrom, ]
  approx_values <- approx(subset_data$tiempo2, subset_data$Ne_real, xout = seq(0, 15000000, by = 5000))$y
  data.frame(tiempo2 = seq(0, 15000000, by = 5000), Ne_real_approx = approx_values, Chrom = chrom)
})
interpolated_df2 <- do.call(rbind, interpolated_values2)


# Calculate the average of the 19 plots
average_values2 <- interpolated_df2 %>%
  group_by(tiempo2) %>%
  summarize(Ne_real_avg = median(Ne_real_approx, na.rm = TRUE)) 

# Create and display the line plot for all chromosomes with the average
p_all_with_avg <- ggplot() +
  geom_line(data = interpolated_df2, aes(x = tiempo2, y = Ne_real_approx, color = Chrom)) +
  geom_line(data = average_values2, aes(x = tiempo2, y = Ne_real_avg, color = "mean"), linetype = "dashed", show.legend = TRUE) +
  labs(title = "Interpolated data for all 19 chromosomes with average",
       x = "tiempo2",
       y = "Ne_real_approx") +
  scale_color_manual(values = c(rainbow(length(unique(basisdaten$Chrom))), "black"), name = "Chrom") +
  xlim(0, 150000)

# Display the line plot for all chromosomes with the average
print(p_all_with_avg)




#######################################################################
#########opcion para quitar duplicado 
##########si sale  EL BUENO REPLICAR CON EL RESTO 

#######opcion con las medianas de legendas
library(ggplot2)
library(RColorBrewer)

# Asegúrate de que los datos sean numéricos
basis_data$tiempo2 <- as.numeric(as.character(basis_data$tiempo2))
average_values$tiempo2 <- as.numeric(as.character(average_values$tiempo2))
median_data$x <- as.numeric(as.character(median_data$x))

# Genera la paleta de colores combinada, asegurándote de que cada cromosoma tiene un color
unique_chromosomes <- unique(basis_data$Chrom)
color_palette <- c(brewer.pal(6, "RdGy"), brewer.pal(8, "PRGn"), brewer.pal(5, "Spectral"))
color_palette <- color_palette[1:length(unique_chromosomes)] # Ajusta la longitud de la paleta a la cantidad de cromosomas

# Extiende la paleta de colores para incluir las líneas de promedio
color_palette <- c(setNames(color_palette, unique_chromosomes), "Average spline" = "black", "Average approx" = "blue")


p <- ggplot(basis_data, aes(x = tiempo2, y = Ne_real)) +
  geom_step(aes(color = Chrom), size = 0.5) +  # Usamos aes() aquí para asegurar que los colores de los cromosomas estén en la leyenda
  geom_line(data = median_data, aes(x = x, y = y, color = "Average spline"), linetype = "dashed", size = 0.8) +
  geom_line(data = average_values2, aes(x = tiempo2, y = Ne_real_avg, color = "Average approx"), linetype = "dashed", size = 0.8) +
  labs(title = "Real and Interpolated Average Population Size Over Time in Trans-Mexican Volcanic Belt",
       x = "Years ago",
       y = "Effective population size") +  # Movemos la etiqueta color a scale_color_manual
  scale_color_manual(values = color_palette, 
                     name = "Chromosomes/Lines", 
                     labels = c(unique(basis_data$Chrom), "Median Approx", "Median spline")) +
  scale_linetype_manual(values = c("solid", "dashed"), 
                        name = "Type", 
                        labels = c("Chromosomes", "Median Approx", "Median spline")) +
  coord_cartesian(xlim = c(0, 150000), ylim = c(0, 20500)) +
  theme(legend.position = "right")

# Muestra el gráfico actualizado
print(p)


head(table1)
head(table2)
head(result)






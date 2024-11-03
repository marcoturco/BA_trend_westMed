# Clear the workspace and graphics, prepare for analysis
rm(list = ls()) # Remove all objects from the current workspace
graphics.off() # Close all open graphics devices
gc() # Perform garbage collection to free up memory

# Set working directory
setwd("/Users/marco/Documents/dati/obs/trend_westmed/data/")

# Libraries
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(cowplot)

# Especificar la ruta de guardado
output_dir <- "/Users/marco/Dropbox/estcena/scripts/trends_westMed/figs/"

# Load previously saved data
load("BA_ONFIRE.RData")   # Burned area data for ONFIRE, already masked
load("lon_lat.RData")     # Longitude and latitude data

# Load and prepare shapefile for spatial domain
file_shp <- file.path("domain.shp")
shp <- st_read(file_shp) %>%
  st_transform("EPSG:4326") %>%
  st_union()  # Merge all polygons into one

#####################
### TASK 1: Average Burned Area per Pixel (1985-2015)
#####################

# Subset BA data to 1985-2015 and convert from m^2 to km^2
BA <- BA_ONFIRE[,,61:432]/ 1000000  # Months from January 1985 to December 2015

# Calculate annual sum burned area for each pixel by summing every 12 months
n_years <- dim(BA)[3] / 12  # 31 years for 1985-2015
annual_sum_BA <- array(NA, dim = c(dim(BA)[1], dim(BA)[2], n_years))

# Loop through each year and sum 12 months
for (year in 1:n_years) {
  month_indices <- ((year - 1) * 12 + 1):(year * 12)
  annual_sum_BA[,,year] <- apply(BA[,,month_indices], c(1, 2), sum, na.rm = TRUE)
}

# Calculate the mean annual burned area per pixel over the entire period (1985-2015)
mean_BA_per_pixel <- apply(annual_sum_BA, c(1, 2), mean, na.rm = TRUE)
mean_BA_per_pixel[mean_BA_per_pixel==0]=NA
# Convertir el array a un data frame para ggplot
lon_lat <- expand.grid(lon, lat)
mean_BA_df <- data.frame(lon_lat, mean_BA = as.vector(mean_BA_per_pixel))

# Verificar y corregir geometr?as no v?lidas en shp_sf
shp_sf <- st_make_valid(shp)

# Transformar a una proyecci?n que sea adecuada para el ?rea de estudio
shp_sf <- st_transform(shp_sf, crs = 4326)  # Cambiamos la proyecci?n a WGS 84 (EPSG:4326)

# Convertir el array a un data frame para ggplot
lon_lat <- expand.grid(lon, lat)
mean_BA_df <- data.frame(lon_lat, mean_BA = as.vector(mean_BA_per_pixel))

# Convertir a un objeto sf, manteniendo lon y lat como columnas
mean_BA_sf <- st_as_sf(mean_BA_df, coords = c("Var1", "Var2"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2])

# Filtrar los datos para eliminar filas con NA en mean_BA
mean_BA_sf <- mean_BA_sf[!is.na(mean_BA_sf$mean_BA), ]

# Cargar el mapa mundial
world <- ne_countries(scale = "medium", returnclass = "sf")

# Transformar el mapa mundial a la proyecci?n WGS 84
world <- st_transform(world, crs = 4326)

# Obtener los l?mites del shapefile para centrar el mapa
x_lim <- range(st_coordinates(shp_sf)[, 1])
y_lim <- range(st_coordinates(shp_sf)[, 2])



# Generar el mapa con la leyenda (para extraerla)
p1 <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +  # Capa base con el mundo en gris claro
  geom_sf(data = shp_sf, fill = "#737373", color = "#737373") +  # Dominio de estudio en gris
  geom_tile(data = mean_BA_sf, aes(x = lon, y = lat, fill = mean_BA), alpha = 0.8) +  # Capa de datos con la media de ?rea quemada
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1, 
    name = expression("Average Burned Area (km"^2*")"), 
    na.value = "transparent"
  ) +  # Apply magma color palette and make NA transparent
  coord_sf(xlim = x_lim, ylim = y_lim, expand = FALSE) +  # Centrar en el shp_sf
  theme_minimal() +
  theme(
    legend.text.align = 0.5,
    legend.title.align = 0.5,
    legend.text = element_text(hjust = 0.5, size = 14),
    legend.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "sans"),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "right",  # Mantener la leyenda a la derecha
    axis.text.x = element_text(size = 16),  # Tama?o del texto del eje X
    axis.text.y = element_text(size = 16),  # Tama?o del texto del eje Y
    axis.title.x = element_text(size = 20, face = "bold"),  # T?tulo del eje X en negrita
    axis.title.y = element_text(size = 20, face = "bold")   # T?tulo del eje Y en negrita
  )

print(p1)
# Extraer la leyenda del gr?fico p1_with_legend
p2 <- get_legend(p1)

# Crear el gr?fico p1 sin la leyenda
p1_ <- p1 + theme(legend.position = "none")

print(p1_)


# Guardar p1_ (sin la leyenda) como PDF
ggsave(filename = paste0(output_dir, "map_without_legend.pdf"), plot = p1_, width = 8, height = 6, units = "in")

# Guardar p1_ (sin la leyenda) como PNG
ggsave(filename = paste0(output_dir, "map_without_legend.png"), plot = p1_, width = 8, height = 6, units = "in", dpi = 300)

# Guardar p2 (solo la leyenda) como PDF
ggsave(filename = paste0(output_dir, "legend.pdf"), plot = p2, width = 8, height = 4, units = "in")

# Guardar p2 (solo la leyenda) como PNG
ggsave(filename = paste0(output_dir, "legend.png"), plot = p2, width = 2, height = 4, units = "in", dpi = 300)


#####################
### TASK 2: MEDIA MENSUAL DE ?REA QUEMADA 1985-2015 #########
#####################

# Array para almacenar las sumas mensuales
monthly_sums <- array(NA, dim = c(12, n_years))  

# Sumar el ?rea quemada para cada mes en todos los a?os
for (i in 1:12) {
  monthly_sums[i, ] <- apply(BA[, , seq(i, dim(BA)[3], by = 12)], 3, sum, na.rm = TRUE)
}

# Calcular la media mensual a lo largo de todos los a?os
mean_monthly_BA <- rowMeans(monthly_sums, na.rm = TRUE)

# Verifica los valores calculados
print(mean_monthly_BA)


# Crear un dataframe para usar con ggplot2
data <- data.frame(
  Month = factor(c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December"), 
                 levels = c("January", "February", "March", "April", "May", "June", 
                            "July", "August", "September", "October", "November", "December")),
  Mean_BA = mean_monthly_BA
)

# Crear una columna de categor?a para la leyenda
data$Season <- ifelse(data$Month %in% c("June", "July", "August", "September"), 
                      "Fire Season", "Off-Fire Season")

# Crear el gr?fico de barras con la est?tica deseada y leyenda
p <- ggplot(data, aes(x = Month, y = Mean_BA, fill = Season)) +
  geom_bar(stat = "identity", color = NA, width = 0.8) +  # Se quita el borde blanco de las barras
  scale_fill_manual(values = c("Fire Season" = "darkred", "Off-Fire Season" = "darkgrey")) +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Fondo del panel blanco sin borde
    plot.background = element_rect(fill = "white", color = NA),   # Fondo del gr?fico blanco sin borde
    panel.grid.major = element_line(color = "grey80"),  # Ajuste del color de las l?neas de la cuadr?cula
    panel.grid.minor = element_line(color = "grey90"),  # Ajuste del color de las l?neas menores de la cuadr?cula
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    legend.position = c(0.95, 0.95),  # Posici?n en la esquina superior derecha
    legend.justification = c("right", "top"),  # Alineaci?n de la leyenda
    legend.text = element_text(color = "black"),  # Texto de la leyenda en negro
    legend.title = element_blank(),  # Sin t?tulo en la leyenda
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Average Burned Area by Month (1985-2015)",
    x = "Month",
    y = expression("Average Burned Area (km"^2*")"),
  )

# Imprimir el gr?fico
print(p)

# Guardar como PDF
ggsave(filename = paste0(output_dir, "histogram.pdf"), plot = p, width = 8, height = 6, units = "in")

# Guardar como PNG
ggsave(filename = paste0(output_dir, "histogram.png"), plot = p, width = 8, height = 6, units = "in", dpi = 300)



# Sumar el total de ?rea quemada para todos los p?xeles y todo el per?odo
total_BA <- sum(BA, na.rm = TRUE)

# Imprimir el total calculado
print(paste("Total de ?rea quemada en todo el dominio y per?odo (1985-2015):", total_BA))


# Suma de la media mensual para todo el periodo
total_BA_from_monthly_means <- sum(mean_monthly_BA) * n_years

# Imprimir la suma de las medias mensuales
print(paste("Suma de la media de ?rea quemada mensual multiplicada por el n?mero de a?os:", total_BA_from_monthly_means))

# Comparar ambos resultados
difference <- abs(total_BA - total_BA_from_monthly_means)

print(paste("Diferencia entre el total de ?rea quemada y la suma de las medias mensuales:", difference))


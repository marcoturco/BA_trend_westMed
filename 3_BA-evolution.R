# Clear the workspace and graphics, prepare for analysis
rm(list = ls()) # Remove all objects from the current workspace
graphics.off() # Close all open graphics devices
gc() # Perform garbage collection to free up memory

# Working directory
# setwd("C:/Users/andri/Desktop/tendencias/trends/datos")
setwd("/Users/marco/Documents/dati/obs/trend_westmed/data/")

# Libraries
library(rgdal)
library(ncdf4)
library(fields)
library(maptools)
library(abind)
data(wrld_simpl)
library(grid)
library(cowplot)
library(ggplot2)

output_dir <- "/Users/marco/Dropbox/estcena/scripts/trends_westMed/figs/"

# FUNCTIONS 

sumBA <- function(BA, season){
  d <- dim(BA)
  nlon <- d[1]
  nlat <- d[2]
  ny <- d[3]/12
  if (all(season==seq(season[1], season[length(season)]))) {
    BAsum <- array(NA, dim = c(nlon, nlat, ny))
    ta <- 0
  }
  else if (all(season==c(12,1,2)) ) {
    BAsum <- array(NA, dim = c(nlon, nlat, (ny-1)))
    ta <- 12
  }
  else {
    ta <- 0
    BAsum <- array(NA, dim = c(nlon, nlat, ny))
  }
  for (i in 1:nlon) {
    for (j in 1:nlat) {
      for (iyear in 1:(dim(BAsum)[3])) {
        time_steps <- season + (iyear - 1) * 12 + ta
        BAsum[i, j, iyear] <- sum(BA[i, j, time_steps], na.rm = TRUE)
      }
    }
  }
  return(BAsum)
}


sumField <- function(field) {
  ny <- dim(field)[3]
  series <- array(0, dim = ny)
  for (iyear in 1:ny) {
    series[iyear] = sum(field[, , iyear], na.rm = TRUE)
  }
  return(series)
}


# Definici?n de las seasons y sus nombres
seasons <- list(
  a = c(1,2,3,4,5,6,7,8,9,10,11,12),
  f = c(6,7,8,9),
  nf = c(1,2,3,4,5,10,11,12)
)

########################################
# LOAD THE TOTAL ANNUAL BURNED AREA
########################################

# Mapping of dataset names to variable names
dataset_mapping <- list(
  "ONFIRE" = "BA_ONFIRE",
  "FIRECCI51" = "BA_FIRECCI51",
  "FIRECCI51_nat" = "BA_FIRECCI51_nat",
  "MCD64A1" = "BA_MCD64A1",
  "MCD64A1_nat" = "BA_MCD64A1_nat",
  "GFED5" = "BA_GFED5",
  "GFED5_nat" = "BA_GFED5_nat"
)


# Iterate over each dataset and process
for (dataset in names(dataset_mapping)) {
  # Load the RData file for the current dataset
  load(paste0("BA_", dataset, ".RData"))
  
  # Access the actual BA variable for this dataset
  BA <- get(dataset_mapping[[dataset]])
  
  for (name in names(seasons)) {
    season <- seasons[[name]]
    
    # Calculate seasonal sum of burned area and comnvert to m^2 to km^2
    BA_sum <- sumBA(BA, season)/1000000
    
    result_name <- paste0("BA_", dataset, "_", name)
    assign(result_name, sumField(BA_sum) )  # Adjust unit as per your requirements
    
    print(paste("Result saved as:", result_name))
  }
}


###########PLOT


theme_set(theme_minimal())


###PLOT ANNUAL
f1a<-ggplot() + 
  geom_line(aes(x = (2001):2020, y = BA_FIRECCI51_a, color = "FIRECCI51"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_FIRECCI51_nat_a, color = "FIRECCI51 NAT"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_MCD64A1_a, color = "MCD64A1"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_MCD64A1_nat_a, color = "MCD64A1 NAT"), size = 1) +
  geom_line(aes(x = (1997):2020, y = BA_GFED5_a, color = "GFED5"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_GFED5_nat_a, color = "GFED5 NAT"), size = 1) +
  geom_line(aes(x = (1985):2015, y= BA_ONFIRE_a[(6):36] , color= "ONFIRE"), size = 1) +
  scale_color_manual(values = c("FIRECCI51" = "blue", "FIRECCI51 NAT" = "lightblue", 
                                "MCD64A1" = "darkgreen", "MCD64A1 NAT" = "green", 
                                "GFED5" = "darkviolet", "GFED5 NAT" = "violet", 
                                "ONFIRE" = "black")) +
  labs(
    title = "a) Annual burned area ",
    x = "Year",
    y= expression("Burned Area (km"^2*")"),
    color = "Dataset"
  ) +
  theme(
    text = element_text(size = 12, family = "sans", face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 0.5, face = "bold"),
    axis.title = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
    legend.box.background = element_rect(colour = "grey50")
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_x_continuous(breaks = seq(1985, 2022, 5)) +
  # scale_y_continuous(labels = scales::comma) +
  scale_y_log10(labels = scales::comma, breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  theme_minimal() +
  theme(legend.title.align = 0.5)

print(f1a)


#########PLOT FIRE SEASON
####### PLOT OFF- FIRE SEASON
f1b<-ggplot() + 
  geom_line(aes(x = (2001):2020, y = BA_FIRECCI51_f, color = "FIRECCI51"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_FIRECCI51_nat_f, color = "FIRECCI51 NAT"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_MCD64A1_f, color = "MCD64A1"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_MCD64A1_nat_f, color = "MCD64A1 NAT"), size = 1) +
  geom_line(aes(x = (1997):2020, y = BA_GFED5_f, color = "GFED5"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_GFED5_nat_f, color = "GFED5 NAT"), size = 1) +
  geom_line(aes(x = (1985):2015, y= BA_ONFIRE_f[(6):36] , color= "ONFIRE"), size = 1) +
  scale_color_manual(values = c("FIRECCI51" = "blue", "FIRECCI51 NAT" = "lightblue", 
                                "MCD64A1" = "darkgreen", "MCD64A1 NAT" = "green", 
                                "GFED5" = "darkviolet", "GFED5 NAT" = "violet", 
                                "ONFIRE" = "black")) +
  labs(
    title = "b) Burned Area during fire season",
    x = "Year",
    y= expression("Burned Area (km"^2*")"),
    color = "Dataset"
  ) +
  theme(
    text = element_text(size = 12, family = "sans", face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 0.5, face = "bold"),
    axis.title = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
    legend.box.background = element_rect(colour = "grey50")
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_x_continuous(breaks = seq(1985, 2022, 5)) +
  # scale_y_continuous(labels = scales::comma) +
  scale_y_log10(labels = scales::comma, breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  theme_minimal() +
  theme(legend.title.align = 0.5)

print(f1b)


####### PLOT OFF- FIRE SEASON
f1c<-ggplot() + 
  geom_line(aes(x = (2001):2020, y = BA_FIRECCI51_nf, color = "FIRECCI51"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_FIRECCI51_nat_nf, color = "FIRECCI51 NAT"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_MCD64A1_nf, color = "MCD64A1"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_MCD64A1_nat_nf, color = "MCD64A1 NAT"), size = 1) +
  geom_line(aes(x = (1997):2020, y = BA_GFED5_nf, color = "GFED5"), size = 1) +
  geom_line(aes(x = (2001):2020, y = BA_GFED5_nat_nf, color = "GFED5 NAT"), size = 1) +
  geom_line(aes(x = (1985):2015, y= BA_ONFIRE_nf[(6):36] , color= "ONFIRE"), size = 1) +
  scale_color_manual(values = c("FIRECCI51" = "blue", "FIRECCI51 NAT" = "lightblue", 
                                "MCD64A1" = "darkgreen", "MCD64A1 NAT" = "green", 
                                "GFED5" = "darkviolet", "GFED5 NAT" = "violet", 
                                "ONFIRE" = "black")) +
  labs(
    title = "c) Burned Area during off-fire season ",
    x = "Year",
    y= expression("Burned Area (km"^2*")"),
    color = "Dataset"
  ) +
  theme(
    text = element_text(size = 12, family = "sans", face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 0.5, face = "bold"),
    axis.title = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
    legend.box.background = element_rect(colour = "grey50")
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_x_continuous(breaks = seq(1985, 2022, 5)) +
  # scale_y_continuous(labels = scales::comma) +
  scale_y_log10(labels = scales::comma, breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  theme_minimal() +
  theme(legend.title.align = 0.5)

print(f1c)



##########SAVE PLOT

# Configuración de tema general
theme_set(theme_minimal())

# Guardar solo la leyenda de f1a
leg <- get_legend(f1a)

# Eliminar leyenda de cada gráfico
f1a_ <- f1a + theme(legend.position = "none")
f1b_ <- f1b + theme(legend.position = "none")
f1c_ <- f1c + theme(legend.position = "none")

# Colocar cada gráfico en una línea, seguido de la leyenda en una cuarta fila
figure1 <- plot_grid(
  f1a_, 
  f1b_, 
  f1c_,
  leg,
  ncol = 1, # Configuración en una sola columna
  rel_heights = c(1, 1, 1, 1) # Ajuste para la leyenda
)

# Guardar la figura final en archivos PNG y PDF
# Guardar la figura con tamaño proporcional a la mitad de un A4
ggsave(paste0(output_dir, "figure1.png"), figure1, width = 5.83, height = 5.83 * 1.5, dpi = 300)
ggsave(paste0(output_dir, "figure1.pdf"), figure1, width = 5.83, height = 5.83 * 1.5)

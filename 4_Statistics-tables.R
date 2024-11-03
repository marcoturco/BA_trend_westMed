# Clear the workspace and graphics, prepare for analysis
rm(list = ls()) # Remove all objects from the current workspace
graphics.off() # Close all open graphics devices
gc() # Perform garbage collection to free up memory

# Working directory 
# setwd("C:/Users/andri/Desktop/tendencias/trends/datos/")
setwd("/Users/marco/Documents/dati/obs/trend_westmed/data/")

# Libraries
# install.packages("openxlsx")
library(openxlsx)
library(trend)
library(modifiedmk)

# Nombre del archivo de salida
output_dir <- "/Users/marco/Dropbox/estcena/scripts/trends_westMed/figs/"
#table-common-period.xlsx"

# Define the common period
# Define start years for each dataset
start_years <- c("ONFIRE" = 1980, "FIRECCI51" = 2001, "FIRECCI51_nat" = 2001, 
                 "MCD64A1" = 2001, "MCD64A1_nat" = 2001, "GFED5" = 1997, "GFED5_nat" = 2001)

# Define the common period
common_start <- 2001
common_end <- 2015

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

# # Crear un nuevo workbook
wb <- createWorkbook()

# Arrays to store statistics
stats_list <- list()
# Calcular estad?sticas por temporada usando el periodo com?n
for (season_name in names(seasons)) {
  means <- array(0, dim = length(dataset_mapping))
  deviations <- array(0, dim = length(dataset_mapping))
  correlations <- array(NA, dim = length(dataset_mapping))
  cor_pvalue <- array(NA, dim = length(dataset_mapping))
  trends <- array(NA, dim = length(dataset_mapping))
  pvalue_mk <- array(NA, dim = length(dataset_mapping))
  pvalue_mmkh <- array(NA, dim = length(dataset_mapping))
  
  # Filter ONFIRE data to the common period
  onfire_db <- get(paste0("BA_ONFIRE_", season_name))
  onfire_start_index <- common_start - start_years["ONFIRE"] + 1
  onfire_end_index <- common_end - start_years["ONFIRE"] + 1
  onfire_db <- onfire_db[onfire_start_index:onfire_end_index]
  
  for (i in 1:(length(dataset_mapping))) {
    
    dataset_name <- names(dataset_mapping)[i]
    db <- get(paste0(dataset_mapping[[dataset_name]], "_", season_name))
    
    # Adjust index based on each dataset's start year
    start_index <- common_start - start_years[dataset_name] + 1
    end_index <- common_end - start_years[dataset_name] + 1
    db <- db[start_index:end_index]
    
    
    # Calcular las estad?sticas b?sicas
    means[i] <- round(mean(db), 2)
    deviations[i] <- round(sd(db), 2)
    
    # Calcular la tendencia como porcentaje
    ss <- sens.slope(db, conf.level = 0.95)
    trends[i] <- round((ss$estimates) * length(db) / means[i] * 100, 2)
    pvalue_mk[i] <- ss$p.value
    mmkh_result <- mmkh(as.vector(db))
    pvalue_mmkh[i] <- mmkh_result["new P-value"]
    
    
    # Calcular la correlaci?n con ONFIRE
    corr <- cor.test(db, onfire_db, method = "spearman", conf.level = 0.95, alternative = "greater")
    correlations[i] <- round(corr[["estimate"]][["rho"]], 2)
    cor_pvalue[i] <- corr[["p.value"]] 
    
  }
  
  # Crear una tabla con las estad?sticas calculadas
  stats_common_period <- data.frame(
    Dataset = names(dataset_mapping),
    Mean = means,
    "Standard Deviation" = deviations,
    "Trend (%)" = trends,
    "Trend p-value MK" = pvalue_mk,
    "Trend p-value MK" = pvalue_mmkh,
    "Correlation with ONFIRE" = correlations,
    "Correlation p-value" = cor_pvalue
    )
  
  # Agregar la tabla a una hoja en el archivo Excel
  addWorksheet(wb, paste0("Season_", toupper(season_name)))
  writeData(wb, paste0("Season_", toupper(season_name)), stats_common_period)
}

output_file=paste0(output_dir,"table-common-period.xlsx")
# Guardar el archivo Excel
saveWorkbook(wb, output_file, overwrite = TRUE)

# Confirmaci?n
cat("Archivo guardado en:", output_file)

#######################################

# Define the full period start and end for each dataset
full_period_years <- list(
  "ONFIRE" = c(1985, 2015),
  "FIRECCI51" = c(2001, 2020),
  "FIRECCI51_nat" = c(2001, 2020),
  "MCD64A1" = c(2001, 2020),
  "MCD64A1_nat" = c(2001, 2020),
  "GFED5" = c(1997, 2020),
  "GFED5_nat" = c(2001, 2020)
)


# # Arrays to store statistics
stats_list <- list()

# Iterate over each dataset and season


# Iterate over each season (annual, fire season, non-fire season)
for (j in seq_along(seasons)) {
  means_full <- array(0, dim = c(length(dataset_mapping)))
  deviations_full <- array(0, dim = c(length(dataset_mapping)))
  trends_full <- array(0, dim = c(length(dataset_mapping)))
  pvalue_mk_full <- array(0, dim = c(length(dataset_mapping)))
  pvalue_mmkh_full <- array(0, dim = c(length(dataset_mapping)))
  
  for (i in seq_along(dataset_mapping)) {
    dataset_name <- names(dataset_mapping)[i]
    
    season_name <- names(seasons)[j]
    
    # Access the full-period dataset for the current season
    BA_data <- get(paste0(dataset_mapping[[dataset_name]], "_", season_name))
    
    # Determine the full period start and end indices
    start_index <- full_period_years[[dataset_name]][1] - start_years[dataset_name] + 1
    end_index <- full_period_years[[dataset_name]][2] - start_years[dataset_name] + 1
    BA_full_period <- BA_data[start_index:end_index]
    
    # Calculate basic statistics
    means_full[i] <- round(mean(BA_full_period, na.rm = TRUE), 2)
    deviations_full[i] <- round(sd(BA_full_period, na.rm = TRUE), 2)
    
    # Calculate trend and significance
    # Calcular la tendencia como porcentaje
    ss <- sens.slope(BA_full_period, conf.level = 0.95)
    
    trends_full[i] <- round((ss$estimates) * length(BA_full_period) / means_full[i]  * 100,
                            2)
    pvalue_mk_full[i] <- ss$p.value
    mmkh_result <- mmkh(as.vector(BA_full_period))
    pvalue_mmkh_full[i] <- mmkh_result["new P-value"]
    
    
    #   # Create a table for the current season
    stats <- matrix(
      c(
        means_full,
        deviations_full,
        trends_full,
        pvalue_mk_full,
        pvalue_mmkh_full
      ),
      ncol = 5
    )
    colnames(stats) <- c("Mean",
                         "Standard Deviation",
                         "Trend (%)",
                         "p-value MK",
                         "p-value MMKH")
    
    # Store the table in a list
    stats_list[[season_name]] <- stats
  }
}


print(stats_list$a)  # Estad?sticas de la temporada anual
print(stats_list$f)  # Estad?sticas de la temporada de incendios
print(stats_list$nf) # Estad?sticas de la temporada fuera de incendios


output_file=paste0(output_dir,"table-allperiod.xlsx")

# # Crear un nuevo workbook
wb <- createWorkbook()
#
# Funci?n para agregar la columna con los nombres de los datasets
add_dataset_names <- function(data, row_names) {
  data_with_names <- cbind(Dataset = row_names, data)
  return(data_with_names)
}

dataset_names <- names(dataset_mapping)
#
# Agregar hojas con los datos para cada temporada
addWorksheet(wb, "Season_A")
writeData(wb, "Season_A", add_dataset_names(as.data.frame(stats_list$a), dataset_names))

addWorksheet(wb, "Season_F")
writeData(wb, "Season_F", add_dataset_names(as.data.frame(stats_list$f), dataset_names))
#
addWorksheet(wb, "Season_NF")
writeData(wb, "Season_NF", add_dataset_names(as.data.frame(stats_list$nf), dataset_names))
#
# # Guardar el archivo Excel en la ruta especificada
saveWorkbook(wb, output_file, overwrite = TRUE)
#
# # Confirmaci?n
cat("Archivo guardado en:", output_file)
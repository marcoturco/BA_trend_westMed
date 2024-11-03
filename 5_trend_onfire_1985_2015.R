# Clear the workspace and graphics, prepare for analysis
rm(list = ls()) # Remove all objects from the current workspace
graphics.off() # Close all open graphics devices
gc() # Perform garbage collection to free up memory

# Set working directory
setwd("/Users/marco/Documents/dati/obs/trend_westmed/data/")

# Load required libraries
library(ggplot2)
library(sf)
library(dplyr)
library(trend)       # For Sen's slope (sens.slope) and Mann-Kendall (mmkh)
library(cowplot)     # For arranging plots into a grid
library(modifiedmk)
library(RColorBrewer)

output_dir <- "/Users/marco/Dropbox/estcena/scripts/trends_westMed/figs/"

# Load the NUTS1 shapefile
nuts_shp <- st_read("nuts1.shp")  # Adjust path to NUTS1 shapefile as needed

# Load precomputed NUTS monthly totals for each dataset
load("BA_ONFIRE_monthly_totals.RData")


# Define common period
common_start <- 1985
common_end <- 2015
start_years <- c("ONFIRE" = 1980)

# Define seasons
seasons <- list(
  a = 1:12,                   # Annual (all months)
  f = 6:9,                    # Fire season (June to September)
  nf = c(1:5, 10:12)          # Off-fire season (Jan-May, Oct-Dec)
)

# Map dataset names to variables
dataset_mapping <- list(
  "ONFIRE" = "BA_ONFIRE_monthly_totals")

# Function to calculate seasonal sums
calculate_seasonal_sums <- function(data, season, start_year, common_start, common_end) {
  years <- common_start:common_end
  num_years <- length(years)
  
  # Calculate start and end indices for each dataset based on the common period
  start_index <- (common_start - start_year) * 12 + 1
  end_index <- (common_end - start_year + 1) * 12
  
  # Initialize matrix for seasonal sums for each region and year
  seasonal_sums <- matrix(NA, nrow = nrow(data), ncol = num_years)
  colnames(seasonal_sums) <- years
  
  # Calculate seasonal sums for each year and region
  for (y in seq_along(years)) {
    year_start <- start_index + (y - 1) * 12
    year_indices <- year_start + season - 1  # Adjust indices for season within the year
    seasonal_sums[, y] <- rowSums(data[, year_indices, drop = FALSE], na.rm = TRUE)
  }
  
  return(seasonal_sums)
}

# Iterate over each dataset and season
for (dataset_name in names(dataset_mapping)) {
  data <- get(dataset_mapping[[dataset_name]])
  start_year <- start_years[dataset_name]
  
  for (season_name in names(seasons)) {
    season <- seasons[[season_name]]
    
    # Calculate seasonal sums for the dataset and season
    seasonal_sums <- calculate_seasonal_sums(data, season, start_year, common_start, common_end)
    
    # Initialize vectors for trends and p-values
    trends <- numeric(nrow(data))
    pvalues <- numeric(nrow(data))
    
    # Calculate trend and p-value for each region
    for (i in 1:nrow(data)) {
      db <- seasonal_sums[i, ]
      
      if (all(is.na(db)) || all(db == 0, na.rm = TRUE)) {
        trends[i] <- NA
        pvalues[i] <- NA
        next
      }
      
      # Calculate trend and p-value using Sen's slope and modified Mann-Kendall
      ss <- sens.slope(db, conf.level = 0.95)
      trends[i] <- (ss$estimates) * length(db) / mean(db, na.rm = TRUE) * 100
      mmkh_result <- mmkh(as.vector(db))
      pvalues[i] <- mmkh_result["new P-value"]
    }
    
    # Apply FDR correction to the p-values for this season
    pvalues <- p.adjust(pvalues, method = "fdr")
    
    
    # Save trends and p-values in nuts_shp for plotting
    nuts_shp[[paste0("trend_", dataset_name, "_", season_name)]] <- trends
    nuts_shp[[paste0("pvalue_", dataset_name, "_", season_name)]] <- pvalues
  }
}

# Plotting function for trend maps
plot_trend_map <- function(nuts_shp, trend_column, pvalue_column, title) {
  # Filter significant trends (p < 0.05)
  significant_nuts <- nuts_shp %>% filter(!is.na(!!sym(pvalue_column)) & !!sym(pvalue_column) < 0.05)
  significant_centroids <- st_centroid(significant_nuts) %>%
    mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])
  
  # Define color scale with reversed PuOr palette excluding middle white
  color_steps <- rev(brewer.pal(11, "PuOr")[c(1:4, 8:11)])
  breaks <- seq(-100, 100, by = 25)
  
  ggplot(nuts_shp) +
    geom_sf(aes_string(fill = trend_column), color = "white") +
    geom_point(data = significant_centroids, aes(x = lon, y = lat), 
               shape = 21, color = "black", fill = "gray", size = 2) +
    scale_fill_stepsn(colors = color_steps, limits = c(-100, 100), breaks = breaks,
                      name = "Trend (%)") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(title = title)
}

# Generate and save plots for each dataset and season
for (dataset_name in names(dataset_mapping)) {
  trend_maps <- list()
  
  for (season_name in names(seasons)) {
    trend_column <- paste0("trend_", dataset_name, "_", season_name)
    pvalue_column <- paste0("pvalue_", dataset_name, "_", season_name)
    title <- paste("Trend in Burned Area -", dataset_name, "-", toupper(season_name))
    
    trend_maps[[season_name]] <- plot_trend_map(nuts_shp, trend_column, pvalue_column, title)
  }
  
  # Arrange and save the plots for all seasons for the current dataset
  final_plot <- plot_grid(plotlist = trend_maps, ncol = 3)
  ggsave(filename = file.path(output_dir, paste0("trend_", dataset_name, "_1985_2015.pdf")),
         plot = final_plot, device = "pdf", width = 12, height = 8)
}
cat("All trend maps have been saved.")

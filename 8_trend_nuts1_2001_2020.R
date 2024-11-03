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
load("BA_FIRECCI51_monthly_totals.RData")
load("BA_FIRECCI51_nat_monthly_totals.RData")
load("BA_MCD64A1_monthly_totals.RData")
load("BA_MCD64A1_nat_monthly_totals.RData")
load("BA_GFED5_monthly_totals.RData")
load("BA_GFED5_nat_monthly_totals.RData")

# Define common period
common_start <- 2001
common_end <- 2020
start_years <- c("FIRECCI51" = 2001, "FIRECCI51_nat" = 2001, 
                 "MCD64A1" = 2001, "MCD64A1_nat" = 2001, "GFED5" = 1997, "GFED5_nat" = 2001)

# Define seasons
seasons <- list(
  a = 1:12,                   # Annual (all months)
  f = 6:9,                    # Fire season (June to September)
  nf = c(1:5, 10:12)          # Off-fire season (Jan-May, Oct-Dec)
)

# Map dataset names to variables
dataset_mapping <- list(
  "FIRECCI51" = "BA_FIRECCI51_monthly_totals",
  "FIRECCI51_nat" = "BA_FIRECCI51_nat_monthly_totals",
  "MCD64A1" = "BA_MCD64A1_monthly_totals",
  "MCD64A1_nat" = "BA_MCD64A1_nat_monthly_totals",
  "GFED5" = "BA_GFED5_monthly_totals",
  "GFED5_nat" = "BA_GFED5_nat_monthly_totals"
)

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

# Plotting function for trend maps with only graticule lines (latitude and longitude) and no labels
plot_trend_map <- function(nuts_shp, trend_column, pvalue_column) {
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
    scale_fill_stepsn(colors = color_steps, limits = c(-100, 100), breaks = breaks) +
    theme_minimal() +
    theme(
      legend.position = "none",          # Remove colorbar
      axis.title = element_blank(),      # No axis titles
      axis.text = element_blank(),       # No axis labels
      panel.grid.major = element_line(color = "grey90"),  # Keep graticules
      panel.grid.minor = element_blank(), # Remove minor grid lines
      plot.margin = unit(c(0, 0, 0, 0), "cm") # Minimize whitespace
    )
}

# Generate all trend maps in a single plot
all_trend_maps <- list()

for (dataset_name in names(dataset_mapping)) {
  for (season_name in names(seasons)) {
    trend_column <- paste0("trend_", dataset_name, "_", season_name)
    pvalue_column <- paste0("pvalue_", dataset_name, "_", season_name)
    
    all_trend_maps[[paste(dataset_name, season_name, sep = "_")]] <- plot_trend_map(
      nuts_shp, trend_column, pvalue_column
    )
  }
}

# Arrange all plots in a grid with minimal whitespace and graticules but without labels
final_combined_plot <- plot_grid(plotlist = all_trend_maps, ncol = 3, align = "hv", 
                                 rel_heights = rep(1, length(all_trend_maps) / 3))

# Save the combined plot
ggsave(filename = file.path(output_dir, "all_trend_maps_2001_2020.pdf"),
       plot = final_combined_plot, device = "pdf", width = 15, height = 10)
cat("Combined trend map plot with graticules (no labels) has been saved.")

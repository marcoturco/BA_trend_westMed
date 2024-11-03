# Clear the workspace and graphics, prepare for analysis
rm(list = ls()) # Remove all objects from the current workspace
graphics.off() # Close all open graphics devices
gc() # Perform garbage collection to free up memory


# Load required libraries
library(ggplot2)
library(sf)
library(dplyr)
library(cowplot)  # For arranging plots into a grid
library(patchwork)


# Define output path
output_dir <- "/Users/marco/Dropbox/estcena/scripts/trends_westMed/figs/"
setwd("/Users/marco/Documents/dati/obs/trend_westmed/data/")

# Load the precomputed NUTS monthly totals
load("BA_ONFIRE_monthly_totals.RData")
load("BA_FIRECCI51_monthly_totals.RData")
load("BA_FIRECCI51_nat_monthly_totals.RData")
load("BA_MCD64A1_monthly_totals.RData")
load("BA_MCD64A1_nat_monthly_totals.RData")
load("BA_GFED5_monthly_totals.RData")
load("BA_GFED5_nat_monthly_totals.RData")

# Define the year indices corresponding to 2001-2015 for ONFIRE (1980-2021) and satellite datasets
years <- 2001:2015
months_in_year <- 12

# Define ONFIRE and satellite indices from 2001 onward
start_index_onfire <- (years[1] - 1980) * months_in_year + 1  # Start at Jan 2001 relative to ONFIRE's start in 1980
end_index_onfire <- start_index_onfire + (length(years) * months_in_year) - 1
time_indices_onfire <- start_index_onfire:end_index_onfire  # ONFIRE time indices for Jan 2001 - Dec 2015

# Define general satellite indices from 2001 onward (e.g., for FIRECCI51, MCD64A1, etc.)
time_indices_satellite <- 1:(length(years) * months_in_year)  # Jan 2001 - Dec 2015 within 2001-2020 data

# Define specific GFED5 indices, accounting for its start in 1997
start_index_gfed5 <- (years[1] - 1997) * months_in_year + 1  # Start at Jan 2001 relative to GFED5's start in 1997
time_indices_gfed5 <- start_index_gfed5:(start_index_gfed5 + length(years) * months_in_year - 1)

# Define aggregation periods
aggregation_periods <- list(
  a = 1:12,                   # Annual
  f = 6:9,                    # Fire season (June to September)
  nf = c(1:5, 10:12)          # Off-fire season (Jan-May, Oct-Dec)
)

# Initialize list to store correlation results
correlation_results <- list()
pvalue_results <- list()


# Calculate and store correlations for each aggregation period
for (agg in names(aggregation_periods)) {
  
  # Extract the months for the current aggregation period
  months <- aggregation_periods[[agg]]
  
  # Initialize matrix to store annual sums for ONFIRE and each "nat" dataset
  BA_ONFIRE_agg <- matrix(NA, nrow = nrow(BA_ONFIRE_monthly_totals), ncol = length(years))
  
  # Calculate annual sums for ONFIRE by NUTS region for the current aggregation period
  for (y in seq_along(years)) {
    year_indices <- ((y - 1) * months_in_year + 1):((y - 1) * months_in_year + months_in_year)
    BA_ONFIRE_agg[, y] <- rowSums(BA_ONFIRE_monthly_totals[, time_indices_onfire[year_indices][months]], na.rm = TRUE)
  }
  
  # Initialize matrix to store correlations for this aggregation period
  correlation_matrix <- matrix(NA, nrow = nrow(BA_ONFIRE_monthly_totals), ncol = 6)
  pvalue_matrix <- matrix(NA, nrow = nrow(BA_ONFIRE_monthly_totals), ncol = 6)
  colnames(correlation_matrix) <- colnames(pvalue_matrix) <- c("FIRECCI51","FIRECCI51_nat", "MCD64A1","MCD64A1_nat", "GFED5","GFED5_nat")
  rownames(correlation_matrix) <- rownames(pvalue_matrix) <- rownames(BA_ONFIRE_monthly_totals)
  
  # Calculate correlations with each "nat" dataset
  datasets <- list(
    FIRECCI51 = BA_FIRECCI51_monthly_totals,
    FIRECCI51_nat = BA_FIRECCI51_nat_monthly_totals,
    MCD64A1 = BA_MCD64A1_monthly_totals,
    MCD64A1_nat = BA_MCD64A1_nat_monthly_totals,
    GFED5 = BA_GFED5_monthly_totals,
    GFED5_nat = BA_GFED5_nat_monthly_totals
    
  )
  
  for (dataset_name in names(datasets)) {
    # Initialize matrix to store annual sums for each "nat" dataset
    BA_nat_agg <- matrix(NA, nrow = nrow(datasets[[dataset_name]]), ncol = length(years))
    
    # Use appropriate time indices based on the dataset
    time_indices <- if (dataset_name %in% c("GFED5")) time_indices_gfed5 else time_indices_satellite
    
    # Calculate annual sums for each dataset by NUTS region for the current aggregation period
    for (y in seq_along(years)) {
      year_indices <- ((y - 1) * months_in_year + 1):((y - 1) * months_in_year + months_in_year)
      BA_nat_agg[, y] <- rowSums(datasets[[dataset_name]][, time_indices[year_indices][months]], na.rm = TRUE)
    }
    
    # Calculate correlation for each NUTS region between ONFIRE and each "nat" dataset
    for (i in 1:nrow(correlation_matrix)) {
      # correlation_matrix[i, dataset_name] <- cor(BA_ONFIRE_agg[i, ], BA_nat_agg[i, ], use = "complete.obs")
      cor_test <- cor.test(BA_ONFIRE_agg[i, ], BA_nat_agg[i, ], method = "spearman", use = "complete.obs")
      correlation_matrix[i, dataset_name] <- cor_test$estimate
      pvalue_matrix[i, dataset_name] <- cor_test$p.value
      
    }
  }
  
  # Apply FDR correction to the p-values for the entire matrix of p-values for this season
  pvalue_matrix <- apply(pvalue_matrix, 2, function(p) p.adjust(p, method = "fdr"))
  
  # Store results for this aggregation period
  correlation_results[[agg]] <- correlation_matrix
  pvalue_results[[agg]] <- pvalue_matrix
  
}


# Save the correlation results
save(correlation_results, file = file.path(output_dir, "correlation_results_by_NUTS_2001_2015.RData"))

# Print a summary of correlations
print("Correlation results by aggregation period (2001-2015):")
for (agg in names(correlation_results)) {
  cat("\nAggregation period:", agg, "\n")
  print(correlation_results[[agg]])
}

nuts_shp <- st_read("nuts1.shp")  # Adjust path as needed
dim(nuts_shp)



######## PLOTS

# Define plot function to apply fixed color scale with discrete color bins
plot_correlation_map <- function(nuts_shp, correlation_matrix, p_values, season_name, dataset_name) {
  # Add correlation and p-value data to the shapefile
  nuts_shp <- nuts_shp %>%
    mutate(correlation = correlation_matrix[, dataset_name],
           p_value = p_values[, dataset_name]) %>%
    st_as_sf()
  
  # Filter for significant correlations (p < 0.05)
  significant_nuts <- nuts_shp %>% filter(!is.na(p_value) & p_value < 0.05)
  significant_centroids <- st_centroid(significant_nuts) %>%
    mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])
  
  # Convert correlation values into discrete bins, adjusting for any values outside ranges
  nuts_shp <- nuts_shp %>%
    mutate(correlation_bin = cut(
      correlation,
      breaks = c(-Inf, 0.2, 0.4, 0.6, 0.8, 1),
      labels = c("<0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1"),
      include.lowest = TRUE, right = TRUE
    ))
  
  # Plot with ggplot2, without color bar, axis labels, and title
  p <- ggplot(nuts_shp) +
    geom_sf(aes(fill = correlation_bin), color = "white") +
    geom_point(data = significant_centroids, aes(x = lon, y = lat), 
               shape = 21, color = "black", fill = "gray", size = 2) +
    scale_fill_manual(
      values = c("<0.2" = "#fde725", "0.2-0.4" = "#5dc863", 
                 "0.4-0.6" = "#21908d", "0.6-0.8" = "#3b528b", "0.8-1" = "#440154"),
      guide = "none"  # Remove color bar
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",  # No legend
      axis.text.x = element_blank(),  # No x-axis labels
      axis.text.y = element_blank(),  # No y-axis labels
      axis.ticks = element_blank()    # No axis ticks
    )
  
  return(p)
}

# List to store plots without individual legends
# List to store plots in the desired order (dataset by rows, season by columns)
all_plots <- list()

# Ensure plots are ordered with each dataset as a row and seasons "a," "f," "nf" in columns
for (dataset_name in colnames(correlation_results[["a"]])) {
  for (agg in c("a", "f", "nf")) {
    correlation_matrix <- correlation_results[[agg]]
    p_values <- pvalue_results[[agg]]
    
    # Generate the plot without a legend for the grid
    plot <- plot_correlation_map(nuts_shp, correlation_matrix, p_values, season_name = agg, dataset_name = dataset_name) 
    
    # Append plot in correct order
    all_plots <- append(all_plots, list(plot))
  }
}

#Combine the plots in a grid (3 columns for seasons per row)
final_plot <- plot_grid(
  plotlist = all_plots,
  ncol = 3, 
  labels = c("Annual (a)", "Fire Season (f)", "Non-Fire Season (nf)"),
  label_size = 14
)

# Save the combined plot with the colorbar as a PDF
output_pdf <- file.path(output_dir, "correlation_maps_significant.pdf")
ggsave(
  filename = output_pdf,
  plot = final_plot,
  device = "pdf",
  width = 12,
  height = 10
)

cat("The plot has been saved to:", output_pdf)
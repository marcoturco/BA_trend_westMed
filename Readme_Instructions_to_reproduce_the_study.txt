#################################################################################################
#		
# Title:	Burned Area tend in Western Mediterranean regions
#				
# Author:    Marco Turco (marco.turco@um.es), Andrina Gincheva University of Murcia 
#
#################################################################################################

#################################################################################################
# A. General instructions 
#################################################################################################

This project is designed to be executed with shell scripts and R codes. 
Execute script files in the order they are listed.

Data sources:

ONFIRE can be found at https:// zenodo.org/record/8289245; 
FireCCI51 on https://geogra.uah.es/fire_cci/firecci51.php; 
MCD64A1at https://lpdaac.usgs.gov/products/mcd64a1v061/; 
GFED5 at https://zenodo.org/records/7668424. 

If you have any questions or wish to express any comment to the authors, please 
contact Dr. Marco Turco at the emails indicated above.


#################################################################################################
# B. Description of script files
#################################################################################################

1_load_data.R: R script to load and preprocess all necessary datasets for the analysis.

2_Study-domain.R: R script defining the study domain, focusing on the Western Mediterranean and setting spatial boundaries.

3_BA-evolution.R: Script to analyze the burned area (BA) evolution over time, producing initial time-series summaries.

4_Statistics-tables.R: Script for generating statistical summaries and tables relevant to the study.

5_trend_onfire_1985_2015.R: Script to analyze trends in ONFIRE data specifically for the period 1985-2015.

6_correlation_onfire_nuts1.R: Script to calculate correlations between ONFIRE data and other datasets at the NUTS1 level.

7_trend_nuts1_2001_2015.R: Script to calculate BA trends at the NUTS1 level for the common period 2001-2015.

8_trend_nuts1_2001_2020.R: Script to extend the trend analysis at the NUTS1 level for the satellite data period 2001-2020.



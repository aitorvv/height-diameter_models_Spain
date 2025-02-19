#!/usr/bin/Rscript

# Code to curate SFNI2, SFNI3 and SFNI4 raw data ----
# Step 3: (c) recalculate tree and plot variables related to position after the correction of plot coordinates on 0.3.1 code
#
# Aitor Vázquez Veloso
# 2024-09-16
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Some decisions that has been taken can be filtered by "# Note:"


# SFNI2 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls
# SFNI3 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# SFNI4 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf



# working directory
setwd('SFNI_Spanish_Forest_National_Inventory-ready_to_use/')



# Initial steps: load data and functions ----

# libraries
library(tidyverse)
library(rgdal)

# load data (trees)
load('1_data/2_processed/0.2_sfni_forest_data.rdata')

# Note:
# some plots positions has been corrected on file 0.3.1_explore_plot_distribution.r 
# the corrected data is loaded here and substituted in the plots data frame
load('1_data/2_processed/tmp/0.3.1_plots_corrected_coords.rdata')
plots <- plots_corrected_coords
plots <- dplyr::select(plots, -zone, -Latitude, -Longitude)
rm(plots_corrected_coords)



# Calculate position variables: plots ====

# get plot functions
source('2_scripts/0.0_support_plot_functions.r')

# get plot coordinates in WGS84
plot_coords <- get_wgs84_coordinates(df = plots, plot_id_column = 'IFN_PLOT_ID', 
                                     province_column = 'Province', x_column = 'X_UTM', y_column = 'Y_UTM')

# merge plot data with coordinates
plots <- merge(plots, plot_coords, by.x = "IFN_PLOT_ID", by.y = "PLOT_ID", all = FALSE)

rm(plot_coords)



# Calculate position variables: trees ====

# get tree functions
source('2_scripts/0.0_support_tree_functions.r')

# get relative coordinates to the plot center
trees <- get_coord_rel(df = trees, distance_column = 'distance', bearing_column = 'bearing', 
                       distance_units = 'm', bearing_units = 'grad')

# join trees with plot center coordinates and zone
plots_coords <- dplyr::select(plots, IFN_PLOT_ID, X_UTM, Y_UTM)
plots_coords <- dplyr::rename(plots_coords, X_UTM_center = X_UTM, Y_UTM_center = Y_UTM)
trees <- merge(trees, plots_coords, by = 'IFN_PLOT_ID', all.x = TRUE)

# get absolute coordinates in UTM
trees <- get_coord_utm(df = trees, distance_column = 'distance', bearing_column = 'bearing', 
                       x_center_column = 'X_UTM_center', y_center_column = 'Y_UTM_center', distance_units = 'm', bearing_units = 'grad')

# get tree coordinates in WGS84
trees_coords <- get_wgs84_coordinates(df = trees, plot_id_column = 'IFN_TREE_ID', 
                                      province_column = 'province', x_column = 'x_utm', y_column = 'y_utm')

# merge tree data with coordinates
trees <- merge(trees, trees_coords, by.x = "IFN_TREE_ID", by.y = "PLOT_ID", all.x = TRUE)

# skip plot coordinates information
trees <- dplyr::rename(trees, latitude = Latitude, longitude = Longitude)
trees <- dplyr::select(trees, -c(X_UTM_center, Y_UTM_center, zone))

# remove everything except trees and plots
rm(get_mean_dbh, get_expan, get_circumference, get_g, get_g_ha, get_bal, get_slenderness, get_coord_rel, get_coord_utm,
   plots_coords, trees_coords, get_100_bigger_trees, get_dominant_value, get_sfni_expan)
rm(get_plot_data, get_plot_data_basic, get_plot_by_species, get_plot_mortality, get_plot_slenderness, 
   get_plot_dominant_slenderness, get_SDI, get_hart_index, get_hart_index_staggered, get_wgs84_coordinates, get_Dg)



# Check plot distribution and save results ----

# check plot distribution
source("SFNI_Spanish_Forest_National_Inventory-ready_to_use/WorldClim/scripts/plot_coordinates.R")
coord_in_spain_no_bounds(plots, long = 'Longitude', lat = 'Latitude')

# save data
save(trees, plots, file = "1_data/2_processed/0.3.2_sfni_corrected_position_data.rdata")

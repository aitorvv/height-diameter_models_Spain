#!/usr/bin/Rscript

# Code to curate SFNI2, SFNI3 and SFNI4 raw data ----
# Step 3: (b) explore plots spatial distribution
#
# Aitor Vázquez Veloso
# 2024-09-13
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Some decisions that has been taken can be filtered by "# Note:"


# SFNI2 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls
# SFNI3 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# SFNI4 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf



# working directory
setwd('SFNI_Spanish_Forest_National_Inventory-ready_to_use/')

# libraries
library(tidyverse)
source("SFNI_Spanish_Forest_National_Inventory-ready_to_use/WorldClim/scripts/plot_coordinates.R")

# load data
load("1_data/2_processed/tmp/0.3_sfni_position_data.rdata")



# Explore plots spatial distribution ----

# graph plots using different bounding boxes
coord_in_spain_no_bounds(plots, long = 'Longitude', lat = 'Latitude')
coord_in_spain_and_canarias(plots, long = 'Longitude', lat = 'Latitude')
coord_in_spain(plots, long = 'Longitude', lat = 'Latitude')



# Filter plots outside Spain and Canary Islands ----

# define the limits in latitude and longitude and filter plots outside
p_lat_min <- plots[plots$Latitude < 27.45, ]
p_lat_max <- plots[plots$Latitude > 43.80, ]
p_long_min <- plots[plots$Longitude < -18.15, ]
p_long_max <- plots[plots$Longitude > 4.30, ]

p_lat_min <- p_lat_min[!is.na(p_lat_min$Latitude), ]
p_lat_max <- p_lat_max[!is.na(p_lat_max$Latitude), ]
p_long_min <- p_long_min[!is.na(p_long_min$Longitude), ]
p_long_max <- p_long_max[!is.na(p_long_max$Longitude), ]

coord_in_spain_no_bounds(p_lat_min, long = 'Longitude', lat = 'Latitude')
coord_in_spain_no_bounds(p_lat_max, long = 'Longitude', lat = 'Latitude')
coord_in_spain_no_bounds(p_long_min, long = 'Longitude', lat = 'Latitude')
coord_in_spain_no_bounds(p_long_max, long = 'Longitude', lat = 'Latitude')


# Longitude maximum error

long_max_ids <- p_long_max$INVENTORY_ID
unique(long_max_ids)

long_max_ids <- p_long_max$PLOT_ID
plots_long_max <- plots[plots$PLOT_ID_short %in% long_max_ids, ]
plots_long_max <- plots_long_max[plots_long_max$INVENTORY_ID == 'IFN3' & 
                                   plots_long_max$Class == 'A' & plots_long_max$Subclass == 1, ]
plots_long_max <- plots_long_max[!is.na(plots_long_max$Latitude), ]

coord_in_spain_no_bounds(plots_long_max, long = 'Longitude', lat = 'Latitude')
plots_long_max
# Note: 
# plots belong to SFNI2; plots with class A and subclass 1 will be corrected using SFNI3 coordinates (2 plots out of 28)


# Latitude maximum error

lat_max_ids <- p_lat_max$INVENTORY_ID
unique(lat_max_ids)

# SFNI2 plots
p_lat_max <- p_lat_max[p_lat_max$INVENTORY_ID == 'IFN2', ]
lat_max_ids <- p_lat_max$PLOT_ID
plots_lat_max <- plots[plots$PLOT_ID_short %in% lat_max_ids, ]
plots_lat_max <- plots_lat_max[plots_lat_max$INVENTORY_ID == 'IFN3' & 
                                     plots_lat_max$Class == 'A' & plots_lat_max$Subclass == 1, ]
plots_lat_max <- plots_lat_max[!is.na(plots_lat_max$Longitude), ]

coord_in_spain_no_bounds(plots_lat_max, long = 'Longitude', lat = 'Latitude')
plots_lat_max
# Note:
# plots belong to SFNI2; plots with class A and subclass 1 will be corrected using SFNI3 coordinates (2 out of 28)
# same plots than in the longitude maximum error


# Longitude minimum error
# Note:
# no plots with longitude minimum error


# Latitude minimum error

lat_min_ids <- p_lat_min$INVENTORY_ID
unique(lat_min_ids)

lat_min_ids <- p_lat_min$PLOT_ID
plots_lat_min <- plots[plots$PLOT_ID_short %in% lat_min_ids, ]
plots_lat_min <- plots_lat_min[plots_lat_min$INVENTORY_ID == 'IFN3' & 
  plots_lat_min$Class == 'A' & plots_lat_min$Subclass == 1, ]
plots_lat_min <- plots_lat_min[!is.na(plots_lat_min$Longitude), ]

coord_in_spain_no_bounds(plots_lat_min, long = 'Longitude', lat = 'Latitude')
plots_lat_min
# Note:
# plots belong to SFNI2; plots with class A and subclass 1 will be corrected using SFNI3 coordinates (34 out of 63)



# Correct plots coordinates ----

# filter plots
plots_to_correct_coords <- plots[plots$PLOT_ID %in% c(plots_long_max$PLOT_ID_short, plots_lat_max$PLOT_ID_short, plots_lat_min$PLOT_ID_short), ]

plots_to_delete_coords <- plots[plots$PLOT_ID %in% c(long_max_ids, lat_max_ids, lat_min_ids), ]
plots_to_delete_coords <- plots_to_delete_coords[plots_to_delete_coords$INVENTORY_ID == 'IFN2', ]
plots_to_delete_coords <- plots_to_delete_coords[!plots_to_delete_coords$PLOT_ID %in% plots_to_correct_coords$PLOT_ID, ]

plots_good_coords <- plots[!plots$PLOT_ID %in% c(plots_to_correct_coords$PLOT_ID, plots_to_delete_coords$PLOT_ID), ]

# correct coordinates
plots_sfni3_a1_to_correct <- rbind(plots_lat_max, plots_lat_min)
plots_sfni3_a1_to_correct <- dplyr::select(plots_sfni3_a1_to_correct, PLOT_ID_short, X_UTM, Y_UTM, Latitude, Longitude)

plots_to_correct_coords <- dplyr::select(plots_to_correct_coords, -X_UTM, -Y_UTM, -Latitude, -Longitude)
plots_to_correct_coords <- merge(plots_to_correct_coords, plots_sfni3_a1_to_correct, 
                                 by.x = 'PLOT_ID', by.y = 'PLOT_ID_short', all.x = TRUE)

coord_in_spain_no_bounds(plots_to_correct_coords, long = 'Longitude', lat = 'Latitude')

# delete plots with wrong coordinates
plots_to_delete_coords$X_UTM <- NA
plots_to_delete_coords$Y_UTM <- NA
plots_to_delete_coords$Latitude <- NA
plots_to_delete_coords$Longitude <- NA

# merge plots
plots_corrected_coords <- rbind(plots_good_coords, plots_to_correct_coords)
plots_corrected_coords <- rbind(plots_corrected_coords, plots_to_delete_coords)



# Check coordinates and save results ----

# check coordinates
coord_in_spain_no_bounds(plots_corrected_coords, long = 'Longitude', lat = 'Latitude')

# save results
save(plots_corrected_coords, file = '1_data/2_processed/tmp/0.3.1_plots_corrected_coords.rdata')

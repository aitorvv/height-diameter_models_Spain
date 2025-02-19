#!/usr/bin/Rscript

# Code to curate SFNI2, SFNI3 and SFNI4 raw data ----
# Step 2: calculate some tree and plot variables related to the forest status
#
# Aitor Vázquez Veloso
# 2024-09-13
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Some decisions that has been taken can be filtered by "# Note:"


# SFNI2 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls
# SFNI3 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# SFNI4 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf



# working directory
setwd('')



# Initial steps: load data and functions ----

# load data
load('1_data/2_processed/0.1_sfni_harmonised.rdata')

# test data (check if all the code works before running the whole dataset)
plots <- head(plots, 10)
trees <- trees[trees$IFN_PLOT_ID %in% plots$IFN_PLOT_ID,]

# libraries
library(tidyverse)

print('Ready to start!')



# Calculate new variables: trees ====

# get tree functions
source('2_scripts/0.0_support_tree_functions.r')

# get mean dbh (cm)
trees$dbh <- get_mean_dbh(trees$dbh_1, trees$dbh_2, dbh_units = 'mm')

# expansion factor
trees$expan <- get_sfni_expan(trees$dbh, dbh_units = 'cm')

# get tree circumference (cm)
trees$circumference <- get_circumference(trees$dbh, dbh_units = 'cm')

# basal area (cm2) and basal area per hectare (m2/ha)
trees$g <- get_g(trees$dbh, dbh_units = 'cm')
trees$g_ha <- get_g_ha(trees$dbh, expan = trees$expan, dbh_units = 'cm')

# basal area larger than subject tree (m2/ha)
trees <- get_bal(df = trees, plot_id_column = 'IFN_PLOT_ID', tree_id_column = 'IFN_TREE_ID', dbh_column = 'dbh', 
                 g_ha_column = 'g_ha')  # using g_ha column

# slenderness
trees$slenderness <- get_slenderness(trees$dbh, trees$h, dbh_units = 'cm', h_units = 'm')

# record dead (1) and alive trees (0)
trees$dead <- ifelse(trees$quality == 6, 1, 0)

# remove functions
rm(get_mean_dbh, get_expan, get_circumference, get_g, get_g_ha, get_bal, get_slenderness, get_coord_rel, get_coord_utm,
   get_sfni_expan)

print('Trees variables calculated!')



# Calculate new variables: plots ====

# get plot functions
source('2_scripts/0.0_support_plot_functions.r')

# get N, DBH, G and G metrics for plots
plots_metrics <- get_plot_data(df_trees = trees, plot_id_column_1 = 'IFN_PLOT_ID', plot_id_column_2 = NA, 
                               dbh_column = 'dbh', h_column = 'h', expan_column = 'expan', g_column = 'g')
# plots_metrics <- get_plot_data_basic(df_trees = trees, plot_id_column_1 = 'IFN_PLOT_ID', plot_id_column_2 = NA, 
#                                      dbh_column = 'dbh', expan_column = 'expan', g_column = 'g')

# get dominant height (m) per plot
plots_Ho <- get_dominant_value(trees, dominant_value_col = "h", output_col_name = "Ho",
                               plot_id_col = 'IFN_PLOT_ID', bigger_value_col = "h", expan_col = "expan")

# get dominant diameter (cm) per plot
plots_Do <- get_dominant_value(trees, dominant_value_col = "dbh", output_col_name = "Do",
                               plot_id_col = 'IFN_PLOT_ID', bigger_value_col = "dbh", expan_col = "expan")

# get plot data by species
plots_sp <- get_plot_by_species(df_trees = trees, plot_id_column_1 = 'IFN_PLOT_ID', 
                                species_column = 'species', expan_column = 'expan', g_column = 'g')

# get plot data according to tree status
plots_status <- get_plot_mortality(df_trees = trees, plot_id_column_1 = 'IFN_PLOT_ID', 
                                   dead_column = 'dead', expan_column = 'expan', g_column = 'g')

# merge all the plot data previously calculated
plots <- merge(plots, plots_metrics, by.x = 'IFN_PLOT_ID', by.y = 'PLOT_ID', all.x = TRUE)
plots <- merge(plots, plots_Ho, by.x = 'IFN_PLOT_ID', by.y = 'PLOT_ID', all.x = TRUE)
plots <- merge(plots, plots_Do, by.x = 'IFN_PLOT_ID', by.y = 'PLOT_ID', all.x = TRUE)
plots <- merge(plots, plots_sp, by.x = 'IFN_PLOT_ID', by.y = 'PLOT_ID', all.x = TRUE)
plots <- merge(plots, plots_status, by.x = 'IFN_PLOT_ID', by.y = 'PLOT_ID', all.x = TRUE)

# remove temporal data
rm(plots_metrics, plots_Ho, plots_Do, plots_sp, plots_status)

# get plot slenderness
plots$slenderness <- get_plot_slenderness(df_plots = plots, hm_column = 'h_mean', dbhm_column = 'dbh_mean')

# get plot dominant slenderness
plots$dominant_slenderness <- get_plot_dominant_slenderness(df_plots = plots, Ho_column = 'Ho', Do_column = 'Do')

# get SDI
plots$SDI <- get_SDI(df_plots = plots, N_column = 'N', Dg_column = 'dg', r_value = 1.605)

# get Hart index for normal and sttagered plots
plots$S <- get_hart_index(df_plots = plots, Ho_column = 'Ho', N_column = 'N')
plots$S_staggered <- get_hart_index_staggered(df_plots = plots, Ho_column = 'Ho', N_column = 'N')

# remove functions
rm(get_plot_data, get_plot_by_species, get_plot_mortality, get_plot_slenderness, 
   get_plot_dominant_slenderness, get_SDI, get_hart_index, get_hart_index_staggered, get_wgs84_coordinates,
   get_Dg, get_100_bigger_trees, get_dominant_value, get_plot_data_basic)

print('Plots variables calculated!')



# Save results ====

save(trees, plots, file = "1_data/2_processed/0.2_sfni_forest_data_test.rdata")

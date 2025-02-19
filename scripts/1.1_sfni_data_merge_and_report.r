#!/usr/bin/Rscript

# Code to adapt curated data to the analysis ----
# Step 0: load required data, merge them and create a data report
#
# Aitor Vázquez Veloso
# 2024-09-22
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Some decisions that has been taken can be filtered by "# Note:"


# SFNI2 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls
# SFNI3 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# SFNI4 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf



# working directory
setwd('SFNI_Spanish_Forest_National_Inventory-ready_to_use/')

# libraries
library(tidyverse)
source('2_scripts/0.0_support_data_report.r')

# get data
load('1_data/2_processed/9.0.2_sfni_forest_data.rdata')
p_df <- plots
t_df <- trees
load('1_data/2_processed/0.4.0_climatic_classification.rdata')
p_clim <- df
load('1_data/2_processed/0.3.2_sfni_corrected_position_data.rdata')
p_pos <- plots
t_pos <- trees
rm(plots, trees, df)

# clean and merge data
p_df <- dplyr::select(p_df, -X_UTM, -Y_UTM)
p_clim <- dplyr::select(p_clim, -Longitude, -Latitude)
p_pos <- dplyr::select(p_pos, IFN_PLOT_ID, X_UTM, Y_UTM, Longitude, Latitude, zone)

t_pos <- dplyr::select(t_pos, IFN_TREE_ID, x_rel, y_rel, x_utm, y_utm, longitude, latitude)

plots <- merge(p_df, p_clim, by = 'IFN_PLOT_ID')
plots <- merge(plots, p_pos, by = 'IFN_PLOT_ID')
trees <- merge(t_df, t_pos, by = 'IFN_TREE_ID')

rm(p_df, p_clim, p_pos, t_df, t_pos)



# Save merged forest, positions and climatic data ====

save(plots, trees, file = '1_data/2_processed/1.1_sfni_clean_data.rdata')



# Create data report ====

columns_to_remove <- c(
  "fito-OBJECTID", "fito-ALLUE", "fito-ORDEN", "fito-Tipo_Fitoc", "fito-Asociacion",
  "fito-Shape_Leng", "fito-Shape_Area", "biogeo-objectid", "biogeo-descripcio",
  "biogeo-st_perimet", "veget-OBJECTID", "veget-SERIES", "veget-REGION", 
  "veget-AZONAL", "veget-PISO", "veget-Ha", "veget-Shape_Leng", "veget-Shape_Area"
)

plots_filtered <- dplyr::select(plots, -columns_to_remove)

data_report(df = plots_filtered, output_file = 
          'SFNI_Spanish_Forest_National_Inventory-ready_to_use/output/data_reports/1.1_sfni_all_data-plots_report.html')
data_report(df = trees, output_file = 
          'SFNI_Spanish_Forest_National_Inventory-ready_to_use/output/data_reports/1.1_sfni_all_data-trees_report.html')



# Include plot initial data discarted in SFNI harmonisation ====

# deal with df labels and load data
rm(plots_filtered, columns_to_remove)
df_plots <- plots
df_trees <- trees

load('1_data/2_processed/tmp/0.1_IFN2_tmp.rdata')
plots2 <- plots
trees2 <- trees
load('1_data/2_processed/tmp/0.1_IFN3_tmp.rdata')
plots3 <- plots
trees3 <- trees
load('1_data/2_processed/tmp/0.1_IFN4_tmp.rdata')
plots4 <- plots
trees4 <- trees

# clean plot data: get unique column names, give suffixes to the columns, remove duplicates and merge data
merge_id <- 'IFN_PLOT_ID'

unique_columns <- setdiff(names(plots2), names(df_plots))
unique_columns <- c(unique_columns, merge_id)
plots2 <- plots2 %>%  dplyr::select(all_of(unique_columns))
plots2 <- plots2 %>%  rename_with(~ paste0('SFNI2_', .x), -all_of(merge_id))
plots2 <- plots2[!duplicated(plots2$IFN_PLOT_ID), ]
df_plots <- merge(df_plots, plots2, by = merge_id, all.x = TRUE)

unique_columns <- setdiff(names(plots3), names(df_plots))
unique_columns <- c(unique_columns, merge_id)
plots3 <- plots3 %>%  dplyr::select(all_of(unique_columns))
plots3 <- plots3 %>%  rename_with(~ paste0('SFNI3_', .x), -all_of(merge_id))
plots3 <- plots3[!duplicated(plots3$IFN_PLOT_ID), ]
df_plots <- merge(df_plots, plots3, by = merge_id, all.x = TRUE)

unique_columns <- setdiff(names(plots4), names(df_plots))
unique_columns <- c(unique_columns, merge_id)
plots4 <- plots4 %>%  dplyr::select(all_of(unique_columns))
plots4 <- plots4 %>%  rename_with(~ paste0('SFNI4_', .x), -all_of(merge_id))
plots4 <- plots4[!duplicated(plots4$IFN_PLOT_ID), ]
df_plots <- merge(df_plots, plots4, by = merge_id, all.x = TRUE)

# clean tree data: get unique column names, give suffixes to the columns, remove duplicates and merge data
merge_id <- 'IFN_TREE_ID'

unique_columns <- setdiff(names(trees2), names(df_trees))
# no additional columns in trees2

unique_columns <- setdiff(names(trees3), names(df_trees))
unique_columns <- c(unique_columns, merge_id)
trees3 <- trees3 %>%  dplyr::select(all_of(unique_columns))
trees3 <- trees3 %>%  rename_with(~ paste0('SFNI3_', .x), -all_of(merge_id))
trees3 <- trees3[!duplicated(trees3$IFN_TREE_ID), ]
df_trees <- merge(df_trees, trees3, by = merge_id, all.x = TRUE)

unique_columns <- setdiff(names(trees4), names(df_trees))
unique_columns <- c(unique_columns, merge_id)
trees4 <- trees4 %>%  dplyr::select(all_of(unique_columns))
trees4 <- trees4 %>%  rename_with(~ paste0('SFNI4_', .x), -all_of(merge_id))
trees4 <- trees4[!duplicated(trees4$IFN_TREE_ID), ]
df_trees <- merge(df_trees, trees4, by = merge_id, all.x = TRUE)



# Save merged forest, positions and climatic data including initial SFNI variables ====

plots <- df_plots
trees <- df_trees
save(plots, trees, file = '1_data/2_processed/1.1_sfni_all_data.rdata')

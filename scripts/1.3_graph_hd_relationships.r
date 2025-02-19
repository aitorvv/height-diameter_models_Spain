#!/usr/bin/Rscript

# Code to adapt curated data to the analysis ----
# Step 2: graph height-diameter relationships for the species and groups already selected
#
# Aitor Vázquez Veloso
# 2024-09-23
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Some decisions that has been taken can be filtered by "# Note:"


# SFNI2 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls
# SFNI3 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# SFNI4 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf



# working directory
setwd('')

# libraries
library(tidyverse)

# get data
df_all_species <- read_csv('1_data/1_raw/SFNI4_species_codes.csv')



# Plot initial datasets ====

# load data and functions
source('2_scripts/1.0_hd_support_functions.r')

# species selection
all_species <- df_all_species[df_all_species$Arborea_Matorral == 'ARB', ]
all_species <- unique(all_species$Codigo_IFN)
all_species <- as.numeric(all_species)
groups <- c('12,16', '45,245')  # Pyrus and Malus; Quercus ilex spp. ballota and Quercus ilex spp. ilex
target_species <- setdiff(all_species, c('12', '16', '45', '245'))
sp_and_groups <- c(target_species, groups)

# load merged data
df <- load_sfni_filtered(species_list = all_species,
                         path = '1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata',
                         tree_vars = c('INVENTORY_ID', 'IFN_PLOT_ID', 'IFN_TREE_ID',  # codes
                                       'species', 'dbh', 'h', 'tree_type'),  # variables
                         plot_vars = c('IFN_PLOT_ID',  # codes
                                       'N', 'G', 'SDI', 'dg',  # density and size
                                       'Stand_origin', 'Species_mixture', 'Stand_type', 'Age_class', 'Structure',  # stand
                                       'climate_region'))  # biogeography



# Plot height-diameter relationships by species ====

# list for number of values
n_values <- tribble(~species_code, ~species_name, ~n_values)

# plot h/d relationships
for(sp in sp_and_groups){
  
  # clean species code when it is a group
  if(nchar(sp) > 3){
    # clean, split and convert species code
    sp <- gsub(" ", "", sp)
    sp <- strsplit(sp, ",")[[1]]
    sp <- as.numeric(sp)
  }

  # filter data
  sp_df <- df[df$species %in% sp, ]
  sp_code <- df_all_species[as.numeric(df_all_species$Codigo_IFN) %in% sp, ]
  
  # get name and codes
  g_code <- paste(sp_code$Codigo_IFN, collapse = '_')
  g_name <- paste(sp_code$Nombre_Especie, collapse = ' and ')
  
  if(nrow(sp_df) > 0){
    plot_hd_by_sp(sp_df, 
                  species_name = g_name,
                  species_code = g_code,
                  sfni_edition = '234',
                  output_path = '3_figures/1.3_hd_baseline_graphs/species/')
  } else {
    print(paste('No data for species', sp_code$Nombre_Especie))
  }
  
  # add number of values
  n_values <- n_values %>% add_row(species_code = g_code, species_name = g_name, n_values = nrow(sp_df)) 
}

write.csv(n_values, file = '3_figures/1.3_hd_baseline_graphs/species/n_values_by_species.csv', row.names = FALSE)



# Plot height-diameter relationships by df ====

# grouping data by the desired labels
df2 <- df[df$INVENTORY_ID == 'IFN2', ]
df3 <- df[df$INVENTORY_ID == 'IFN3', ]
df4 <- df[df$INVENTORY_ID == 'IFN4', ]

df_mix <- df[df$Species_mixture == 'mix', ]
df_pure <- df[df$Species_mixture == 'pure', ]          

df_conifers <- df[df$tree_type %in% 'conifer', ]
df_broadleaved <- df[df$tree_type %in% 'broadleaved', ]

df_atlan <- df[df$climate_region == 'Atlántica', ]
df_atlan <- df_atlan[!is.na(df_atlan$climate_region), ]  # some climate regions are NAs and must be excluded
df_medit <- df[df$climate_region == 'Mediterránea', ]
df_medit <- df_medit[!is.na(df_medit$climate_region), ]  # some climate regions are NAs and must be excluded
df_macar <- df[df$climate_region == 'Macaronésica', ]
df_macar <- df_macar[!is.na(df_macar$climate_region), ]  # some climate regions are NAs and must be excluded
df_alpin <- df[df$climate_region == 'Alpina', ]
df_alpin <- df_alpin[!is.na(df_alpin$climate_region), ]  # some climate regions are NAs and must be excluded

# pack data in a list of df
df_list <- list(df, df2, df3, df4, df_mix, df_pure, df_conifers, df_broadleaved, 
                df_atlan, df_medit, df_macar, df_alpin)
df_names <- c('df', 'df2', 'df3', 'df4', 'df_mix', 'df_pure', 'df_conifers', 'df_broadleaved',
              'df_atlan', 'df_medit', 'df_macar', 'df_alpin')

# list for number of values
n_values <- tribble(~species_code, ~df_name, ~n_values)
count <- 0

# plot h/d relationships
for(my_df in df_list){
  
  count <- count + 1
  
  # get df name
  g_name <- df_names[count]
  
  if(nrow(my_df) > 0){
  plot_hd_by_df(my_df, 
                df_name = g_name,
                output_path = '3_figures/1.3_hd_baseline_graphs/data/')
  } else {
    print(paste('No data for data set ', g_name, sep = ''))
  }
  
  # add number of values
  n_values <- n_values %>% add_row(species_code = 'all', df_name = g_name, n_values = nrow(my_df)) 
}

write.csv(n_values, file = '3_figures/1.3_hd_baseline_graphs/data/n_values_by_df.csv', row.names = FALSE)

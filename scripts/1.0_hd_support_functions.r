#!/usr/bin/Rscript

# Code to adapt curated data to the analysis ----
# Step 0: support functions
#
# Aitor Vázquez Veloso
# 2024-09-22
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# Get species names from species codes
# written by: Aitor Vázquez Veloso
# date: 2024-09-09
# parameters:
# - code_list: a list of species codes
# - path: path to the species codes file
# returns:
# - a data frame with the species codes and names

get_species <- function(code_list,
                        path = '1_data/1_raw/SFNI4_species_codes.csv'){
  
  codes <- read.csv(path, sep = ',', header = TRUE)
  species_code <- codes[codes$Codigo_IFN %in% code_list, ]
  species_code <- dplyr::select(species_code, Codigo_IFN, Nombre_Especie)
  
  return(species_code)
}


# Get species names and codes filtered by a given column and value
# written by: Aitor Vázquez Veloso
# date: 2024-09-09
# parameters:
# - column: column name to filter
# - value: value to filter
# - path: path to the species codes file
# returns:
# - a data frame with the species codes and names

get_species_filtered <- function(column,
                                 value,
                                 path = '1_data/1_raw/SFNI4_species_codes.csv'){
  
  codes <- read.csv(path, sep = ',', header = TRUE)
  species_code <- codes[codes[[column]] == value, ]
  species_code <- dplyr::select(species_code, Codigo_IFN, Nombre_Especie)
  
  return(species_code)
}


# Function to load SFNI filtered data from a given species code list
# written by: Aitor Vázquez Veloso
# date: 2024-09-22
# parameters:
# - species_list: a list of species codes
# - path: path to the data already curated
# - tree_vars: variables to keep from the tree data (must be available in all the SFNI editions)
# - plot_vars: variables to keep from the plot data (must be available in all the SFNI editions)
# returns:
# - a merged data frame with the tree and plot data from the selected species

load_sfni_filtered <- function(species_list, 
                               path = '1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata',
                               tree_vars, 
                               plot_vars) {
  
  load(path)
  
  # clean tree data
  trees <- trees[trees$species %in% species_list,]
  trees <- dplyr::select(trees, tree_vars)
  
  # clean plot data
  plots <- dplyr::select(plots, plot_vars)
  
  # merge all tree and plot data
  my_trees <- merge(trees, plots, by = 'IFN_PLOT_ID', all.x = TRUE, all.y = FALSE)
  my_trees <- my_trees[!duplicated(my_trees$IFN_TREE_ID), ]
  my_trees <- my_trees[!is.na(my_trees$Species_mixture), ]
  return(my_trees)
}



# Function to plot the h/d relationship of a given species and dataset
# written by: Aitor Vázquez Veloso
# date: 2024-09-09
# parameters:
# - df: a data frame with the tree and plot data
# - dbh_col: the name of the dbh column
# - h_col: the name of the height column
# - species_name: the name of the species
# - species_code: the code of the species
# - sfni_edition: the edition of the SFNI (2, 3, 4, or 234)
# - output_path: the path to save the output
# returns:
# - a ggplot object with the h/d relationship saved as a png file on the output path

plot_hd_by_sp <- function(df, 
                     dbh_col = 'dbh', 
                     h_col = 'h',
                     species_name,
                     species_code,
                     sfni_edition,
                     output_path = '3_figures/1.3_hd_baseline_graphs/'){
  
  # axis limits
  max_x <- round(max(df[[dbh_col]] + 5))
  max_y <- round(max(df[[h_col]] + 2.5))
  
  g <- 
  ggplot(df, aes(x = df[[dbh_col]], y = df[[h_col]])) +
    geom_point(shape = 1, size = 1) +
    #geom_smooth(method = 'lm', se = FALSE) +
    labs(title = paste('h/d relationship on ', species_name, sep = ''),
         subtitle = paste('SFNI edition: ', sfni_edition, sep = ''),
         x = 'dbh (cm)',
         y = 'height (m)') + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5, face = 'italic')) + 
    theme(
      panel.background = element_rect(fill = "white", color = NA),   # Remove panel border
      plot.background = element_rect(fill = "white", color = NA),    # Remove plot border
      panel.grid.major = element_blank(),                            # Remove major grid lines
      panel.grid.minor = element_blank(),                            # Remove minor grid lines
      axis.line = element_line(color = "black")                      # Keep axis lines
    ) +
    # axis limits
    scale_x_continuous(limits = c(0, max_x), breaks = seq(0, max_x, 10)) +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, 5)) 
    # write a line in diagonal
    #geom_abline(intercept = 0, slope = 1, color = 'darkgrey') 
  
  ggsave(filename = paste(output_path, 'sp', species_code, '_sfni', sfni_edition, '.png', sep = ''), plot = g, 
         dpi = 300, width = 7, height = 5)
}



# Function to plot the h/d relationship of a given dataset (adapted from previous one)
# written by: Aitor Vázquez Veloso
# date: 2024-09-24
# parameters:
# - df: a data frame with the tree and plot data
# - dbh_col: the name of the dbh column
# - h_col: the name of the height column
# - df_name: the name of the dataset
# - output_path: the path to save the output
# returns:
# - a ggplot object with the h/d relationship saved as a png file on the output path

plot_hd_by_df <- function(df, 
                          dbh_col = 'dbh', 
                          h_col = 'h',
                          df_name,
                          output_path = '3_figures/1.3_hd_baseline_graphs/'){
  
  # axis limits
  max_x <- round(max(df[[dbh_col]] + 5))
  max_y <- round(max(df[[h_col]] + 2.5))
  
  g <- 
    ggplot(df, aes(x = df[[dbh_col]], y = df[[h_col]])) +
    geom_point(shape = 1, size = 1) +
    #geom_smooth(method = 'lm', se = FALSE) +
    labs(title = 'h/d relationship for all the available species',
         subtitle = paste('Data used: ', df_name, sep = ''),
         x = 'dbh (cm)',
         y = 'height (m)') + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5, face = 'italic')) + 
    theme(
      panel.background = element_rect(fill = "white", color = NA),   # Remove panel border
      plot.background = element_rect(fill = "white", color = NA),    # Remove plot border
      panel.grid.major = element_blank(),                            # Remove major grid lines
      panel.grid.minor = element_blank(),                            # Remove minor grid lines
      axis.line = element_line(color = "black")                      # Keep axis lines
    ) +
    # axis limits
    scale_x_continuous(limits = c(0, max_x), breaks = seq(0, max_x, 10)) +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, 5)) 
  # write a line in diagonal
  #geom_abline(intercept = 0, slope = 1, color = 'darkgrey') 
  
  ggsave(filename = paste(output_path, 'df_', df_name, '.png', sep = ''), plot = g, 
         dpi = 300, width = 7, height = 5)
}

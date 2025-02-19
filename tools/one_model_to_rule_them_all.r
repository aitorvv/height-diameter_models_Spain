#!/usr/bin/Rscript

# Code to use height-diameter models ----
#
# Aitor Vázquez Veloso
# 2024-11-25
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# How to cite this Rscript:

#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# load required library
library(tidyverse)



# Model equations ====

#' @article{el_mamoun_modelling_2013,
#'   title = {Modelling height-diameter relationships of selected economically important natural forests species},
#'   volume = {2},
#'   url = {https://www.researchgate.net/profile/Elmamoun-Osman/publication/284581695_Modelling_height-diameter_relationships_of_selected_economically_important_natural_forests_species/links/62b844c589e4f1160c9dff23/Modelling-height-diameter-relationships-of-selected-economically-important-natural-forests-species.pdf},
#'   journal = {Journal of forest products \& industries},
#'   author = {El Mamoun, H Osman and El Zein, A Idris and El Mugira, M Ibrahim},
#'   year = {2013},
#'   pages = {34--42},
#' }
#' 

# best model equation
# created by Aitor Vázquez Veloso, 2024-11-25
# parameters:
# - a: 1st parameter of the equation
# - b: 2nd parameter of the equation
# - dbh: tree diameter at the breast height (cm)
# output:
# - estimated tree height (m)

elmamoun_2013_M18 <- function(a, b, dbh) {
  h <- 1.3 + a * (log(1 + dbh))^b
  return(h)
}



# Model parameters ====

# best model equation
# created by Aitor Vázquez Veloso, 2024-11-25
# parameters:
# - a: 1st parameter of the equation
# - b: 2nd parameter of the equation
# - dbh: tree diameter at the breast height (cm)
# output:
# - estimated tree height (m)

read_pars <- function(path_to_pars){
  
  ab <- read.csv(paste(path_to_pars, 'ab_coefs.csv', sep = ''))
  fe <- read.csv(paste(path_to_pars, 'fe_coefs.csv', sep = ''))
  
  return(list(ab, fe))
}

# select the parameters for the tree characteristics
# the function must receive just one value, not a dataframe with several rows
# created by Aitor Vázquez Veloso, 2024-11-25
# parameters:
# - tree: dataframe with the tree values (just one row)
# - ab: dataframe with the parameters for the species-specific model
# - fe: dataframe with the parameters for the fixed effects included in the model
# - species_value: 'name' or 'code' to select the species-specific parameters by the desired column; 'name' by default
# - species_col: column name with the species name (or code, as selected before) to filter parameters; 'species_name' by default
# - clim_col: column name with the climate region; 'climate' by default
# - mix_col: column name with the mixture type; 'mixture' by default
# - or_col: column name with the origin type; 'origin' by default
# output:
# - specific parameters for the tree

select_pars <- function(tree, ab, fe, species_value = 'name', species_col = 'species_name', clim_col = 'climate', 
                        mix_col = 'mixture', or_col = 'origin'){
  
  # select species-specific parameters - when an error is detected, then the parameters for all the species are used
  ifelse(is.null(tree[[species_col]]), 
         pars_ab <- 'ERROR', 
         # read the parameters using species name
         if(species_value == 'name'){
          ifelse(tree[[species_col]] %in% ab$Species.name, 
                 pars_ab <- ab[ab$Species.name == tree[[species_col]], ],
                 pars_ab <- ab[ab$Species.name == 'All the species', ])
         # read the parameters using species code
           } else if(species_value == 'code'){
            ifelse(tree[[species_col]] %in% ab$Species.code, 
                   pars_ab <- ab[ab$Species.code == tree[[species_col]], ],
                   pars_ab <- ab[ab$Species.code == 0, ])
             # when the species code is not found, then the parameters for all the species are used
             } else {
              pars_ab <- ab[ab$Species.name == 'All the species', ]
        }
  )
  
  # climate region parameters - when an error is detected, then the parameters for Mediterranean region are used
  ifelse(is.null(tree[[clim_col]]), 
         a_clim <- b_clim <- 0,
        if(tree[[clim_col]] == 'Atlantic'){
          a_clim <- fe$a...fe3..Atlantic.region.
          b_clim <- fe$b...fe3..Atlantic.region.
        } else if(tree[[clim_col]] == 'Alpine'){
          a_clim <- fe$a...fe3..Alpine.region.
          b_clim <- fe$b...fe3..Alpine.region.
        } else if(tree[[clim_col]] == 'Macaronesian'){
          a_clim <- fe$a...fe3..Macaronesian.region.
          b_clim <- fe$b...fe3..Macaronesian.region.
        } else {
          a_clim <- b_clim <- 0
        }
  )
  
  # mixture parameters - when an error is detected, then the parameters for pure stand are used
  ifelse(is.null(tree[[mix_col]]), 
         a_mix <- b_mix <- 0,     
          if(tree[[mix_col]] == 'mix'){
          a_mix <- fe$a...fe1..mix.stand.
          b_mix <- fe$b...fe1..mix.stand.
        } else {
          a_mix <- b_mix <- 0
        }
  )
  
  # origin parameters - when an error is detected, then the parameters for natural stand are used
  ifelse(is.null(tree[[or_col]]), 
         a_or <- b_or <- 0,
        if(tree[[or_col]] == 'plantation'){
        a_or <- fe$a...fe2..artificial.stand.
        b_or <- fe$b...fe2..artificial.stand.
      } else {
        a_or <- b_or <- 0
      }
  )

  # get final parameters
  if(is.character(pars_ab)){
    print('Select the proper column name for the species name or code and rerun that code.')
    return('ERROR')
  } else {
      a <- pars_ab$a + a_clim + a_mix + a_or
      b <- pars_ab$b + b_clim + b_mix + b_or
      return(list(a, b))
  }
  
}



# Model application ====

# function to estimate the tree height iterating over the rows of a dataframe
# the function must receive a dataframe with the tree values and the column names
# created by Aitor Vázquez Veloso, 2024-11-25
# parameters:
# - df: dataframe with the tree values 
# - dbh_col: column name with the tree diameter at the breast height (cm); 'dbh' by default
# - species_value: 'name' or 'code' to select the species-specific parameters by the desired column; 'name' by default
# - species_col: column name with the species name (or code, as selected before) to filter parameters; 'species_name' by default
# - clim_col: column name with the climate region; 'climate' by default
# - mix_col: column name with the mixture type; 'mixture' by default
# - or_col: column name with the origin type; 'origin' by default
# output:
# - original dataframe with the predicted tree height (m) appended

predict_height <- function(df, path_to_pars = 'data/', dbh_col = 'dbh', species_value = 'name', 
                   species_col = 'species_name', clim_col = 'climate', mix_col = 'mixture', or_col = 'origin'){
  
  # get parameters database
  pars <- read_pars(path_to_pars)
  ab <- pars[[1]]
  fe <- pars[[2]]

  # variable to append values
  new_df <- tibble()
  
  # select parameters for each row
  for(tree in 1:nrow(df)){
    
    # select tree values
    tree <- df[tree, ]
    
    # get tree specific parameters
    pars <- select_pars(tree = tree, ab = ab, fe = fe, species_value = species_value, species_col = species_col, 
                clim_col = clim_col, mix_col = mix_col, or_col = or_col)
    
    # check if the parameters are correct and stop the loop if an error is detected
    if (length(pars) == 1) {
      if(pars == 'ERROR'){
        break
      }
    }
    
    # apply height-diameter equation
    tree$pred_h <- elmamoun_2013_M18(dbh = tree[[dbh_col]], a = pars[[1]], b = pars[[2]])

    # add data to the new df    
    new_df <- rbind(new_df, tree) 
  } 

  return(new_df)  
}



# Sample dataset ====

# functions to load a sample dataset
# it contains the minimum required values for different combinations of species, climate regions, mixtures and origins
# one (test_hd_df) contains the by default names and the other (test_hd_df_2) custom names
# created by Aitor Vázquez Veloso, 2024-11-25
# parameters: none
# output:
# - test dataframe with the minimum required values for the get_height function

test_hd_df <- function(){

  df <- tibble(species_name = c('Pinus pinaster', 'Pinus pinaster', 'Pinus pinaster', 'Pinus pinaster', 
                                'Populus alba', 'Populus alba', 'Quercus robur', 'Quercus robur'), 
               species_code = c(26, 26, 26, 26, 51, 51, 41, 41),
               dbh = c(20, 20, 20, 20, 15, 15, 30, 30), 
               climate = c('Alpine', 'Atlantic', 'Macaronesian', 'Mediterranean',
                           'Mediterranean', 'Mediterranean', 'Atlantic', 'Atlantic'), 
               mixture = c('pure', 'pure', 'pure', 'pure', 'pure', 'pure', 'pure', 'mix'), 
               origin = c('natural', 'natural', 'natural', 'natural', 'natural', 'plantation', 'natural', 'natural')
  )
  
  return(df)             
}
 
test_hd_df_2 <- function(){
  
  df <- tibble(species = c('Pinus pinaster', 'Pinus pinaster', 'Pinus pinaster', 'Pinus pinaster', 
                                'Populus alba', 'Populus alba', 'Quercus robur', 'Quercus robur'), 
               codes = c(26, 26, 26, 26, 51, 51, 41, 41),
               dbh_cm = c(20, 20, 20, 20, 15, 15, 30, 30), 
               region = c('Alpine', 'Atlantic', 'Macaronesian', 'Mediterranean',
                           'Mediterranean', 'Mediterranean', 'Atlantic', 'Atlantic'), 
               pure_mix = c('pure', 'pure', 'pure', 'pure', 'pure', 'pure', 'pure', 'mix'), 
               nat_plant = c('natural', 'natural', 'natural', 'natural', 'natural', 'plantation', 'natural', 'natural')
  )
  
  return(df)             
}            
             

# Example of application with the test dataset ====

# # set working directory
# setwd('')
# 
# # get sample dataset
# df <- test_hd_df()
# 
# # export dataset 
# #write.csv(df, '8_tools/data/test_hd_df.csv', row.names = FALSE)
# 
# # apply the function using detailed column names
# new_df <- predict_height(df = df, path_to_pars = 'data/', dbh_col = 'dbh', species_value = 'name', 
#                    species_col = 'species_name', clim_col = 'climate_region', mix_col = 'mixture', or_col = 'origin')
# 
# # apply function using default column names
# new_df_default <- predict_height(df = df)

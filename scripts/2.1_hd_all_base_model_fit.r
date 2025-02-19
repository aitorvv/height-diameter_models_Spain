#!/usr/bin/Rscript

# Code to get hd models ----
# Step 1: fit the better hd model for each data set provided - 73 candidate models
#
# Aitor Vázquez Veloso
# 2024-09-26
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# working directory
setwd('')

# libraries
library(tidyverse)  # data manipulation
library(broom)  # tidying model outputs
library(minpack.lm)  # for nlsLM



# Load data and functions ====

# load functions
source('2_scripts/1.0_hd_support_functions.r')
source('2_scripts/2.0_hd_equations.r')


# load species list and merged data
species <- read_csv('1_data/1_raw/SFNI4_species_codes.csv')
sp_values <- read_csv('3_figures/1.3_hd_baseline_graphs/species/n_values_by_species.csv')
load('1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata')

# reduce variables to save memory
plots <- dplyr::select(plots, IFN_PLOT_ID, N, G, SDI, dg, Stand_origin, Species_mixture,        
                       Stand_type, Age_class, Structure, climate_region)
trees <- dplyr::select(trees, INVENTORY_ID, IFN_PLOT_ID, IFN_TREE_ID, species, dbh, h, tree_type)

# merge trees and plots data to get a single df
df <- merge(trees, plots, by = 'IFN_PLOT_ID', all.x = TRUE)
rm(trees, plots)

# species groups
# sp_groups <- sp_values[sp_values$n_values > 10000 | sp_values$species_code == '034', ]  # 20 species + Pseudotsuga menziesii
sp_groups <- sp_values[sp_values$n_values > 50, ]
sp_groups <- as.numeric(sp_groups$species_code)
sp_groups <- na.omit(sp_groups)  # 25_245 changes to NA as cannot be numeric
sp_groups <- c(sp_groups, 45)  # add Quercus ilex (I will change the code of 245 to 45 to homogeneity)
# Note: I leave the following code commented as it is not needed for the current analysis, but maybe in other one

# change Quercus ilex code to 45 (both subsp.)
df$species <- ifelse(df$species == 245, 45, df$species)
df2 <- df[df$INVENTORY_ID == 'IFN2', ]
df3 <- df[df$INVENTORY_ID == 'IFN3', ]
df4 <- df[df$INVENTORY_ID == 'IFN4', ]
# Note: species code 245 changed to 45

# pack data in a list of df
# df_list <- list(df, df2, df3, df4)
df_list <- list(df)
# df_names <- c('df', 'df2', 'df3', 'df4')
df_names <- c('df')

# species not in the groups
# all_species <- sp_values$species_code
# all_species <- as.numeric(all_species)
# all_species <- all_species[!is.na(all_species)]
# sp_other <- setdiff(all_species, sp_groups)
# all_species <- c(all_species, 25, 245, 12, 16)  # add grouped species

# add the rest of species in just one group
# sp_groups <- c(sp_groups, c('45,245', 'other', 'other_conifers', 'other_broadleaved', 
#                             'all', 'all_conifers', 'all_broadleaved'))

# additional groups to filter
# sp_conifers <- df[df$tree_type == 'conifer', ]
# sp_conifers <- unique(sp_conifers$species)
# sp_broadleaved <- df[df$tree_type == 'broadleaved', ]
# sp_broadleaved <- sp_broadleaved[!is.na(sp_broadleaved$species), ]
# sp_broadleaved <- unique(sp_broadleaved$species)

# save information to reuse later
save.image('1_data/2_processed/2.1_hd_model_data_workspace.rdata')



# Model selection and start coefficients management ====

# list of models to fit based just on dbh as predictor
models <- get_models_list()

# helper function to adjust starting values dynamically
adjust_start <- function(start_params, adjustment_factor = 0.5) {
  lapply(start_params, function(x) {
    x + runif(1, -adjustment_factor, adjustment_factor)  # randomly adjust within a small range
  })
}

# maximum number of retries
max_retries <- 3



# Helper functions ====

# helper function to calculate RMSE
calculate_rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}

# helper function to calculate MAE
calculate_mae <- function(observed, predicted) {
  mean(abs(observed - predicted))
}



# Fit the models ====

# for loop for species group
for(sp in sp_groups){
  
  df_count <- 0
  sp_filter <- as.numeric(sp)
  
  # choose species group
  # if (sp == '45,245') {
  #   sp_filter <- c(25, 245)
  # } else if (sp == 'other') {
  #   sp_filter <- sp_other
  # } else if (sp == 'other_conifers') {
  #   sp_filter <- sp_conifers
  #   sp_filter <- sp_filter[sp_filter %in% sp_other]
  # } else if (sp == 'other_broadleaved') {
  #   sp_filter <- sp_broadleaved
  #   sp_filter <- sp_filter[sp_filter %in% sp_other]
  # } else if (sp == 'all') {
  #   sp_filter <- all_species
  # } else if (sp == 'all_conifers') {
  #   sp_filter <- sp_conifers
  # } else if (sp == 'all_broadleaved') {
  #   sp_filter <- sp_broadleaved
  # } else {
  #   sp_filter <- as.numeric(sp)
  # }
  # Note: I leave the following code commented as it is not needed for the current analysis, but maybe in other one
  
  # for each dataset in the list of datasets to fit
  for(my_df in df_list){
  
    # filter species data
    df_sp <- my_df[my_df$species %in% sp_filter, ]
    if (nrow(df_sp) == 0) {
      next
    }
      
    # print progress
    df_count <- df_count + 1
    print(paste("Fitting models for dataset ", df_count, " of ", length(df_list), " named ", 
                df_names[df_count], sep = ''))
    
    # initialize empty list to store results
    results <- list()
  
    # for loop to fit each model
    for (model_name in names(models)) {
      
      # extract model information and set default values
      model_function <- models[[model_name]]$func
      start_params <- models[[model_name]]$start
      attempt <- 1
      fit_success <- FALSE
      
      # try fitting the model with multiple attempts if no success
      while (attempt <= max_retries && !fit_success) {
        
        # dynamically create the formula for nls adapted to each model
        param_names <- names(start_params)
        formula_str <- paste0("h ~ model_function(", paste(param_names, collapse = ", "), ", dbh)")
        model_formula <- as.formula(formula_str)
        
        # try fitting the model
        fit_result <- tryCatch({
          
          fit <- nlsLM(model_formula, data = df_sp, start = start_params)
          
          # predicted values
          predicted <- predict(fit, newdata = df_sp)
          
          # calculate R²
          ss_total <- sum((df_sp$h - mean(df_sp$h))^2)
          ss_residual <- sum(residuals(fit)^2)
          r_squared <- 1 - (ss_residual / ss_total)
          
          # calculate RMSE
          rmse <- calculate_rmse(df_sp$h, predicted)
          
          # calculate MAE
          mae <- calculate_mae(df_sp$h, predicted)
          
          # calculate BIC
          bic <- BIC(fit)
          
          # calculate AIC
          aic <- AIC(fit)
          
          # if the fit is successful, mark success and return the model info
          fit_success <- TRUE        
          print(paste('Model ', model_name, " fitted successfully for species group ", sp, 
                      " (dataset ", df_count, " of ", length(df_list), ")", sep = ''))
          list(model = model_name, 
               bic = bic,            
               r_squared = r_squared,
               rmse = rmse,          
               mae = mae,             
               aic = aic, 
               coefficients = tidy(fit))
          
        }, error = function(e) {
          
          # in case of error, print the attempt and adjust starting values
          message(paste("Error in model ", model_name, " on attempt ", attempt, ": ", e$message, sep = ''))
          
          # adjust start values and try again
          start_params <- adjust_start(start_params)  
          NULL
        })
        
        # if successful, store the result
        if (!is.null(fit_result)) {
          results[[model_name]] <- fit_result
        }
        
        # increment the attempt counter
        attempt <- attempt + 1
      }
    }
    
    # if the model failed after max retries, store NA
    if (!fit_success) {
      results[[model_name]] <- list(model = model_name, bic = NA, r_squared = NA, rmse = NA, 
                                    mae = NA, aic = NA, coefficients = NA)
    }
    
    # skip empty results
    if (length(results) == 0) {
      next
    }
    
  
  
    # Ranking and export results ====
    
    # convert the results into a tidy data frame for easy comparison
    results_df <- do.call(rbind, lapply(results, function(x) {
      data.frame(model = x$model, bic = x$bic, r_squared = x$r_squared, rmse = x$rmse, 
                 mae = x$mae, aic = x$aic)
    }))
    
    # show the results ordered by AIC (lower AIC is better)
    results_df <- results_df %>%
      arrange(aic)
    
    # export ordered results in a .csv file
    write.csv(results_df, paste('3_figures/2.1_hd_all_base_model_fit/stats/', df_names[df_count], 
                                '_models_stats-sp_group_', sp, '.csv', sep = ''), row.names = FALSE)
    
    # export the coefficients of the models that were able to fit
    results_df <- results_df[!is.na(results_df$aic), ]
    coefs <- tibble(model = character(), term = character(), estimate = numeric(), std.error = numeric(),
                    statistic = numeric(), p.value = numeric())
    
    for(fitted_model in results_df$model){
    
      # filter model information
      model <- results[[fitted_model]]
      model <- bind_cols(model = model$model, model$coefficients)
      
      # append to the coefficients data frame
      coefs <- rbind(coefs, model)
    }
    
    # export coefficients
    write.csv(coefs, paste('3_figures/2.1_hd_all_base_model_fit/coefficients/', 
            df_names[df_count], '_models_coefs-sp_group_', sp, '.csv', sep = ''), row.names = FALSE)
  }
}



# Compile all the models coefficients and stats on a single data set ====

# compile all the coefficients and stats in a single data frame
coefs_path <- "3_figures/2.1_hd_all_base_model_fit/coefficients/"
coefs_file_list <- list.files(path = coefs_path, full.names = TRUE)
all_coefs <- coefs_file_list %>% map_dfr(~ read_csv(.x) %>% mutate(filename = basename(.x)))

stats_path <- "3_figures/2.1_hd_all_base_model_fit/stats/"
stats_file_list <- list.files(path = stats_path, full.names = TRUE)
all_stats <- stats_file_list %>% map_dfr(~ read_csv(.x) %>% mutate(filename = basename(.x)))

# identify df and species
case_study <- str_split_fixed(all_coefs$filename, "_", 3)[, 1:2]
all_coefs$df <- paste(case_study[, 1], case_study[, 2], sep = "_")
all_coefs$df <- ifelse(all_coefs$df == 'df_models', 'df', all_coefs$df)
all_coefs$df <- ifelse(all_coefs$df == 'df2_models', 'df2', all_coefs$df)
all_coefs$df <- ifelse(all_coefs$df == 'df3_models', 'df3', all_coefs$df)
all_coefs$df <- ifelse(all_coefs$df == 'df4_models', 'df4', all_coefs$df)
all_coefs$species <- str_extract(all_coefs$filename, "(?<=group_)[^\\.]+(?=\\.csv)")

case_study <- str_split_fixed(all_stats$filename, "_", 3)[, 1:2]
all_stats$df <- paste(case_study[, 1], case_study[, 2], sep = "_")
all_stats$df <- ifelse(all_stats$df == 'df_models', 'df', all_stats$df)
all_stats$df <- ifelse(all_stats$df == 'df2_models', 'df2', all_stats$df)
all_stats$df <- ifelse(all_stats$df == 'df3_models', 'df3', all_stats$df)
all_stats$df <- ifelse(all_stats$df == 'df4_models', 'df4', all_stats$df)
all_stats$species <- str_extract(all_stats$filename, "(?<=group_)[^\\.]+(?=\\.csv)")

# save results
save(all_coefs, all_stats, file = '3_figures/2.1_hd_all_base_model_fit/all_models.rdata')

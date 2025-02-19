#!/usr/bin/Rscript

# Code to get hd models ----
# Step 4: improve nlme models by including additional variables easy to measure
#
# Aitor Vázquez Veloso
# 2024-10-14
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# Some decisions that has been taken can be filtered by "# Note:"

# reference code: https://github.com/cran/nlme/tree/master
# to read as a guide: https://socserv.socsci.mcmaster.ca/jfox/books/companion/appendices/Appendix-Nonlinear-Regression.pdf



# working directory
setwd('')

# install and load necessary libraries
library(nlme)
library(tidyverse)       
library(broom.mixed)  # useful for extracting residuals, fitted values, etc.



# Load data and functions ====

# load functions
source('2_scripts/2.0_hd_equations.r')

# load species list and merged data
load('1_data/2_processed/2.2_hd_top_model_selection.rdata')



# Helper functions ====

# helper function to calculate RMSE
calculate_rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}

# helper function to calculate MAE
calculate_mae <- function(observed, predicted) {
  mean(abs(observed - predicted))
}



# Select model name and average values ====

models_list <- 'linear_model_II'

for(model_name in models_list){
  
  print(paste0("Model: ", model_name))
  
  # select average coefs from the previous fit
  avg_values <- all_coefs[all_coefs$model == model_name, ]
  avg_values <- avg_values %>%
    group_by(term) %>%
    summarise(avg_estimate = mean(estimate, na.rm = TRUE))
  
  
  
  # Select the better model information: same model for all species ====
  
  # get model details
  models <- get_models_list()
  model_function <- models[[model_name]]$func
  model_coefs <- models[[model_name]]$start
  param_names <- names(model_coefs)
  formula_str <- paste0("h ~ model_function(", paste(param_names, collapse = ", "), ", dbh)")
  model_formula <- as.formula(formula_str)
  # rm(list = setdiff(ls(), c("df", "sp_list", "avg_values", "model_name", "model_function", "model_coefs", "param_names", 
  # "formula_str", "model_formula")))
  
  
  
  # Prepare fixed and random effects combinations: same model for all the species ====
  
  # model terms, terms combinations and values 
  terms <- names(model_coefs)
  terms_combis <- paste(terms, collapse = " + ")
  terms_combined <- terms_combis[!terms_combis %in% terms]
  terms_combis <- unlist(lapply(1:length(terms), function(x) {
    combn(terms, x, FUN = function(y) paste(y, collapse = " + "))
  }))
  
  start_values <- avg_values$avg_estimate
  names(start_values)  <- names(model_coefs)
  
  # define the fixed effect combinations
  fe_vars_list <- c(fe1 = 'Species_mixture', fe2 = 'Stand_origin', fe3 = 'climate_region')
  fe_vars <- c('fe1', 'fe2', 'fe3')
  fe_vars <- unlist(lapply(1:length(fe_vars), function(x) {
    combn(fe_vars, x, FUN = function(y) paste(y, collapse = " + "))
  }))
  
  # get all the possible combinations of each term with the variables included as fixed effects
  # step 1: combinations without the same fixed effects on more than one variable
  fe_combis <- formula_1 <- formula_2 <- list()
  
  for(fe_comb in combn(c('1', fe_vars), 2, simplify = FALSE)) {
    
    # create the formula for each parameter with its corresponding fixed effect
    param1a <- paste(terms[1], "~", fe_comb[1])
    param2a <- paste(terms[2], "~", fe_comb[2])
    param1b <- paste(terms[1], "~", fe_comb[2])
    param2b <- paste(terms[2], "~", fe_comb[1])
    
    # combine the two formulas into a single expression
    formula_1 <- list(as.formula(param1a), as.formula(param2a))
    formula_2 <- list(as.formula(param1b), as.formula(param2b))
    # combined_formula_2 <- paste(param2, ", ", param1, sep = '')
    
    # store the combined formula in the list
    fe_combis <- c(fe_combis, list(formula_1), list(formula_2))
  }
  # Note: if the function has more than two parameters, the combn function should be modified accordingly
  
  
  # step 2: combinations with the same fixed effects on more than one variable
  for(fe in fe_vars){
    for(term in terms_combined){
      combi <- paste(term, "~", fe)
      combi <- as.formula(combi)
      fe_combis <- c(fe_combis, list(combi))
    }
  }
  # Note: if the function has more than two parameters, the combn function should be modified accordingly
  
  
  # define random effects variables as factors and list them (just species)
  df <- df %>% mutate(re1 = as.factor(species))
  re_vars <- 're1'
  
  # get all the possible combinations of terms ~ random effects 
  re_combis <- list()
  
  for(re in re_vars){
    for(term in terms_combis){
      random_str <- paste(term, "~ 1", "|", re)
      random_str <- as.formula(random_str)
      re_combis <- c(re_combis, list(random_str))
    }
  }
  
  
  
  # Loop over the fixed effect combinations ====
  
  # initialize an empty list to store model results and its metrics
  model_results <- list()
  metrics <- tibble(model_name = character(), n_re_combi = numeric(), n_fe_combi = numeric(),
                    aic = numeric(), bic = numeric(), logLik = numeric(), rmse = numeric(), mae = numeric())
  all_adapted_dfs <- list()
  
  # loop over each random effect combination
  for (i in seq_along(fe_combis)){
    print(i)
    # skip already finished models
    if(i < 57){next}
    
    # fixed effect combination
    fe_combi <- fe_combis[[i]]  
    
    # copy the original data frame and declare needed variables
    df_adapted <- df
    text_fe_combi <- paste(as.character(fe_combi), collapse = ' ')
    levels_fe1 <- levels_fe2 <- levels_fe3 <- levels_count <- 0
    
    # adapt the data frame according to the fixed effect combination
    if(grepl("fe1", text_fe_combi)){
      df_adapted <- df_adapted[!is.na(df_adapted[[fe_vars_list["fe1"]]]), ]
      df_adapted$fe1 <- as.factor(df_adapted[[fe_vars_list["fe1"]]])
      levels_fe1 <- length(unique(df_adapted$fe1))
      levels_count <- levels_count + 1
    }
    if(grepl("fe2", text_fe_combi)){
      df_adapted <- df_adapted[df_adapted[[fe_vars_list["fe2"]]] %in% c('natural', 'artificial'), ]
      df_adapted <- df_adapted[!is.na(df_adapted[[fe_vars_list["fe2"]]]), ]
      df_adapted$fe2 <- as.factor(df_adapted[[fe_vars_list["fe2"]]])
      levels_fe2 <- length(unique(df_adapted$fe2))
      levels_count <- levels_count + 1
    }
    if(grepl("fe3", text_fe_combi)){
      df_adapted <- df_adapted[!is.na(df_adapted[[fe_vars_list["fe3"]]]), ]
      df_adapted$fe3 <- as.factor(df_adapted[[fe_vars_list["fe3"]]])
      levels_fe3 <- length(unique(df_adapted$fe3))
      levels_count <- levels_count + 1
    }
    
    
    
    # Define the initial values for the parameters ====
    
    # define the objective function to minimize (residual sum of squares)
    rss <- function(params, dbh, h, model_function){
      
      # conditionally assign the parameters to the model function
      if(!is.na(params[1])){a <- params[1]}
      if(!is.na(params[2])){b <- params[2]} else {h_pred <- model_function(a, df$dbh)}
      if(!is.na(params[3])){
        c <- params[3]
        h_pred <- model_function(a, b, c, df$dbh)
      } else {h_pred <- model_function(a, b, df$dbh)}
      
      # calculate the residual sum of squares
      sum((h - h_pred)^2)
    }
    
    
    # use optim to find the best-fitting parameters for all the species studied
    initial_optim <- optim(
      par = start_values,  
      fn = rss, 
      dbh = df$dbh, 
      h = df$h,
      model_function = model_function
    )
    
    # for each parameter
    optimal_params <- list()
    
    for(k in 1:length(initial_optim$par)){
      
      # each parameter has to have at least one value
      optimal_params <- c(optimal_params, initial_optim$par[k])
      
      # for each fixed effect combination with different parameters
      
      if(inherits(fe_combi, 'formula')){
        fc <- fe_combi
        # include the appopriate fixed effect values according to the classes of each fixed effect
        if(grepl("fe1", deparse(fc)) && grepl(names(initial_optim$par[k]), deparse(fc))){optimal_params <- c(optimal_params, initial_optim$par[k])}  # fe1 has two level
        if(grepl("fe2", deparse(fc)) && grepl(names(initial_optim$par[k]), deparse(fc))){optimal_params <- c(optimal_params, rep(initial_optim$par[k]))}  # fe2 has two levels
        if(grepl("fe3", deparse(fc)) && grepl(names(initial_optim$par[k]), deparse(fc))){optimal_params <- c(optimal_params, 
                                                                                                             initial_optim$par[k], initial_optim$par[k], initial_optim$par[k])}  # fe3 has four levels
      } else {
        for(fc in fe_combi){
          # include the appopriate fixed effect values according to the classes of each fixed effect
          if(grepl("fe1", deparse(fc)) && grepl(names(initial_optim$par[k]), deparse(fc))){optimal_params <- c(optimal_params, initial_optim$par[k])}  # fe1 has two level
          if(grepl("fe2", deparse(fc)) && grepl(names(initial_optim$par[k]), deparse(fc))){optimal_params <- c(optimal_params, rep(initial_optim$par[k]))}  # fe2 has two levels
          if(grepl("fe3", deparse(fc)) && grepl(names(initial_optim$par[k]), deparse(fc))){optimal_params <- c(optimal_params, 
                                                                                                               initial_optim$par[k], initial_optim$par[k], initial_optim$par[k])}  # fe3 has four levels
        } 
      }
    }
    
    # include the appopriate fixed effect values according to the classes of each fixed effect
    # if(grepl("fe1", deparse(fe_combi))){optimal_params <- c(optimal_params, initial_optim$par[k])}  # fe1 has two level
    # if(grepl("fe2", deparse(fe_combi))){optimal_params <- c(optimal_params, rep(initial_optim$par[k]))}  # fe2 has two levels
    # if(grepl("fe3", deparse(fe_combi))){optimal_params <- c(optimal_params, 
    # initial_optim$par[k], initial_optim$par[k], initial_optim$par[k])}  # fe3 has four levels
    # Note: here we have to adapt that conditions to the degrees of freedom of each variable
    
    
    
    # Loop over the random effect combinations ====
    
    for (j in seq_along(re_combis)) {
      
      # random effect combination
      re_combi <- re_combis[[j]]  
      
      
      
      # Fit the NLME model ====
      
      fit <- tryCatch({
        nlme(
          model = model_formula,
          data = df_adapted,
          fixed = fe_combi,  
          random = re_combi,
          start = unlist(optimal_params)  
        )
        
      }, error = function(e) {
        message(paste("Error in model:", i, ":", e$message))
        return(NULL)  # return NULL if there's an error
      })
      
      
      
      # Save the model results and metrics ====
      
      # check if the model fit was successful
      if (!is.null(fit)) {
        
        print('Model fitted successfully!')
        
        # save the model results
        # model_results[[i]] <- fit
        model_results <- list(model_name = model_name,
                              re_combi = re_combi, 
                              n_re_combi = j,
                              fe_combi = fe_combi,
                              n_fe_combi = i,
                              start = unlist(optimal_params),  
                              error = FALSE)  # mark as error if fitting failed
        
        # prefict the height based on the model
        df_adapted$predicted_h <- predict(fit)
        
        metrics <- bind_rows(metrics, tibble(model_name = model_name,
                                             n_re_combi = j,
                                             n_fe_combi = i,
                                             aic = AIC(fit), 
                                             bic = BIC(fit), 
                                             logLik = fit$logLik,
                                             rmse = calculate_rmse(df_adapted$h, df_adapted$predicted_h),
                                             mae = calculate_mae(df_adapted$h, df_adapted$predicted_h)))
        
      } else {
        print('Model not fitted...')
        model_results <- list(model_name = model_name,
                              re_combi = re_combi, 
                              n_re_combi = j,
                              fe_combi = fe_combi,
                              n_fe_combi = i,
                              start = unlist(optimal_params),  
                              error = TRUE)  # mark as error if fitting failed
        metrics <- bind_rows(metrics, tibble(model_name = model_name, n_re_combi = j, n_fe_combi = i, 
                                             aic = NA, bic = NA, logLik = NA, rmse = NA, mae = NA))
        df_adapted <- NULL
      }
      
      # save the adapted data frame
      # all_adapted_dfs[[i]] <- df_adapted
      
      # save results
      save(df_adapted, model_results, metrics, sp_list, file = 
             paste('1_data/2_processed/2.3_fit_nlme_models/', model_name, '__re_', j, '__fe_', i, '.rdata', sep = ''))
      
      print(paste('Model ', model_name, ' fitted with fe_combi ', i, ' of ', length(fe_combis), ' and re_combi ', j, 
                  ' of ', length(re_combis), sep = ''))
    }
  }
  
  # save results
  # print('Saving results...')
  # save(all_adapted_dfs, model_results, metrics, sp_list, file = paste('1_data/2_processed/2.3_fit_nlme_models_', 
  #                                                                     model_name , '_all_combis.rdata', sep = ''))
  # print(paste('Results saved for model ', model_name, '; fe_combi ', i, ' de ', length(fe_combis), sep = ''))
  # Note: when trying a lot of combinations the code is stopped, so I will save them independently
  
}

# save metrics results
save(metrics, file = '1_data/2_processed/2.3_fit_nlme_models_metrics.rdata')
print('Metrics saved!')
print('End of the script!')



# Select the better nlme model based on species ~ random effect combination ====
# 
# print('Selecting the best model...')
# 
# # select the best model based on AIC
# metrics <- metrics[order(metrics$aic, decreasing = FALSE), ]
# metrics[1, ]
# 
# best_model <- model_results[[metrics$nlme_model[1]]]
# metrics <- metrics[1, ]
# 
# # height prediction using the best model
# df_adapted <- all_adapted_dfs[[metrics$nlme_model]]
# # df_adapted$predicted_h <- predict(best_model)
# 
# # save results and required data for the next analysis
# save(df, df_adapted, sp_list, metrics, best_model,
#      file = '1_data/2_processed/2.3_fit_nlme_models_best_model.rdata')
# print('Best model selected and saved!')

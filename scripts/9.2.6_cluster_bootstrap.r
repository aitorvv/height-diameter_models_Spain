#!/usr/bin/Rscript

# Code to get hd models ----
# Step 6: cluster bootstrap
#
# Aitor Vázquez Veloso
# 2024-10-31
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# Some decisions that has been taken can be filtered by "# Note:"



# working directory
setwd('')

# install and load necessary libraries
library(tidyverse)       
library(nlme)



# Load the best model information and additional resources ====

# load the model results
load('1_data/2_processed/2.3_best_model_fe_good_order.rdata')
rm(fit, model_results)



# Helper functions ====

# helper function to calculate RMSE
calculate_rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}

# helper function to calculate MAE
calculate_mae <- function(observed, predicted) {
  mean(abs(observed - predicted))
}



# Run cluster bootstrap ====

# update optimal parameters
optimal_params[1:6] <- 1.4096
optimal_params[7:12] <- 1.9796

# set the number of bootstrap samples
n_boot <- 1000

# create a loop to store bootstrap results
boots_results <- tibble(n_boot = numeric(),  # fit stats
                        aic = numeric(), bic = numeric(), logLik = numeric(), rmse = numeric(), mae = numeric())
pred_vs_obs <- tibble(n_boot = numeric(), n_tree = character(),  # predictions
                      h = numeric(), boots_h = numeric(), mb = numeric())  
fe_params <- tibble("model" = numeric(),  # fixed effects parameters
                    "a.(Intercept)" = numeric(), "a.fe1mix" = numeric(), "a.fe2artificial" = numeric(), 
                    "a.fe3Atlántica" = numeric(), "a.fe3Alpina" = numeric(), "a.fe3Macaronésica" = numeric(), 
                    "b.(Intercept)" = numeric(), "b.fe1mix" = numeric(), "b.fe2artificial" = numeric(), 
                    "b.fe3Atlántica" = numeric(), "b.fe3Alpina" = numeric(), "b.fe3Macaronésica" = numeric())
re_params <- tibble("model" = numeric(),  # random effects parameters
                    "a.(Intercept)" = numeric(), "b.(Intercept)" = numeric(), "species" = character())
fe_stats <- tibble("model" = numeric(),  # fixed effects statistics
                   "a.variance" = numeric(), "a.stddev" = numeric(), "a.corr" = numeric(),
                   "b.variance" = numeric(), "b.stddev" = numeric(), 
                   "residual.variance" = numeric(), "residual.stddev" = numeric())

lapply(1:n_boot, function(i){
  
  # step 1: sample unique tree IDs with replacement
  uniq_ids <- unique(df_adapted$IFN_TREE_ID)
  bootstrap_ids <- sample(uniq_ids, size = length(uniq_ids), replace = TRUE)
  
  # include all rows of data that belong to the sampled trees
  bootstrap_sample <- df_adapted[df_adapted$IFN_TREE_ID %in% bootstrap_ids, ]
  
  # step 2: fit the nonlinear mixed-effects model on the bootstrap sample
  
  fit_boots <- tryCatch({
    nlme(model_formula,
         data = bootstrap_sample,
         fixed = fe_combi,
         random = re_combi,
         start = unlist(optimal_params))
    
  }, error = function(e) {
    message(paste("Error in model:", i, ":", e$message))
    return(NULL)  # return NULL if there's an error
  })
  
  # check if the model fit was successful
  if(exists("fit_boots")) {
    if(!is.null(fit_boots)){
      print(paste('Model ', i, ' fitted successfully!'), sep = '')
      
      # predict the height based on the model
      bootstrap_sample$boots_h <- predict(fit_boots)
      
      # fit stats
      boots_results <- bind_rows(boots_results, tibble(
        n_boot = i,
        aic = AIC(fit_boots), 
        bic = BIC(fit_boots), 
        logLik = fit_boots$logLik,
        rmse = calculate_rmse(bootstrap_sample$h, bootstrap_sample$boots_h),
        mae = calculate_mae(bootstrap_sample$h, bootstrap_sample$boots_h)))
      
      # predictions
      pred_vs_obs <- bind_rows(pred_vs_obs, tibble(
        n_boot = i,
        n_tree = bootstrap_sample$IFN_TREE_ID,
        h = bootstrap_sample$h,
        boots_h = bootstrap_sample$boots_h,
        mb = bootstrap_sample$h - bootstrap_sample$boots_h))
      
      # fixed effects parameters
      fixed_table <- fixef(fit_boots)
      fixed_table <- as.data.frame(t(fixed_table))
      fixed_table$model <- i
      fe_params <- bind_rows(fe_params, fixed_table)
      
      # fixed effects statistics
      fe_varcorr <- VarCorr(fit_boots)
      fe_stats_i <- data.frame("model" = i,
                               "a.variance" = as.numeric(fe_varcorr["a.(Intercept)", "Variance"]), 
                               "a.stddev" = as.numeric(fe_varcorr["a.(Intercept)", "StdDev"]), 
                               "a.corr" = as.numeric(fe_varcorr["b.(Intercept)", "Corr"]),
                               "b.variance" = as.numeric(fe_varcorr["b.(Intercept)", "Variance"]), 
                               "b.stddev" = as.numeric(fe_varcorr["b.(Intercept)", "StdDev"]), 
                               "residual.variance" = as.numeric(fe_varcorr["Residual", "Variance"]), 
                               "residual.stddev" = as.numeric(fe_varcorr["Residual", "StdDev"]))
      fe_stats <- bind_rows(fe_stats, fe_stats_i)
      
      # random effects parameters
      random_table <- ranef(fit_boots)
      random_table <- as.data.frame(random_table)
      random_table$species <- rownames(random_table)
      random_table$model <- i
      re_params <- bind_rows(re_params, random_table)
      
    } else {
      print(paste('Model ', i, ' not fitted!'), sep = '')
      
      boots_results <- bind_rows(boots_results, tibble(n_boot = i,  # fit stats
                                                       aic = NA, bic = NA, logLik = NA, rmse = NA, mae = NA))
      pred_vs_obs <- bind_rows(pred_vs_obs, tibble(n_boot = i, n_tree = bootstrap_sample$IFN_TREE_ID,  # predictions
                                                   h = bootstrap_sample$h, boots_h = NA, mb = NA))
      fe_params <- bind_rows(fe_params, tibble("model" = i,  # fixed effects parameters
                                               "a.(Intercept)" = NA, "a.fe1mix" = NA, "a.fe2artificial" = NA, 
                                               "a.fe3Atlántica" = NA, "a.fe3Alpina" = NA, "a.fe3Macaronésica" = NA, 
                                               "b.(Intercept)" = NA, "b.fe1mix" = NA, "b.fe2artificial" = NA, 
                                               "b.fe3Atlántica" = NA, "b.fe3Alpina" = NA, "b.fe3Macaronésica" = NA))
      re_params <- bind_rows(re_params, tibble("model" = i,  # random effects parameters
                                               "a.(Intercept)" = NA, "b.(Intercept)" = NA, "species" = NA))
      fe_stats <- bind_rows(fe_stats, tibble("model" = i,  # fixed effects statistics
                                             "a.variance" = NA, "a.stddev" = NA, "a.corr" = NA,
                                             "b.variance" = NA, "b.stddev" = NA, 
                                             "residual.variance" = NA, "residual.stddev" = NA))
    }
  }
  
  # save results
  save(boots_results, pred_vs_obs, fe_params, re_params, fe_stats, 
       file = paste('1_data/2_processed/2.6/2.6_cluster_bootstrap_results_', i, '.rdata', sep = ''))
  print(paste('Model ', i, ' saved!', sep = ''))
})

#!/usr/bin/Rscript

# Code to get hd models ----
# Step 4: improve nlme models by including additional variables easy to measure
#
# Aitor Vázquez Veloso
# 2024-10-03
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

# load species list and merged data
load('1_data/2_processed/2.3_fit_nlme_models_by_species-hyperbolic_model_I-best_combi.rdata')



# Prepare fixed and random effects combinations: same model for all the species ====

# model terms, terms combinations and values 
terms <- names(model_coefs)
terms_combis <- paste(terms, collapse = " + ")
terms_combined <- terms_combis[!terms_combis %in% terms]

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


# define random effects variables as the results of the previous analysis
re_combi <- best_model$re_combi
df <- df %>% mutate(re1 = as.factor(species))



# Helper functions ====

# helper function to calculate RMSE
calculate_rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2))
}

# helper function to calculate MAE
calculate_mae <- function(observed, predicted) {
  mean(abs(observed - predicted))
}



# Loop over the fixed effect combinations and fit the nlme model ====

# initialize an empty list to store model results and its metrics
model_results <- list()
metrics <- tibble(nlme_model = numeric(), aic = numeric(), bic = numeric(), logLik = numeric(),
                  rmse = numeric(), mae = numeric())
all_adapted_dfs <- list()

# loop over each random effect combination
for (i in seq_along(fe_combis)){
  
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
  
  # # estimate the degrees of freedom and set the start values according to the fixed effect combination
  # degrees_freedom <- levels_count - 1
  # total_coefficients <- levels_fe1 + levels_fe2 + levels_fe3 - degrees_freedom
  # start_values_in_combi <- unlist(lapply(names(start_values), function(name) {
  #   grepl(name, text_fe_combi)
  # }))
  # start_values_in_combi <- start_values[start_values_in_combi]
  # 
  # # automatize the repetition of starting values based on the coefficient names
  # start_values_combi <- unlist(lapply(names(start_values_in_combi), function(name) {
  #   rep(start_values[[name]], total_coefficients)
  # }))
  # 
  # # set correct names for the repeated coefficients (if needed)
  # names(start_values_combi) <- paste(rep(names(start_values_in_combi), each = total_coefficients), 
  #                                       seq_len(total_coefficients), sep = "")
  
  
  
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
  
  # get the optimized parameters with the proper structure
  # n <- length(initial_optim$par)
  # optimal_params <- rep(initial_optim$par, each = n)
  
  # for each parameter
  optimal_params <- list()
  
  for(k in 1:length(initial_optim$par)){
    
    # each parameter has to have at least one value
    optimal_params <- c(optimal_params, initial_optim$par[k])
    
    # include the appopriate fixed effect values according to the classes of each fixed effect
    if(grepl("fe1", deparse(fe_combi))){optimal_params <- c(optimal_params, initial_optim$par[k])}  # fe1 has two level
    if(grepl("fe2", deparse(fe_combi))){optimal_params <- c(optimal_params, rep(initial_optim$par[k]))}  # fe2 has two levels
    if(grepl("fe3", deparse(fe_combi))){optimal_params <- c(optimal_params, 
                                                            initial_optim$par[k], initial_optim$par[k], initial_optim$par[k])}  # fe3 has four levels
    # Note: here we have to adapt that conditions to the degrees of freedom of each variable
  }
  
  
  
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
    model_results[[i]] <- fit
    
    # prefict the height based on the model
    df_adapted$predicted_h <- predict(model_results[[i]])
    
    metrics <- bind_rows(metrics, tibble(nlme_model = i, 
                                         aic = AIC(model_results[[i]]), 
                                         bic = BIC(model_results[[i]]), 
                                         logLik = model_results[[i]]$logLik,
                                         rmse = calculate_rmse(df_adapted$h, df_adapted$predicted_h),
                                         mae = calculate_mae(df_adapted$h, df_adapted$predicted_h)))
    
  } else {
    print('Model not fitted...')
    model_results[[i]] <- list(re_combi = re_combi, 
                               fe_combi = fe_combi,
                               error = TRUE)  # mark as error if fitting failed
    metrics <- bind_rows(metrics, tibble(nlme_model = i, aic = NA, bic = NA, logLik = NA, rmse = NA, mae = NA))
  }
  
  # save the adapted data frame
  all_adapted_dfs[[i]] <- df_adapted
}

# save results
save(all_adapted_dfs, model_results, metrics, sp_list, file = '1_data/2_processed/2.4_fit_nlme_models_by_species-hyperbolic_model_I-all_models.rdata')



# Select the better nlme model based on species ~ random effect combination ====

# select the best model based on AIC
metrics <- metrics[order(metrics$aic, decreasing = FALSE), ]
metrics[1, ]

best_model <- model_results[[metrics$nlme_model[1]]]
metrics <- metrics[1, ]

# height prediction using the best model
df_adapted <- all_adapted_dfs[[metrics$nlme_model]]
df_adapted$predicted_h <- predict(best_model)

# save results and required data for the next analysis
save(df, df_adapted, sp_list, metrics, best_model,
     file = '1_data/2_processed/2.4_fit_nlme_models_by_species-hyperbolic_model_I-best_models.rdata')

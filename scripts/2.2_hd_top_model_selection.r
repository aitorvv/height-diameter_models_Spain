#!/usr/bin/Rscript

# Code to get hd models ----
# Step 2: select top 5 models from candidates and filter model results
#
# Aitor Vázquez Veloso
# 2024-10-01
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


# load previous information
load('1_data/2_processed/2.1_hd_model_data_workspace.rdata')
load('3_figures/2.1_hd_all_base_model_fit/all_models.rdata')
rm(df2, df3, df4, df_names, df_list)



# Select the top models by its performance: same model for all species ====

# filter dataset
df_stats <- all_stats[all_stats$df == 'df', ]

# filter models with no NAs in AIC and count number of times each model appears as the best for a species
df_stats_best <- df_stats %>%
  group_by(species) %>%
  filter(aic == min(aic, na.rm = TRUE)) %>%
  ungroup()
df_stats_counts <- df_stats_best %>%
  count(model) %>%
  arrange(desc(n))
df_stats_counts

# filter top 4 models
# top_models <- df_stats_counts$model[1:4]
# all_stats <- all_stats[all_stats$model %in% top_models, ]
# all_coefs <- all_coefs[all_coefs$model %in% top_models, ]

# check if all the species fit on the top models
models_list <- c()
for(model in df_stats_counts$model){
  tmp_stats <- all_stats[all_stats$model == model, ]
  tmp_stats <- tmp_stats[tmp_stats$df == 'df', ]
  # condition to check if all species fit on the model
  if(nrow(tmp_stats) >= length(sp_groups)){
    models_list <- c(models_list, model)  
  }
  # select top 5 models
  if(length(models_list) == 5){
    break
  }
}

# filter the models results (stats and coefs) for those top models that fit all species
all_stats <- all_stats[all_stats$model %in% models_list, ]
all_coefs <- all_coefs[all_coefs$model %in% models_list, ]

# clean results for split analysis by df
all_stats <- all_stats[all_stats$df == 'df', ]
all_coefs <- all_coefs[all_coefs$df == 'df', ]

# save best models and required data for the next step
sp_list <- sp_groups
save(df, models_list, sp_list, all_stats, all_coefs, file = '1_data/2_processed/2.2_hd_top_model_selection.rdata')



# Export a table with the top models and their metrics averaged across species ====

# estimate the average AIC for each model 
top_models <- all_stats %>%
  group_by(model) %>%              # group by model
  summarize(avg_aic = mean(aic),
            avg_bic = mean(bic),
            avg_r2 = mean(r_squared),
            avg_rmse = mean(rmse),
            avg_mae = mean(mae)
            ) %>%  # calculate the average metrics for each model
  arrange(avg_aic)                 # arrange by the lowest average AIC

# export the table to latex
source('2_scripts/2.0_support_functions.r')
export_to_latex(top_models, file_name = '3_figures/2.2_hd_top_model_selection/2.2_top_models.tex')
export_to_word(top_models, file_name = '3_figures/2.2_hd_top_model_selection/2.2_top_models.docx')

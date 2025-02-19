#!/usr/bin/Rscript

# Code to get hd models ----
# Step 5: select the better model based on metrics
#
# Aitor Vázquez Veloso
# 2024-10-24
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# Some decisions that has been taken can be filtered by "# Note:"



# working directory
setwd('')

# install and load necessary libraries
library(tidyverse)      
library(nlme)



# Group all the metrics in a single df (run in different scripts and stopped and some point) ====

# load data
# load('1_data/2_processed/2.3_fit_nlme_models/elmamoun_2013_M13__re_3__fe_63.rdata')
# all_metrics <- metrics
# load('1_data/2_processed/2.3_fit_nlme_models/elmamoun_2013_M13__re_3__fe_30.rdata')
# all_metrics <- rbind(all_metrics, metrics)
# load('1_data/2_processed/2.3_fit_nlme_models/elmamoun_2013_M18__re_3__fe_63.rdata')
# all_metrics <- rbind(all_metrics, metrics)
# load('1_data/2_processed/2.3_fit_nlme_models/elmamoun_2013_M18__re_3__fe_22.rdata')
# all_metrics <- rbind(all_metrics, metrics)
# load('1_data/2_processed/2.3_fit_nlme_models/linear_model_II__re_3__fe_63.rdata')
# all_metrics <- rbind(all_metrics, metrics)
# load('1_data/2_processed/2.3_fit_nlme_models/linear_model_II__re_3__fe_56.rdata')
# metrics <- dplyr::select(metrics, -nlme_model)
# all_metrics <- rbind(all_metrics, metrics)
# load('1_data/2_processed/2.3_fit_nlme_models/elmamoun_2013_M12__re_3__fe_63.rdata')
# all_metrics <- rbind(all_metrics, metrics)
# load('1_data/2_processed/2.3_fit_nlme_models/hyperbolic_model_I__re_3__fe_63.rdata')
# all_metrics <- rbind(all_metrics, metrics)
# load('1_data/2_processed/2.3_fit_nlme_models/hyperbolic_model_I__re_3__fe_52.rdata')
# metrics <- dplyr::select(metrics, -nlme_model)
# all_metrics <- rbind(all_metrics, metrics)
# rm(metrics, model_results, df_adapted, sp_list)
# save(all_metrics, file = '1_data/2_processed/2.3_all_models_metrics.rdata')



# Select the better model ====

# load data and functions
load('1_data/2_processed/2.3_all_models_metrics.rdata')
source('2_scripts/2.0_support_functions.r')

# select the best model based on AIC
all_metrics <- all_metrics[order(all_metrics$aic, decreasing = FALSE), ]
all_metrics[1, ]
best_model <- all_metrics[1, ]

# select the best combination for each base model and export it as a table
top_base_model <- all_metrics %>% 
  group_by(model_name) %>% 
  slice_min(aic) 
top_base_model <- top_base_model[order(top_base_model$aic, decreasing = FALSE), ]
export_to_latex(top_base_model, file_name = '3_figures/2.4-5_best_model/2.4_top_base_model_table.tex', decimals = 2, non_numeric_rows = 2)
export_to_word(top_base_model, '3_figures/2.4-5_best_model/2.4_top_base_model_table.docx')

# save(df_adapted, fit, model_results, model_name, model_formula, fe_combi, re_combi, optimal_params, sp_list,
# file = '1_data/2_processed/2.3_best_model.rdata')
# Note: at that point, the best model was run again to get the fit and predicted values using the previous code



# Load the best model and reorganize the data included as fixed effects ====

# load data
# load('1_data/2_processed/2.3_best_model.rdata')

# reorganize the data included as fixed effects
# Note: here I stablish the "by default" order of the variables
# when applying the model, pure, natural and Mediterránea will be the reference levels (no need to include them in the model as an additional parameter)
# df_adapted$fe1 <- factor(df_adapted$fe1, levels = c("pure", "mix"))
# df_adapted$fe2 <- factor(df_adapted$fe2, levels = c("natural", "artificial"))
# df_adapted$fe3 <- factor(df_adapted$fe3, levels = c("Mediterránea", "Atlántica", "Alpina", "Macaronésica"))
# Note: now I have to run again the model with the new order of the variables, using the same code as before

# save(df_adapted, fit, model_results, model_name, model_formula, model_function, fe_combi, re_combi, optimal_params, sp_list,
# file = '1_data/2_processed/2.3_best_model_fe_good_order.rdata')
# Note: at that point, the best model was run again to get the fit and predicted values using the fixed effect desired order



# Load the best model with the new order of the variables ====

# load data
load('1_data/2_processed/2.3_best_model_fe_good_order.rdata')
source('2_scripts/2.0_support_functions.r')

# commands to access the best model fit information
# here an explanation https://fhernanb.github.io/libro_modelos_mixtos/apli-nlme.html
fixed_table <- fixef(fit)  # fixed effects

# convert to a table preserving the rownames
fixed_table <- as.data.frame(fixed_table)
fixed_table$variable <- rownames(fixed_table)
export_to_latex(fixed_table, file_name = '3_figures/2.4-5_best_model/2.4_fixed_effects_table.tex', decimals = 4, non_numeric_rows = 0)
export_to_word(fixed_table, '3_figures/2.4-5_best_model/2.4_fixed_effects_table.docx')

random_table <- ranef(fit)  # random effects
summary(fit)  # summary of the model
coefs <- coef(fit)  # coefficients for each random and fixed effect

# work with coefficients table
coefs$sp_code <- rownames(coefs)
coefs <- as_tibble(coefs)

# merge coefficients with species names
species <- read.csv('1_data/1_raw/SFNI4_species_codes.csv')
species <- select(species, Codigo_IFN, Nombre_Especie)
species <- rename(species, sp_name = Nombre_Especie)
coefs <- merge(coefs, species, by.x = 'sp_code', by.y = 'Codigo_IFN', all.x = TRUE)
coefs <- rename(coefs, `Species code` = sp_code,  `Species name` = sp_name, 
                 a = `a.(Intercept)`, `a ~ fe1 (mix stand)` = a.fe1mix, `a ~ fe2 (artificial stand)` = a.fe2artificial, 
                `a ~ fe3 (Atlantic region)` = a.fe3Atlántica, `a ~ fe3 (Alpine region)` = a.fe3Alpina, `a ~ fe3 (Macaronesian region)` = a.fe3Macaronésica,
                 b = `b.(Intercept)`, `b ~ fe1 (mix stand)` = b.fe1mix, `b ~ fe2 (artificial stand)` = b.fe2artificial,
                `b ~ fe3 (Atlantic region)` = b.fe3Atlántica, `b ~ fe3 (Alpine region)` = b.fe3Alpina, `b ~ fe3 (Macaronesian region)` = b.fe3Macaronésica)
coefs <- select(coefs, `Species code`, `Species name`, a, `a ~ fe1 (mix stand)`, `a ~ fe2 (artificial stand)`,
                `a ~ fe3 (Atlantic region)`, `a ~ fe3 (Alpine region)`, `a ~ fe3 (Macaronesian region)`,
                b, `b ~ fe1 (mix stand)`, `b ~ fe2 (artificial stand)`, `b ~ fe3 (Atlantic region)`,
                `b ~ fe3 (Alpine region)`, `b ~ fe3 (Macaronesian region)`)

# export tables: a and b parameters
coefs_ab <- select(coefs, `Species code`, `Species name`, a, b)
rownames(coefs_ab) <- NULL
coefs_ab <- coefs_ab %>% arrange(as.numeric(`Species code`))
write.csv(coefs_ab, '3_figures/2.4-5_best_model/2.4.ab_coefs_table.csv', row.names = FALSE)
write.table(coefs_ab, '3_figures/2.4-5_best_model/2.4.ab_coefs_table.txt', row.names = FALSE, sep = '\t', dec = ',')
export_to_latex(coefs_ab, file_name = '3_figures/2.4-5_best_model/2.4.ab_coefs_table.tex', 
                decimals = 4, non_numeric_rows = 2, italic_columns = 2)
export_to_word(coefs_ab, '3_figures/2.4-5_best_model/2.4.ab_coefs_table.docx')

coefs_ab <- coefs_ab %>% select(-`Species code`) 
coefs_ab <- coefs_ab[order(coefs_ab$`Species name`),]
rownames(coefs_ab) <- NULL
coefs_ab <- coefs_ab %>% mutate_if(is.numeric, ~round(., 4))
export_to_latex(coefs_ab, file_name = '3_figures/2.4-5_best_model/2.4.ab_coefs_table_no_codes.tex', 
                decimals = 4, non_numeric_rows = 1, italic_columns = 1)
export_to_word(coefs_ab, file_name = '3_figures/2.4-5_best_model/2.4.ab_coefs_table_no_codes.docx')

# export tables: fixed effects
coefs_fe <- coefs[3,]
coefs_fe <- dplyr::select(coefs_fe, c(-`Species code`, -`Species name`))
write.csv(coefs_fe, '3_figures/2.4-5_best_model/2.4.fe_coefs_table.csv', row.names = FALSE)
write.table(coefs_fe, '3_figures/2.4-5_best_model/2.4.fe_coefs_table.txt', row.names = FALSE, sep = '\t', dec = ',')
export_to_latex(coefs_fe, file_name = '3_figures/2.4-5_best_model/2.4.fe_coefs_table.tex', decimals = 4, non_numeric_rows = 2)
export_to_word(coefs_fe, '3_figures/2.4-5_best_model/2.4.fe_coefs_table.docx')

# get and export errors for each parameter
errors <- sqrt(diag(fit[["varFix"]]))
errors <- as.data.frame(t(errors))
errors$variable <- 'errors'
errors <- dplyr::select(errors, variable, everything())
export_to_latex(errors, file_name = '3_figures/2.4-5_best_model/2.4.fe_coefs_errors_table.tex', 
                decimals = 4, non_numeric_rows = 2)
export_to_word(errors, file_name = '3_figures/2.4-5_best_model/2.4.fe_coefs_errors_table.docx')



# Table for initial data values ====

# load data
load('1_data/2_processed/2.3_best_model_fe_good_order.rdata')
source('2_scripts/2.0_support_functions.r')

# group data
summary_table <- df_adapted %>%
  group_by(climate_region, Stand_origin, Species_mixture) %>%  # Replace with your actual categorical variable names
  summarise(
    dbh_min = min(dbh, na.rm = TRUE),
    dbh_max = max(dbh, na.rm = TRUE),
    dbh_mean = mean(dbh, na.rm = TRUE),
    dbh_sd = sd(dbh, na.rm = TRUE),
    h_min = min(h, na.rm = TRUE),
    h_max = max(h, na.rm = TRUE),
    h_mean = mean(h, na.rm = TRUE),
    h_sd = sd(h, na.rm = TRUE)
  ) %>%
  ungroup()

# export table on latex
export_to_latex(summary_table, file_name = '3_figures/2.4-5_best_model/2.4.initial_data_summary_table.tex', decimals = 2, non_numeric_rows = 2)
export_to_word(summary_table, '3_figures/2.4-5_best_model/2.4.initial_data_summary_table.docx')



# Table for parameter errors ====

# extract variance and correlation components
var_comp <- VarCorr(fit)

# extract fixed effects values
fix <- fixef(fit)

# export table on latex
# export_to_latex(var_comp, file_name = '3_figures/2.4-5_best_model/2.4.var_comp_table.tex', decimals = 4, non_numeric_rows = 1)

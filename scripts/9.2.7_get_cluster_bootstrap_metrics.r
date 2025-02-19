#!/usr/bin/Rscript

# Code to get hd models ----
# Step 7: cluster bootstrap results analysis - get data
#
# Aitor Vázquez Veloso
# 2025-02-17
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# Some decisions that has been taken can be filtered by "# Note:"



# working directory
setwd('')

# install and load necessary libraries
library(tidyverse)       

# function to get the metrics
metric_analysis <- function(df){
  
  cluster <- tibble(median = numeric(), mean = numeric(), min = numeric(), max = numeric(),
                    p_975 = numeric(), p_025 = numeric())
  
  median <- quantile(df, 0.5, na.rm = TRUE)
  mean <- mean(df, na.rm = TRUE)
  min <- min(df, na.rm = TRUE)
  max <- max(df, na.rm = TRUE)
  p_975 <- quantile(df, 0.975, na.rm = TRUE)
  p_025 <- quantile(df, 0.025, na.rm = TRUE)
  
  cluster <- bind_rows(cluster, tibble(median = median, mean = mean,
                                       min = min, max = max, p_975 = p_975, p_025 = p_025))
  
  return(cluster)
}

print('Loading data...')


# Load the cluster bootstrap information information ====

# load the model results
load('1_data/2_processed/2.6/2.6_cluster_bootstrap_results_1000.rdata')
print('Data loaded!')

# list of metrics to analyze
mb_results <- select(pred_vs_obs, mb)
mb_results <- do.call(rbind, mb_results)

metrics_df <- list(mb_results, boots_results$aic, boots_results$bic, boots_results$logLik, 
                   boots_results$rmse, boots_results$mae,
                   
                   fe_params$`a.(Intercept)`, fe_params$`a.fe1mix`, fe_params$`a.fe2artificial`, 
                   fe_params$`a.fe3Atlántica`, fe_params$`a.fe3Alpina`, fe_params$`a.fe3Macaronésica`,
                   fe_params$`b.(Intercept)`, fe_params$`b.fe1mix`, fe_params$`b.fe2artificial`,
                   fe_params$`b.fe3Atlántica`, fe_params$`b.fe3Alpina`, fe_params$`b.fe3Macaronésica`,
                   
                   fe_stats$`a.variance`, fe_stats$`a.stddev`, fe_stats$`a.corr`, 
                   fe_stats$`b.variance`, fe_stats$`b.stddev`,
                   fe_stats$`residual.variance`, fe_stats$`residual.stddev`)

# list to store the metrics
cluster_metrics <- list()

for (i in 1:length(metrics_df)){
  cluster_metrics[[i]] <- metric_analysis(metrics_df[i][[1]])  
  print(paste('Metric', i, 'done!', sep = ' '))
}

names(cluster_metrics) <- c('mb', 'aic', 'bic', 'logLik', 'rmse', 'mae', 
                            'a.(Intercept)', 'a.fe1mix', 'a.fe2artificial', 'a.fe3Atlántica', 
                            'a.fe3Alpina', 'a.fe3Macaronésica', 'b.(Intercept)', 'b.fe1mix', 
                            'b.fe2artificial', 'b.fe3Atlántica', 'b.fe3Alpina', 'b.fe3Macaronésica',
                            'a.variance', 'a.stddev', 'a.corr', 'b.variance', 'b.stddev',
                            'residual.variance', 'residual.stddev')

# save the results
print('Saving data...')
save(cluster_metrics, file = '1_data/2_processed/2.7_cluster_bootstrap_results.rdata')
print('Data saved!')

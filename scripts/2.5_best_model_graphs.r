#!/usr/bin/Rscript

# Code to get hd models ----
# Step 5: graph results of the best model
#
# Aitor Vázquez Veloso
# 2025-06-13
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# Some decisions that has been taken can be filtered by "# Note:"



# working directory
setwd('')

# install and load necessary libraries
library(tidyverse)       



# Load the best model information and additional resources ====

# load the model results
load('1_data/2_processed/2.3_best_model_fe_good_order.rdata')

# load the species codes
sp_codes <- read.csv('1_data/1_raw/SFNI4_species_codes.csv')



# Graph the results for the best model ====

# graph by species
for(sp in sp_list){

  # filter the data for the species
  df_sp <- df_adapted[df_adapted$species == sp, ]

  # maximum axis values
  max_value_h <- max(c(df_sp$h, df_sp$predicted_h), na.rm = TRUE) # calculate the max value from both variables
  max_value_dbh <- max(df_sp$dbh, na.rm = TRUE) # calculate the max value from dbh
  
  # graph observed vs predicted values
  ggplot(df_sp, aes(x = h, y = predicted_h)) +
    geom_point(color = "black", size = 2, alpha = 0.6) +      # Plot observed values
    geom_abline(intercept = 0, slope = 1, color = "red") +    # Add a 1:1 line
    labs(title = "Observed vs Predicted tree height values",
         subtitle = paste('Species nº ', sp, ' - ', sp_codes$Nombre_Especie[as.numeric(sp_codes$Codigo_IFN) == sp]
                          , sep = ''),
         x = "Observed height (m)",
         y = "Predicted height (m)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13)) +
    coord_equal() +           # ensure equal scaling on both axes
    xlim(0, max_value_h) +      # set limits for the x-axis
    ylim(0, max_value_h)        # set limits for the y-axis
  ggsave(paste("3_figures/2.4-5_best_model/sp_predict_vs_observed/pred_vs_obs_sp_", sp, ".png", sep = '')
  , dpi = 300, width = 7, height = 5)

  # same graph without titles and axis to be included in the paper
  if(sp %in% c(21, 42, 61, 236, 258)){
  
    ggplot(df_sp, aes(x = h, y = predicted_h)) +
      geom_point(color = "black", size = 2, alpha = 0.6) +      # Plot observed values
      geom_abline(intercept = 0, slope = 1, color = "red") +    # Add a 1:1 line
      # labs(title = "Observed vs Predicted tree height values",
      #      subtitle = paste('Species nº ', sp, ' - ', sp_codes$Nombre_Especie[as.numeric(sp_codes$Codigo_IFN) == sp]
      #                       , sep = ''),
      #      x = "Observed height (m)",
      #      y = "Predicted height (m)") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15)) +
      coord_equal() +           # ensure equal scaling on both axes
      xlim(0, max_value_h) +      # set limits for the x-axis
      ylim(0, max_value_h)        # set limits for the y-axis
    ggsave(paste("3_figures/2.4-5_best_model/sp_predict_vs_observed-paper/pred_vs_obs_sp_", sp, ".png", sep = '')
           , dpi = 300, width = 7, height = 5)
  }
    
  # graph observed vs predicted values + dbh
  ggplot(df_sp, aes(x = dbh, y = h)) +
    geom_point(color = "black", size = 2, alpha = 0.6) +      # Plot observed values
    geom_point(aes(y = predicted_h), color = "red", size = 1) +  # Plot predicted values
    labs(title = "Observed (black) vs Predicted tree height (red)",
         subtitle = paste('Species nº ', sp, ' - ', sp_codes$Nombre_Especie[as.numeric(sp_codes$Codigo_IFN) == sp]
                          , sep = ''),
         x = "dbh (cm)",
         y = "height (m)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    xlim(0, max_value_dbh) +      # Set limits for the x-axis
    ylim(0, max_value_h)        # Set limits for the y-axis
  ggsave(paste("3_figures/2.4-5_best_model/sp_fit/best_model_sp_", sp, ".png", sep = '')
         , dpi = 300, width = 7, height = 5)

  # Residuals vs Fitted plot using ggplot2
  ggplot(df_sp, aes(x = dbh, y = h - predicted_h)) +
    geom_point() +
    geom_hline(yintercept = 0, col = "red") +
    labs(title = "Residuals vs Fitted Values",
         subtitle = paste('Species nº ', sp, ' - ', sp_codes$Nombre_Especie[as.numeric(sp_codes$Codigo_IFN) == sp]
                          , sep = ''),
         x = "dbh (cm)",
         y = "residuals (m)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  ggsave(paste("3_figures/2.4-5_best_model/sp_residuals/best_model_-residuals_vs_fitted-sp_", sp, ".png", sep = ''),
         dpi = 300, width = 7, height = 5)

  # Histogram of residuals
  ggplot(df_sp, aes(x = h - predicted_h)) +
    geom_histogram(bins = 50, fill = "black", alpha = 0.7, color = "black") +
    labs(title = "Histogram of Residuals",
         x = "residuals (m)", y = "count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  ggsave(paste("3_figures/2.4-5_best_model/sp_histogram/best_model_-histogram_residuals-sp_", sp, ".png", sep = ''),
         dpi = 300, width = 7, height = 5)
}


# extract fitted values and residuals
data <- data.frame(
  Fitted = fitted(fit),
  Residuals = residuals(fit),
  Std_Residuals = residuals(fit, type = "normalized")
)

# extract maximum values for the plots
max_value_h <- max(c(df_adapted$h, df_adapted$predicted_h), na.rm = TRUE) # calculate the max value from both variables
max_value_dbh <- max(df_adapted$dbh, na.rm = TRUE) # calculate the max value from dbh

# graph observed vs predicted values
ggplot(df_adapted, aes(x = h, y = predicted_h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +      # Plot observed values
  geom_abline(intercept = 0, slope = 1, color = "red") +    # Add a 1:1 line
  labs(title = "Observed vs Predicted tree height values",
       subtitle = 'All species',
       x = "Observed height (m)",
       y = "Predicted height (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  coord_equal() +           # ensure equal scaling on both axes
  xlim(0, max_value_h) +      # set limits for the x-axis
  ylim(0, max_value_h)        # set limits for the y-axis
ggsave(paste("3_figures/2.4-5_best_model/pred_vs_obs.png", sep = ''), dpi = 300, width = 7, height = 5)

# graph observed vs predicted values
ggplot(df_adapted, aes(x = h, y = predicted_h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +      # Plot observed values
  geom_abline(intercept = 0, slope = 1, color = "red") +    # Add a 1:1 line
  # labs(title = "Observed vs Predicted tree height values",
  #      subtitle = 'All species',
  #      x = "Observed height (m)",
  #      y = "Predicted height (m)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_equal() +           # ensure equal scaling on both axes
  xlim(0, max_value_h) +      # set limits for the x-axis
  ylim(0, max_value_h)        # set limits for the y-axis
ggsave(paste("3_figures/2.4-5_best_model/sp_predict_vs_observed-paper/pred_vs_obs.png", sep = ''), dpi = 300, width = 7, height = 5)

# graph observed vs predicted values + dbh
ggplot(df_adapted, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +      # Plot observed values
  geom_point(aes(y = predicted_h), color = "red", size = 1) +  # Plot predicted values
  labs(title = "Observed (black) vs Predicted tree height (red)",
       subtitle = "All species",
       x = "dbh (cm)",
       y = "height (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlim(0, max_value_dbh) +      # Set limits for the x-axis
  ylim(0, max_value_h)        # Set limits for the y-axis
ggsave(paste("3_figures/2.4-5_best_model/best_model_fit.png", sep = ''), dpi = 300, width = 7, height = 5)

# Residuals vs Fitted plot using ggplot2
ggplot(data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Residuals vs Fitted Values",
  x = "dbh (cm)",
  y = "residuals (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("3_figures/2.4-5_best_model/best_model-residuals_vs_fitted.png", dpi = 300, width = 7, height = 5)

# QQ plot for residuals
ggplot(data, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "QQ Plot of Residuals") +
  theme_minimal()
ggsave("3_figures/2.4-5_best_model/best_model-qq_plot_residuals.png", dpi = 300, width = 7, height = 5)

# Histogram of residuals
ggplot(data, aes(x = Residuals)) +
  geom_histogram(bins = 50, fill = "black", alpha = 0.7, color = "black") +
  labs(title = "Histogram of Residuals", x = "residuals (m)", y = "count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("3_figures/2.4-5_best_model/best_model-histogram_residuals.png", dpi = 300, width = 7, height = 5)

# Standardized residuals vs fitted values
ggplot(data, aes(x = Fitted, y = Std_Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Standardized Residuals vs Fitted Values", x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()
ggsave("3_figures/2.4-5_best_model/best_model-std_residuals_vs_fitted.png", dpi = 300, width = 7, height = 5)



# Graphs by fixed effect variables ====

# 1. Climate region

# species codes
sp = 26  # (Ppinaster)

# filter the data for the species
df_sp <- df_adapted[df_adapted$species == sp, ]

# create a sequence of dbh values covering the range of interest
dbh_seq <- seq(min(df_sp$dbh), max(df_sp$dbh), length.out = 100)

# parameter sets
param_sets <- data.frame(
  # model_id = c("Alpina", "Atlántica", "Macaronesia", "Mediterránea"),  # spanish
  model_id = c("Alpine", "Atlantic", "Macaronesian", "Mediterranean"),
  a = c(0.3851, 0.619, 0.6013, 0.4175),  
  b = c(2.7087,  2.6048, 2.4744, 2.6739)   
)

# generate predicted values for each parameter set
predicted_data <- param_sets %>%
  rowwise() %>%
  mutate(data = list(data.frame(
    dbh = dbh_seq,
    pred_h = 1.3 + a * (log(1 + dbh_seq))^b,  
    model_id_region = model_id
  ))) %>%
  unnest(data)

# plot the results
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('goldenrod', 'darkblue', 'darkred', 'darkgreen')) +  # use custom colors
  labs(title = "Observed data and model predictions for each climate region parameterization",
       subtitle = "Pinus pinaster", 
       x = "dbh (cm)",
       y = "height (m)",
       color = "Biogeographical region") +  # legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic") 
  )
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables/best_model-climate_region_ppinaster.png", dpi = 300, 
       width = 7, height = 5)

# plot results without titles
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('goldenrod', 'darkblue', 'darkred', 'darkgreen')) +  # use custom colors
  labs(#title = "Observed data and model predictions for each climate region parameterization",
       #subtitle = "Pinus pinaster", 
       x = "dbh (cm)",
       y = "height (m)",
       # y = "altura (m)",  # spanish
       color = "Biogeographical region") +
       # color = "Región biogeográfica") +  # spanish
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
       axis.text = element_text(size = 13),
       axis.title = element_text(size = 15),
       legend.title = element_text(size = 15),
       legend.text = element_text(size = 13))
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables-paper/best_model-climate_region_ppinaster.png", dpi = 300, 
       width = 7, height = 5)

# plot results without titles (legend bottom)
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('goldenrod', 'darkblue', 'darkred', 'darkgreen')) +  # use custom colors
  labs(#title = "Observed data and model predictions for each climate region parameterization",
    #subtitle = "Pinus pinaster", 
    x = "dbh (cm)",
    y = "height (m)",
    # y = "altura (m)",  # spanish
    color = "Biogeographical region") +
  # color = "Región biogeográfica") +  # spanish
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        legend.position = "bottom",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables-paper/best_model-climate_region_ppinaster_2.png", dpi = 300, 
       width = 7, height = 5)


# 2. Stand origin

# species codes
sp = 51  # (Palba)

# filter the data for the species
df_sp <- df_adapted[df_adapted$species == sp, ]

# create a sequence of dbh values covering the range of interest
dbh_seq <- seq(min(df_sp$dbh), max(df_sp$dbh), length.out = 100)

# parameter sets
param_sets <- data.frame(
  model_id = c("Natural", "Plantation"),
  # model_id = c("Natural", "Artificial"),  # spanish
  a = c(1.2932, 1.2812),  
  b = c(2.0692, 2.1074)   
)

# generate predicted values for each parameter set
predicted_data <- param_sets %>%
  rowwise() %>%
  mutate(data = list(data.frame(
    dbh = dbh_seq,
    pred_h = 1.3 + a * (log(1 + dbh_seq))^b,  
    model_id_region = model_id
  ))) %>%
  unnest(data)

# plot the results
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('darkgreen', 'gold4')) +  # use custom colors
  labs(title = "Observed data and model predictions for each stand origin",
       subtitle = "Populus alba", 
       x = "dbh (cm)",
       y = "height (m)",
       color = "Stand origin") +  # legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables/best_model-stand_origin_palba.png", dpi = 300, width = 7, height = 5)

# plot the results without titles
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('darkgreen', 'gold4')) +  # use custom colors
  labs(#title = "Observed data and model predictions for each stand origin",
       #subtitle = "Populus alba", 
       x = "dbh (cm)",
       y = "height (m)",
       # y = "altura (m)",  # spanish
       color = "Stand origin") +
       # color = "Origen rodal") +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic") ,
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables-paper/best_model-stand_origin_palba.png", dpi = 300, width = 7, height = 5)

# plot the results without titles (legend bottom)
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('darkgreen', 'gold4')) +  # use custom colors
  labs(#title = "Observed data and model predictions for each stand origin",
    #subtitle = "Populus alba", 
    x = "dbh (cm)",
    y = "height (m)",
    # y = "altura (m)",  # spanish
    color = "Stand origin") +
  # color = "Origen rodal") +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        legend.position = "bottom",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables-paper/best_model-stand_origin_palba_2.png", dpi = 300, width = 7, height = 5)


# 3. Stand mixture

# species codes
sp = 41  # (Qrobur)

# filter the data for the species
df_sp <- df_adapted[df_adapted$species == sp, ]

# create a sequence of dbh values covering the range of interest
dbh_seq <- seq(min(df_sp$dbh), max(df_sp$dbh), length.out = 100)

# parameter sets
param_sets <- data.frame(
  model_id = c("Pure", "Mixture"),
  # model_id = c("Pura", "Mixta"),  # spanish
  a = c(1.6668, 1.6848),  
  b = c(1.7846, 1.7456)   
)

# generate predicted values for each parameter set
predicted_data <- param_sets %>%
  rowwise() %>%
  mutate(data = list(data.frame(
    dbh = dbh_seq,
    pred_h = 1.3 + a * (log(1 + dbh_seq))^b,  
    model_id_region = model_id
  ))) %>%
  unnest(data)

# plot the results
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('sienna2', 'darkgreen')) +  # use custom colors
  labs(title = "Observed data and model predictions for each stand mixture",
       subtitle = "Quercus robur", 
       x = "dbh (cm)",
       y = "height (m)",
       color = "Stand mixure") +  # legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic") 
  )
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables/best_model-stand_mixture_qrobur.png", dpi = 300, 
       width = 7, height = 5)

# plot the results without titles
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('sienna2', 'darkgreen')) +  # use custom colors
  labs(#title = "Observed data and model predictions for each stand mixture",
       #subtitle = "Quercus robur", 
       x = "dbh (cm)",
       y = "height (m)",
       # y = "altura (m)",  # spanish
       color = "Stand mixture") +
       # color = "Composición específica") +  # spanish
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables-paper/best_model-stand_mixture_qrobur.png", dpi = 300, 
       width = 7, height = 5)

# plot the results without titles (legend bottom)
ggplot(df_sp, aes(x = dbh, y = h)) +
  geom_point(color = "black", size = 2, alpha = 0.6) +  # observed data
  geom_line(data = predicted_data, aes(x = dbh, y = pred_h, color = model_id), size = 1) +  # predicted lines
  scale_color_manual(values = c('sienna2', 'darkgreen')) +  # use custom colors
  labs(#title = "Observed data and model predictions for each stand mixture",
    #subtitle = "Quercus robur", 
    x = "dbh (cm)",
    y = "height (m)",
    # y = "altura (m)",  # spanish
    color = "Stand mixture") +
  # color = "Composición específica") +  # spanish
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        legend.position = "bottom",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
ggsave("3_figures/2.4-5_best_model/graph_by_fe_variables-paper/best_model-stand_mixture_qrobur_2.png", dpi = 300, 
       width = 7, height = 5)



# # graph observed vs predicted values + dbh
# ggplot(df_sp, aes(x = dbh, y = h)) +
#   geom_point(color = "black", size = 2, alpha = 0.6) +      # Plot observed values
#   geom_point(aes(y = predicted_h), color = "red", size = 1) +  # Plot predicted values
#   labs(title = "Observed (black) vs Predicted tree height (red)",
#        subtitle = paste('Species nº ', sp, ' - ', sp_codes$Nombre_Especie[as.numeric(sp_codes$Codigo_IFN) == sp]
#                         , sep = ''),
#        x = "dbh (cm)",
#        y = "height (m)") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))
#   # xlim(0, max_value_dbh) +      # Set limits for the x-axis
#   # ylim(0, max_value_h)        # Set limits for the y-axis
# ggsave(paste("3_figures/2.4-5_best_model/sp_fit/best_model_sp_", sp, ".png", sep = '')
#        , dpi = 300, width = 7, height = 5)  # Save the plot 

# climate regions: 26 pinaster
# origin: 273 betula alba
# mixture: 41 quercus robur



# Filter and export data to graph the plots distribution ====

# load best model data
load('1_data/2_processed/2.3_best_model_fe_good_order.rdata')

best_model_plot_ids <- df_adapted$IFN_PLOT_ID
best_model_plot_ids <- unique(best_model_plot_ids)

# load original data (all variables)
load('1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata')
plots <- plots[plots$IFN_PLOT_ID %in% best_model_plot_ids,]

# export data
write.csv(plots, '1_data/2_processed/2.5_best_model_data-plots_to_graph.csv', row.names = FALSE)

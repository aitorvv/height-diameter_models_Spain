#!/usr/bin/Rscript

# Functions for plot data variables specifically developed to SFNI data ----
#
# Aitor Vázquez Veloso
# 2024-09-12
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# calculate the basic plot data (N, G, DBH, H) from the trees data
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - df_trees: data frame with the trees data
# - plot_id_column_1: name of the column with the first plot ID
# - plot_id_column_2: name of the column with the second plot ID; default is NA
# - dbh_column: name of the column with the dbh data in the df_trees data frame; default is 'dbh'; units must be cm
# - h_column: name of the column with the h data in the df_trees data frame; default is 'h'; units must be m
# - expan_column: name of the column with the expan data in the df_trees data frame; default is 'expan'
# - g_column: name of the column with the g data in the df_trees data frame; default is 'g'; units must be cm²
# return:
# df_plots: data frame with the plot data calculated from trees in the plot

get_plot_data <- function(df_trees, plot_id_column_1, plot_id_column_2 = NA, 
                          dbh_column = 'dbh', h_column = 'h', expan_column = 'expan', g_column = 'g'){ 
  # , dead_column = NA){
  
  # check variable names in df_trees
  if(dbh_column != 'dbh'){df_trees$dbh <- df_trees[[dbh_column]]}
  if(expan_column != 'expan'){df_trees$expan <- df_trees[[expan_column]]}
  if(h_column != 'h'){df_trees$h <- df_trees[[h_column]]}
  if(g_column != 'g'){df_trees$g <- df_trees[[g_column]]}
  # if(!is.na(dead_column)){df_trees$dead <- df_trees[[dead_column]]}
  
  # get plot data from trees
  df_plots <- df_trees %>%
    
    # group by plot IDs
    group_by(df_trees[[plot_id_column_1]], df_trees[[plot_id_column_2]]) %>%
    summarise(
      
      # plot density (N trees/ha)
      N = ifelse(all(is.na(expan)), NA, sum(expan, na.rm = TRUE)),
      
      # plot diametric classes distribution (N trees/ha)
      N_0_75 = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh <= 7.5, expan, 0), na.rm = TRUE)),
      N_75_125 = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh > 7.5 & dbh <= 12.5, expan, 0), na.rm = TRUE)),
      N_125_175 = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh > 12.5 & dbh <= 17.5, expan, 0), na.rm = TRUE)),
      N_175_225 = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh > 17.5 & dbh <= 22.5, expan, 0), na.rm = TRUE)),
      N_225_275 = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh > 22.5 & dbh <= 27.5, expan, 0), na.rm = TRUE)),
      N_275_325 = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh > 27.5 & dbh <= 32.5, expan, 0), na.rm = TRUE)),
      N_325_375 = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh > 32.5 & dbh <= 37.5, expan, 0), na.rm = TRUE)),
      N_375_425 = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh > 37.5 & dbh <= 42.5, expan, 0), na.rm = TRUE)),
      N_425_ = ifelse(all(is.na(expan)), NA, sum(ifelse(dbh > 42.5, expan, 0), na.rm = TRUE)),
      
      # minimum and maximum dbh (cm) - handle all NA values
      dbh_min = ifelse(all(is.na(dbh)), NA, min(dbh, na.rm = TRUE)),
      dbh_max = ifelse(all(is.na(dbh)), NA, max(dbh, na.rm = TRUE)),                     
      
      # plot basal area (m²/ha); minimum and maximum basal area (cm²) - handle all NA values
      G = ifelse(all(is.na(expan)) | all(is.na(g)), NA, sum(g * expan / 10000, na.rm = TRUE)),
      g_min = ifelse(all(is.na(g)), NA, min(g, na.rm = TRUE)),    
      g_max = ifelse(all(is.na(g)), NA, max(g, na.rm = TRUE)),
      
      # minimum and maximum height (m) - handle all NA values
      h_min = ifelse(all(is.na(h)), NA, min(h, na.rm = TRUE)),
      h_max = ifelse(all(is.na(h)), NA, max(h, na.rm = TRUE)),
      
      # helpful variables to use in the next step
      SUM_DBH = sum(dbh * expan, na.rm = TRUE),
      SUM_G = sum(g * expan, na.rm = TRUE),
      SUM_H = sum(h * expan, na.rm = TRUE)
    )
  
  # rename columns
  if(is.na(plot_id_column_2)){
    
    # rename columns
    df_plots <- dplyr::rename(df_plots, c(PLOT_ID = `df_trees[[plot_id_column_1]]`))
    
  } else {
    
    # rename columns
    df_plots <- dplyr::rename(df_plots, c(PLOT_ID = `df_trees[[plot_id_column_1]]`,
                                          PLOT_ID_2 = `df_trees[[plot_id_column_2]]`))
  }
  
  # mean dbh (cm)
  df_plots$dbh_mean <- ifelse(df_plots$N > 0 & !is.na(df_plots$N), df_plots$SUM_DBH / df_plots$N, NA)
  
  # quadratic mean dbh (cm)
  df_plots$dg <- ifelse(df_plots$N > 0 & !is.na(df_plots$N), 200 * (df_plots$G / df_plots$N / pi)^0.5, NA)
  
  # mean basal area (cm²)
  df_plots$g_mean <- ifelse(df_plots$N > 0 & !is.na(df_plots$N), df_plots$SUM_G / df_plots$N, NA)
  
  # mean height (m)
  df_plots$h_mean <- ifelse(df_plots$N > 0 & !is.na(df_plots$N), df_plots$SUM_H / df_plots$N, NA)
  
  # clean temporal variables
  df_plots <- df_plots %>%
    dplyr::select(-c(SUM_DBH, SUM_G, SUM_H))
  
  # reorder variables
  if(is.na(plot_id_column_2)){
    df_plots <- df_plots %>%
      dplyr::select(PLOT_ID, N, N_0_75, N_75_125, N_125_175, N_175_225, N_225_275, N_275_325, N_325_375, N_375_425, N_425_, dbh_min, dbh_max, dbh_mean, dg, G, g_min, g_max, g_mean, h_min, h_max, h_mean)
  } else {
    df_plots <- df_plots %>%
      dplyr::select(PLOT_ID, PLOT_ID_2, N, N_0_75, N_75_125, N_125_175, N_175_225, N_225_275, N_275_325, N_325_375, N_375_425, N_425_, dbh_min, dbh_max, dbh_mean, dg, G, g_min, g_max, g_mean, h_min, h_max, h_mean)
  }
  
  # return
  return(df_plots)
}



# calculate plot N, G, mean dbh and dg from the trees data
# written by: Aitor Vázquez Veloso
# date: 2024-09-12
# parameters:
# - df_trees: data frame with the trees data
# - plot_id_column_1: name of the column with the first plot ID
# - plot_id_column_2: name of the column with the second plot ID; default is NA
# - dbh_column: name of the column with the dbh data in the df_trees data frame; default is 'dbh'; units must be cm
# - expan_column: name of the column with the expan data in the df_trees data frame; default is 'expan'
# - g_column: name of the column with the g data in the df_trees data frame; default is 'g'; units must be cm²
# return:
# df_plots: data frame with the plot data calculated from trees in the plot

get_plot_data_basic <- function(df_trees, plot_id_column_1, plot_id_column_2 = NA, 
                          dbh_column = 'dbh', expan_column = 'expan', g_column = 'g'){ 
  # , dead_column = NA){
  
  # check variable names in df_trees
  if(dbh_column != 'dbh'){df_trees$dbh <- df_trees[[dbh_column]]}
  if(expan_column != 'expan'){df_trees$expan <- df_trees[[expan_column]]}
  if(g_column != 'g'){df_trees$g <- df_trees[[g_column]]}
  # if(!is.na(dead_column)){df_trees$dead <- df_trees[[dead_column]]}
  
  # get plot data from trees
  df_plots <- df_trees %>%
    
    # group by plot IDs
    group_by(df_trees[[plot_id_column_1]], df_trees[[plot_id_column_2]]) %>%
    summarise(
      
      # plot density (N trees/ha)
      N = ifelse(all(is.na(expan)), NA, sum(expan, na.rm = TRUE)),
      
      # plot basal area (m²/ha); minimum and maximum basal area (cm²)
      G = ifelse(all(is.na(expan)) | all(is.na(g)), NA, sum(g * expan / 10000, na.rm = TRUE)),
      
      # helpful variables to use in the next step
      SUM_DBH = sum(dbh * expan, na.rm = TRUE)
    )
  
  # rename columns
  if(is.na(plot_id_column_2)){
    
    # rename columns
    df_plots <- dplyr::rename(df_plots, c(PLOT_ID = `df_trees[[plot_id_column_1]]`))
    
  } else {
    
    # rename columns
    df_plots <- dplyr::rename(df_plots, c(PLOT_ID = `df_trees[[plot_id_column_1]]`,
                                          PLOT_ID_2 = `df_trees[[plot_id_column_2]]`))
  }
  
  # mean dbh (cm)
  df_plots$dbh_mean <- ifelse(df_plots$N > 0 & !is.na(df_plots$N), df_plots$SUM_DBH / df_plots$N, NA)
  
  # quadratic mean dbh (cm)
  df_plots$dg <- ifelse(df_plots$N > 0 & !is.na(df_plots$N), 200 * (df_plots$G / df_plots$N / pi)^0.5, NA)
  
  # clean temporal variables
  df_plots <- df_plots %>%
    dplyr::select(-c(SUM_DBH))
  
  # reorder variables
  if(is.na(plot_id_column_2)){
    df_plots <- df_plots %>%
      dplyr::select(PLOT_ID, N, dbh_mean, dg, G)
  } else {
    df_plots <- df_plots %>%
      dplyr::select(PLOT_ID, PLOT_ID_2, N, dbh_mean, dg, G)
  }
  
  # return
  return(df_plots)
}


# calculate the plot dominant height (m) from the height of the tallest trees in the plot
# written by: Cristóbal Ordóñez Alonso
# modified by Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_trees: data frame with the tree records
# - dbh_column: name of the column with the dbh data; if NA, the function will look for a column named 'dbh'; units must be cm
# - h_column: name of the column with the height data; if NA, the function will look for a column named 'h'; units must be m
# - expan_column: name of the column with the expansion factor data; if NA, the function will look for a column named 'expan'
# - plot_id_column: name of the column with the plot ID data; if NA, the function will look for a column named 'PLOT_ID'
# return:
# Ho: data frame with the plot id and the plot dominant height (m)

# get_Ho <- function(df_trees, dbh_column = 'dbh', h_column = 'h', expan_column = 'expan', plot_id_column = "PLOT_ID"){
#   
#   # if plot_id_column is in the data frame, calculate the dominant height for each plot
#   if(plot_id_column %in% names(df_trees)) {
#     
#     # unique plot IDs for Ho calculation
#     PLOT_ID = unique(df_trees[[plot_id_column]])
#     Ho = rep(NA, length(PLOT_ID))
#     names(Ho) = PLOT_ID
#     
#     # for each plot
#     for(i in 1:length(PLOT_ID)) {
#       
#       # get dominant height (m)
#       Ho[i] = Ho_support(h = df_trees[[h_column]][df_trees[[plot_id_column]] == PLOT_ID[i]],
#                          dbh = df_trees[[dbh_column]][df_trees[[plot_id_column]]  == PLOT_ID[i]],
#                          expan = df_trees[[expan_column]][df_trees[[plot_id_column]]  == PLOT_ID[i]])
#     }
#     
#     # merge PLot_ID and Ho
#     Ho_final <- data.frame(PLOT_ID, Ho)
#     
#     # return
#     return(Ho_final)
#     
#   } else {
#     print('No plot ID column available on df_trees data frame provided.')
#   }
#   
#   # return is plot_id_column is not in the data frame
#   return(Ho_support(df_trees[[h_column]], df_trees[[dbh_column]], df_trees[[expan_column]]))
# }



# support function to calculate the plot dominant height (m) 
# written by: Cristóbal Ordóñez Alonso
# modified by Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - h: height of the trees in the plot
# - dbh: dbh of the trees in the plot
# - expan: expansion factor of the trees in the plot
# return:
# dominant height (m) of the plot
# Ho_support <- function(h, dbh, expan) {
#   
#   # order trees by dbh
#   o <- order(dbh, decreasing = TRUE)
#   h = h[o]
#   expan = expan[o]
#   
#   # sum of the expan in the plot
#   expan_sum = 0 
#   for(i in 1:length(h)) {
#     
#     # accumulate expan
#     expan_sum = expan_sum + expan[i]
#     
#     # if expan_sum > 100, return dominant height
#     if(expan_sum > 100) return(sum(h[1:i] * expan[1:i], na.rm = TRUE) / sum(h[1:i] * expan[1:i] / h[1:i], na.rm = TRUE))
#   }
#   
#   # return dominant height
#   return(sum(h * expan) / sum(expan))
# }



# calculate the plot dominant diameter (cm) from the diameter of the bigger trees in the plot
# written by: Cristóbal Ordóñez Alonso
# modified by Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_trees: data frame with the tree records
# - dbh_column: name of the column with the dbh data; if NA, the function will look for a column named 'dbh'; units must be cm
# - expan_column: name of the column with the expansion factor data; if NA, the function will look for a column named 'expan'
# - plot_id_column: name of the column with the plot ID data; if NA, the function will look for a column named 'PLOT_ID'
# return:
# Do: data frame with the plot id and the plot dominant diameter (cm)
# get_Do <- function(df_trees, dbh_column = 'dbh', expan_column = 'expan', plot_id_column = "PLOT_ID"){
#   
#   # if plot_id_column is in the data frame, calculate the dominant height for each plot
#   if(plot_id_column %in% names(df_trees)) {
#     
#     # unique plot IDs for Ho calculation
#     PLOT_ID = unique(df_trees[[plot_id_column]])
#     Do = rep(NA, length(PLOT_ID))
#     names(Do) = PLOT_ID
#     
#     # for each plot
#     for(i in 1:length(PLOT_ID)) {
#       
#       # get dominant dbh (cm)
#       Do[i] = Do_support(dbh = df_trees[[dbh_column]][df_trees[[plot_id_column]]  == PLOT_ID[i]],
#                          expan = df_trees[[expan_column]][df_trees[[plot_id_column]]  == PLOT_ID[i]])
#     }
#     
#     # merge PLot_ID and Do
#     Do_final <- data.frame(PLOT_ID, Do)
#     
#     # return
#     return(Do_final)
#     
#   } else {
#     print('No plot ID column available on df_trees data frame provided.')
#   }
#   
#   # return is plot_id_column is not in the data frame
#   return(Do_support(df_trees[[dbh_column]], df_trees[[expan_column]]))
# }



# support function to calculate the plot dominant diameter (cm) 
# written by: Cristóbal Ordóñez Alonso
# modified by Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - dbh: dbh of the trees in the plot
# - expan: expansion factor of the trees in the plot
# return:
# dominant diameter (cm) of the plot
# Do_support <- function(dbh, expan) {
#   
#   # order trees by dbh
#   o <- order(dbh, decreasing = TRUE)
#   expan = expan[o]
#   
#   # sum of the expan in the plot
#   expan_sum = 0 
#   for(i in 1:length(dbh)) {
#     
#     # accumulate expan
#     expan_sum = expan_sum + expan[i]
#     
#     # if expan_sum > 100, return dominant height
#     if(expan_sum > 100) return(sum(dbh[1:i] * expan[1:i], na.rm = TRUE) / sum(dbh[1:i] * expan[1:i] / dbh[1:i], na.rm = TRUE))
#   }
#   
#   # return dominant diameter
#   return(sum(dbh * expan) / sum(expan))
# }



# support function to select the 100 biggest trees based on the value of a column (e.g., dbh or height)
# written by: Aitor Vázquez Veloso
# date: 2024-09-10
# parameters:
# - list_of_trees: a data frame containing the tree records
# - bigger_value_col: the name of the column to be used for selecting the biggest trees; default is "dbh"
# - expan_col: the name of the column with the expansion factor data; default is "expan"
# return:
# a data frame with the 100 biggest trees based on the value of the selected column

get_100_bigger_trees <- function(list_of_trees, bigger_value_col = "dbh", expan_col = "expan") {
  
  # sort the trees by the desired value in descending order
  sorted_trees <- list_of_trees %>%
    arrange(desc(list_of_trees[[bigger_value_col]]))
  
  # initialize variables
  tree_expansion <- 0
  selected_trees <- list()
  
  # iterate through the sorted trees and select the largest ones until expansion >= 100
  for (i in 1:nrow(sorted_trees)) {
    
    # skip rows where expan_col has NA
    if (is.na(sorted_trees[[expan_col]][i])) {
      next
    }
    
    # accumulate the expansion of the selected trees
    tree_expansion <- tree_expansion + sorted_trees[[expan_col]][i]
    
    # append the selected tree
    selected_trees <- append(selected_trees, list(sorted_trees[i, ]))
    
    # break the loop once the sum of expansion reaches or exceeds 100
    if (tree_expansion >= 100) {
      break
    } else if (i == nrow(sorted_trees)) {
      break
    }
  }
  
  # convert selected_trees back to a data frame
  selected_df <- do.call(rbind, selected_trees)
  if(is.null(selected_df)){
    return(list())
  } else {
    return(selected_df)
  }
}



# Function to calculate dominant value (typically dominant height or dominant diameter) for each plot
# written by: Aitor Vázquez Veloso
# date: 2024-09-10
# parameters:
# - trees: a data frame containing the tree records
# - dominant_value_col: the name of the column to be used for calculating the dominant value; 
#                       default is "h" to calculate dominant height, change to "dbh" for dominant diameter
# - plot_id_col: the name of the column with the plot ID data; default is "PLOT_ID"
# - bigger_value_col: the name of the column to be used for selecting the biggest trees; default is "dbh"
# - expan_col: the name of the column with the expansion factor data; default is "expan"
# return:
# a data frame with the plot ID and the dominant value for each plot

get_dominant_value <- function(trees, dominant_value_col = "h", output_col_name = 'Ho',
                               plot_id_col = 'PLOT_ID', bigger_value_col = "dbh", expan_col = "expan") {
  
  # list to return
  dominant_values <- tibble(PLOT_ID = character(), value = numeric())
  
  for(plot in unique(trees[[plot_id_col]])){
    
    # select the trees for the current plot
    list_of_trees <- trees[trees[[plot_id_col]] == plot, ]
    selected_trees <- get_100_bigger_trees(list_of_trees, bigger_value_col, expan_col)
    
    # avoid processing empty data frames
    if(is.list(selected_trees) && length(selected_trees) == 0){
      
      # initialize variables
      accumulate <- 0
      
    } else {
    
      # initialize variables
      accumulate <- 0
      result <- 0
      final_value <- 0
      
      # iterate over each tree in the selected trees
      for (i in 1:nrow(selected_trees)) {
        
        # get tree data
        tree_value <- selected_trees[[dominant_value_col]][i]
        tree_expan <- selected_trees[[expan_col]][i]         
        
        # skip trees with missing or zero values
        if (!is.na(tree_value) && tree_value != 0) {
          if ((accumulate + tree_expan) < 100) {
            result <- result + (tree_value * tree_expan)
            accumulate <- accumulate + tree_expan
          } else {
            result <- result + (100 - accumulate) * tree_value
            accumulate <- 100
            break
          }
        }
      }
    }
    
    # return the result if accumulate is not zero, otherwise return 0
    if (accumulate != 0) {
      final_value <- result / accumulate
    } else {
      final_value <- NA
    }
    
    # append the result to the list
    dominant_values <- dominant_values %>% add_row(tibble_row(PLOT_ID = plot, value = final_value))
  }
  
  # rename the value column
  names(dominant_values)[2] <- output_col_name
  
  return(dominant_values)
}



# calculate the quadratic mean dbh (cm) from plot basal area (m²/ha) and plot density (N trees/ha)
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - df: data frame in which the relative coordinates will be added
# - G_column: name of the column with the plot basal area (m²/ha); if NA, the function will look for a column named 'G'
# - N_column: name of the column with the plot density (N trees/ha); if NA, the function will look for a column named 'N'
# - G: plot basal area (m²/ha); if NA, the function will look for a column named 'G'
# - N: plot density (N trees/ha); if NA, the function will look for a column named 'N'
# return:
# dg: quadratic mean dbh (cm)

get_Dg <- function(df, G_column = NA, N_column = NA, G = NA, N = NA){
  
  # select variables according to the information provided
  if(!is.na(G_column)){G <- df[[G_column]]} else if(!is.na(G)){G <- G} else {G <- df$G}
  if(!is.na(N_column)){N <- df[[N_column]]} else if(!is.na(N)){N <- N} else {N <- df$N}
  
  # quadratic mean dbh (cm)
  dg <- 200 * (df$G / df$N / pi) ^ 0.5
  
  # return
  return(df)
}



# calculate the plot slenderness
# written by: Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_plots: data frame with the plot records
# - hm_column: name of the column with the mean height data; if NA, the function will look for a column named 'h_mean'; units must be m
# - dbhm_column: name of the column with the mean dbh data; if NA, the function will look for a column named 'dbh_mean'; units must be cm
# return:
# slenderness: plot slenderness

get_plot_slenderness <- function(df_plots, hm_column = 'h_mean', dbhm_column = 'dbh_mean'){
  
  # calculate the plot slenderness  
  slenderness <- df_plots[[hm_column]] * 100 / df_plots[[dbhm_column]]
  
  # return
  return(slenderness)
}



# calculate the plot dominant slenderness
# written by: Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_plots: data frame with the plot records
# - Ho_column: name of the column with the mean height data; if NA, the function will look for a column named 'Ho'; units must be m
# - Do_column: name of the column with the mean dbh data; if NA, the function will look for a column named 'Do'; units must be cm
# return:
# dominant_slenderness: plot dominant slenderness

get_plot_dominant_slenderness <- function(df_plots, Ho_column = 'Ho', Do_column = 'Do'){
  
  # calculate the plot dominant slenderness
  dominant_slenderness <- df_plots[[Ho_column]] * 100 / df_plots[[Do_column]]
  
  # return
  return(dominant_slenderness)
}



# calculate the stand density index
# written by: Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_plots: data frame with the plot records
# - N_column: name of the column with the plot density data; if NA, the function will look for a column named 'N'
# - Dg_column: name of the column with the quadratic mean dbh data; if NA, the function will look for a column named 'Dg'; units must be cm
# - r_value: exponent value; if NA, the function will use the default value of 1.605
# return:
# SDI: stand density index

get_SDI <- function(df_plots, N_column = 'N', Dg_column = 'dg', r_value = 1.605){
  
  # calculate the stand density index
  SDI <- df_plots[[N_column]] * ((25 / df_plots[[Dg_column]]) ** r_value)
  
  # return
  return(SDI)
}



# calculate the Hart index
# written by: Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_plots: data frame with the plot records
# - Ho_column: name of the column with the mean height data; if NA, the function will look for a column named 'Ho'; units must be m
# - N_column: name of the column with the plot density data; if NA, the function will look for a column named 'N'
# return:
# S: Hart index

get_hart_index <- function(df_plots, Ho_column = 'Ho', N_column = 'N'){
  
  # calculate the Hart index
  S <- 10000 / (df_plots[[Ho_column]] * sqrt(df_plots[[N_column]]))
  
  # return
  return(S)
}



# calculate the Hart index for staggered plots
# written by: Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_plots: data frame with the plot records
# - Ho_column: name of the column with the mean height data; if NA, the function will look for a column named 'Ho'; units must be m
# - N_column: name of the column with the plot density data; if NA, the function will look for a column named 'N'
# return:
# S_staggered: Hart index for staggered plots

get_hart_index_staggered <- function(df_plots, Ho_column = 'Ho', N_column = 'N'){
  
  # calculate the Hart index
  S_staggered <- (10000 / df_plots[[Ho_column]]) * sqrt(2 / df_plots[[N_column]] * sqrt(3))
  
  # return
  return(S_staggered)
}



# Calculate plot basal area (G m2/ha) and plot density (N trees/ha) by species
# written by: Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_trees: data frame with the tree records
# - plot_id_column_1: name of the column with the plot ID data; if NA, the function will look for a column named 'PLOT_ID'
# - species_column: name of the column with the species ID data; if NA, the function will look for a column named 'species'
# - expan_column: name of the column with the expansion factor data; if NA, the function will look for a column named 'expan'
# - g_column: name of the column with the basal area data; if NA, the function will look for a column named 'g'
# return:
# plots_sp: data frame with the plot basal area (G m2/ha) and plot density (N trees/ha) by species

get_plot_by_species <- function(df_trees, plot_id_column_1 = 'PLOT_ID', species_column = 'species', 
                                expan_column = 'expan', g_column = 'g'){
  
  # check variable names in df_trees
  if(expan_column != 'expan'){df_trees$expan <- df_trees[[expan_column]]}
  if(g_column != 'g'){df_trees$g <- df_trees[[g_column]]}
  
  # calculate G and N by species
  plots_sp <- df_trees %>%
    
    # group by plot IDs and species ID
    group_by(df_trees[[plot_id_column_1]], df_trees[[species_column]]) %>%
    summarise(
      
      # plot density (N trees/ha) by species
      N_sp = ifelse(all(is.na(expan)), NA, sum(expan, na.rm = TRUE)),
      
      # plot basal area (G m2/ha) by species
      G_sp = ifelse(all(is.na(expan)) | all(is.na(g)), NA, sum(g * expan / 10000, na.rm = TRUE))
    )
  
  # rename columns
  plots_sp <- dplyr::rename(plots_sp, c(PLOT_ID = `df_trees[[plot_id_column_1]]`, 
                                        species = `df_trees[[species_column]]`))
  
  # order information by plot IDs and G 
  plots_sp <- plots_sp %>%
    arrange(PLOT_ID, -G_sp)
  
  # temporal data frame to calculate N and G for the three main species of the plot
  plots_sp_123 <- data.frame()
  for(k in unique(plots_sp$PLOT_ID)){
    
    # subset data by plot ID
    plots_k <- plots_sp[plots_sp$PLOT_ID %in% k,]
    
    # add columns with the three main species and their N and G
    plots_k$sp_1 <- plots_k[[species_column]][1]
    plots_k$sp_2 <- plots_k[[species_column]][2] 
    plots_k$sp_3 <- plots_k[[species_column]][3] 
    plots_k$G_sp_1 <- plots_k$G_sp[1]
    plots_k$G_sp_2 <- plots_k$G_sp[2]
    plots_k$G_sp_3 <- plots_k$G_sp[3]
    plots_k$N_sp_1 <- plots_k$N_sp[1]
    plots_k$N_sp_2 <- plots_k$N_sp[2]
    plots_k$N_sp_3 <- plots_k$N_sp[3]
    
    # delete duplicated data
    plots_k <- plots_k[!duplicated(plots_k$PLOT_ID), ]
    
    # add data to the temporal data frame
    plots_sp_123 <- rbind(plots_sp_123, plots_k)
  }
  
  # clean previous data
  plots_sp_123 <- dplyr::select(plots_sp_123, -c(species_column, N_sp, G_sp))
  
  # return
  return(plots_sp_123)
}



# Calculate plot basal area (G m2/ha) and plot density (N trees/ha) by dead/alive
# written by: Aitor Vázquez Veloso
# date: 2024-02-09
# parameters:
# - df_trees: data frame with the tree records
# - plot_id_column_1: name of the column with the plot ID data; if NA, the function will look for a column named 'PLOT_ID'
# - dead_column: name of the column with the dead/alive data; if NA, the function will look for a column named 'dead'
# - expan_column: name of the column with the expansion factor data; if NA, the function will look for a column named 'expan'
# - g_column: name of the column with the basal area data; if NA, the function will look for a column named 'g'
# return:
# plots_status: data frame with the plot basal area (G m2/ha) and plot density (N trees/ha) by dead/alive

get_plot_mortality <- function(df_trees, plot_id_column_1 = 'PLOT_ID', 
                               dead_column = 'dead', expan_column = 'expan', g_column = 'g'){
  
  # check variable names in df_trees
  if(expan_column != 'expan'){df_trees$expan <- df_trees[[expan_column]]}
  if(g_column != 'g'){df_trees$g <- df_trees[[g_column]]}
  if(dead_column != 'dead'){df_trees$dead <- df_trees[[dead_column]]}
  
  # calculate G and N by dead/alive
  plots_status <- df_trees %>%
    
    # group by plot IDs and species ID
    group_by(df_trees[[plot_id_column_1]], df_trees[[dead_column]]) %>%
    summarise(
      
      # plot density (N trees/ha) of dead trees
      N_status = ifelse(all(is.na(expan)), NA, sum(expan, na.rm = TRUE)),
      
      # plot basal area (G m2/ha) of dead trees
      G_status = ifelse(all(is.na(expan)) | all(is.na(g)), NA, sum(g * expan / 10000, na.rm = TRUE))
    )
  
  # rename columns
  plots_status <- dplyr::rename(plots_status, c(PLOT_ID = `df_trees[[plot_id_column_1]]`,
                                                dead = `df_trees[[dead_column]]`))
  
  # order information by plot IDs and status
  plots_status <- plots_status %>%
    arrange(PLOT_ID, dead)
  
  # temporal data frame to calculate N and G for the status (dead/alive) of the plot
  plots_by_status <- data.frame()
  for (k in unique(plots_status$PLOT_ID)){
    
    # subset data by plot ID
    plots_k <- plots_status[plots_status$PLOT_ID %in% k,]
    
    # add columns with the status (dead/alive) and their N and G
    plots_k$G_alive <- plots_k$G_status[1]
    plots_k$G_dead <- plots_k$G_status[2]
    plots_k$N_alive <- plots_k$N_status[1]
    plots_k$N_dead <- plots_k$N_status[2]
    
    # delete duplicated data
    plots_k <- plots_k[!duplicated(plots_k$PLOT_ID), ]
    
    # add data to the temporal data frame
    plots_by_status <- rbind(plots_by_status, plots_k)
  }
  
  # change NAs by 0
  # plots_by_status[is.na(plots_by_status)] <- 0
  
  # clean previous data
  plots_by_status <- dplyr::select(plots_by_status, -c(dead, N_status, G_status))
  
  # return
  return(plots_by_status)
}



# calculate the WGS84 coordinates of the plots using the UTM coordinates
# written by: Aitor Vázquez Veloso
# date: 2024-02-12
# parameters:
# - df: data frame with the plot records
# - plot_id_column: name of the column with the plot ID data; if NA, the function will look for a column named 'PLOT_ID'
# - province_column: name of the column with the province data; if NA, the function will look for a column named 'Province'
# - x_column: name of the column with the UTM X coordinates; if NA, the function will look for a column named 'X_UTM'
# - y_column: name of the column with the UTM Y coordinates; if NA, the function will look for a column named 'Y_UTM'
# return:
# df: data frame with the WGS84 coordinates of the plots, the zone code and the plot ID

get_wgs84_coordinates <- function(df, plot_id_column = 'PLOT_ID', province_column = 'Province', x_column = 'X_UTM', y_column = 'Y_UTM'){
  
  # check variable names in the original df
  if(plot_id_column != 'PLOT_ID'){df$PLOT_ID <- df[[plot_id_column]]}
  if(province_column != 'Province'){df$Province <- df[[province_column]]}
  if(x_column != 'X_UTM'){df$X_UTM <- df[[x_column]]}
  if(y_column != 'Y_UTM'){df$Y_UTM <- df[[y_column]]}
  
  # stablish the CRS for the plots according to each zona
  CRS_28 <- "+proj=utm +zone=28 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
  CRS_29 <- "+proj=utm +zone=29 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
  CRS_30 <- "+proj=utm +zone=30 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
  CRS_31 <- "+proj=utm +zone=31 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
  
  # include the zone in the plots df
  df$zone <- with(df,
                  case_when( 
                    Province %in% c(38, 35) ~ 28, # 28: Canarias
                    Province %in% c(15, 27, 36, 32, 21)  ~ 29, # 29: Galicia y Huelva (casi seguro que no tiene nada en el 30)
                    Province %in% c(33, 24, 49, 37, 10, 6, 41, 11) ~ ifelse(X_UTM > 500000, 29, 30),  # 29 y 30: Asturias, León, Zamora, Salamanca, Extremadura (ambas provincias), Sevilla y Cádiz
                    Province %in% c(31, 39, 1, 20, 48, 47, 9, 34, 42, 5, 40, 26, 28, 2, 13, 16, 19, 45, 30, 4, 14, 18, 23, 29, 51)  ~ 30, # 30: Cantabria, Pais Vasco, Castilla y León (salvo las 3 provincias leonesas), La Rioja, Madrid, Castilla La Mancha, Murcia, Andalucía (Salvo las 3 provincias más occidentales mencionadas)
                    Province %in% c(46, 12, 3, 22, 50, 44) ~ ifelse(X_UTM > 500000, 30, 31),  # 30 y 31: Comunidad Valenciana y Aragon
                    Province %in% c(8, 25, 17, 43, 7)  ~ 31,  # 31: Cataluña y Baleares
                  )
  )
  
  # skip data without information about the UTM coordinates
  df_nas <- df[is.na(df$X_UTM) | is.na(df$Y_UTM),]
  df <- df[!is.na(df$X_UTM) & !is.na(df$Y_UTM),]
  
  # create a df to store the coordinates
  coords_df <- data.frame()
  
  # loop through the zones
  for(zone in c(28, 29, 30, 31)){
    
    # check if the zone is in the df
    ifelse(zone %in% unique(df$zone), 
           {
             # select CRS according to the zone
             CRS_zone <- ifelse(zone == 28, CRS_28,
                                ifelse(zone == 29, CRS_29,
                                       ifelse(zone == 30, CRS_30, CRS_31)))
             
             # split df plots by zone
             coord_zone <- df[df$zone == zone, ]
             
             # convert plot coordinates to spatial points according to the zone
             coord.spt_zone <- SpatialPointsDataFrame(coord_zone[ ,c('X_UTM','Y_UTM')], coord_zone, proj4string = CRS(CRS_zone))
             
             # convert plot coordinates to global coordinates in WGS84
             coordLT_zone <- spTransform(coord.spt_zone, CRS("+proj=longlat +ellps=WGS84"))
             
             # save the coordinates in a data frame
             tmp_coords <- tibble()
             tmp_coords <- tibble(PLOT_ID = coordLT_zone@data$PLOT_ID)
             tmp_coords$zone <- zone
             tmp_coords$coordinates <- coordLT_zone@coords
             tmp_coords$Longitude <- tmp_coords$coordinates[ ,"X_UTM"]
             tmp_coords$Latitude <- tmp_coords$coordinates[ ,"Y_UTM"]
             tmp_coords <- tmp_coords[ ,c('PLOT_ID', 'zone', 'Longitude', 'Latitude')]
             coords_df <- rbind(coords_df, tmp_coords)
           }
           # if zone not in the df, go to the next zone
           , next)
  }
  
  # adapt the data with NAs format
  df_nas <- df_nas[ ,c('PLOT_ID', 'zone')]
  df_nas <- df_nas %>% mutate(Longitude = NA, Latitude = NA)
  
  # append the data with NAs
  coords_df <- rbind(coords_df, df_nas)
  
  # return
  return(coords_df)
}

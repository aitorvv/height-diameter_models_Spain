#!/usr/bin/Rscript

# Functions for tree data variables specifically developed to SFNI data ----
#
# Aitor Vázquez Veloso
# 2024-09-12
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# support libraries
library(measurements)  # convert units



# calculate the mean tree dbh on cm based on the two dbh measurements and the units
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - dbh_1: first dbh measurement
# - dbh_2: second dbh measurement
# - dbh_units: units of the dbh measurements (mm, cm, dm, m); default = 'cm'
# return:
# - mean_dbh: mean dbh in cm

get_mean_dbh <- function(dbh_1, dbh_2, dbh_units = 'cm'){
  
  # convert dbh_1 and dbh_2 units to cm if needed
  if(dbh_units != 'cm'){
    dbh_1 <- measurements::conv_unit(dbh_1, dbh_units, 'cm')
    dbh_2 <- measurements::conv_unit(dbh_2, dbh_units, 'cm')
  }
  
  # get mean dbh in cm
  mean_dbh <- (dbh_1 + dbh_2) / 2
  
  return(mean_dbh)
}



# calculate the tree expansion factor according to its dbh (SFNI plots)
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - dbh: dbh measurement
# - dbh_units: units of the dbh measurements (mm, cm, dm, m); default = 'cm'
# return:
# - expan: expansion factor of the tree

get_sfni_expan <- function(dbh, dbh_units = 'cm'){
  
  # convert dbh units to cm if needed
  ifelse(dbh_units != 'cm', dbh <- measurements::conv_unit(dbh, dbh_units, 'cm'),
         dbh <- dbh)
  
  # get expansion factor
  ifelse(is.na(dbh), expan <- 0,
         expan <- ifelse (dbh < 7.5, 0, 
                          ifelse(dbh < 12.5, 10000/(pi*(5^2)), 
                                 ifelse(dbh < 22.5, 10000/(pi*(10^2)), 
                                        ifelse(dbh < 42.5, 10000/(pi*(15^2)),
                                               10000/(pi*(25^2)))))))
  
  return(expan)
}



# calculate the tree expansion factor according to the plot area
# written by: Aitor Vázquez Veloso
# date: 2024-09-12
# parameters:
# - area: plot area in m^2
# - units: units of the area measurements (m2, ha); default = 'm2'
# return:
# - expan: expansion factor of the tree

get_expan <- function(area, units = 'm2'){
  
  # convert area units to m2 if needed
  ifelse(units == 'ha', area <- area * 10000,
         area <- area)
  expan <- 10000 / area
  
  return(expan)
}



# calculate the tree circunference on cm based on the tree dbh and its units
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - dbh: dbh measurement
# - dbh_units: units of the dbh measurements (mm, cm, dm, m); default = 'cm'
# return:
# - circ: tree circumference in cm

get_circumference <- function(dbh, dbh_units = 'cm'){
  
  # convert dbh units to cm if needed
  ifelse(dbh_units != 'cm', dbh <- measurements::conv_unit(dbh, dbh_units, 'cm'),
         dbh <- dbh)
  
  # get tree circumference in cm
  circ <- dbh * pi
  
  return(circ)
}



# calculate the tree basal area on cm2 based on the tree dbh and its units
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - dbh: dbh measurement
# - dbh_units: units of the dbh measurements (mm, cm, dm, m); default = 'cm'
# return:
# - g: basal area in cm2

get_g <- function(dbh, dbh_units = 'cm'){
  
  # convert dbh units to cm if needed
  ifelse(dbh_units != 'cm', dbh <- measurements::conv_unit(dbh, dbh_units, 'cm'),
         dbh <- dbh)
  
  # get g in cm2
  g <- (pi / 4) * (dbh^2)
  
  return(g)
}



# calculate the tree basal area on m2/ha based on the tree dbh or tree basal area and tree expan
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - dbh: dbh measurement
# - expan: expansion factor
# - dbh_units: units of the dbh measurements (mm, cm, dm, m); default = 'cm'
# return:
# - g_ha: basal area in m2/ha

get_g_ha <- function(dbh = NA, g = NA, expan, dbh_units = 'cm'){
  
  # get g in cm2
  ifelse(!is.na(g), g <- g,
         g <- get_g(dbh, dbh_units = dbh_units))
  
  # get g_ha in m2/ha
  g_ha <- g * expan / 10000
  
  return(g_ha)
}



# calculate the tree basal area larger than subject tree (bal, in m2/ha) according with the tree variables available
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - df: data frame with the tree variables
# - plot_id_column: column name with the plot id
# - tree_id_column: column name with the tree id
# - dbh_column: column name with the dbh measurements
# - dbh_units: units of the dbh measurements (mm, cm, dm, m); default = 'cm'
# - g_column: column name with the basal area measurements
# - expan_column: column name with the expansion factor
# - g_ha_column: column name with the basal area per hectare measurements
# return:
# - bal_results: data frame with the basal area larger than subject tree (bal, in m2/ha) for each tree in each plot

get_bal <- function(df, plot_id_column, tree_id_column, dbh_column, dbh_units = 'cm',
                    expan_column, g_column = NA, g_ha_column = NA){
  
  # order df by plot id and dbh
  df <- df[order(df[[plot_id_column]],
                 df[[dbh_column]],
                 decreasing = TRUE), ]
  
  # variables to store the results
  bal_results <- data.frame() 
  
  # for each plot
  for(plot in unique(df[[plot_id_column]])){
    
    # get the trees of the plot
    df_tmp <- df[df[[plot_id_column]] %in% plot, ] 
    
    # when no trees, skip the plot
    if(nrow(df_tmp) == 0){
      next
    }
    
    # set the bal to 0 and create a list to store the results
    bal <- 0
    trees_list <- data.frame() 
    
    # for each tree in the plot
    for(tree in df_tmp[[tree_id_column]]){
      
      # select the tree
      tree_tmp <- df_tmp[df_tmp[[tree_id_column]] == tree, ] 
      
      # add bal value and add the tree to the list
      tree_tmp$bal <- bal
      trees_list <- rbind(trees_list, tree_tmp) 
      
      # update bal
      ifelse(!is.na(g_ha_column), bal <- bal + tree_tmp[[g_ha_column]],
             ifelse(!is.na(g_column) & !is.na(expan_column), 
                    bal <- bal + tree_tmp[[g_column]] * tree_tmp[[expan_column]] / 10000,
                    bal <- bal + get_g_ha(dbh = tree_tmp[[dbh_column]], expan = tree_tmp[[expan_column]], dbh_units = dbh_units)))
    }
    
    # add the results to the final data frame
    bal_results <- rbind(bal_results, trees_list) 
  }
  
  return(bal_results)
}



# calculate the tree slenderness
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - dbh: dbh measurement
# - h: tree height
# - dbh_units: units of the dbh measurements (mm, cm, dm, m); default = 'cm'
# - h_units: units of the height measurements (mm, cm, dm, m); default = 'm'
# return:
# - slenderness: tree slenderness

get_slenderness <- function(dbh, h, dbh_units = 'cm', h_units = 'm'){
  
  # convert dbh units to cm if needed
  ifelse(dbh_units != 'cm', dbh <- measurements::conv_unit(dbh, dbh_units, 'cm'),
         dbh <- dbh)
  
  # convert h units to m if needed
  ifelse(h_units != 'm', h <- measurements::conv_unit(h, h_units, 'm'),
         h <- h)
  
  # get tree slenderness
  slenderness <- h*100 / dbh
  
  return(slenderness)
}



# calculate the relative tree coordinates respect the plot center based on the tree distance and bearing
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - df: data frame in which the relative coordinates will be added
# - distance_column: column name with the distance measurements
# - bearing_column: column name with the bearing measurements
# - distance_units: units of the distance measurements (mm, cm, dm, m); default = 'm'
# - bearing_units: units of the bearing measurements (degree, grad, radian); default = 'grad'
# return:
# - df: original data frame with the new columns x_rel and y_rel 

get_coord_rel <- function(df, distance_column, bearing_column, distance_units = 'm', bearing_units = 'grad'){
  
  # convert distance units to m if needed
  ifelse(distance_units != 'm', df[[distance_column]] <- measurements::conv_unit(df[[distance_column]], distance_units, 'm'),
         df[[distance_column]] <- df[[distance_column]])
  
  # convert bearing units to radian if needed
  ifelse(bearing_units != 'radian', df[[bearing_column]] <- measurements::conv_unit(df[[bearing_column]], bearing_units, 'radian'),
         df[[bearing_column]] <- df[[bearing_column]])
  
  # get tree relative coordinates
  df$x_rel <- df[[distance_column]]*cos(df[[bearing_column]]) 
  df$y_rel <- df[[distance_column]]*sin(df[[bearing_column]]) 
  
  return(df)
}



# calculate the absolute and relative tree coordinates based on the tree distance and bearing respect the plot center 
# and the plot center coordinates
# written by: Aitor Vázquez Veloso
# date: 2024-01-25
# parameters:
# - df: data frame in which the relative coordinates will be added
# - distance_column: column name with the distance measurements
# - bearing_column: column name with the bearing measurements
# - x_center: x coordinate of the plot center; it must be provided if x_center_column is NA
# - y_center: y coordinate of the plot center; it must be provided if y_center_column is NA
# - x_center_column: column name with the x coordinate of the plot center; it must be provided if x_center is NA
# - y_center_column: column name with the y coordinate of the plot center; it must be provided if y_center is NA
# - distance_units: units of the distance measurements (mm, cm, dm, m); default = 'm'
# - bearing_units: units of the bearing measurements (degree, grad, radian); default = 'grad'
# return:
# - df: original data frame with the new columns x_rel and y_rel (relative coordinates) and 
#       x_abs and y_abs (absolute coordinates)

get_coord_utm <- function(df, distance_column, bearing_column, x_center = NA, y_center = NA, 
                          x_center_column = NA, y_center_column = NA,
                          distance_units = 'm', bearing_units = 'grad'){
  
  # get relative coordinates
  df <- get_coord_rel(df, distance_column, bearing_column, 
                      distance_units = distance_units, bearing_units = bearing_units)
  
  # select provided plot center coordinates
  if(!is.na(x_center_column) & !is.na(y_center_column)){
    x_center <- df[[x_center_column]]
    y_center <- df[[y_center_column]]
  } else {
    if(!is.na(x_center) & !is.na(y_center)){
      x_center <- x_center
      y_center <- y_center
    } else {
      stop('You must provide the plot center coordinates or the column names where they are stored')
    }
  }
  
  # get absolute coordinates
  df$x_utm <- df$x_rel + x_center
  df$y_utm <- df$y_rel + y_center
  
  return(df)
}

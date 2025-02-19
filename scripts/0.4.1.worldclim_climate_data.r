#!/usr/bin/Rscript

# Code to curate SFNI2, SFNI3 and SFNI4 raw data ----
# Step 4: extract climate data for each plot using WorldClim
#
# Aitor Vázquez Veloso
# 2024-09-16
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Some decisions that has been taken can be filtered by "# Note:"


# SFNI2 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls
# SFNI3 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# SFNI4 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf



# working directory
setwd('SFNI_Spanish_Forest_National_Inventory-ready_to_use/')

# libraries
library(tidyverse)
library(rgdal)

# load data (just plots)
load("1_data/2_processed/0.3.2_sfni_corrected_position_data.rdata")
rm(trees)

# Note:
# the code has several sections where the progress is saved, just to avoid repeating the whole process in case of error



# Get historic climate data ====

# get WorldClim functions (historic monthly data) and Martonne index
source('SFNI_Spanish_Forest_National_Inventory-ready_to_use/WorldClim/scripts/wc_historic_monthly_data.R')
source('SFNI_Spanish_Forest_National_Inventory-ready_to_use/WorldClim/scripts/wc_climate_index.R')

# copy points and add ID (column must be named ID)
my_points <- dplyr::select(plots, Longitude, Latitude, IFN_PLOT_ID)
my_points <- dplyr::rename(my_points, ID = IFN_PLOT_ID)

# skip points with NA coordinates
my_points <- my_points[!is.na(my_points$Longitude) & !is.na(my_points$Latitude), ]

# convert to spatial points
coordinates(my_points) <- c("Longitude", "Latitude")
proj4string(my_points) <- CRS("+proj=longlat +datum=WGS84")  

# path to the folder with the WorldClim data: historical monthly data
folder_path <- "SFNI_Spanish_Forest_National_Inventory-ready_to_use/WorldClim/historical_monthly_data/"

# select periods of time
all_years <- c(1951:2021)
period <- c(2000:2020)

# get monthly data
# wc_month <- wc_monthly_climate(wc_path = folder_path,
#                                points = my_points, 
#                                data_period = period)

# process on packages of 100 points (avoiding lack of memory)
for (i in seq(1, nrow(my_points), by = 100)) {
  
  # select points
  if(i + 99 > nrow(my_points)){
    points <- my_points[i:nrow(my_points), ]
  } else {
    points <- my_points[i:(i+99), ]
  }
  
  # get monthly data
  wc_month_tmp <- wc_monthly_climate(wc_path = folder_path,
                                     points = points, 
                                     data_period = all_years)
  
  save(wc_month_tmp, file = paste("1_data/2_processed/tmp/0.4.1/0.4.1.tmp_historic_monthly_climate-i_", i, ".rdata", sep = ''))
  print(paste("Historic monthly climate: processed and saved plots", i, "to", i + 99, sep = ' '))
  
  # append data to the final data frame
  if (i == 1) {
    wc_month <- wc_month_tmp
  } else {
    wc_month <- rbind(wc_month, wc_month_tmp)
  }
}

# get Martonne index by month
m_month <- wc_martonne(df = wc_month, 
                       prec = wc_month$prec, 
                       tmean = wc_month$tmean)

save(my_points, m_month, file = "1_data/2_processed/0.4.1.sfni_historic_monthly_climate_data.rdata")
print("Historic monthly climate and Martonne: processed and saved all plots")



# get yearly data
wc_year <- wc_annual_climate(df = wc_month)
m_year <- wc_martonne(df = wc_year, 
                      prec = wc_year$prec, 
                      tmean = wc_year$tmean)
save(my_points, m_year, file = "1_data/2_processed/0.4.1.sfni_historic_yearly_climate_data.rdata")
print("Historic yearly climate and Martonne: processed and saved all plots")

# get mean by period - all years of historic data
wc_all_years <- wc_average_climate(df = wc_year)
m_all_years <- wc_martonne(df = wc_all_years, 
                           prec = wc_all_years$prec, 
                           tmean = wc_all_years$tmean)
save(my_points, m_all_years, file = "1_data/2_processed/0.4.1.sfni_70years_period_climate_data.rdata")
print("Historic climate and Martonne for 70 years period: processed and saved all plots")

# get mean by period - 2000-2020
wc_period <- wc_year[wc_year$year %in% period, ]
wc_period <- wc_average_climate(df = wc_period)
m_period <- wc_martonne(df = wc_period, 
                        prec = wc_period$prec, 
                        tmean = wc_period$tmean)
save(my_points, m_period, file = "1_data/2_processed/0.4.1.sfni_2000-2020_period_climate_data.rdata")
print("Historic climate and Martonne for period 2000-2020: processed and saved all plots")



# Get future climate data ====

# get WorldClim functions: future data using MIROC6 model
source('SFNI_Spanish_Forest_National_Inventory-ready_to_use/WorldClim/scripts/wc_future_MIROC6_data.R')

# select SSPs (all or just 1)
SSPs <- c(1, 2, 3, 5)
# SSPs <- 3  # middle on the road
for(ssp in SSPs){
  
  # select data folder
  folder_path <- paste('SFNI_Spanish_Forest_National_Inventory-ready_to_use/WorldClim/MIROC6_SSP', ssp, '/', sep = '')
  
  # run function to get future climate data
  future_clima <- wc_future_climate(folder_path = folder_path, 
                                    points = my_points)

  # calculate martonne for the future periods of time
  m_fut_period <- wc_martonne(df = future_clima, 
                              prec = future_clima$prec, 
                              tmean = future_clima$tmean) 
  
  # add model and ssp to the data
  m_fut_period$model <- 'MIROC6'
  m_fut_period$ssp <- paste('SSP', ssp, sep = '')  
  
  # save data
  save(my_points, m_fut_period, 
       file = paste("1_data/2_processed/0.4.1.sfni_future_climate_data_MIROC6_SSP", ssp, ".rdata", sep = ''))
  print(paste("Future climate and Martonne data: processed and saved all plots using MIROC6 model on SSP", 
              ssp, sep = ''))
  
}



# Save all the results in one file ====

save(my_points, m_month, m_year, m_all_years, m_period, m_fut_period,
     file = "1_data/2_processed/0.4.1.worldclim_climate_data.rdata")
print("All climate data: processed and saved all plots")
print("End of the script")



#!/usr/bin/Rscript

# Code to curate SFNI2, SFNI3 and SFNI4 raw data ----
# Step 4: (a) extract regional information about climatic classification for each plot
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
library(sf)
library(raster)

# load data (just plots)
load("1_data/2_processed/0.3.2_sfni_corrected_position_data.rdata")
rm(trees)

# load climatic classification data
fito <- st_read("1_data/1_raw/subregiones_fitoclimaticas/allue_p_tcm30-199930/Allue_p.shp")
biogeo <- st_read("1_data/1_raw/regiones_biogeograficas_espana/RegionesBio.shp")
veget_sp <- st_read("1_data/1_raw/mapas_series_vegetacion/series_peninsula_tcm30-199924/Series_p.shp")
veget_ca <- st_read("1_data/1_raw/mapas_series_vegetacion/series_canarias_tcm30-199925/Series_c.shp")



# Data management ==== 

# convert points to spatial dataframe
df_no_coords <- plots[is.na(plots$Longitude) | is.na(plots$Latitude), ]
df <- dplyr::select(plots, IFN_PLOT_ID, Longitude, Latitude)
df <- df[!is.na(df$Longitude) & !is.na(df$Latitude), ]
df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = ("+proj=longlat +datum=WGS84"))

# check points crs
st_crs(df_sf)
ggplot() + geom_sf(data = df_sf) + theme_minimal()

# convert shapefiles crs and check
st_crs(fito)
fito <- st_transform(fito, crs = ("+proj=longlat +datum=WGS84"))
ggplot() + geom_sf(data = fito) + theme_minimal()

st_crs(biogeo)
biogeo <- st_transform(biogeo, crs = ("+proj=longlat +datum=WGS84"))
ggplot() + geom_sf(data = biogeo) + theme_minimal()

st_crs(veget_sp)
veget_sp <- st_transform(veget_sp, crs = ("+proj=longlat +datum=WGS84"))
ggplot() + geom_sf(data = veget_sp) + theme_minimal()

st_crs(veget_ca)
veget_ca <- st_transform(veget_ca, crs = ("+proj=longlat +datum=WGS84"))
ggplot() + geom_sf(data = veget_ca) + theme_minimal()

veget <- rbind(veget_sp, veget_ca)
ggplot() + geom_sf(data = veget) + theme_minimal()


# check if the geometry is valid and correct if needed
fito_check <- st_is_valid(fito)
unique(fito_check)

biogeo_check <- st_is_valid(biogeo)
unique(biogeo_check)

veget_sp_check <- st_is_valid(veget_sp)
unique(veget_sp_check)  # must be corrected

veget_ca_check <- st_is_valid(veget_ca)
unique(veget_ca_check)

# visualize invalid geometries
invalid_geometries <- veget_sp[!veget_sp_check, ]
ggplot() + geom_sf(data = invalid_geometries) + theme_minimal()

# correct invalid geometries
veget_sp_cleaned <- st_make_valid(veget_sp)
all(st_is_valid(veget_sp_cleaned))  # should return TRUE if everything is fixed
ggplot() + geom_sf(data = veget_sp_cleaned) + theme_minimal()

# merge vegetation shapefiles
veget <- rbind(veget_sp_cleaned, veget_ca)



# Extract information from the shapefiles and merge in a single df ====

df_fito <- st_join(df_sf, fito)
df_fito <- df_fito %>% rename_with(~paste0("fito-", .))

df_regions <- st_join(df_sf, biogeo)
df_regions <- df_regions %>% rename_with(~paste0("biogeo-", .))

df_veget <- st_join(df_sf, veget)
df_veget <- df_veget %>% rename_with(~paste0("veget-", .))

# merge all data and clean
df <- bind_cols(df, df_fito, df_regions, df_veget)
df <- dplyr::select(df, -`fito-IFN_PLOT_ID`, -`biogeo-IFN_PLOT_ID`, -`veget-IFN_PLOT_ID`,
                    -`fito-geometry`, -`biogeo-geometry`, -`veget-geometry`)



# Manage general climate region labels ====

# include climate region labels on data already processed
df$climate_region <- ifelse(df$`biogeo-descripcio` %in% c("Región Biogeográfica Atlántica", "Región Marina Atlántica"), "Atlántica",
                            ifelse(df$`biogeo-descripcio` %in% c("Región Biogeográfica Mediterránea", "Región Marina Mediterránea"), "Mediterránea", 
                                   ifelse(df$`biogeo-descripcio` %in% c("Región Biogeográfica Macaronésica", "Región Marina Macaronésica"), "Macaronésica",
                                          ifelse(df$`biogeo-descripcio` %in% c("Región Biogeográfica Alpina"), "Alpina", 
                                                 NA))))

# include climate region labels on data without coordinates (SFNI2 with wrong value)
df_no_coords$climate_region <- ifelse(df_no_coords$Province_name %in% c('Coruña', 'Lugo', 'Pontevedra', 'Asturias', 'Cantabria'), 'Atlántica',
                                  ifelse(df_no_coords$Province_name %in% c('Palmas', 'Santa Cruz de Tenerife'), 'Macaronésica',
                                         ifelse(df_no_coords$Province_name %in% c('Ourense', 'León', 'Navarra', "Girona", "Huesca", "Lleida", "Barcelona"), NA,  # provinces between two regions
                                                'Mediterránea')))

# homogeneous column names
df_no_coords <- dplyr::select(df_no_coords, IFN_PLOT_ID, climate_region)
missing_vars <- setdiff(colnames(df), colnames(df_no_coords))
df_no_coords[missing_vars] <- NA

# merge data
df <- rbind(df, df_no_coords)



# Save data ====

save(df, file = "1_data/2_processed/0.4.0_climatic_classification.rdata")

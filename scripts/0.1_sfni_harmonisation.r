#!/usr/bin/Rscript

# Code to curate SFNI2, SFNI3 and SFNI4 raw data ----
# Step 1: merge datasets and create IDs
#
# Aitor Vázquez Veloso
# 2024-09-13
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Some decisions that has been taken can be filtered by "# Note:"


# SFNI2 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls
# SFNI3 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# SFNI4 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf



# working directory
setwd('SFNI_Spanish_Forest_National_Inventory-ready_to_use/')

# libraries
library(tidyverse)



# Initial steps: SFNI2 ----

# load data
trees <- read.csv('1_data/1_raw/IFN2/pcmayores2.csv', sep = ',')
plots <- read.csv('1_data/1_raw/IFN2/plot2.csv', sep = ',')



# Create IDs ====

trees <- trees %>%
  mutate(INVENTORY_ID = 'IFN2',
         PLOT_ID = paste(trees$Origen, trees$Estadillo, sep = '_'),
         PLOT_ID_short = paste(trees$Origen, trees$Estadillo, sep = '_'),
         IFN_PLOT_ID = paste('IFN2', trees$Origen, trees$Estadillo, sep = '_'),
         TREE_ID = paste(trees$Origen, trees$Estadillo, trees$NumOrden, sep = '_'),
         IFN_TREE_ID = paste('IFN2', trees$Origen, trees$Estadillo, trees$NumOrden, sep = '_'))

plots <- plots %>%
  mutate(INVENTORY_ID = 'IFN2',
         PLOT_ID = paste(plots$PROVINCIA, plots$ESTADILLO, sep = '_'),
         PLOT_ID_short = paste(plots$PROVINCIA, plots$ESTADILLO, sep = '_'),
         IFN_PLOT_ID = paste('IFN2', plots$PROVINCIA, plots$ESTADILLO, sep = '_'))



# Original variables management: trees ====

# check original tree data status
str(trees)

# manage variables
trees <- trees %>%
  # change commas to dots
  mutate(h = str_replace(Altura, ',', '.'),
         distance = str_replace(Distanci, ',', '.'),
         special_param = str_replace(ParamEsp, ',', '.')) %>%
  # change chr to numeric
  mutate(h = as.numeric(h),
         distance = as.numeric(distance),
         special_param = as.numeric(special_param))

# rename variables
trees <- dplyr::rename(trees, c(
  province = Origen,
  n_plot = Estadillo,
  n_tree = NumOrden,
  bearing = Rumbo,
  species = Especie,
  dbh_1 = Diametro1,
  dbh_2 = Diametro2,
  quality = Calidad,
  shape = Forma)
)

# remove old variables
trees <- trees %>%
  dplyr::select(-c(Altura, Distanci, ParamEsp))

# create empty additional variables
trees$class <- NA
trees$subclass <- NA


# Original variables management: plots ====

# check original plot data status
names(plots)
str(plots)

# calculate mean plot altitude after data curation
plots$ALTITUD1 <- ifelse(grepl('W', plots$ALTITUD1), 0, plots$ALTITUD1)
plots$ALTITUD1 <- as.numeric(plots$ALTITUD1) * 100
plots$ALTITUD2 <- as.numeric(plots$ALTITUD2) * 100
plots$Altitude <- ifelse(is.na(plots$ALTITUD2), plots$ALTITUD1, 
                         ifelse(is.na(plots$ALTITUD1), plots$ALTITUD2, 
                                (plots$ALTITUD1 + plots$ALTITUD2) / 2))

# calculate maximum plot slope after data curation
plots$MAXPEND1 <- as.numeric(str_replace(plots$MAXPEND1, ',', '.'))
plots$MAXPEND2 <- as.numeric(str_replace(plots$MAXPEND2, ',', '.'))
plots$Slope_max <- ifelse(is.na(plots$MAXPEND2), plots$MAXPEND1,
                          ifelse(is.na(plots$MAXPEND1), plots$MAXPEND2, 
                                 ifelse(plots$MAXPEND1 > plots$MAXPEND2, plots$MAXPEND1, plots$MAXPEND2)))

# correct coordinates
plots$COORDEX <- as.numeric(plots$COORDEX) * 1000
plots$COORDEY <- as.numeric(plots$COORDEY) * 1000

# remove coordinates from Asturias, Cantabria, Navarra and Baleares
# Note: 
# these provinces were measured at the beggining of the SFNI2 project and the coordinates were not recorded correctly
prov_codes <- read.csv('1_data/1_raw/ine_provincias.csv')
plots <- merge(plots, prov_codes, by.x = 'PROVINCIA', by.y = 'Codigo', all.x = TRUE)
plots$COORDEX <- ifelse(plots$Provincia %in% c('Asturias', 'Cantabria', 'Navarra', 'Baleares'), NA, plots$COORDEX)
plots$COORDEY <- ifelse(plots$Provincia %in% c('Asturias', 'Cantabria', 'Navarra', 'Baleares'), NA, plots$COORDEY)

# delete X variable (not needed) 
plots <- plots %>%
  dplyr::select(-X)

# rename plot variables
plots <- dplyr::rename(plots, c(
  Province = PROVINCIA,
  Province_name = Provincia,
  N_plot = ESTADILLO,
  X_UTM = COORDEX,
  Y_UTM = COORDEY
)
)

# create empty additional variables
plots$Class <- NA
plots$Subclass <- NA


# Check if all plots with trees have also plot information (like position) ====

t_ids <- unique(trees$IFN_PLOT_ID)
p_ids <- unique(plots$IFN_PLOT_ID)
t_in_p <- t_ids %in% p_ids
unique(t_in_p)
t_ids_not_in_p <- t_ids[t_ids %in% p_ids == FALSE]
trees_not_in_p <- trees[trees$IFN_PLOT_ID %in% t_ids_not_in_p, ]
p_ids_not_in_t <- p_ids[p_ids %in% t_ids == FALSE]
# Note: 
# there are 66 trees from 6 different plots without plot information
# missing plot information IFN_PLOT_ID: 
# "IFN2_5_2130"  "IFN2_5_2334"  "IFN2_5_2579"  "IFN2_8_3600"  "IFN2_8_3601"  "IFN2_25_1955"
# a total of 31.156 plots have no trees



# Save results ====

save(trees, plots, file = '1_data/2_processed/tmp/0.1_IFN2_tmp.rdata')
rm(list = ls())



# Initial steps: SFNI3 ----

# load data
trees <- read.csv('1_data/1_raw/IFN3/pcmayores.csv', sep = ',')
plots_type <- read.csv('1_data/1_raw/IFN3/pcparcelas.csv', sep = ',')  # forest type
plots_age <- read.csv('1_data/1_raw/IFN3/pcespparc.csv')  # age values
plots <- read.csv('1_data/1_raw/IFN3/pcdatosmap.csv')  # coordinates all plots

# separate plot_age data in different columns according to the species position in the plot
plots_age$PosEsp <- ifelse(plots_age$PosEsp == 0, 1, plots_age$PosEsp)
plots_age_pe1 <- plots_age %>%
  filter(PosEsp == 1) %>%
  dplyr::select(-PosEsp)
plots_age_pe2 <- plots_age %>% 
  filter(PosEsp == 2) %>%
  dplyr::select(-PosEsp)
plots_age_pe3 <- plots_age %>%
  filter(PosEsp == 3) %>%
  dplyr::select(-PosEsp)
plots_age_pe4 <- plots_age %>%
  filter(PosEsp == 4) %>%
  dplyr::select(-PosEsp)
plots_age_pe12 <- merge(plots_age_pe1, plots_age_pe2, by = c('Origen', 'Estadillo', 'Cla', 'Subclase'), 
                   all.x = TRUE, suffixes = c('_pe1', '_pe2'))
plots_age_pe34 <- merge(plots_age_pe3, plots_age_pe4, by = c('Origen', 'Estadillo', 'Cla', 'Subclase'),
                   all.x = TRUE, suffixes = c('_pe3', '_pe4'))
plots_age <- merge(plots_age_pe12, plots_age_pe34, by = c('Origen', 'Estadillo', 'Cla', 'Subclase'),
                   all.x = TRUE)

# Note:
# I checked that plots IDs are unique in each data set, but some are missing on plots_age and plots
# code:
# plots$id <- paste(plots$Provincia, plots$Estadillo, plots$Cla, plots$Subclase, sep = '_')
# plots_type$id <- paste(plots_type$Provincia, plots_type$Estadillo, plots_type$Cla, plots_type$Subclase, sep = '_')
# plots_age$id <- paste(plots_age$Origen, plots_age$Estadillo, plots_age$Cla, plots_age$Subclase, sep = '_')
# length(unique(plots$id))
# length(unique(plots_type$id))
# length(unique(plots_age$id))



# Clean and merge plots data ====

plots <- plots %>% rename_with(~paste0(., "_pcd"))
plots <- dplyr::select(plots, -Subclase_pcd)
plots_age <- plots_age %>% rename_with(~paste0(., "_pce"))
plots_type <- plots_type %>% rename_with(~paste0(., "_pcp"))

# delete spaces in ID columns
plots_type$Subclase_pcp <- str_replace_all(plots_type$Subclase_pcp, ' ', '')
plots_age$Subclase_pce <- str_replace_all(plots_age$Subclase_pce, ' ', '')
trees$Subclase <- str_replace_all(trees$Subclase, ' ', '')

# plots have not Subclase column
plots <- merge(plots, plots_age, 
               by.x = c('Origen_pcd', 'Estadillo_pcd', 'Clase_pcd'), by.y = c('Origen_pce', 'Estadillo_pce', 'Cla_pce'),
               all = TRUE)
plots <- merge(plots, plots_type, 
               by.x = c('Origen_pcd', 'Estadillo_pcd', 'Clase_pcd', 'Subclase_pce'), 
               by.y = c('Origen_pcp', 'Estadillo_pcp', 'Cla_pcp', 'Subclase_pcp'), all = TRUE)

plots <- dplyr::rename(plots, Origen = Origen_pcd, Estadillo = Estadillo_pcd, 
                       Clase = Clase_pcd, Subclase = Subclase_pce)

# Note:
# after merging, some plots with common Origen, Estadillo and Clase are duplicated and with missing Subclase value
# I split the data in two data sets: one with Subclase value and the other with missing Subclase value (NA)
# I checked if all the plots of SFNI3 match with the plots with Subclase value, and they do
# after that, I saved the data without Subclase value in a file (just in case) and I removed them from the data set
psub_na <- plots[is.na(plots$Subclase),]
save(psub_na, file = '1_data/2_processed/tmp/0.1_IFN3_plots_subclase_NA.rdata')
plots <- plots[!is.na(plots$Subclase),]

rm(plots_age, plots_age_pe1, plots_age_pe2, plots_age_pe3, plots_age_pe4, plots_age_pe12, plots_age_pe34, plots_type,
   psub_na)



# Create IDs ====

trees <- trees %>%
  mutate(INVENTORY_ID = 'IFN3',
         PLOT_ID = paste(trees$Origen, trees$Estadillo, trees$Cla, trees$Subclase, sep = '_'),
         PLOT_ID_short = paste(trees$Origen, trees$Estadillo, sep = '_'),
         IFN_PLOT_ID = paste('IFN3', trees$Origen, trees$Estadillo, trees$Cla, trees$Subclase, sep = '_'),
         TREE_ID = paste(trees$Origen, trees$Estadillo, trees$Cla, trees$Subclase, nArbol, sep = '_'),
         TREE_ID_IFN2 = paste(trees$Origen, trees$Estadillo, OrdenIf2, sep = '_'),
         TREE_ID_IFN3 = paste(trees$Origen, trees$Estadillo, trees$Cla, trees$Subclase, OrdenIf3, sep = '_'),
         IFN_TREE_ID = paste('IFN3', trees$Origen, trees$Estadillo, trees$Cla, trees$Subclase, nArbol, sep = '_'))

plots <- plots %>%
  mutate(INVENTORY_ID = 'IFN3',
         PLOT_ID = paste(plots$Origen, plots$Estadillo, plots$Clase, plots$Subclase, sep = '_'),
         PLOT_ID_short = paste(plots$Origen, plots$Estadillo, sep = '_'),
         IFN_PLOT_ID = paste('IFN3', plots$Origen, plots$Estadillo, plots$Clase, plots$Subclase, sep = '_'))



# Original variables management: trees ====

# check original tree data status
str(trees)

# rename variables
trees <- dplyr::rename(trees, c(
  province = Origen,
  n_plot = Estadillo,
  class = Cla,
  subclass = Subclase,
  n_tree = nArbol,
  n_IFN2 = OrdenIf2,
  n_IFN3 = OrdenIf3,
  bearing = Rumbo,
  distance = Distanci,
  distance_reduced = DRed,
  species = Especie,
  dbh_1 = Dn1,
  dbh_2 = Dn2,
  quality = Calidad,
  shape = Forma,
  h = Ht,
  special_param = ParEsp,
  damage_agent = Agente,
  damage_level = Import,
  damage_element = Elemento,
  internal_code = Compara)
)



# Original variables management: plots ====

# check original plot data status
names(plots)
str(plots)

# calculate maximum plot slope
plots$Slope_max <- ifelse(is.na(plots$MaxPend2_pcp), plots$MaxPend1_pcp,
                          ifelse(is.na(plots$MaxPend1_pcp), plots$MaxPend2_pcp, 
                                 ifelse(plots$MaxPend1_pcp > plots$MaxPend2_pcp, plots$MaxPend1_pcp, plots$MaxPend2_pcp)))

# get province name
prov_codes <- read.csv('1_data/1_raw/ine_provincias.csv', sep = ',')
plots <- merge(plots, prov_codes, by.x = 'Provincia_pcp', by.y = 'Codigo', all.x = TRUE)

# rename plot variables
plots <- dplyr::rename(plots, c(
  Province = Provincia_pcd,
  Province_name = Provincia,
  Class = Clase,
  Subclass = Subclase,
  N_plot = Estadillo,
  X_UTM = CoorX_pcd,
  Y_UTM = CoorY_pcd
)
)



# Check if all plots with trees have also plot information (like position) ====

t_ids <- unique(trees$IFN_PLOT_ID)
p_ids <- unique(plots$IFN_PLOT_ID)
t_in_p <- t_ids %in% p_ids
unique(t_in_p)
t_ids_not_in_p <- t_ids[t_ids %in% p_ids == FALSE]
trees_not_in_p <- trees[trees$IFN_PLOT_ID %in% t_ids_not_in_p, ]
p_ids_not_in_t <- p_ids[p_ids %in% t_ids == FALSE]
# Note: 
# all trees have plot information (TRUE)
# a total of 15.748 plots have no trees



# Save results ====

save(trees, plots, file = '1_data/2_processed/tmp/0.1_IFN3_tmp.rdata')
rm(list = ls())



# Initial steps: SFNI4 ----



# load data
trees <- read.csv('1_data/1_raw/IFN4/IFN4_PCMayores.csv', sep = ';')  # all trees
plots <- read.csv('1_data/1_raw/IFN4/IFN4_PCParcelas.csv', sep = ';')  # plots
maps <- read.csv('1_data/1_raw/IFN4/IFN4_PCDatosMap.csv', sep = ';')  # maps
plots_sp <- read.csv('1_data/1_raw/IFN4/IFN4_PCEspParc.csv', sep = ';')  # plots spatial data

# separate plot data in different columns according to the species
plots_sp_pe1 <- plots_sp %>%
  filter(PosEsp == 1) %>%
  dplyr::select(-PosEsp)
plots_sp_pe2 <- plots_sp %>% 
  filter(PosEsp == 2) %>%
  dplyr::select(-PosEsp)
plots_sp_pe3 <- plots_sp %>%
  filter(PosEsp == 3) %>%
  dplyr::select(-PosEsp)

plots_sp_pe12 <- merge(plots_sp_pe1, plots_sp_pe2, by = c('Provincia', 'Estadillo', 'Cla', 'Subclase'), 
                        all.x = TRUE, suffixes = c('_pe1', '_pe2'))
plots_sp_pe3 <- plots_sp_pe3 %>% rename_with(~paste0(., "_pe3"))
plots_sp <- merge(plots_sp_pe12, plots_sp_pe3, by.x = c('Provincia', 'Estadillo', 'Cla', 'Subclase'),
                  by.y = c('Provincia_pe3', 'Estadillo_pe3', 'Cla_pe3', 'Subclase_pe3'), all.x = TRUE)

# Note:
# I checked that plots IDs are unique in each dataset, but some are missing on maps and plots_sp
# code:
# plots$id <- paste(plots$Provincia, plots$Estadillo, plots$Cla, plots$Subclase, sep = '_')
# plots$id2 <- paste(plots$Provincia, plots$Estadillo, plots$Cla, sep = '_')
# maps$id <- paste(maps$Provincia, maps$Estadillo, maps$Clase, maps$Subclase, sep = '_')
# maps$id2 <- paste(maps$Provincia, maps$Estadillo, maps$Clase, sep = '_')
# plots_sp$id <- paste(plots_sp$Provincia, plots_sp$Estadillo, plots_sp$Cla, plots_sp$Subclase, sep = '_')
# plots_sp$id2 <- paste(plots_sp$Provincia, plots_sp$Estadillo, plots_sp$Cla, sep = '_')
# length(unique(plots$id))
# length(unique(maps$id))
# length(unique(plots_sp$id))



# Clean and merge plots data ====

maps <- maps %>% rename_with(~paste0(., "_pcdm"))
maps <- select(maps, -Subclase_pcdm)
plots_sp <- plots_sp %>% rename_with(~paste0(., "_pcep"))
plots <- plots %>% rename_with(~paste0(., "_pcp"))

# delete spaces in ID columns
plots$Subclase_pcp <- str_replace_all(plots$Subclase_pcp, ' ', '')
plots_sp$Subclase_pcep <- str_replace_all(plots_sp$Subclase_pcep, ' ', '')
trees$Subclase <- str_replace_all(trees$Subclase, ' ', '')
trees$Subclase <- ifelse(trees$Subclase == '6c', '6C', trees$Subclase)

# merge plot data
plots_merged <- merge(plots_sp, maps, by.x = c('Provincia_pcep', 'Estadillo_pcep', 'Cla_pcep'), 
                      by.y = c('Provincia_pcdm', 'Estadillo_pcdm', 'Clase_pcdm'), all = TRUE)
plots <- merge(plots_merged, plots, by.x = c('Provincia_pcep', 'Estadillo_pcep', 'Cla_pcep', 'Subclase_pcep'), 
               by.y = c('Provincia_pcp', 'Estadillo_pcp', 'Cla_pcp', 'Subclase_pcp'), all = TRUE)

rm(maps, plots_sp, plots_merged, plots_sp_pe1, plots_sp_pe2, plots_sp_pe3, plots_sp_pe12)



# Note:
# after merging, some plots with common Origen, Estadillo and Clase are duplicated and with missing Subclase value
# I split the data in two data sets: one with Subclase value and the other with missing Subclase value (NA)
# I checked if all the plots of SFNI3 match with the plots with Subclase value, and they do
# after that, I saved the data without Subclase value in a file (just in case) and I removed them from the data set
psub_na <- plots[is.na(plots$Subclase_pcep),]
save(psub_na, file = '1_data/2_processed/tmp/0.1_IFN4_plots_subclase_NA.rdata')
plots <- plots[!is.na(plots$Subclase_pcep),]
plots_no_coord <- plots[is.na(plots$CoorX_pcdm),]
plots_coord <- plots[!is.na(plots$CoorX_pcdm),]
plots_no_coord <- dplyr::select(plots_no_coord, -CoorX_pcdm, -CoorY_pcdm)
psub_na <- dplyr::select(psub_na, Provincia_pcep, Estadillo_pcep, Cla_pcep, CoorX_pcdm, CoorY_pcdm)
plots_no_coord <- merge(plots_no_coord, psub_na, by = c('Provincia_pcep', 'Estadillo_pcep', 'Cla_pcep'), all.x = TRUE)
plots <- rbind(plots_coord, plots_no_coord)

rm(plots_type, psub_na, plots_coord, plots_no_coord)



# Create IDs ====

trees <- trees %>%
  mutate(INVENTORY_ID = 'IFN4',
         PLOT_ID = paste(trees$Provincia, trees$Estadillo, trees$Cla, trees$Subclase, sep = '_'),
         PLOT_ID_short = paste(trees$Provincia, trees$Estadillo, sep = '_'),
         IFN_PLOT_ID = paste('IFN4', trees$Provincia, trees$Estadillo, trees$Cla, trees$Subclase, sep = '_'),
         TREE_ID = paste(trees$Provincia, trees$Estadillo, trees$Cla, trees$Subclase, nArbol, sep = '_'),
         TREE_ID_IFN3 = paste(trees$Provincia, trees$Estadillo, trees$Cla, trees$Subclase, OrdenIf3, sep = '_'),
         TREE_ID_IFN4 = paste(trees$Provincia, trees$Estadillo, trees$Cla, trees$Subclase, OrdenIf4, sep = '_'),
         IFN_TREE_ID = paste('IFN4', trees$Provincia, trees$Estadillo, trees$Cla, trees$Subclase, nArbol, sep = '_'))

plots <- plots %>%
  mutate(INVENTORY_ID = 'IFN4',
         PLOT_ID = paste(plots$Provincia_pcep, plots$Estadillo_pcep, plots$Cla_pcep, plots$Subclase_pcep, sep = '_'),
         PLOT_ID_short = paste(plots$Provincia_pcep, plots$Estadillo_pcep, sep = '_'),
         IFN_PLOT_ID = paste('IFN4', plots$Provincia_pcep, plots$Estadillo_pcep, plots$Cla_pcep, plots$Subclase_pcep, sep = '_'))


# Original variables management: trees ====

# check original tree data status
str(trees)

# rename variables
trees <- dplyr::rename(trees, c(
  province = Provincia,
  n_plot = Estadillo,
  class = Cla,
  subclass = Subclase,
  n_tree = nArbol,
  n_IFN3 = OrdenIf3,
  n_IFN4 = OrdenIf4,
  bearing = Rumbo,
  distance = Distanci,
  species = Especie,
  dbh_1 = Dn1,
  dbh_2 = Dn2,
  quality = Calidad,
  shape = Forma,
  h = Ht,
  special_param = ParEsp,
  damage_agent = Agente,
  damage_level = Import,
  damage_element = Elemento,
  h_crown = Hcopa,
  npae = NPae,
  cpae = CPae,
  vpae = VPae)
)


# Original variables management: plots ====

# check original plot data status
names(plots)
str(plots)

# calculate mean plot altitude after data curation
# plots$ALTITUD1 <- ifelse(grepl('W', plots$ALTITUD1), 0, plots$ALTITUD1)
# plots$ALTITUD1 <- as.numeric(plots$ALTITUD1) * 100
# plots$ALTITUD2 <- as.numeric(plots$ALTITUD2) * 100
# plots$Altitude <- ifelse(is.na(plots$ALTITUD2), plots$ALTITUD1, 
#                          ifelse(is.na(plots$ALTITUD1), plots$ALTITUD2, 
#                                 (plots$ALTITUD1 + plots$ALTITUD2) / 2))

# calculate maximum plot slope
plots$Slope_max <- ifelse(is.na(plots$MaxPend2_pcp), plots$MaxPend1_pcp,
                          ifelse(is.na(plots$MaxPend1_pcp), plots$MaxPend2_pcp, 
                                 ifelse(plots$MaxPend1_pcp > plots$MaxPend2_pcp, plots$MaxPend1_pcp, plots$MaxPend2_pcp)))

# get province name
prov_codes <- read.csv('1_data/1_raw/ine_provincias.csv', sep = ',')
plots <- merge(plots, prov_codes, by.x = 'Provincia_pcep', by.y = 'Codigo', all.x = TRUE)

# rename plot variables
plots <- dplyr::rename(plots, c(
  Province = Provincia_pcep,
  Province_name = Provincia,
  N_plot = Estadillo_pcep,
  Class = Cla_pcep,
  Subclass = Subclase_pcep,
  X_UTM = CoorX_pcdm,
  Y_UTM = CoorY_pcdm)
)

# correct coordinates on plot IFN4_47_242_A_1 based on the same plot in SFNI3
plot_to_correct <- plots[plots$IFN_PLOT_ID == 'IFN4_47_242_A_1', ]
plots <- plots[plots$IFN_PLOT_ID != 'IFN4_47_242_A_1', ]
plot_to_correct$X_UTM <- 353000
plot_to_correct$Y_UTM <- 4605000
plots <- rbind(plots, plot_to_correct)



# Check if all plots with trees have also plot information (like position) ====

t_ids <- unique(trees$IFN_PLOT_ID)
p_ids <- unique(plots$IFN_PLOT_ID)
t_in_p <- t_ids %in% p_ids
unique(t_in_p)
t_ids_not_in_p <- t_ids[t_ids %in% p_ids == FALSE]
trees_not_in_p <- trees[trees$IFN_PLOT_ID %in% t_ids_not_in_p, ]
p_ids_not_in_t <- p_ids[p_ids %in% t_ids == FALSE]
# Note: 
# all trees have plot information (TRUE)
# a total of 5.182 plots have no trees
# Note: 
# all trees have plot information (TRUE)



# Save results ====

save(trees, plots, file = '1_data/2_processed/tmp/0.1_IFN4_tmp.rdata')
rm(list = ls())



# Compile 3 editions into a single dataset ====

# load data and assign edition
load('1_data/2_processed/tmp/0.1_IFN2_tmp.rdata')
plots_2 <- plots
trees_2 <- trees

load('1_data/2_processed/tmp/0.1_IFN3_tmp.rdata')
plots_3 <- plots
trees_3 <- trees

load('1_data/2_processed/tmp/0.1_IFN4_tmp.rdata')
plots_4 <- plots
trees_4 <- trees

rm(plots, trees)

# extract common variables
common_plot_vars <- intersect(names(plots_2), names(plots_3))
common_plot_vars <- intersect(common_plot_vars, names(plots_4))
common_tree_vars <- intersect(names(trees_2), names(trees_3))
common_tree_vars <- intersect(common_tree_vars, names(trees_4))

# filter common variables
plots_2 <- plots_2[, common_plot_vars]
plots_3 <- plots_3[, common_plot_vars]
plots_4 <- plots_4[, common_plot_vars]

trees_2 <- trees_2[, common_tree_vars]
trees_3 <- trees_3[, common_tree_vars]
trees_4 <- trees_4[, common_tree_vars]

# merge datasets
plots <- rbind(plots_2, plots_3, plots_4)
trees <- rbind(trees_2, trees_3, trees_4)

# order variables
plots <- plots[, c("INVENTORY_ID", "Province", "Province_name", "N_plot", "Class", "Subclass", 
                   "PLOT_ID", "PLOT_ID_short", "IFN_PLOT_ID", "X_UTM", "Y_UTM", "Slope_max")]
trees <- trees[, c("INVENTORY_ID", "province", "n_plot", "class", "subclass", "n_tree", "PLOT_ID", "PLOT_ID_short", 
                   "IFN_PLOT_ID", "TREE_ID", "IFN_TREE_ID", "species", "dbh_1", "dbh_2", "h", 
                   "distance", "bearing", "quality", "shape", "special_param"  )]



# Save results ====

save(trees, plots, file = '1_data/2_processed/0.1_sfni_harmonised.rdata')
rm(list = ls())

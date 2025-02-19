#!/usr/bin/Rscript

# Code to adapt curated data to the analysis ----
# Step 1: add useful labels to the dataset
#
# Aitor Vázquez Veloso
# 2024-09-23
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Some decisions that has been taken can be filtered by "# Note:"


# SFNI2 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls
# SFNI3 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf
# SFNI4 documentation: https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf



# working directory
setwd('')

# libraries
library(tidyverse)

# get data
load('1_data/2_processed/1.1_sfni_all_data.rdata')
species_codes <- read.csv('1_data/1_raw/SFNI4_species_codes.csv')



# Tree data cleaning for h/d models ====

# tree quality: keep 3 out of 6 degrees of quality
trees <- trees[trees$quality %in% c(1, 2, 3), ]

# tree shape: avoid trees with a lot of branches in the bottom and also sinuosity shapes
trees <- trees[trees$shape %in% c(1, 2, 3), ]

# tree special parameters - bifurcations, inclinations...
trees2 <- trees[trees$INVENTORY_ID == "IFN2", ]
trees <- trees[trees$INVENTORY_ID != "IFN2", ]
trees2a <- trees2[substr(trees2$special_param, start = 1, stop = 1) %in% c(1, 2), ]
trees2b <- trees2[is.na(trees2$special_param), ]
trees2 <- rbind(trees2a, trees2b)
trees <- trees[!substr(trees$special_param, start = 1, stop = 2) %in% c(30, 39, 41, 42, 45, 49), ]
trees <- rbind(trees, trees2)
rm(trees2a, trees2b, trees2)
# Note: version 2 didn't use the same notation rules as version 3 and 4

# tree special parameters - resinated trees
trees <- trees[!trees$special_param %in% c(21:28), ]

# tree special parameters - debarked trees (Quercus suber)
trees <- trees[!trees$special_param %in% c(10:13), ]

# label to conifer and broadleaf trees
trees$tree_type <- ifelse(trees$species %in% c(14, 17:39, 217:238, 317:337, 435, 436), 'conifer', 'broadleaved')

# clean species without a name in the documentation or with NAs
trees <- trees[trees$species %in% species_codes$Codigo_IFN,]

# clean trees with non common values for dbh and height (height too low or too high ~ dbh)
trees <- trees[!(trees$dbh > 40 & trees$species == 8), ]  # Phillyrea latifolia
trees <- trees[!(trees$dbh > 20 & trees$h < 10 & trees$species == 8), ]  # Phillyrea latifolia
trees <- trees[!(trees$dbh > 120 & trees$h < 20 & trees$species == 21), ]  # Pinus sylvestris
trees <- trees[!(trees$dbh > 35 & trees$h < 4 & trees$species == 22), ]  # Pinus uncinata
trees <- trees[!(trees$dbh > 43 & trees$h > 32 & trees$species == 24), ]  # Pinus halepensis
trees <- trees[!(trees$dbh > 82 & trees$h < 12 & trees$species == 31), ]  # Abies alba
trees <- trees[!(trees$dbh > 140 & trees$species == 31), ]  # Abies alba
trees <- trees[!(trees$dbh > 100 & trees$species == 36), ]  # Cupressus sempervirens
trees <- trees[!(trees$h > 18 & trees$species == 38), ]  # Juniperus thurifera
trees <- trees[!(trees$dbh > 60 & trees$h < 4 & trees$species == 41), ]  # Quercus robur
trees <- trees[!(trees$dbh > 120 & trees$h < 15 & trees$species == 41), ]  # Quercus robur
trees <- trees[!(trees$dbh > 180 & trees$species == 42), ]  # Quercus petraea
trees <- trees[!(trees$h > 40 & trees$species == 42), ]  # Quercus petraea
trees <- trees[!(trees$dbh > 100 & trees$h < 10 & trees$species == 43), ]  # Quercus pyrenaica
trees <- trees[!(trees$dbh > 90 & trees$species == 45), ]  # Quercus ilex
trees <- trees[!(trees$dbh > 110 & trees$h > 20 & trees$species == 46), ]  # Quercus suber
trees <- trees[!(trees$dbh > 90 & trees$h > 15.5 & trees$species == 46), ]  # Quercus suber
trees <- trees[!(trees$dbh > 35 & trees$h < 5 & trees$species == 47), ]  # Quercus canariensis
trees <- trees[!(trees$dbh > 110 & trees$species == 48), ]  # Quercus rubra
trees <- trees[!(trees$dbh > 100 & trees$species == 51), ]  # Populus alba
trees <- trees[!(trees$dbh > 80 & trees$species == 52), ]  # Populus tremula
trees <- trees[!(trees$dbh > 95 & trees$species == 54), ]  # Alnus glutinosa
trees <- trees[!(trees$dbh > 100 & trees$species == 55), ]  # Fraxinus angustifolia
trees <- trees[!(trees$dbh > 80 & trees$species == 57), ]  # Salix spp.
trees <- trees[!(trees$dbh > 150 & trees$species == 58), ]  # Populus nigra
trees <- trees[!(trees$dbh > 200 & trees$species == 61), ]  # Eucalyptus globulus
trees <- trees[!(trees$dbh > 60 & trees$h < 12 & trees$species == 61), ]  # Eucalyptus globulus
trees <- trees[!(trees$dbh > 40 & trees$h < 6 & trees$species == 61), ]  # Eucalyptus globulus
trees <- trees[!(trees$dbh > 120 & trees$species == 64), ]  # Eucalyptus nitens
trees <- trees[!(trees$h > 20 & trees$species == 65), ]  # Ilex aquifolium
trees <- trees[!(trees$dbh > 143 & trees$species == 71), ]  # Fagus sylvatica
trees <- trees[!(trees$dbh > 125 & trees$species == 72), ]  # Castanea sativa
trees <- trees[!(trees$dbh > 110 & trees$h < 13 & trees$species == 72), ]  # Castanea sativa
trees <- trees[!(trees$dbh > 80 & trees$species == 75), ]  # Juglans regia
trees <- trees[!(trees$dbh > 75 & trees$h < 15 & trees$species == 77), ]  # Tilia spp.
trees <- trees[!(trees$dbh > 30 & trees$h < 7 & trees$species == 79), ]  # Platanus hispanica
trees <- trees[!(trees$dbh > 70 & trees$h < 20 & trees$species == 79), ]  # Platanus hispanica
trees <- trees[!(trees$dbh > 70 & trees$species == 92), ]  # Robinia pseudoacacia
trees <- trees[!(trees$h < 2 & trees$species == 94), ]  # Laurus nobilis
trees <- trees[!(trees$h < 2.5 & trees$species == 95), ]  # Prunus spp.
trees <- trees[!(trees$dbh > 69 & trees$h < 20 & trees$species == 207), ]  # Acacia melanoxylon
trees <- trees[!(trees$dbh > 90 & trees$species == 243), ]  # Quercus pubescens
trees <- trees[!(trees$dbh > 50 & trees$h < 17 & trees$species == 256), ]  # Ulmus glabra
trees <- trees[!(trees$dbh > 115 & trees$species == 257), ]  # Salix alba
trees <- trees[!(trees$dbh > 70 & trees$h < 18 & trees$species == 258), ]  # Populus x canadensis
trees <- trees[!(trees$dbh > 70 & trees$species == 273), ]  # Betula alba
trees <- trees[!(trees$h > 33 & trees$species == 273), ]  # Betula alba
trees <- trees[!(trees$dbh > 40 & trees$species == 276), ]  # Acer monspessulanum
trees <- trees[!(trees$dbh > 35 & trees$h < 10 & trees$species == 278), ]  # Sorbus aria
trees <- trees[!(trees$dbh > 40 & trees$species == 307), ]  # Acacia dealbata
trees <- trees[!(trees$dbh > 110 & trees$species == 364), ]  # Eucalyptus gomphocephala
trees <- trees[!(trees$dbh > 50 & trees$h < 15 & trees$species == 373), ]  # Betula pendula
trees <- trees[!(trees$dbh > 60 & trees$species == 373), ]  # Betula pendula
trees <- trees[!(trees$h > 33 & trees$species == 373), ]  # Betula pendula
trees <- trees[!(trees$dbh > 55 & trees$h < 15 & trees$species == 395), ]  # Prunus avium
trees <- trees[!(trees$dbh > 65 & trees$h < 20 & trees$species == 395), ]  # Prunus avium
trees <- trees[!(trees$h > 25 & trees$species == 469), ]  # Phoenix canariensis
trees <- trees[!(trees$dbh > 75 & trees$species == 576), ]  # ACer pseudoplatanus
trees <- trees[!(trees$dbh > 50 & trees$species == 578), ]  # Sorbus torminalis
trees <- trees[!(trees$dbh > 49 & trees$species == 657), ]  # Salix caprea
trees <- trees[!(trees$dbh > 70 & trees$species == 857), ]  # Salix fragilis

trees <- trees[!trees$species %in% c(19, 29, 59, 87, 99), ]  # Otras coníferas/pinos/ripícolas/frondosas, Ocotea foetens



# Plot data cleaning for h/d models ====

# split data by SFNI edition and rescue variables needed from original data
plots2 <- plots[plots$INVENTORY_ID == "IFN2", ]
plots2 <- plots2[!duplicated(plots2), ]
plots3 <- plots[plots$INVENTORY_ID == "IFN3", ]
plots3 <- plots3[!duplicated(plots3), ]
plots4 <- plots[plots$INVENTORY_ID == "IFN4", ]
plots4 <- plots4[!duplicated(plots4), ]

# origin of the stand
# plots2$Stand_origin <- 'not available'  # documentador IFN2 page 102 - not clear information
# plots2$Stand_artificial_origin <- 'not available'
plots3$Stand_origin <- ifelse(plots3$SFNI3_OrgMasa1_pe1_pce == 1, 'natural', 
                              ifelse(plots3$SFNI3_OrgMasa1_pe1_pce == 2, 'artificial', 'naturalised'))
plots3$Stand_artificial_origin <- ifelse(plots3$SFNI3_OrgMasa2_pe1_pce == 1, 'seedling', 
                                         ifelse(plots3$SFNI3_OrgMasa2_pe1_pce == 2, 'plantation', 
                                                ifelse(plots3$SFNI3_OrgMasa2_pe1_pce == 3, 'resprout', 
                                                       ifelse(plots3$SFNI3_OrgMasa2_pe1_pce == 4, 'seedling and resprout', 
                                                              ifelse(plots3$SFNI3_OrgMasa2_pe1_pce == 5, 'seedling and plantation',
                                                                     'plantation and respout')))))
plots4$Stand_origin <- ifelse(plots4$SFNI4_OrgMasa1_pe1_pcep == 1, 'natural', 
                              ifelse(plots4$SFNI4_OrgMasa1_pe1_pcep == 2, 'artificial', 'naturalised'))
plots4$Stand_artificial_origin <- ifelse(plots4$SFNI4_OrgMasa2_pe1_pcep == 1, 'seedling', 
                                         ifelse(plots4$SFNI4_OrgMasa2_pe1_pcep == 2, 'plantation', 
                                                ifelse(plots4$SFNI4_OrgMasa2_pe1_pcep == 3, 'resprout', 
                                                       ifelse(plots4$SFNI4_OrgMasa2_pe1_pcep == 4, 'seedling and resprout', 
                                                              ifelse(plots4$SFNI4_OrgMasa2_pe1_pcep == 5, 'seedling and plantation',
                                                                     'plantation and respout')))))

# pure vs mix stands
plots2$Species_mixture <- ifelse(!is.na(plots2$sp_2) & (plots2$G_sp_2/plots2$G > 0.15), 'mix', 'pure')
plots3$Species_mixture <- ifelse(!is.na(plots3$sp_2) & (plots3$G_sp_2/plots3$G > 0.15), 'mix', 'pure')
plots4$Species_mixture <- ifelse(!is.na(plots4$sp_2) & (plots4$G_sp_2/plots4$G > 0.15), 'mix', 'pure')

# stand type
# plots2$Stand_type <- 'not available'
plots3$Stand_type <- ifelse(plots3$SFNI3_TratMasa_pe1_pce == 1, 'high forest',
                            ifelse(plots3$SFNI3_TratMasa_pe1_pce == 2, 'medium forest', 'coppice forest'))
plots4$Stand_type <- ifelse(plots4$SFNI4_TratMasa_pe1_pcep == 1, 'high forest',
                            ifelse(plots4$SFNI4_TratMasa_pe1_pcep == 2, 'medium forest', 'coppice forest'))

# age class
# plots2$Age_class <- 'not available'
plots3$Age_class <- ifelse(plots3$SFNI3_FPMasa_pe1_pce == 1, 'seedling stage',
                           ifelse(plots3$SFNI3_Edad_pe1_pce == 2, 'thicket stage', 
                                  ifelse(plots3$SFNI3_Edad_pe1_pce == 3, 'pole stage', 'timber stage')))
plots4$Age_class <- ifelse(plots4$SFNI4_FPMasa_pe1_pcep == 1, 'seedling stage',
                           ifelse(plots4$SFNI4_Edad_pe1_pcep == 2, 'thicket stage', 
                                  ifelse(plots4$SFNI4_Edad_pe1_pcep == 3, 'pole stage', 'timber stage')))

# forest structure
# plots2$Structure <- 'not available'
plots3$Structure <- 
  ifelse(plots3$SFNI3_Nivel2_pcp %in% c(1, 2) & plots3$SFNI3_Nivel3_pcp == 3, 'dehesa', 
         ifelse(plots3$SFNI3_Nivel2_pcp %in% c(1, 2) & plots3$SFNI3_Nivel3_pcp == 4, 'non forest', 
                ifelse(plots3$SFNI3_Nivel2_pcp == 3 & plots3$SFNI3_Nivel3_pcp == 1, 'harvested', 
                       ifelse(plots3$SFNI3_Nivel2_pcp == 3 & plots3$SFNI3_Nivel3_pcp == 2, 'burned', 
                              ifelse(plots3$SFNI3_Nivel2_pcp == 3 & plots3$SFNI3_Nivel3_pcp == 3, 'natural damages', 
                                     ifelse(plots3$SFNI3_Nivel2_pcp == 6 & plots3$SFNI3_Nivel3_pcp == 1, 'riparian',
                                            ifelse(plots3$SFNI3_Nivel2_pcp == 6 & plots3$SFNI3_Nivel3_pcp == 3, 'alineations', 
                                                   ifelse(plots3$SFNI3_Nivel2_pcp == 7 & plots3$SFNI3_Nivel3_pcp == 7, 'dehesa', 
                                                          ifelse(plots3$SFNI3_Nivel2_pcp == 7 & plots3$SFNI3_Nivel3_pcp == 8, 
                                                                 'non forest', '')))))))))
plots4$Structure <- ifelse(plots4$SFNI4_TipEstr_pcdm == 13, 'dehesa', 
                           ifelse(plots4$SFNI4_TipEstr_pcdm == 14, 'riparian', 
                                  ifelse(plots4$SFNI4_TipEstr_pcdm == 14, 'alineations', 
                                         ifelse(plots4$SFNI4_TipEstr_pcdm == 101, 'harvested', 
                                                ifelse(plots4$SFNI4_TipEstr_pcdm == 102, 'burned', 
                                                       ifelse(plots4$SFNI4_TipEstr_pcdm == 103, 'firebreak', ''))))))

# complete SFNI2 database using permanent plots
p3_in_2 <- plots3[plots3$PLOT_ID_short %in% plots2$PLOT_ID_short, ]
p3_in_2 <- p3_in_2[p3_in_2$Class == 'A' & p3_in_2$Subclass == '1', ]
p3_in_2 <- dplyr::select(p3_in_2, PLOT_ID_short, Stand_origin, Stand_artificial_origin, Stand_type, Age_class, 
                         Structure)
plots2 <- merge(plots2, p3_in_2, by = 'PLOT_ID_short', all.x = TRUE)

# merge data
plots <- rbind(plots2, plots3, plots4)
rm(plots2, plots3, plots4, species_codes, p3_in_2)

# remove damaged plots
plots <- plots[!plots$Structure %in% c('alineations', 'harvested', 'burned', 'firebreak', 'natural damages', 
                                       'non forest', 'dehesa'), ]



# Tree and plot filters ====

# filter trees with plot data and the other way around
trees <- trees[trees$IFN_PLOT_ID %in% plots$IFN_PLOT_ID, ]
plots <- plots[plots$IFN_PLOT_ID %in% trees$IFN_PLOT_ID, ]



# Save filtered results ====
save(plots, trees, file = '1_data/2_processed/1.2_sfni_all_data_to_hd_models.rdata')

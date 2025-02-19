#!/usr/bin/Rscript

# Functions to manage SFNI data ----
#
# Aitor Vázquez Veloso
# 2024-09-12
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Load SFNI data previously processed
load_IFN_processed <- function(){
  
  # working directory
  setwd('SFNI_Spanish_Forest_National_Inventory-ready_to_use/')
  
  # IFN2 data
  load("1_data/2_processed/IFN2_data.RData")
  plots2 <- plots
  trees2 <- trees
  
  # IFN3 data
  load("1_data/2_processed/IFN3_data.RData")
  plots3 <- plots
  trees3 <- trees
  
  # IFN4 data
  load("1_data/2_processed/IFN4_data.RData")
  plots4 <- plots
  trees4 <- trees
  
  rm(plots, trees)
  
  # return data in a list
  return(list(plots2 = plots2, trees2 = trees2,
              plots3 = plots3, trees3 = trees3,
              plots4 = plots4, trees4 = trees4))
}

# Filter data

filter_IFN <- function(df, model, plots_vars, tree_vars){
  
  # print a message
  print(paste('Filtering data for', unique(model[[1]])))
  
  # load data
  ifelse(df == '', df <- load_IFN_processed(), df <- df)
    
  # unpack data
  plots2 <- df[[1]]
  trees2 <- df[[2]]
  plots3 <- df[[3]]
  trees3 <- df[[4]]
  plots4 <- df[[5]]
  trees4 <- df[[6]]
  
  # filter inventories (just desired species, not even trees in the same plot but different species)
  my_trees_2 <- trees2[trees2$species %in% model[[2]] & trees2$province %in% model[[3]], ]
  my_plots_2 <- plots2[plots2$PLOT_ID %in% my_trees_2$PLOT_ID, ]
  my_trees_3 <- trees3[trees3$species %in% model[[2]] & trees3$province %in% model[[3]], ]
  my_plots_3 <- plots3[plots3$PLOT_ID %in% my_trees_3$PLOT_ID, ]
  my_trees_4 <- trees4[trees4$species %in% model[[2]] & trees4$province %in% model[[3]], ]
  my_plots_4 <- plots4[plots4$PLOT_ID %in% my_trees_4$PLOT_ID, ]
  
  # select desired variables
  my_plots_2 <- dplyr::select(my_plots_2, all_of(plots_vars))
  my_trees_2 <- dplyr::select(my_trees_2, all_of(tree_vars))
  my_plots_3 <- dplyr::select(my_plots_3, all_of(plots_vars))
  my_trees_3 <- dplyr::select(my_trees_3, all_of(tree_vars))
  my_plots_4 <- dplyr::select(my_plots_4, all_of(plots_vars))
  my_trees_4 <- dplyr::select(my_trees_4, all_of(tree_vars))
  
  # group filtered data
  my_plots <- rbind(my_plots_2, my_plots_3, my_plots_4)
  my_trees <- rbind(my_trees_2, my_trees_3, my_trees_4)
  
  # skip dead trees, expan = 0 and height = 0
  my_trees <- my_trees[my_trees$dead == 0 & my_trees$expan != 0 & my_trees$h > 0, ]
  my_plots <- my_plots[my_plots$IFN_PLOT_ID %in% my_trees$IFN_PLOT_ID, ]
  my_trees <- my_trees[my_trees$IFN_PLOT_ID %in% my_plots$IFN_PLOT_ID, ]
  
  # return filtered data
  return(list(plots = my_plots, trees = my_trees))
}

filter_mix_IFN <- function(df, model, plots_vars, tree_vars){
  
  # print a message
  print(paste('Filtering data for', unique(model[[1]])))
  
  # load data
  ifelse(df == '', df <- load_IFN_processed(), df <- df)
  
  # unpack data
  plots2 <- df[[1]]
  trees2 <- df[[2]]
  plots3 <- df[[3]]
  trees3 <- df[[4]]
  plots4 <- df[[5]]
  trees4 <- df[[6]]
  
  # filter inventories (just desired species, not even trees in the same plot but different species)
  my_plots_2a <- plots2[plots2$sp_1 == model[[2]] & plots2$sp_2 == model[[3]], ]
  my_plots_2b <- plots2[plots2$sp_1 == model[[3]] & plots2$sp_2 == model[[2]], ]
  my_plots_2 <- rbind(my_plots_2a, my_plots_2b)
  my_plots_2 <- my_plots_2[!is.na(my_plots_2$IFN_PLOT_ID), ]
  my_trees_2 <- trees2[trees2$IFN_PLOT_ID %in% my_plots_2$IFN_PLOT_ID, ]
  
  my_plots_3a <- plots3[plots3$sp_1 == model[[2]] & plots3$sp_2 == model[[3]], ]
  my_plots_3b <- plots3[plots3$sp_1 == model[[3]] & plots3$sp_2 == model[[2]], ]
  my_plots_3 <- rbind(my_plots_3a, my_plots_3b)
  my_plots_3 <- my_plots_3[!is.na(my_plots_3$IFN_PLOT_ID), ]
  my_trees_3 <- trees3[trees3$IFN_PLOT_ID %in% my_plots_3$IFN_PLOT_ID, ]
  
  my_plots_4a <- plots4[plots4$sp_1 == model[[2]] & plots4$sp_2 == model[[3]], ]
  my_plots_4b <- plots4[plots4$sp_1 == model[[3]] & plots4$sp_2 == model[[2]], ]
  my_plots_4 <- rbind(my_plots_4a, my_plots_4b)
  my_plots_4 <- my_plots_4[!is.na(my_plots_4$IFN_PLOT_ID), ]
  my_trees_4 <- trees4[trees4$IFN_PLOT_ID %in% my_plots_4$IFN_PLOT_ID, ]
  
  # select desired variables
  my_plots_2 <- dplyr::select(my_plots_2, all_of(plots_vars))
  my_trees_2 <- dplyr::select(my_trees_2, all_of(tree_vars))
  my_plots_3 <- dplyr::select(my_plots_3, all_of(plots_vars))
  my_trees_3 <- dplyr::select(my_trees_3, all_of(tree_vars))
  my_plots_4 <- dplyr::select(my_plots_4, all_of(plots_vars))
  my_trees_4 <- dplyr::select(my_trees_4, all_of(tree_vars))
  
  # group filtered data
  my_plots <- rbind(my_plots_2, my_plots_3, my_plots_4)
  my_trees <- rbind(my_trees_2, my_trees_3, my_trees_4)
  
  # skip dead trees, expan = 0 and height = 0
  my_trees <- my_trees[my_trees$dead == 0 & my_trees$expan != 0 & my_trees$h > 0, ]
  my_plots <- my_plots[my_plots$IFN_PLOT_ID %in% my_trees$IFN_PLOT_ID, ]
  my_trees <- my_trees[my_trees$IFN_PLOT_ID %in% my_plots$IFN_PLOT_ID, ]
  
  # return filtered data
  return(list(plots = my_plots, trees = my_trees))
}

# SIMANFOR models data filters
simanfor_data <- function(model){
  
  # species code
  bpubescens <- 273
  cladanifer <- 1101
  fsylvatica <- 71
  phalepensis <- 24
  pnigra <- 25
  ppinaster <- 26
  ppinea <- 23
  pradiata <- 28
  psylvestris <- 21
  puncinata <- 22
  qfaginea <- 44
  qilex <- 45
  qpetraea <- 42
  qpyrenaica <- 43
  qrobur <- 41
  qsuber <- 46

  # models code
  all_models <- tribble()
  
  # stand models
  all_models <- rbind(all_models, tibble('model' = 'Phalepensis_ar', 'species' = phalepensis, 'provinces' = get_provinces_code('', c('Huesca', 'Teruel', 'Zaragoza'))))
  all_models <- rbind(all_models, tibble('model' = 'Pradiata_gal', 'species' = pradiata, 'provinces' = get_provinces_code('', c('Coruña', 'Lugo', 'Ourense', 'Pontevedra'))))
  all_models <- rbind(all_models, tibble('model' = 'Qpyrenaica_cyl', 'species' = qpyrenaica, 'provinces' = get_provinces_code('', c('Ávila', 'Burgos', 'León', 'Palencia', 'Salamanca', 'Segovia', 'Soria', 'Valladolid', 'Zamora'))))
  all_models <- rbind(all_models, tibble('model' = 'Bpubescens', 'species' = bpubescens, 'provinces' = get_provinces_code('', c('Coruña', 'Lugo', 'Ourense', 'Pontevedra'))))
  all_models <- rbind(all_models, tibble('model' = 'Cladanifer', 'species' = cladanifer, 'provinces' = get_provinces_code('', c('Zamora'))))
  all_models <- rbind(all_models, tibble('model' = 'Pnigra_cyl', 'species' = pnigra, 'provinces' = get_provinces_code('', c('Ávila', 'Burgos', 'León', 'Palencia', 'Salamanca', 'Segovia', 'Soria', 'Valladolid', 'Zamora'))))
  all_models <- rbind(all_models, tibble('model' = 'Ppinaster_gal_coast', 'species' = ppinaster, 'provinces' = get_provinces_code('', c('Coruña', 'Lugo', 'Asturias', 'Pontevedra'))))
  all_models <- rbind(all_models, tibble('model' = 'Ppinaster_gal_inland', 'species' = ppinea, 'provinces' = get_provinces_code('', c('León', 'Lugo', 'Ourense'))))
  all_models <- rbind(all_models, tibble('model' = 'Psylvestris_gal', 'species' = psylvestris, 'provinces' = get_provinces_code('', c('Coruña', 'Lugo', 'Ourense', 'Pontevedra'))))
  all_models <- rbind(all_models, tibble('model' = 'Ppinaster_spain', 'species' = ppinaster, 'provinces' = get_provinces_code('', 'all')))
  all_models <- rbind(all_models, tibble('model' = 'Qpetraea_ccant', 'species' = qpetraea, 'provinces' = get_provinces_code('', c('León', 'Palencia'))))
  all_models <- rbind(all_models, tibble('model' = 'Qrobur_gal', 'species' = qrobur, 'provinces' = get_provinces_code('', c('Coruña', 'Lugo', 'Ourense', 'Pontevedra'))))
  all_models <- rbind(all_models, tibble('model' = 'SILVES_mad', 'species' = psylvestris, 'provinces' = get_provinces_code('', c('Madrid'))))
  all_models <- rbind(all_models, tibble('model' = 'SILVES_sisc', 'species' = psylvestris, 'provinces' = get_provinces_code('', c('Madrid', 'Segovia', 'Soria', 'Burgos'))))
  all_models <- rbind(all_models, tibble('model' = 'Ppinaster_gal', 'species' = ppinaster, 'provinces' = get_provinces_code('', c('Coruña', 'Lugo', 'Ourense', 'Pontevedra'))))

  # return filtered data
  return(all_models[all_models$model == model, ])
}

# SIMANFOR mix stands models data filters
simanfor_mix_data <- function(species_1, species_2){
  
  # create mixture name
  if(species_1 == 'all' & species_2 == 'all'){
    mixture <- 'all'
  } else{
    mixture_1 <- paste(species_1, species_2, sep = '')
    mixture_2 <- paste(species_2, species_1, sep = '')
    mixture <- c(mixture_1, mixture_2)
  }
  
  # species code
  bpubescens <- 273
  cladanifer <- 1101
  fsylvatica <- 71
  phalepensis <- 24
  pnigra <- 25
  ppinaster <- 26
  ppinea <- 23
  pradiata <- 28
  psylvestris <- 21
  puncinata <- 22
  qfaginea <- 44
  qilex <- 45
  qpetraea <- 42
  qpyrenaica <- 43
  qrobur <- 41
  qsuber <- 46
  
  # models code
  all_models <- tribble()
  
  # stand models - table 2 of https://www.mdpi.com/1999-4907/13/1/119
  all_models <- rbind(all_models, tibble('mixture' = 'PhalPnig', 'species_1' = phalepensis, 'species_2' = pnigra))
  all_models <- rbind(all_models, tibble('mixture' = 'PhalPpinaster', 'species_1' = phalepensis, 'species_2' = ppinaster))
  all_models <- rbind(all_models, tibble('mixture' = 'PhalPpinea', 'species_1' = phalepensis, 'species_2' = ppinea))
  all_models <- rbind(all_models, tibble('mixture' = 'PhalQfag', 'species_1' = phalepensis, 'species_2' = qfaginea))
  all_models <- rbind(all_models, tibble('mixture' = 'PhalQile', 'species_1' = phalepensis, 'species_2' = qilex))
  
  all_models <- rbind(all_models, tibble('mixture' = 'PnigPpinaster', 'species_1' = pnigra, 'species_2' = ppinaster))
  all_models <- rbind(all_models, tibble('mixture' = 'PnigPsyl', 'species_1' = pnigra, 'species_2' = psylvestris))
  all_models <- rbind(all_models, tibble('mixture' = 'PnigQfag', 'species_1' = pnigra, 'species_2' = qfaginea))
  all_models <- rbind(all_models, tibble('mixture' = 'PnigQile', 'species_1' = pnigra, 'species_2' = qilex))
  
  all_models <- rbind(all_models, tibble('mixture' = 'PpinasterPpinea', 'species_1' = ppinaster, 'species_2' = ppinea))
  all_models <- rbind(all_models, tibble('mixture' = 'PpinasterPsyl', 'species_1' = ppinaster, 'species_2' = psylvestris))
  all_models <- rbind(all_models, tibble('mixture' = 'PpinasterQile', 'species_1' = ppinaster, 'species_2' = qilex))
  all_models <- rbind(all_models, tibble('mixture' = 'PpinasterQpyr', 'species_1' = ppinaster, 'species_2' = qpyrenaica))
  all_models <- rbind(all_models, tibble('mixture' = 'PpinasterQsub', 'species_1' = ppinaster, 'species_2' = qsuber))
  
  all_models <- rbind(all_models, tibble('mixture' = 'PpineaQile', 'species_1' = ppinea, 'species_2' = qilex))
  all_models <- rbind(all_models, tibble('mixture' = 'PpineaQsub', 'species_1' = ppinea, 'species_2' = qsuber))

  all_models <- rbind(all_models, tibble('mixture' = 'PsylFsyl', 'species_1' = psylvestris, 'species_2' = fsylvatica))
  all_models <- rbind(all_models, tibble('mixture' = 'PsylPunc', 'species_1' = psylvestris, 'species_2' = puncinata))
  all_models <- rbind(all_models, tibble('mixture' = 'PsylQfag', 'species_1' = psylvestris, 'species_2' = qfaginea))
  all_models <- rbind(all_models, tibble('mixture' = 'PsylQile', 'species_1' = psylvestris, 'species_2' = qilex))
  all_models <- rbind(all_models, tibble('mixture' = 'PsylQpet', 'species_1' = psylvestris, 'species_2' = qpetraea))
  all_models <- rbind(all_models, tibble('mixture' = 'PsylQpyr', 'species_1' = psylvestris, 'species_2' = qpyrenaica))
  
  all_models <- rbind(all_models, tibble('mixture' = 'FsylQpet', 'species_1' = fsylvatica, 'species_2' = qpetraea))
  all_models <- rbind(all_models, tibble('mixture' = 'FsylQpyr', 'species_1' = fsylvatica, 'species_2' = qpyrenaica))
  all_models <- rbind(all_models, tibble('mixture' = 'FsylQrob', 'species_1' = fsylvatica, 'species_2' = qrobur))

  all_models <- rbind(all_models, tibble('mixture' = 'QfagQile', 'species_1' = qfaginea, 'species_2' = qilex))
  
  all_models <- rbind(all_models, tibble('mixture' = 'QileQpyr', 'species_1' = qilex, 'species_2' = qpyrenaica))
  all_models <- rbind(all_models, tibble('mixture' = 'QileQsub', 'species_1' = qilex, 'species_2' = qsuber))
  
  all_models <- rbind(all_models, tibble('mixture' = 'QpyrQrob', 'species_1' = qpyrenaica, 'species_2' = qrobur))

  # return filtered data
  ifelse(mixture == 'all', return(all_models), return(all_models[all_models$mixture == mixture[1] | all_models$mixture == mixture[2], ]))
}

# get provinces code
get_provinces_code <- function(path, provinces){
  # get path and read data
  ifelse(path == '', path <- 'SFNI_Spanish_Forest_National_Inventory-ready_to_use/1_data/1_raw/ine_provincias.csv', path <- path)
  provinces_list <- read.csv(path)
  # return codes
  ifelse(provinces == 'all', return(provinces_list$Codigo),
  return(provinces_list$Codigo[provinces_list$Provincia %in% c(provinces)]))
}

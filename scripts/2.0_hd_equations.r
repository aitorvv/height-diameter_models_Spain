#!/usr/bin/Rscript

# Code to get hd models ----
# Step 0: Height-diameter equations compilation
#
# Aitor Vázquez Veloso
# 2024-09-10
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# cabanillas_bases_2010: 30 models ====

#' @phdthesis{cabanillas_bases_2010,
#'   type = {{PhD} {Thesis}},
#'   title = {Bases para la gestión de masas naturales de \textit{{Pinus} halepensis} {Mill}. en el {Valle} del {Ebro}},
#'   url = {http://oa.upm.es/4960/},
#'   school = {Universidad Politécnica de Madrid},
#'   author = {Cabanillas, A.},
#'   year = {2010},
#'   annote = {[In Spanish]. Available at http://oa.upm.es/4960/},
#' }

curtis_1967 <- function(b0, b1, b2, b3, dbh, dg, t) {
  h <- 10 * (b0 + b1 * (1/dbh) + b2 * (1/t) + b3 * (1/(dg * t)))
  return(h)
}

cox_1994 <- function(b0, b1, b2, b3, dbh, dg, N) {
  h <- exp(b0 + b1 * log(dg) + b2 * log(N) + b3 * sqrt(dbh))
  return(h)
}

clutter_allison_1974 <- function(b0, b1, b2, b3, b4, dbh, t, N) {
  h <- 1.3 + 10 * (b0 + b1 * (1/dbh) + b2 * (1/sqrt(t)) + b3 * (1/(dbh * sqrt(t))) + b4 * log(N)/sqrt(t))
  return(h)
}

monness_1982 <- function(b0, dbh, D0, H0) {
  h <- 1.3 + b0 * ((1/dbh) - (1/D0)) + (1 / (H0 - 1.3))^(1/3) - 3
  return(h)
}

canadas_i_1999 <- function(b0, dbh, D0, H0) {
  h <- 1.3 + (H0 - 1.3) * (dbh / D0)^b0
  return(h)
}

canadas_ii_1999 <- function(dbh, D0, H0, b0) {
  h <- 1.3 + (dbh / ((D0 / (H0 - 1.3)) + b0 * (D0 - dbh)))
  return(h)
}

canadas_iii_1999 <- function(H0, b0, dbh, D0) {
  h <- 1.3 + (H0 - 1.3) * (1 - exp(b0 * dbh)) / (1 - exp(b0 * D0))
  return(h)
}

canadas_iv_1999 <- function(H0, b0, dbh, D0) {
  term1 <- b0 * (1/dbh - 1/D0)
  term2 <- 1 / (H0 - 1.3) ^ (1/2)
  h <- 1.3 + (term1 + term2) ^ (-2)
  return(h)
}

gaffrey_1988 <- function(H0, dg, dbh, b0, b1) {
  h = 1.3 + (H0 - 1.3) * exp(b0 * (1 - (dg / dbh)) + b1 * ((1 / dg) - (1 / dbh)))
  return(h)
}

sloboda_1993 <- function(Hm, dbh, dg, b0, b1) {
  h = 1.3 + (Hm - 1.3) * exp(b0 * (1 - (dbh / dg))) * exp(b1 * ((dbh / dg) - (1 / dbh)))
  return(h)
}

sloboda_1993_mod <- function(Hm, dbh, dg, b0, b1) {
  h = 1.3 + (Hm - 1.3) * exp(b0 * (1 - (dg / dbh))) * exp(b1 * ((dbh / dg) - (1 / dbh)))
  return(h)
}

harrison_1986 <- function(H0, dbh, b0, b1, b2) {
  h = H0 * (1 + b0 * exp(b1 * H0)) * (1 - exp(-b2 * dbh / H0))
  return(h)
}

pienaar_1991_mod <- function(H0, dbh, dg, b0, b1, b2) {
  h = b0 * H0 * (1 - exp(-b1 * dbh / dg))^b2
  return(h)
}

hui_gadow_1993_I <- function(H0, dbh, b0, b1, b2, b3) {
  h = 1.3 + b0 * H0^b1 * dbh^(b2 * H0^b3)
  return(h)
}

mirkovich_1958 <- function(H0, dg, dbh, b0, b1, b2, b3) {
  h = 1.3 + (b0 + b1 * H0 - b2 * dg) * exp(-b3 / dbh)
  return(h)
}

schroder_alvarez_2001_I <- function(H0, dg, dbh, b0, b1, b2, b3) {
  h = 1.3 + (b0 + b1 * H0 - b2 * dg) * exp(-b3 / sqrt(dbh))
  return(h)
}

cox_III_1994_mod <- function(Hm, dg, dbh, N, b0, b1, b2, b3, b4) {
  h = Hm * (b0 + b1 * Hm + b2 * (Hm / dg) + b3 * dbh + b4 * (N / ((dg * Hm * dg) / dbh)))
  return(h)
}

schroder_alvarez_2001_II <- function(H0, dg, dbh, G, b0, b1, b2, b3, b4) {
  h = 1.3 + (b0 + b1 * H0 - b2 * dg + b3 * G) * exp(-b4 / sqrt(dbh))
  return(h)
}

cox_II_1994_mod1 <- function(Hm, dg, dbh, b0, b1, b2, b3, b4, b5) {
  h = b0 + b1 * Hm + b2 * dg^0.95 + b3 * exp(-0.08 * dbh) + 
    b4 * Hm^3 * exp(-0.08 * dbh) + b5 * dg^3 * exp(-0.08 * dbh)
  return(h)
}

cox_II_1994_mod2 <- function(Hm, dg, dbh, b0, b1, b2, b3, b4, b5, b6, b7, b8, b9) {
  h = b0 + b1 * Hm + b2 * dg + b3 * exp(b4 * dbh) + 
    b5 * Hm^b6 * exp(b4 * dbh) + b7 * dg^b8 * exp(b4 * dbh)
  return(h)
}

soares_tome_2002_I <- function(H0, N, D_max, dbh, b0, b1, b2, b3, b4) {
  h = H0 * (1 + (b0 + b1 * (N / 1000) + b2 * D_max) * exp(b3 * H0)) * (1 - exp(b4 * dbh / H0))
  return(h)
}

soares_tome_2002_III <- function(H0, dg, dbh, b0, b1, b2, b3, b4) {
  h = H0 * (1 + (b0 + b1 * H0 + b2 * dg) * exp(b3 * H0)) * (1 - exp(b4 * dbh / H0))
  return(h)
}

soares_tome_2002_II <- function(H0, t, N, D0, dbh, b0, b1, b2, b3, b4, b5) {
  h = H0 * (1 + (b0 + b1 * t + b2 * (N / 1000) + b3 * D0) * exp(b4 * H0)) * (1 - exp(b5 * dbh / H0))
  return(h)
}

soares_tome_2002_IV <- function(H0, t, dg, dbh, b0, b1, b2, b3, b4, b5) {
  h = H0 * (1 + (b0 + b1 * t + b2 * H0 + b3 * dg) * exp(b4 * H0)) * (1 - exp(b5 * dbh / H0))
  return(h)
}

tome_1989_I <- function(H0, N, D0, dbh, t, b0, b1, b2, b3) {
  h = H0 * exp((b0 + b1 * H0 + b2 * (N / 1000) + b3 * t) * ((1 / dbh) - (1 / D0)))
  return(h)
}

tome_1988_II <- function(H0, dg, dbh, D0, t, b0, b1, b2, b3) {
  h = H0 * exp((b0 + b1 * H0 + b2 * dg + b3 * t) * ((1 / dbh) - (1 / D0)))
  return(h)
}

lenhart_1968 <- function(H0, dbh, D_max, N, t, b0, b1, b2, b3, b4) {
  h = H0 / exp(b0 + ((1 / dbh) - (1 / D_max)) * (b1 + b2 * log(N) + b3 * (1 / t) + b4 * log(H0)))
  return(h)
}

amateis_1995 <- function(H0, dbh, D_max, N, t, b0, b1, b2, b3, b4) {
  h = b0 * H0^b1 * 10^((b2 / t) + ((1 / dbh) - (1 / D_max)) * (b3 + b4 * log10(N) / t))
  return(h)
}

burkhart_strub_1974 <- function(H0, N, dbh, t, b0, b1, b2, b3, b4, b5) {
  h = exp(b0 + b1 * log(H0) + b2 * (1 / t) + b3 * log(N) / dbh + b4 * (1 / (dbh * t)) + b5 * (1 / dbh))
  return(h)
}

pascoa_1987 <- function(H0, G, N, t, b0, b1, b2, b3, b4, b5, dbh) {
  h <- b0 * H0^b1 * G^b2 * N^b3 * exp((b4 / t) + (b5 / dbh))
  return(h)
}


# rodriguez_de_prado_species_2022: 11 additional models ====

#' @article{rodriguez_de_prado_species_2022,
#'   title = {Species {Mixing} {Proportion} and {Aridity} {Influence} in the {Height}–{Diameter} {Relationship} for {Different} {Species} {Mixtures} in {Mediterranean} {Forests}},
#'   volume = {13},
#'   issn = {1999-4907},
#'   url = {https://www.mdpi.com/1999-4907/13/1/119},
#'   doi = {10.3390/f13010119},
#'   abstract = {Estimating tree height is essential for modelling and managing both pure and mixed forest stands. Although height–diameter (H–D) relationships have been traditionally ﬁtted for pure stands, attention must be paid when analyzing this relationship behavior in stands composed of more than one species. The present context of global change makes also necessary to analyze how this relationship is inﬂuenced by climate conditions. This study tends to cope these gaps, by ﬁtting new H–D models for 13 different Mediterranean species in mixed forest stands under different mixing proportions along an aridity gradient in Spain. Using Spanish National Forest Inventory data, a total of 14 height–diameter equations were initially ﬁtted in order to select the best base models for each pair species-mixture. Then, the best models were expanded including species proportion by area (mi) and the De Martonne Aridity Index (M). A general trend was found for coniferous species, with taller trees for the same diameter size in pure than in mixed stands, being this trend inverse for broadleaved species. Regarding aridity inﬂuence on H–D relationships, humid conditions seem to beneﬁciate tree height for almost all the analyzed species and species mixtures. These results may have a relevant importance for Mediterranean coppice stands, suggesting that introducing conifers in broadleaves forests could enhance height for coppice species. However, this practice only should be carried out in places with a low probability of drought. Models presented in our study can be used to predict height both in different pure and mixed forests at different spatio-temporal scales to take better sustainable management decisions under future climate change scenarios.},
#'   language = {en},
#'   number = {1},
#'   urldate = {2024-01-19},
#'   journal = {Forests},
#'   author = {Rodríguez De Prado, Diego and Riofrío, Jose and Aldea, Jorge and McDermott, James and Bravo, Felipe and Herrero De Aza, Celia},
#'   month = jan,
#'   year = {2022},
#'   keywords = {machine learning, adaptive silviculture, climate-smart forestry, height–diameter relationship, mixed forests performance, national forest inventory data, NLMM, programming, species mixing proportions},
#'   pages = {119}
#' }

delrio_1999 <- function(dbh, d0, beta0, beta1) {
  h <- beta0 * exp(beta1 * (1/dbh - 1/d0))
  return(h)
}

michailoff_1943 <- function(H0, beta0, dbh, d0) {
  h <- H0 * exp(beta0 * (1/dbh - 1/d0))
  return(h)
}

nilson_1999 <- function(H0, beta0, beta1, dbh, d0) {
  h <- H0 / (1 - beta0 * (1 - (d0 / dbh)^beta1))
  return(h)
}

huang_1992 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 * exp(-beta1 * exp(-beta2 * dbh))
  return(h)
}

meyer_1940 <- function(beta0, beta1, dbh) {
  h <- beta0 * (1 - exp(-beta1 * dbh))
  return(h)
}

pearl_1920 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 / (1 + beta1 * exp(-beta2 * dbh))
  return(h)
}

ratkowsky_1986 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 / (1 + beta1^(-1) * dbh^(-beta2))
  return(h)
}

richards_1959 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 * (1 - exp(beta1 * dbh))^beta2
  return(h)
}

schumacher_1939 <- function(beta0, beta1, dbh) {
  h <- 1.3 + beta0 * exp(beta1 / dbh)
  return(h)
}

seber_1989 <- function(beta0, beta1, beta2, dbh) {
  h <- beta0 * exp(-exp(-beta1 * (dbh - beta2)))
  return(h)
}

zeide_1992 <- function(beta0, beta1, beta2, beta3, dbh) {
  h <- beta0 * exp(-beta1 * exp(-beta2 * dbh^beta3))
  return(h)
}


# wagle_characterizing_2024: 23 additional models ====

#' @inproceedings{wagle_characterizing_2024,
#'   address = {Athens, GA},
#'   title = {Characterizing height-diameter relationships in the {PMRC} {Culture} x {Density} trials using a mixed-effects modeling approach for loblolly pine},
#'   author = {Wagle, Samjhana and Yang, SI and Bullock, Bronson P.},
#'   year = {2024},
#' }


huang_1992_I <- function(a, b, c, dbh) {
  h <- a * exp(-b / (dbh + c))
  return(h)
}

huang_1992_II <- function(a, b, c, dbh) {
  h <- a * (1 - exp(-b * dbh^c))
  return(h)
}

huang_1992_III <- function(a, b, c, dbh) {
  h <- a * (1 - exp(-b * dbh))^c
  return(h)
}

huang_2000_I <- function(a, b, c, dbh) {
  h <- a / (1 + b * dbh^(-c))
  return(h)
}

peschel_1938 <- function(a, b, c, dbh) {
  h <- a / (1 + 1 / (b * dbh^c))
  return(h)
}

huang_2000_II <- function(a, b, c, dbh) {
  h <- a * dbh * exp(-b * dbh) 
  return(h)
}

curtis_1967_I <- function(a, b, c, dbh) {
  h <- a * (1 - exp(-b * dbh)) 
  return(h)
}

huang_1992_IV <- function(a, b, c, dbh) {
  h <- a * dbh^(b * dbh^(-c)) 
  return(h)
}

flewelling_jong_1994 <- function(a, b, c, dbh) {
  h <- a * exp(-b * dbh^(-c)) 
  return(h)
}

huang_1992_V <- function(a, b, c, dbh) {
  h <- (a * dbh) / (b + dbh) 
  return(h)
}

huang_1992_VI <- function(a, b, c, dbh) {
  h <- a * exp(-b * exp(-c * dbh)) + dbh
  return(h)
}

curtis_1967_II <- function(a, b, c, dbh) {
  h <- (a * dbh) / (1 + b * dbh) 
  return(h)
}

stoffels_1953 <- function(a, b, c, dbh) {
  h <- a * dbh^b 
  return(h)
}

peschel_1938_II <- function(a, b, c, dbh) {
  h <- dbh^2 / ((a * dbh + b)^2)
  return(h)
}

ogana_2018 <- function(a, b, c, dbh) {
  h <- ((a / dbh)^b)^(-1)
  return(h)
}

wykoff_1982_I <- function(a, b, c, dbh) {
  h <- exp(a - b * (dbh + 1)^(-1))
  return(h)
}

larsen_hann_1987 <- function(a, b, c, dbh) {
  h <- exp(a + b * dbh^c) 
  return(h)
}

curtis_1967_III <- function(a, b, c, dbh) {
  h <- a * (1 + 1 / dbh)^b 
  return(h)
}

huang_2000_III <- function(a, b, c, dbh) {
  h <- a * (dbh / (1 + dbh))^b 
  return(h)
}

staudhammer_lemay_2000 <- function(a, b, c, dbh) {
  h <- exp(a + b / dbh)
  return(h)
}

burkhart_strub_1974 <- function(a, b, c, dbh) {
  h <- a * exp(-b * dbh^(-1)) 
  return(h)
}

huang_1992_VII <- function(a, b, c, dbh) {
  h <- a / (1 + b * exp(-c * dbh))
  return(h)
}

strand_1958 <- function(a, b, c, dbh) {
  h <- dbh^2 / (a * dbh^2 + b * dbh + c) 
  return(h)
}



# el_mamoun_modelling_2013: 22 additional models ====

#' @article{el_mamoun_modelling_2013,
#'   title = {Modelling height-diameter relationships of selected economically important natural forests species},
#'   volume = {2},
#'   url = {https://www.researchgate.net/profile/Elmamoun-Osman/publication/284581695_Modelling_height-diameter_relationships_of_selected_economically_important_natural_forests_species/links/62b844c589e4f1160c9dff23/Modelling-height-diameter-relationships-of-selected-economically-important-natural-forests-species.pdf},
#'   journal = {Journal of forest products \& industries},
#'   author = {El Mamoun, H Osman and El Zein, A Idris and El Mugira, M Ibrahim},
#'   year = {2013},
#'   pages = {34--42},
#' }

elmamoun_2013_M1 <- function(a, b, dbh) {
  h <- 1.3 + a * dbh^b
  return(h)
}

elmamoun_2013_M2 <- function(a, b, dbh) {
  h <- 1.3 + a * exp(b / dbh)
  return(h)
}

elmamoun_2013_M3 <- function(a, b, c, dbh) {
  h <- 1.3 + exp(a + (b * (dbh^c)))
  return(h)
}

elmamoun_2013_M4 <- function(a, b, c, dbh) {
  h <- 1.3 + dbh^2 / (a + b * dbh + c * (dbh^2))
  return(h)
}

elmamoun_2013_M5 <- function(a, b, c, dbh) {
  h <- 1.3 + a * dbh^(b + (c * dbh))
  return(h)
}

elmamoun_2013_M6 <- function(a, b, dbh) {
  h <- 1.3 + a * (dbh^2) / (dbh + b)^2
  return(h)
}

elmamoun_2013_M7 <- function(a, b, dbh) {
  h <- 1.3 + dbh^2 / (a + b * dbh)^2
  return(h)
}

elmamoun_2013_M8 <- function(a, b, dbh) {
  h <- 1.3 + (a + b / dbh)^(-5)
  return(h)
}

elmamoun_2013_M9 <- function(a, b, c, dbh) {
  h <- 1.3 + a * (1 - exp(b * dbh))^c
  return(h)
}

elmamoun_2013_M10 <- function(a, b, dbh) {
  h <- 1.3 + exp(a + (b / (dbh + 1)))
  return(h)
}

elmamoun_2013_M11 <- function(a, b, c, dbh) {
  h <- 1.3 + (exp(a * exp(-b * (dbh^(-c)))))
  return(h)
}

elmamoun_2013_M12 <- function(a, b, dbh) {
  h <- 1.3 + a * dbh / (b + dbh)
  return(h)
}

elmamoun_2013_M13 <- function(a, b, dbh) {
  h <- 1.3 + dbh^2 / (a + b * dbh)^2
  return(h)
}

elmamoun_2013_M14 <- function(a, b, dbh) {
  h <- 1.3 + (a + b / dbh)^(-2.5)
  return(h)
}

elmamoun_2013_M15 <- function(a, b, dbh) {
  h <- 1.3 + (a + b / dbh)^(-8)
  return(h)
}

elmamoun_2013_M16 <- function(a, b, dbh) {
  h <- 1.3 + a * (1 + 1 / dbh)^(-b)
  return(h)
}

elmamoun_2013_M17 <- function(a, b, dbh) {
  h <- 1.3 + exp(a + b / dbh)
  return(h)
}

elmamoun_2013_M18 <- function(a, b, dbh) {
  h <- 1.3 + a * (log(1 + dbh))^b
  return(h)
}

# elmamoun_2013_M18_structure_a <- function(a, b, dbh, dg, N) {
#   h <- 1.3 + a * (log(1 + dbh + dbh/dg))^b
#   return(h)
# }
# 
# elmamoun_2013_M18_structure_b <- function(a, b, dbh, dg, N) {
#   h <- 1.3 + a * (log(1 + dbh + (dbh^2)/(N * (dg^2))))^b
#   return(h)
# }

elmamoun_2013_M19 <- function(a, b, c, dbh) {
  h <- 1.3 + a * (1 - exp(b * dbh^c))
  return(h)
}

elmamoun_2013_M20 <- function(a, b, c, dbh) {
  h <- 1.3 + a / (1 + b^(-1) * dbh^(-c))
  return(h)
}

elmamoun_2013_M21 <- function(a, b, c, dbh) {
  h <- 1.3 + exp(a + b * (dbh^(-c)))
  return(h)
}

elmamoun_2013_M22 <- function(a, b, c, dbh) {
  h <- 1.3 + dbh^a / (b + (c * (dbh^a)))
  return(h)
}



# moore_height-diameter_1996: 2 additional models ====

#' @article{moore_height-diameter_1996,
#'   title = {Height-diameter equations for ten tree species in the {Inland} {Northwest}},
#'   volume = {11},
#'   url = {https://objects.lib.uidaho.edu/iftnc/iftnc4813.pdf},
#'   number = {4},
#'   journal = {Western Journal of Applied Forestry},
#'   author = {Moore, James A and Zhang, Lianjun and Stuck, Dean},
#'   year = {1996},
#'   note = {Publisher: Oxford University Press},
#'   pages = {132--137},
#' }

wykoff_1982_II <- function(a, b, dbh) {
  # h <- 4.5 + exp(a + (b / (dbh + 1)))  # feets
  h <- 1.3 + exp(a + (b / (dbh + 1)))  # meters
  return(h)
}

lundqvist_1989 <- function(a, b, c, dbh) {
  # h <- 4.5 + a * exp(-b * dbh^(-c))  # feets
  h <- 1.3 + a * exp(-b * dbh^(-c))  # meters
  return(h)
}


# temesgen_regional_2007: 4 additional models ====

#' @article{temesgen_regional_2007,
#'   title = {Regional {Height}–{Diameter} {Equations} for {Major} {Tree} {Species} of {Southwest} {Oregon}},
#'   volume = {22},
#'   issn = {0885-6095},
#'   url = {https://doi.org/10.1093/wjaf/22.3.213},
#'   doi = {10.1093/wjaf/22.3.213},
#'   abstract = {Selected tree height and diameter functions were evaluated for their predictive abilities for major tree species of southwest Oregon. Two sets of equations were evaluated. The first set included four base equations for estimating height as a function of individual tree diameter, and the remaining 16 equations enhanced the four base equations with alternative measures of stand density and relative position. The inclusion of the crown competition factor in larger trees (CCFL) and basal area (BA), which simultaneously indicates the relative position of a tree and stand density, into the base height–diameter equations increased the accuracy of prediction for all species. On the average, root mean square error values were reduced by 45 cm (15\% improvement). On the basis of the residual plots and fit statistics, two equations are recommended for estimating tree heights for major tree species in southwest Oregon. The equation coefficients are documented for future use.},
#'   number = {3},
#'   urldate = {2024-09-09},
#'   journal = {Western Journal of Applied Forestry},
#'   author = {Temesgen, Hailemariam and Hann, David W. and Monleon, Vincente J.},
#'   month = jul,
#'   year = {2007},
#'   pages = {213--219},
#' }

yang_1978 <- function(a, b, c, dbh) {
  h <- 1.3 + a * (1 - exp(b * (dbh^c)))
  return(h)
}

chapman_richards_1959 <- function(a, b, c, dbh) {
  h <- 1.3 + a * (1 - exp(b * dbh))^c
  return(h)
}

ratkowsky_1990 <- function(a, b, c, dbh) {
  h <- 1.3 + exp(a + b / (dbh + c))
  return(h)
}

hanus_1999 <- function(a, b, dbh) {
  h <- 1.3 + exp(a + b * (dbh)^c)
  return(h)
}



# temesgen_generalized_2004: 3 additional models ====

#' @article{temesgen_generalized_2004,
#'   title = {Generalized height–diameter models—an application for major tree species in complex stands of interior {British} {Columbia}},
#'   volume = {123},
#'   issn = {1612-4677},
#'   url = {https://doi.org/10.1007/s10342-004-0020-z},
#'   doi = {10.1007/s10342-004-0020-z},
#'   abstract = {Using permanent sample-plot data, selected tree height and diameter functions were evaluated for their predictive abilities for major tree species in complex (multiple age, size and species cohort) stands of interior British Columbia (BC), Canada. Two sets of models were evaluated. The first set included five models for estimating height as a function of individual tree diameter, the second set also included five models for estimating height as a function of individual tree diameter and other stand-level attributes. The inclusion of the BAL index (which simultaneously indicates the relative position of a tree and stand density) into the base height–diameter models increased the accuracy of prediction for all species. On average, by including stand level attributes, root mean square values were reduced by 30.0 cm. Based on the residual plots and fit statistics, these models can be recommended for estimating tree heights for major tree species in complex stands of interior BC. The model coefficients are documented for future use.},
#'   language = {en},
#'   number = {1},
#'   urldate = {2024-09-09},
#'   journal = {European Journal of Forest Research},
#'   author = {Temesgen, H. and v. Gadow, K.},
#'   month = apr,
#'   year = {2004},
#'   keywords = {Canada, BAL index, Multi-age forests},
#'   pages = {45--51},
#' }

wykoff_1982_III <- function(a, b, dbh) {
  h <- 1.3 + exp(a + b / (dbh + 1))
  return(h)
}

hui_gadow_1993_II <- function(a, b, dbh) {
  h <- 1.3 + a * exp(b / (dbh + 1))
  return(h)
}

hui_gadow_1993_III <- function(a, b, dbh) {
  h <- 1.3 + a * dbh^b
  return(h)
}



# scaranello_height-diameter_2012: 11 additional models ====

#' @article{scaranello_height-diameter_2012,
#'   title = {Height-diameter relationships of tropical {Atlantic} moist forest trees in southeastern {Brazil}},
#'   volume = {69},
#'   issn = {1678-992X},
#'   url = {https://www.scielo.br/j/sa/a/nYKKc7HrZ4wX4kFfxDWpZ9q/},
#'   doi = {10.1590/S0103-90162012000100005},
#'   abstract = {Site-specific height-diameter models may be used to improve biomass estimates for forest inventories where only diameter at breast height (DBH) measurements are available. In this study, we fit height-diameter models for vegetation types of a tropical Atlantic forest using field measurements of height across plots along an altitudinal gradient. To fit height-diameter models, we sampled trees by DBH class and measured tree height within 13 one-hectare permanent plots established at four altitude classes. To select the best model we tested the performance of 11 height-diameter models using the Akaike Information Criterion (AIC). The Weibull and Chapman-Richards height-diameter models performed better than other models, and regional site-specific models performed better than the general model. In addition, there is a slight variation of height-diameter relationships across the altitudinal gradient and an extensive difference in the stature between the Atlantic and Amazon forests. The results showed the effect of altitude on tree height estimates and emphasize the need for altitude-specific models that produce more accurate results than a general model that encompasses all altitudes. To improve biomass estimation, the development of regional height-diameter models that estimate tree height using a subset of randomly sampled trees presents an approach to supplement surveys where only diameter has been measured.},
#'   language = {en},
#'   urldate = {2024-09-09},
#'   journal = {Scientia Agricola},
#'   author = {Scaranello, Marcos Augusto da Silva and Alves, Luciana Ferreira and Vieira, Simone Aparecida and Camargo, Plinio Barbosa de and Joly, Carlos Alfredo and Martinelli, Luiz Antônio},
#'   month = feb,
#'   year = {2012},
#'   note = {Publisher: Escola Superior de Agricultura "Luiz de Queiroz"},
#'   keywords = {elevation, tree height},
#'   pages = {26--37},
#' }

linear_model_I <- function(a, b, dbh) {
  h <- a + b * dbh
  return(h)
}

linear_model_II <- function(a, b, dbh) {
  h <- a + b * log(dbh)
  return(h)
}

hyperbolic_model_I <- function(a, b, dbh) {
  h <- a * dbh / (b + dbh)
  return(h)
}

hyperbolic_model_II <- function(a, b, c, dbh) {
  h <- (dbh^2) / ((a + b * dbh)^2)
  return(h)
}

power_model <- function(a, b, dbh) {
  h <- a * dbh^b
  return(h)
}

exponential_model <- function(a, b, dbh) {
  h <- exp(a + b / (dbh + 1))
  return(h)
}

chapman_richards_model <- function(a, b, c, dbh) {
  h <- a * (1 - exp(b * dbh))^c
  return(h)
}

weibull_model <- function(a, b, c, dbh) {
  h <- a * (1 - exp(-b * dbh^c))
  return(h)
}

monomolecular_model <- function(a, b, c, dbh) {
  h <- a * (1 - b * exp(-c * dbh))
  return(h)
}

gompertz_model <- function(a, b, c, dbh) {
  h <- a * exp(b * exp(-c * dbh))
  return(h)
}

logistic_model <- function(a, b, c, dbh) {
  h <- a / (1 + b * exp(-c * dbh))
  return(h)
}

###

logistic_model_13 <- function(b, c, dbh) {
  h <- 1.3 / (1 + b * exp(-c * dbh))  # logistic model with a = 1.3
  return(h)
}



# lebedev_new_2020: 24 additional models ====

#' @article{lebedev_new_2020,
#'   title = {New generalised height-diameter models for the birch stands in {European} {Russia}},
#'   volume = {26},
#'   url = {https://www.researchgate.net/profile/Aleksandr-Lebedev-3/publication/346470846_New_generalised_height-diameter_models_for_the_birch_stands_in_European_Russia/links/5fc3b7cfa6fdcc6cc67fe2f6/New-generalised-height-diameter-models-for-the-birch-stands-in-European-Russia.pdf},
#'   number = {2},
#'   journal = {Baltic Forestry},
#'   author = {Lebedev, ALEKSANDR V},
#'   year = {2020},
#'   note = {Publisher: Lietuvos Misku Institutas},
#'   pages = {1--7},
#' }

# Note: silenced equations mean that the same model is already implemented in previous sections

# lebedev_M1 <- function(dbh, a, b) {
#   h <- 1.3 + a * dbh^b
#   return(h)
# }

lebedev_M2 <- function(dbh, a, b) {
  h <- 1.3 + (dbh / (a + b * dbh))^2
  return(h)
}

lebedev_M3 <- function(dbh, a, b) {
  h <- 1.3 + (a * dbh) / (b + dbh)
  return(h)
}

lebedev_M4 <- function(dbh, a, b) {
  h <- 1.3 + a * (dbh / (1 + dbh))^b
  return(h)
}

lebedev_M5 <- function(dbh, a, b) {
  h <- 1.3 + (a * dbh) / ((1 + dbh)^b)
  return(h)
}

lebedev_M6 <- function(dbh, a, b) {
  h <- 1.3 + a * (1 - exp(-b * dbh))
  return(h)
}

# lebedev_M7 <- function(dbh, a, b) {
#   h <- 1.3 + exp(a + b / (dbh + 1))
#   return(h)
# }

lebedev_M8 <- function(dbh, a, b) {
  h <- 1.3 + (a * dbh) / ((dbh + 1) + b * dbh)
  return(h)
}

lebedev_M9 <- function(dbh, a, b) {
  h <- 1.3 + a * dbh * exp(-b * dbh)
  return(h)
}

# lebedev_M10 <- function(dbh, a, b) {
#   h <- 1.3 + a * exp(b / dbh)
#   return(h)
# }

# lebedev_M11 <- function(dbh, a, b) {
#   h <- 1.3 + a * (log(1 + dbh))^b
#   return(h)
# }

# lebedev_M12 <- function(dbh, a, b) {
#   h <- 1.3 + (a + b / dbh)^(-5)
#   return(h)
# }

lebedev_M13 <- function(dbh, a, b, c) {
  h <- 1.3 + a / (1 + b * dbh^(-c))
  return(h)
}

lebedev_M14 <- function(dbh, a, b, c) {
  h <- 1.3 + (dbh^2) / (a + b * dbh + c * dbh^2)
  return(h)
}

lebedev_M15 <- function(dbh, a, b, c) {
  h <- 1.3 + a / (1 + b * exp(-c * dbh))
  return(h)
}

lebedev_M16 <- function(dbh, a, b, c) {
  h <- 1.3 + a * (1 - exp(-b * dbh^c))
  return(h)
}

lebedev_M17 <- function(dbh, a, b, c) {
  h <- 1.3 + a * (1 - exp(-b * dbh))^c
  return(h)
}

lebedev_M18 <- function(dbh, a, b, c) {
  h <- 1.3 + a * exp(-b * exp(-c * dbh))
  return(h)
}

lebedev_M19 <- function(dbh, a, b, c) {
  h <- 1.3 + exp(a + b * dbh^c)
  return(h)
}

# lebedev_M20 <- function(dbh, a, b, c) {
#   h <- 1.3 + exp(a + b / (dbh + c))
#   return(h)
# }

# lebedev_M21 <- function(dbh, a, b, c) {
#   h <- 1.3 + a * exp(-b * dbh^(-c))
#   return(h)
# }

lebedev_M22 <- function(dbh, a, b, c) {
  h <- 1.3 + a * sqrt(dbh) + b * dbh + c * dbh^2
  return(h)
}

lebedev_M23 <- function(dbh, a, b, c) {
  h <- 1.3 + a / (1 + (b * dbh^c)^(-1))
  return(h)
}

lebedev_M24 <- function(dbh, a, b, c) {
  h <- 1.3 + a * dbh^(b * dbh^(c * dbh^(-c)))
  return(h)
}



# lebedev_verification_2020: 29 additional models (repeated not coded) ====

#' @article{lebedev_verification_2020,
#'   title = {Verification of two-and three-parameter simple height-diameter models for birch in the {European} part of {Russia}.},
#'   url = {https://jfs.agriculturejournals.cz/pdfs/jfs/2020/09/04.pdf},
#'   author = {Lebedev, Aleksandr and Kuzmichev, Valery},
#'   year = {2020},
#' }

lebedev_M16_b <- function(dbh, a, b) {
  h <- 1.3 + a / (1 + (b * dbh)^(-1))
  return(h)
}

lebedev_M25_b <- function(dbh, a, b, c) {
  h <- ((1.3^a + (b^a - 1.3^a)) * (1 - exp(-c * dbh)) / (1 - exp(-100 * c)))^(1 / a)
  return(h)
}

# lebedev_M26 <- function(dbh, a, b, c) {
#   h <- 1.3 + a * sqrt(dbh) + b * dbh + c * dbh^2
#   return(h)
# }

# lebedev_M27 <- function(dbh, a, b, c) {
#   h <- 1.3 + a / (1 + (b * dbh^c)^(-1))
#   return(h)
# }

lebedev_M28_b <- function(dbh, a, b, c) {
  h <- 1.3 + a * dbh^(b * dbh^(-c))
  return(h)
}

lebedev_M29_b <- function(dbh, a, b, c) {
  h <- 1.3 + dbh^(a / (b + c * dbh^a))
  return(h)
}



# Models list including the functions and starting values for those models that only use dbh as predictor ====

# Function that creates the models list, referencing the already defined functions and providing start values
# list structure: model_name = list(func = function_name, start = list(starting_values))

get_models_list <- function(){
  
  models <- list(
    
    # models from Rodríguez de Prado et al., 2022
    huang_1992 = list(
      func = huang_1992,
      start = list(beta0 = 1, beta1 = 1, beta2 = 0.1)
    ),
    meyer_1940 = list(
      func = meyer_1940,
      start = list(beta0 = 1, beta1 = 1)
    ),
    pearl_1920 = list(
      func = pearl_1920,
      start = list(beta0 = 1, beta1 = 1, beta2 = 0.1)
    ),
    ratkowsky_1986 = list(
      func = ratkowsky_1986,
      start = list(beta0 = 1, beta1 = 1, beta2 = 0.1)
    ),
    richards_1959 = list(
      func = richards_1959,
      start = list(beta0 = 1, beta1 = -0.1, beta2 = 0.1)
    ),
    schumacher_1939 = list(
      func = schumacher_1939,
      start = list(beta0 = 1, beta1 = -0.1)
    ),
    seber_1989 = list(
      func = seber_1989,
      start = list(beta0 = 1, beta1 = 1, beta2 = 10)
    ),
    zeide_1992 = list(
      func = zeide_1992,
      start = list(beta0 = 1, beta1 = 1, beta2 = 0.1, beta3 = 1)
    ),
    
    # Additional models from Wagle et al. (2024)
    huang_1992_I = list(
      func = huang_1992_I,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_1992_II = list(
      func = huang_1992_II,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_1992_III = list(
      func = huang_1992_III,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_2000_I = list(
      func = huang_2000_I,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    peschel_1938 = list(
      func = peschel_1938,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_2000_II = list(
      func = huang_2000_II,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    curtis_1967_I = list(
      func = curtis_1967_I,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_1992_IV = list(
      func = huang_1992_IV,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    flewelling_jong_1994 = list(
      func = flewelling_jong_1994,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_1992_V = list(
      func = huang_1992_V,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_1992_VI = list(
      func = huang_1992_VI,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    curtis_1967_II = list(
      func = curtis_1967_II,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    stoffels_1953 = list(
      func = stoffels_1953,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    peschel_1938_II = list(
      func = peschel_1938_II,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    ogana_2018 = list(
      func = ogana_2018,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    wykoff_1982_I = list(
      func = wykoff_1982_I,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    larsen_hann_1987 = list(
      func = larsen_hann_1987,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    curtis_1967_III = list(
      func = curtis_1967_III,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_2000_III = list(
      func = huang_2000_III,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    staudhammer_lemay_2000 = list(
      func = staudhammer_lemay_2000,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    burkhart_strub_1974 = list(
      func = burkhart_strub_1974,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    huang_1992_VII = list(
      func = huang_1992_VII,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    strand_1958 = list(
      func = strand_1958,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    
    # Additional models from El Mamoun et al. (2013)
    elmamoun_2013_M1 = list(
      func = elmamoun_2013_M1,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M2 = list(
      func = elmamoun_2013_M2,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M3 = list(
      func = elmamoun_2013_M3,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    elmamoun_2013_M4 = list(
      func = elmamoun_2013_M4,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    elmamoun_2013_M5 = list(
      func = elmamoun_2013_M5,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    elmamoun_2013_M6 = list(
      func = elmamoun_2013_M6,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M7 = list(
      func = elmamoun_2013_M7,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M8 = list(
      func = elmamoun_2013_M8,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M9 = list(
      func = elmamoun_2013_M9,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    elmamoun_2013_M10 = list(
      func = elmamoun_2013_M10,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M11 = list(
      func = elmamoun_2013_M11,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    elmamoun_2013_M12 = list(
      func = elmamoun_2013_M12,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M13 = list(
      func = elmamoun_2013_M13,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M14 = list(
      func = elmamoun_2013_M14,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M15 = list(
      func = elmamoun_2013_M15,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M16 = list(
      func = elmamoun_2013_M16,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M17 = list(
      func = elmamoun_2013_M17,
      start = list(a = 1, b = 0.5)
    ),
    elmamoun_2013_M18 = list(
      func = elmamoun_2013_M18,
      start = list(a = 1, b = 0.5)
    ),
    # elmamoun_2013_M18_structure_a = list(
    #   func = elmamoun_2013_M18_structure_a,
    #   start = list(a = 1, b = 0.5)
    # ),
    # elmamoun_2013_M18_structure_b = list(
    #   func = elmamoun_2013_M18_structure_b,
    #   start = list(a = 1, b = 0.5)
    # ),
    elmamoun_2013_M19 = list(
      func = elmamoun_2013_M19,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    elmamoun_2013_M20 = list(
      func = elmamoun_2013_M20,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    elmamoun_2013_M21 = list(
      func = elmamoun_2013_M21,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    elmamoun_2013_M22 = list(
      func = elmamoun_2013_M22,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    
    # Additional models from Moore et al. (1996)
    wykoff_1982_II = list(
      func = wykoff_1982_II,
      start = list(a = 1, b = 0.5)
    ),
    lundqvist_1989 = list(
      func = lundqvist_1989,
      start = list(a = 1, b = 0.5, c = 0.1)
    ), 
    
    # New models from Temesgen et al. (2007)
    yang_1978 = list(
      func = yang_1978,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    chapman_richards_1959 = list(
      func = chapman_richards_1959,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    ratkowsky_1990 = list(
      func = ratkowsky_1990,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    hanus_1999 = list(
      func = hanus_1999,
      start = list(a = 1, b = 0.5)
    ),
    
    # New models from Temesgen et al. (2004)
    wykoff_1982_III = list(
      func = wykoff_1982_III,
      start = list(a = 1, b = 0.5)
    ),
    hui_gadow_1993_II = list(
      func = hui_gadow_1993_II,
      start = list(a = 1, b = 0.5)
    ),
    hui_gadow_1993_III = list(
      func = hui_gadow_1993_III,
      start = list(a = 1, b = 0.5)
    ),
    
    # New models from Scaranello et al. (20012)
    linear_model_I = list(
      func = linear_model_I,
      start = list(a = 1, b = 0.5)
    ),
    linear_model_II = list(
      func = linear_model_II,
      start = list(a = 1, b = 0.5)
    ),
    hyperbolic_model_I = list(
      func = hyperbolic_model_I,
      start = list(a = 1, b = 0.5)
    ),
    hyperbolic_model_II = list(
      func = hyperbolic_model_II,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    power_model = list(
      func = power_model,
      start = list(a = 1, b = 0.5)
    ),
    exponential_model = list(
      func = exponential_model,
      start = list(a = 1, b = 0.5)
    ),
    chapman_richards_model = list(
      func = chapman_richards_model,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    weibull_model = list(
      func = weibull_model,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    monomolecular_model = list(
      func = monomolecular_model,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    gompertz_model = list(
      func = gompertz_model,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    logistic_model = list(
      func = logistic_model,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    logistic_model_13 = list(
      func = logistic_model_13,
      start = list(b = 0.5, c = 0.1)
    ),
    lebedev_M2 = list(
      func = lebedev_M2,
      start = list(a = 1, b = 0.5)
    ),
    lebedev_M3 = list(
      func = lebedev_M3,
      start = list(a = 1, b = 0.5)
    ),
    lebedev_M4 = list(
      func = lebedev_M4,
      start = list(a = 1, b = 0.5)
    ),
    lebedev_M5 = list(
      func = lebedev_M5,
      start = list(a = 1, b = 0.5)
    ),
    lebedev_M6 = list(
      func = lebedev_M6,
      start = list(a = 1, b = 0.5)
    ),
    lebedev_M8 = list(
      func = lebedev_M8,
      start = list(a = 1, b = 0.5)
    ),
    lebedev_M9 = list(
      func = lebedev_M9,
      start = list(a = 1, b = 0.5)
    ),
    lebedev_M13 = list(
      func = lebedev_M13,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M14 = list(
      func = lebedev_M14,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M15 = list(
      func = lebedev_M15,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M16 = list(
      func = lebedev_M16,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M17 = list(
      func = lebedev_M17,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M18 = list(
      func = lebedev_M18,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M19 = list(
      func = lebedev_M19,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M22 = list(
      func = lebedev_M22,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M23 = list(
      func = lebedev_M23,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M24 = list(
      func = lebedev_M24,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M16_b = list(
      func = lebedev_M16_b,
      start = list(a = 1, b = 0.5)
    ),
    lebedev_M25_b = list(
      func = lebedev_M25_b,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M28_b = list(
      func = lebedev_M28_b,
      start = list(a = 1, b = 0.5, c = 0.1)
    ),
    lebedev_M29_b = list(
      func = lebedev_M29_b,
      start = list(a = 1, b = 0.5, c = 0.1)
    )
  )
  return(models)
}

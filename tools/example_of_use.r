# Code to call height-diameter models functions ----
#
# Aitor Vázquez Veloso
# 2024-11-25
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Cite as:
# Vázquez-Veloso, A., Yang, S.-I., Bullock, B. P., & Bravo, F. (2025). One model to rule them all: A nationwide height–diameter model for 91 Spanish forest species. Forest Ecology and Management, 595, 122981. https://doi.org/10.1016/j.foreco.2025.122981

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# example of use (data frame with by default column names)
source('one_model_to_rule_them_all.r')
df <- test_hd_df()
new_df <- predict_height(df, species_col = "species_name")
print(new_df)

# example of use (data frame with custom column names)
source('one_model_to_rule_them_all.r')
df <- test_hd_df_2()
new_df <- predict_height(df, path_to_pars = 'data/', dbh_col = 'dbh_cm', species_value = 'code', 
                         species_col = 'codes', clim_col = 'region', mix_col = 'pure_mix', or_col = 'nat_plant')
print(new_df)

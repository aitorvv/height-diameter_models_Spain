#!/usr/bin/Rscript

# Code to get hd models ----
# Step 8: cluster bootstrap results analysis - explore results
#
# Aitor Vázquez Veloso
# 2025-02-17
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



# Some decisions that has been taken can be filtered by "# Note:"



# working directory
setwd('')

# install and load necessary libraries
library(tidyverse)       

# load data
load("1_data/2_processed/2.7_cluster_bootstrap_results.rdata")



# Latex table with MB and RMSE results ====

# lists
table_headers <- c("Metric", "Q_0.5", "Q_0.025", "Q_0.975")
table_rows <- c("mb", "rmse")

# start latex table
latex_table <- "\\begin{table}[h]\\centering\n"
latex_table <- paste0(latex_table, "\\begin{tabular}{lccc}\n")

# headers
latex_table <- paste0(latex_table, paste(table_headers, collapse = " & "), " \\\\ \\hline\n")

# rows
row_values <- c(table_rows[1], cluster_metrics[[table_rows[1]]]$median, 
                cluster_metrics[[table_rows[1]]]$p_025, cluster_metrics[[table_rows[1]]]$p_975)
latex_table <- paste0(latex_table, paste(row_values, collapse = " & ", sep = ""), " \\\\ \n")

row_values <- c(table_rows[2], cluster_metrics[[table_rows[2]]]$median, 
                cluster_metrics[[table_rows[2]]]$p_025, cluster_metrics[[table_rows[2]]]$p_975)
latex_table <- paste0(latex_table, paste(row_values, collapse = " & ", sep = ""), " \\\\ \n")

latex_table <- paste0(latex_table, "\\end{tabular}\n\\caption{Resultados de métricas por clúster}\n\\label{tab:cluster_metrics}\n\\end{table}")

# save and print
writeLines(latex_table, "3_figures/2.8_cluster_tables/baseline_cluster_metrics_table.tex")
cat(latex_table)



# Latex table with parameters results ====

# lists
table_headers <- c("Parameter", "Value", "sigma", "Mixed", "Plantation", "Atlantic", "Alpine", "Macaronesian")
table_rows <- c("beta_a", "beta_b", "e")

# extract and format list values
extract_values <- function(metric) {
  median_val <- cluster_metrics[[metric]]$median
  p025_val <- cluster_metrics[[metric]]$p_025
  p975_val <- cluster_metrics[[metric]]$p_975
  
  # Formatear con 3 decimales
  formatted_value <- sprintf("%.3f \\\\ (%.3f/%.3f)", median_val, p025_val, p975_val)
  return(formatted_value)
}

# start latex table
latex_table <- "\\begin{table}[h]\\centering\n"
latex_table <- paste0(latex_table, "\\begin{tabular}{lccccccc}\n")

# headers
latex_table <- paste0(latex_table, paste(table_headers, collapse = " & "), " \\\\ \\hline\n")

# rows
row_values <- c(table_rows[1], extract_values("a.(Intercept)"), extract_values("a.stddev"), 
                extract_values("a.fe1mix"), extract_values("a.fe2artificial"), 
                extract_values("a.fe3Atlántica"), extract_values("a.fe3Alpina"), extract_values("a.fe3Macaronésica"))
latex_table <- paste0(latex_table, paste("\\makecell{", row_values, "}", collapse = " & ", sep = ""), " \\\\ \n")

row_values <- c(table_rows[2], extract_values("b.(Intercept)"), extract_values("b.stddev"), 
                extract_values("b.fe1mix"), extract_values("b.fe2artificial"), 
                extract_values("b.fe3Atlántica"), extract_values("b.fe3Alpina"), extract_values("b.fe3Macaronésica"))
latex_table <- paste0(latex_table, paste("\\makecell{", row_values, "}", collapse = " & ", sep = ""), " \\\\ \n")

row_values <- c(table_rows[3], '', extract_values("residual.stddev"), '', '', '', '', '')
latex_table <- paste0(latex_table, paste("\\makecell{", row_values, "}", collapse = " & ", sep = ""), " \\\\ \n")

latex_table <- paste0(latex_table, "\\end{tabular}\n\\caption{Resultados de métricas por clúster}\n\\label{tab:cluster_metrics}\n\\end{table}")

# save and print
writeLines(latex_table, "3_figures/2.8_cluster_tables/baseline_cluster_parameters_table.tex")
cat(latex_table)

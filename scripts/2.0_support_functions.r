#!/usr/bin/Rscript

# Code with functions used into exporting and graph results ----
# Step 0: support functions
#
# Aitor Vázquez Veloso
# 2024-10-24
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


library(xtable)

# Export a table to a LaTeX file
# written by: Aitor Vázquez Veloso
# date: 2024-10-24
# parameters:
#   df: dataframe to export
#   n_rows: number of rows to export (optional)
#   n_cols: number of columns to export (optional)
#   file_name: name of the output LaTeX file 
#   decimals: number of decimals for the table (default = 2)
#   non_numeric_rows: number of non-numeric rows of the table (default = 1); must be placed at the beginning
#   italic_columns: columns to convert to italic in LaTeX formatting (optional; must be the position of the column)
# return: none
# output: LaTeX file with the table

export_to_latex <- function(df, n_rows = NULL, n_cols = NULL, file_name = "output.tex", decimals = 2, 
                            non_numeric_rows = 1, italic_columns = NULL){
  
  # subset the dataframe if rows or columns are specified
  if (!is.null(n_rows)) {
    df <- df[1:min(n_rows, nrow(df)), ]
  }
  if (!is.null(n_cols)) {
    df <- df[, 1:min(n_cols, ncol(df))]
  }
  
  # convert specified columns to italic in LaTeX formatting
  if (!is.null(italic_columns)) {
    df[italic_columns] <- lapply(df[italic_columns], function(x) paste0("\\textit{", x, "}"))
  }
  
  # set the number of decimals for each column (first element for row names, rest for columns)
  non_numeric <- numeric(non_numeric_rows + 1)
  digits <- c(non_numeric, rep(decimals, ncol(df) - 1))
  
  # convert the dataframe to a LaTeX table with specified decimals
  latex_table <- xtable(df, digits = digits)
  
  # Export to a LaTeX file
  print(latex_table, type = "latex", file = file_name, include.rownames = FALSE)
  
  cat(paste("Table successfully exported to", file_name, "\n"))
}

# Example of using the function
# df <- data.frame(A = 1:5, B = letters[1:5])
# export_to_latex(df, n_rows = 3, n_cols = 2, file_name = "table.tex")



# Export to .doc file ====

library(officer)
library(flextable)

# Export a table to a .doc file
# written by: Aitor Vázquez Veloso
# date: 2024-12-12
# parameters:
#   df: dataframe to export
#   file_name: name of the output doc file 
# return: none
# output: .doc file with the table

export_to_word <- function(df, file_name) {
  
  # create a word document
  doc <- read_docx()
  
  # convert dataframe to flextable
  table <- regulartable(df)
  
  # add table to the document
  doc <- body_add_flextable(doc, value = table)
  
  # save file
  print(doc, target = file_name)
  
  message("File saved in: ", paste(getwd(), '/', file_name, sep = ''))
}

# example of use
# mi_df <- data.frame(
#   Nombre = c("Juan", "Ana", "Luis"),
#   Edad = c(28, 34, 29),
#   Ciudad = c("Madrid", "Barcelona", "Valencia")
# )
# export_to_word(mi_df, "example.docx")



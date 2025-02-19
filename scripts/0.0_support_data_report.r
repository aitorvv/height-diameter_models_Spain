#!/usr/bin/Rscript

# Functions to create data reports ----
#
# Aitor Vázquez Veloso
# 2024-09-12
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



library(rmarkdown)
library(knitr)
library(ggplot2)

# generate a report for a dataset
# created by Aitor Vázquez Veloso, 2024-07-10
# parameters:
# - df: dataset to explore
# - output_file: name of the output file and path
data_report <- function(df, output_file = "data_report.html") {
  
  # Ensure the directory exists
  # output_dir <- dirname(output_file)
  # if (!dir.exists(output_dir)) {
  #   dir.create(output_dir, recursive = TRUE)
  # }
  
  # Create a temporary .Rmd file
  temp_report <- tempfile(fileext = ".Rmd")
  
  # Write the R Markdown header content
  writeLines(c(
    "---",
    "title: 'Data Overview Report'",
    "author: 'Aitor Vázquez Veloso'",
    "date: '`r Sys.Date()`'",
    "output: html_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE)",
    "```",
    "",
    "## Data Overview",
    "",
    "This report provides an overview of the dataset.",
    ""
  ), temp_report)
  
  # Loop through each column
  for (col in colnames(df)) {
    my_col <- df[[col]]
    type <- typeof(my_col)
    n_missing <- sum(is.na(my_col))
    
    # variable header
    column_section <- paste0(
      "### Column: ", col, "\n\n",
      "Type: ", type, "\n\n",
      "Number of missing values: ", n_missing, "\n\n"
    )
    
    cat(column_section, file = temp_report, append = TRUE)
    
    if (type == "integer" || type == "double") {
      cat(c(
        "```{r, echo=FALSE}",
        paste0("summary(df[['", col, "']])"),
        "```",
        "",
        "```{r, echo=FALSE, fig.width=7, fig.height=5}",
        paste0("ggplot(df[!is.na(df[['", col, "']]), ], aes(x = ", col, ")) + ",
               "geom_histogram(binwidth = 30, fill = 'blue', color = 'black', alpha = 0.7) + ",
               "labs(title = 'Histogram of ", col, "')"),
        "```",
        ""
      ), file = temp_report, append = TRUE, sep = "\n")
      
    } else if (type == "character") {
      cat(c(
        # "```{r, echo=FALSE}",
        # paste0("unique(df[['", col, "']])"),
        # "```",
        # "",
        "```{r, echo=FALSE}",
        paste0("table(df[['", col, "']])"),
        "```",
        ""
      ), file = temp_report, append = TRUE, sep = "\n")
      
    } else if (type == "logical") {
      cat(c(
        "```{r, echo=FALSE}",
        paste0("table(df[['", col, "']])"),
        "```",
        ""
      ), file = temp_report, append = TRUE, sep = "\n")
    } else {
      cat(paste("This is a column of type", type, "\n\n"), file = temp_report, append = TRUE)
    }
  }
  
  # Render the R Markdown to HTML
  rmarkdown::render(temp_report, output_file = output_file)
  
  # Print the location of the output file
  message("Report generated: ", output_file)
}

# generate an extended report for a dataset
# created by Aitor Vázquez Veloso, 2024-07-10
# parameters:
# - df: dataset to explore
# - output_file: name of the output file and path
data_report_extended <- function(df, output_file = "data_overview_report.html") {
  
  # Create a temporary .Rmd file
  temp_report <- tempfile(fileext = ".Rmd")
  
  # Write the R Markdown header content
  writeLines(c(
    "---",
    "title: 'Data Overview Report'",
    "author: 'Aitor Vázquez Veloso'",
    "date: '`r Sys.Date()`'",
    "output: html_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "```",
    "",
    "## Data Overview",
    "",
    "This report provides an overview of the dataset.",
    ""
  ), temp_report)
  
  # Loop through each column
  for (col in colnames(df)) {
    my_col <- df[[col]]
    type <- typeof(my_col)
    n_missing <- sum(is.na(my_col))
    
    column_section <- paste0(
      "### Column: ", col, "\n\n",
      "Type: ", type, "\n\n",
      "Number of missing values: ", n_missing, "\n\n"
    )
    
    cat(column_section, file = temp_report, append = TRUE)
    
    if (type == "integer" || type == "double") {
      cat(c(
        "```{r, echo=FALSE}",
        paste0("summary(df[['", col, "']])"),
        "```",
        "",
        "```{r, echo=FALSE, fig.width=7, fig.height=5}",
        paste0("ggplot(df, aes(x = ", col, ")) + ",
               "geom_histogram(binwidth = 30, fill = 'blue', color = 'black', alpha = 0.7) + ",
               "geom_density(alpha = 0.2, fill = 'red') + ",
               "labs(title = 'Histogram and Density Plot of ", col, "')"),
        "```",
        ""
      ), file = temp_report, append = TRUE, sep = "\n")
      
    } else if (type == "character" || is.factor(my_col)) {
      cat(c(
        "```{r, echo=FALSE}",
        paste0("unique(df[['", col, "']])"),
        "```",
        "",
        "```{r, echo=FALSE}",
        paste0("table(df[['", col, "']])"),
        "```",
        "",
        "```{r, echo=FALSE, fig.width=7, fig.height=5}",
        paste0("ggplot(df, aes(x = ", col, ")) + ",
               "geom_bar(fill = 'blue', color = 'black', alpha = 0.7) + ",
               "labs(title = 'Bar Plot of ", col, "')"),
        "```",
        ""
      ), file = temp_report, append = TRUE, sep = "\n")
      
    } else if (type == "logical") {
      cat(c(
        "```{r, echo=FALSE}",
        paste0("table(df[['", col, "']])"),
        "```",
        ""
      ), file = temp_report, append = TRUE, sep = "\n")
    } else {
      cat(paste("This is a column of type", type, "\n\n"), file = temp_report, append = TRUE)
    }
  }
  
  # Render the R Markdown to HTML
  rmarkdown::render(temp_report, output_file = output_file)
  
  # Print the location of the output file
  message("Report generated: ", output_file)
}


# explore all the columns of a dataset
# created by Aitor Vázquez Veloso, 2024-07-10
# parameters:
# - df: dataset to explore
data_overview <- function(df){
  
  # loop to select each df column
  for(col in colnames(df)){
    
    # select column
    my_col <- df[[col]]
    
    # inspect column
    type <- typeof(my_col)
    if(type == "integer" | type == "double"){
      
      print(paste("Column", col, "is numeric"))
      # get summary and histogram
      print(summary(my_col))
      hist(my_col)
      
    } else if(type == "character"){
      
      print(paste("Column", col, "is character"))
      # get summary
      print(unique(my_col))
      print(table(my_col))
      
    } else if(type == "logical"){
      print(paste("Column", col, "is logical"))
    } else {
      print(paste("Column", col, "is of type", type))
    }
  }
}  

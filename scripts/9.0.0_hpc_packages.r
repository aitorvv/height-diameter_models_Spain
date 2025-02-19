#!/usr/bin/Rscript

install.packages('tidyverse')
# install.packages('rgdal')
install.packages("~/Downloads/rgdal_1.6-7.tar.gz", repos = NULL, type = "source")
install.packages('broom')
install.packages('minpack.lm')
install.packages('nlme')
install.packages('ggplot2')
install.packages('rmarkdown')
install.packages('sp')
install.packages('raster')

print('Packages installed successfully!')
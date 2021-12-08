# clean up workspace
rm(list=ls())

# close all figure windows created with x11()
graphics.off()

# load packages
library(tidyverse)
library(dataRetrieval)
library(zoo)
library(lubridate)
library(mapdata)
library(ggsci)
library(RColorBrewer)
library(purrr)
library(colorspace)
library(modifiedmk)
library(scales)
library(ggrepel)
library(ggsci)

# change directory
my_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_dir)

# list  functions in 'Functions'
my_R_files <- list.files(path='./Functions/',
                         pattern = '*.R',
                         full.names=TRUE)

# Load all functions in R
sapply(my_R_files, source)

# Import data script
source('01-import_and_clean_data.R')

# Import data visualization script
source('02-Data_Visualization.R')


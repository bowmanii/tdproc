# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR1-R1
# Author: Isabella Bowman
# Created: Apr 22 2024
# Last updated: May 03, 2025
# Description: Barometric Efficiency

# https://github.com/bowmanii
# https://github.com/jkennel

# activate applicable packages
library(data.table)
library(ggplot2)
library(plotly)
library(remotes)
library(readxl)
library(viridis)
library(dplyr)
library(lubridate)
library(collapse)

library(rsk)
library(transducer)
library(hydrorecipes)

# read in RBR data -------------------------------------------------------------

# set where data files are located
file_dir <- "data/"
# assign loc variable to excel file that has port depths, file names, s/n's, etc
# ensure NA in file is read as na in R
loc <- read_xlsx("./metadata/transducer_locations.xlsx", na = "NA")
# create a data.table using the metadata file we just read in
setDT(loc)
# redefine DT to only include for ELR1-R1
#loc <- loc[well == "ELR1-R1"]
#loc <- loc[well %in% c("ELR1-R1", "ELR1-R2")]
#loc <- loc[well == "ELR1-R2" | serial == "213655"]
#loc <- loc[well == "ELR2-R1" | serial == "213655"]
loc <- loc[well == "ELR2-R2" | serial == "213655"]
# use grep to only include rsk files from the file_name column
loc <- loc[grep("rsk", file_name)]
# list all file names from "data" folder, return full file path, only .rsk files
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.rsk")
# cross reference the data files to the data table file_name column
fn <- fn[basename(fn) %in% loc$file_name]

# grep = matches to an argument 

# subset to only include files from first seal
# use fn_sub for trail transducers as 2 download dates
# use fn for road transducers as only 1 download date

#fn_sub <- fn[grep("20240626", fn)] # for R1-R1_1
# return everything except for 20240626
#fn_sub <- fn[grep("20240626", fn, invert = TRUE)] # for R1-R1_2
#fn_sub <- fn[grep("20240624|20240626", fn, invert = FALSE)] # for R1-R2_1
#fn_sub <- fn[grep("20240624|20240626", fn, invert = TRUE)] # for R1-R2_2

# read transducer files using Kennel's package
baro <- rsk::read_rsk(fn[c(1)],
                    return_data_table = TRUE,
                    include_params = c('file_name'),
                    simplify_names = TRUE,
                    keep_raw = FALSE,
                    raw = TRUE)[variable == "pressure"]

wl <- rsk::read_rsk(fn[c(3)],
                      return_data_table = TRUE,
                      include_params = c('file_name'),
                      simplify_names = TRUE,
                      keep_raw = FALSE,
                      raw = TRUE)[variable == "pressure"]

# clean up filename col by removing file paths
baro[, file_name := basename(file_name)]
wl[, file_name := basename(file_name)]

# combine dt's while also calculating the mean value of each
wl_baro <- baro[,.(datetime, baro = value - mean(value))][wl[, .(datetime, value = value - mean(value))], on = "datetime", nomatch = 0]

# subset date range
# goal = times where there is no pumping, its "stable"
wl_baro <- wl_baro[between(datetime, as.POSIXct("2024-04-17", tz = "UTC"), as.POSIXct("2024-04-24", tz = "UTC"))] #R1-R1_1, R1-R2_1
#wl_baro <- wl_baro[between(datetime, as.POSIXct("2024-07-14", tz = "UTC"), as.POSIXct("2024-07-19", tz = "UTC"))] #R1-R1_2, R1-R2_2

# visualize the data
# dependent variable = wl pressure
# independent variable = baro pressure
# range is 0 to 1, with increments of 0.05, 0.01, etc
hydrorecipes::be_visual(wl_baro,
                        dep = "value", ind = "baro", time = "datetime",
                        be_tests = seq(0, 1.0, 0.01), inverse = FALSE)


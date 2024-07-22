# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR1-R1, ELR1-R2
# Author: Isabella Bowman
# Created: Thursday July 18 2024
# Last updated: July 20, 2024
# Description: Processing temporary deployment data from April 5 2024 - June 24/25 2024 on trail wells

# https://github.com/jkennel
# https://github.com/bowmanii

# activate applicable packages
library(data.table)
library(ggplot2)
library(plotly)
library(remotes)
library(readxl)
library(viridis)

# pull in Kennel's packages if don't already have
#remotes::install_github("jkennel/rsk")
#remotes::install_github("jkennel/transducer")
#remotes::install_github("jkennel/hydrorecipes")

library(rsk)
library(transducer)
library(hydrorecipes)

# constants
dbar_to_m <- 1.0199773339984 # rbr data reads pressure in dbar, convert to m of H20

#####################################################################
#well1 <- "ELR1-R1"
#well2 <- "ELR1-R2"

# Q for Kennel:
## bad practice to use a variable that has char attached to it in another variables name?
## aka air calib? need? (time btwn programmed and installed, aka air calib?)
## how do you organize your files? project folder? etc? (for excel files for data (meta data?) vs data files, etc)
## changed .xlsx to have zeros in front of S/N, port#, did not import that way in R?
## diff btwn "../.." vs "./filename" vs "~/" (could not find answer on google)
## how do you only analyze data from the one TD set we are interested in? (see line 83,84)
## file_dir not working?? (line87)
## can you have the pattern return 2 file types? (c() did not work)

#####################################################################
#### For future use after trial run w Jennie ####
# air calibration
#air_start_well1 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC")
#air_end_well1 <- as.POSIXct("2024-04-02 16:01:00", tz = "UTC")
#air_start_well2 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC")
#air_end_well2 <- as.POSIXct("2024-04-02 16:54:00", tz = "UTC")

# OH monitoring
#blend_start_well1 <- as.POSIXct("2024-04-02 16:26:00", tz = "UTC")
#blend_end_well1 <- as.POSIXct("2024-04-05 17:46:00", tz = "UTC")
#blend_start_well2 <- as.POSIXct("2024-04-02 17:12:00", tz = "UTC")
#blend_end_well2 <- as.POSIXct("2024-04-05 14:49:00", tz = "UTC")

# sealed hole
#seal_start_well1 <- as.POSIXct("2024-04-05 18:33", tz = "UTC")
#seal_end_well1 <- as.POSIXct("2024-06-25 19:28", tz = "UTC")
#seal_start_well2 <- as.POSIXct("2024-04-05 15:43", tz = "UTC")
#seal_end_well2 <- as.POSIXct("2024-06-24 14:35", tz = "UTC")
#####################################################################

# start and end times of sealed conditions, ELR1-R1
s <- as.POSIXct("2024-04-05 18:33:00", tz = "UTC")
e <- as.POSIXct("2024-06-25 19:28:00:", tz = "UTC")

# set where data files are located
file_dir <- "../..data"

# assign loc variable to excel file that has port depths, file names, s/n's, etc
# ensure NA in file is read as na in R
loc <- read_xlsx("./metadata/transducer_locations.xlsx", na = "NA")
# create a data.table using the metadata file we just read in
setDT(loc)
# redefine DT to only include for ELR1-R1
loc <- loc[well == "ELR1-R1"]
# use grep to only include rsk files from the file_name column
loc <- loc[grep("rsk", file_name)]
###############################################################################
## have rbr_short here, don't want to use it, from previous deployment (row19)
## want to include diver file (.csv or .dat?)
###############################################################################

#fn <- list.files(file_dir, full.names = TRUE, pattern = "*.rsk")
#fn <- list.files("./data", full.names = TRUE, pattern = c("*.rsk", "*.csv"))
##############################################################################

# list all file names from "data" folder, return full file path, only .rsk files
fn <- list.files("./data", full.names = TRUE, pattern = "*.rsk")
# cross reference the data files to the data table file_name column
fn <- fn[basename(fn) %in% loc$file_name]

###############################################################################
# using Kennels transducer package, read 18 transducers from our fn variable
##??  not sure here what fn[1] is doing?
### this is breaking R for me. aborting is this for jennie??
#transducer::read_transducer(fn[1])
###############################################################################

###########################
## tried to read in 18 files, wont do it, super long and never finishes
###########################

# using Kennels rsk package, read 18 transducers from our fn variable
pr <- rsk::read_rsk(fn[1],
                    return_data_table = FALSE,
                    include_params = c('file_name'),
                    keep_raw = TRUE,
                    raw = TRUE)
pr <- pr[variable %in% c("pressure")]
## something not right with this code...
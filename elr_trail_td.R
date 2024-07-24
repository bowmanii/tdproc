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
# Q for Kennel:
## " vs '
## how is c() being used
## for the diver baro, use .dat or .csv file?
## why do I get "rbr_channels = <Promise>" after running line 93
## line ~103-105
## what does on= do for dt manipulation? direct sub "on" the same values?
## follow up, by=??

#####################################################################
#### For future use after trial run w Jennie ####

#well1 <- "ELR1-R1"
#well2 <- "ELR1-R2"

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
#seal_start_well1 <- as.POSIXct("2024-04-05 18:33:00", tz = "UTC")
#seal_end_well1 <- as.POSIXct("2024-06-25 19:28:00", tz = "UTC")
#seal_start_well2 <- as.POSIXct("2024-04-05 15:43:00", tz = "UTC")
#seal_end_well2 <- as.POSIXct("2024-06-24 14:35:00", tz = "UTC")

# t-profile
# for well1: estimated times, no notes taken?
#tprof_start_well1 <- as.POSIXct("2024-06-26 14:41:00", tz = "UTC")
#tprof_end_well1 <- as.POSIXct("2024-06-26 14:44:00", tz = "UTC")
#tprof_start_well2 <- as.POSIXct("2024-06-25 17:04:00", tz = "UTC")
#tprof_end_well2 <- as.POSIXct("2024-06-25 17:10:00", tz = "UTC")

#####################################################################

# start and end times of sealed conditions, ELR1-R1
seal_start_well1 <- as.POSIXct("2024-04-05 18:33:00", tz = "UTC")
seal_end_well1 <- as.POSIXct("2024-06-25 19:28:00:", tz = "UTC")

# set where data files are located
file_dir <- "data/"

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

# list all file names from "data" folder, return full file path, only .rsk files
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.rsk")
# cross reference the data files to the data table file_name column
fn <- fn[basename(fn) %in% loc$file_name]

# using Kennels rsk package, read 1 transducer file from our fn variable to get the pressure data
# returns the stored data as a data.table, includes the file name
pr <- rsk::read_rsk(fn[c(1,3)],
                    return_data_table = TRUE,
                    include_params = c('file_name'),
                    keep_raw = TRUE,
                    raw = TRUE)
###################################################################
#dont understand this line of code really, what does c() do here?
#why are we seeing if variable is in pressure? isnt it backwards?
# is it selecting rows within the variable col??
###################################################################
# redefine pr to look in the variable column for pressure (4 columns)
# look in the variable column for this exact string(match)
pr <- pr[variable %in% c("pressure")]
###################################################################
# no idea what this does
# does it match the rbr pr data to our file names, then we reference that below to merge?
###################################################################
# ignore rows (no manipulation), in cols, beside the file_name col, add the following substitution:
# for all subs type "data/ "? followed by file_name, use exact matching (use existing column)
pr[, file_name := gsub('data/', '', file_name, fixed = TRUE)]

# merges both data tables together into pr (13 cols)
# rows = pr data.table, matches data by file name?
# bring in the loc DT to pr, match data with file_name
pr <- loc[pr, on = "file_name"]
##################################################################

# create baro dt from pr subset using condition when port is equal to baro_rbr
baro <- pr[port == "baro_rbr"]
# create wl dt from pr subset using condition that excludes all ports equal to baros or liners
wl <- pr[!port %in% c("baro_rbr", "liner")]
#wl <- pr[!port %in% c("baro_rbr", "liner", "rbr_diver")]
# brings in the baro value to line up with port data
# no rows will be returned if no match
# using the baro dt, exclude looking at rows, look at cols:
# using baro dt, use datetime to match columns between both dts to the wl dt, create new column baro that has the baro value, if no match, no value
wl <- baro[, list(datetime, baro = value)][wl, on = "datetime", nomatch = 0]

# calculate water height above transducer from pressure, baro pr, port depth (make new col called "head")
wl[, head := (value - baro) * dbar_to_m - monitoring_location]

# sorts wl data table by date time
####### descending order??
setkey(wl, datetime)

# subset the wl dt by desired times
wl_sub <- wl[datetime %between% c(seal_start_well1,seal_end_well1)]
# make new col in dt, calculation is pressure - the first pressure entry (2024-04-05 18:33:00)
wl_sub[, value_adj := value - value[1], by = port]

# show subset in a plot
# set 300 entries to 0, means looking at every 5 min data
p1 <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~value_adj,
              color = ~port,
              colors = viridis(20),
              type = "scatter", mode = "lines")

# call the plot
p1

# plot baro
p2 <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~baro,
              type = "scatter", mode = "lines")

# display numerous plots in single view
subplot(p1, p2, shareX = TRUE, nrows = 2)












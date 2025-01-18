# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR1-R1, ELR1-R2
# Author: Isabella Bowman
# Created: Jan 16, 2025
# Last updated: Jan 17, 2025
# Description: Processing air monitoring period for trail wells

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

# pull in Kennel's packages if don't already have
#remotes::install_github("jkennel/rsk")
#remotes::install_github("jkennel/transducer")
#remotes::install_github("jkennel/hydrorecipes")

library(rsk)
library(transducer)
library(hydrorecipes)

###############################################################################
# Q for Kennel:
## 

###############################################################################
#### Constants ####

dbar_to_m <- 1.0199773339984 # rbr data reads pressure in dbar, convert to m of H20

#well1 <- "ELR1-R1"
#well2 <- "ELR1-R2"

# well elevation (m amsl)
elev1 <- 377.540 + 0.580
elev2 <- 379.612 + 0.530

# air calibration
air_start_well1 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC") # R1-R1
#air_end_well1 <- as.POSIXct("2024-04-02 16:01:00", tz = "UTC") # R1-R1
# trim the interval (excludes erroneous data from transporting)
air_end_well1 <- as.POSIXct("2024-04-02 14:30:00", tz = "UTC") # R1-R1
#air_start_well2 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC") # R1-R2
#air_end_well2 <- as.POSIXct("2024-04-02 16:54:00", tz = "UTC") # R1-R2
air_start_well3 <- as.POSIXct("2024-06-26 13:00:00", tz = "UTC") # R1-R1
air_end_well3 <- as.POSIXct("2024-07-09 14:50:00", tz = "UTC") # R1-R1
#air_start_well4 <- as.POSIXct("2024-06-25 13:00:00", tz = "UTC") # R1-R2
#air_end_well4 <- as.POSIXct("2024-06-25 19:52:00", tz = "UTC") # R1-R2

# OH monitoring (blended head)
blend_start_well1 <- as.POSIXct("2024-04-02 16:26:00", tz = "UTC") # R1-R1
blend_end_well1 <- as.POSIXct("2024-04-05 17:46:00", tz = "UTC") # R1-R1
#blend_start_well2 <- as.POSIXct("2024-04-02 17:12:00", tz = "UTC") # R1-R2
#blend_end_well2 <- as.POSIXct("2024-04-05 14:49:00", tz = "UTC") # R1-R2
blend_start_well3 <- as.POSIXct("2024-07-29 17:35:00", tz = "UTC") # R1-R1
blend_end_well3 <- as.POSIXct("2024-08-09 18:20:00", tz = "UTC") # R1-R1

###############################################################################
#### Data Manipulation ####

# set where data files are located
file_dir <- "data/"

# assign loc variable to excel file that has port depths, file names, s/n's, etc
# ensure NA in file is read as na in R
loc <- read_xlsx("./metadata/transducer_locations.xlsx", na = "NA")
# create a data.table using the metadata file we just read in
setDT(loc)
# redefine DT to only include for ELR1-R1
loc <- loc[well == "ELR1-R1"]
#loc <- loc[well %in% c("ELR1-R1", "ELR1-R2")]
#loc <- loc[well == "ELR1-R2" | serial == "213655"]
# use grep to only include rsk files from the file_name column
loc <- loc[grep("rsk", file_name)]

# list all file names from "data" folder, return full file path, only .rsk files
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.rsk")
# cross reference the data files to the data table file_name column
fn <- fn[basename(fn) %in% loc$file_name]

# using Kennels rsk package, read 1 transducer file from our fn variable to get the pressure data
# returns the stored data as a data.table, includes the file name
# simplify names uses "pressure_compensated" values when they exist (new RBR's record this), 
# if not, use the "pressure" value instead. TRUE = do this command
# raw, keep_raw is about what data it retains
pr <- rsk::read_rsk(fn[c(1:36)],
                    return_data_table = TRUE,
                    include_params = c('file_name'),
                    simplify_names = TRUE,
                    keep_raw = FALSE,
                    raw = TRUE)

# subset pr to only include this exact string (match) in the variable column
pr <- pr[variable %in% c("pressure")]

# clean up filename col by removing file paths
pr[, file_name := basename(file_name)]
# ignore rows (no manipulation), in cols, beside the file_name col, add the following substitution:
# text replacement, replace w empty string, looking in file_name (basename wasn't working)
#pr[, file_name := gsub('data/', '', file_name, fixed = TRUE)]

# make loc, pr dt smaller before merging
loc[, c("site", "is_baro", "use") := NULL]
pr[, c("variable") := NULL]
# make tables smaller before manipulations
pr <- pr[datetime %between% c(air_start_well1, air_end_well1)]

# bring in the loc DT to pr (13 cols), match data on file_name col
pr <- loc[pr, on = "file_name"]

# create a new dt to perform further manipulations
air <- pr

# clean up unneeded cols
air[, c("well", "serial", "screen_top", "screen_bottom") := NULL]

# add port name to monitoring location
air[, portloc := paste(paste(port, monitoring_location, sep = " - "), "mbtoc")]

# calculate elevation of transducer monitoring point
air[, sensor_elev := elev1 - monitoring_location]

# make new col in dt, calculation is pressure - the first pressure entry
air[, value_adj := value - value[1], by = port]

# convert all pressures to m H20
air[, value_m := value * dbar_to_m]

# sorts wl data table by date time (ascending order)
setkey(air, datetime)

###############################################################################
#### Plots ####

# locate all ports that are not baro from port col
ports <- unique(air$port[air$port != "baro_rbr"])

# assign custom colour to baro, all other ports follow viridis
# make sure to match viridis # to # of ports for each well
# viridis(#) = # of colours needed in the colour palette
#custom_colors <- c("baro_rbr" = "#ee8326", setNames(viridis(16), ports))

# alternate option
# have it set to auto calculate # of colours based on # of ports
custom_colors <- c("baro_rbr" = "#ee8326", setNames(viridis(16)[1:length(ports)], ports))

p_air <- plot_ly(air,
                 x = ~datetime,
                 y = ~value_adj, #or value, value_adj, value_m, etc
                 color = ~port,
                 colors = custom_colors,
                 name = ~portloc,
                 type = "scatter", mode = "lines") %>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Δ Pressure (dbar)"), # Δ Pressure (dbar), Pressure (dbar)
    legend = list(traceorder = "reversed")
  )

















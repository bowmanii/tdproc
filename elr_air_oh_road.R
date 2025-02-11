# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR1-R1, ELR1-R2
# Author: Isabella Bowman
# Created: Feb 05, 2025
# Last updated: Feb 06, 2025
# Description: Processing air monitoring period for trail wells - ELR2-R1

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

#well1 <- "ELR2-R1"
#well2 <- "ELR2-R2"

# well elevation (m amsl)
elev1 <- 402.491 + 0.210
elev2 <- 402.013 + 0.600

# air calibration
air_start_well1 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC")
air_end_well1 <- as.POSIXct("2024-04-02 18:36:00", tz = "UTC")
#air_start_well2 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC")
#air_end_well2 <- as.POSIXct("2024-04-02 18:13:00", tz = "UTC")

# OH monitoring
blend_start_well1 <- as.POSIXct("2024-04-02 18:50:00", tz = "UTC")
blend_end_well1 <- as.POSIXct("2024-04-04 17:45:00", tz = "UTC")
#blend_start_well2 <- as.POSIXct("2024-04-02 18:26:00", tz = "UTC")
#blend_end_well2 <- as.POSIXct("2024-04-04 15:05:00", tz = "UTC")

###############################################################################
#### Data Manipulation ####

# trim air calibration periods (to account for erroneous data)
airtrim_start_well1 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC")
airtrim_end_well1 <- as.POSIXct("2024-04-02 14:30:00", tz = "UTC")
#airtrim_start_well2 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC")
#airtrim_end_well2 <- as.POSIXct("2024-04-02 14:30:00", tz = "UTC")

# set where data files are located
file_dir <- "data/"

# assign loc variable to excel file that has port depths, file names, s/n's, etc
# ensure NA in file is read as na in R
loc <- read_xlsx("./metadata/transducer_locations.xlsx", na = "NA")
# create a data.table using the metadata file we just read in
setDT(loc)
# only include entries that read either well name from the well column
#loc <- loc[well == "ELR2-R2" | port == "baro_rbr"]
loc <- loc[well == "ELR2-R1" | serial == "213655"]
#loc <- loc[well == "ELR2-R1"]
#loc <- loc[well %in% c("ELR2-R1", "ELR2-R2")]
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
pr <- rsk::read_rsk(fn[c(1:2, 4:18)],
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
pr <- pr[datetime %between% c(blend_start_well1, blend_end_well1)] # air, airtrim, blend

# bring in the loc DT to pr (13 cols), match data on file_name col
pr <- loc[pr, on = "file_name"]

# dt to process all ports together (keep baro,liner as rows)
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

# dt to proccess ports separately from baro,liner (pull in as cols)
# create baro dt from pr subset using condition when port is equal to baro_rbr
baro <- pr[port == "baro_rbr"]
# create liner dt from pr subset
liner <- pr[port == "liner"]
# new dt
wl <- pr[!port %in% c("baro_rbr", "liner")]
# clean up unneeded cols
wl[, c("well", "serial", "screen_top", "screen_bottom") := NULL]
# add port name to monitoring location
wl[, portloc := paste(paste(port, monitoring_location, sep = " - "), "mbtoc")]
# add baro to dt
wl <- baro[, .(datetime, baro = value)][wl, on = "datetime", nomatch = NA]
# add liner to dt
wl <- liner[, .(datetime, liner = value)][wl, on = "datetime", nomatch = NA]
# calculate elevation of transducer monitoring point
wl[, sensor_elev := elev1 - monitoring_location]
# make new col in dt, calculation is pressure - the first pressure entry
wl[, value_adj := value - value[1], by = port]
# convert all pressures to m H20
wl[, value_m := value * dbar_to_m]
wl[, baro_m := baro * dbar_to_m]
wl[, liner_m := liner * dbar_to_m]
# calculate head
wl[, head_masl := sensor_elev + (value_m - baro_m)]
# sorts wl data table by date time (ascending order)
setkey(wl, datetime)

###############################################################################
#### Plots ####

# locate all ports that are not baro from port col
#ports <- unique(air$port[air$port != "baro_rbr"])
ports <- unique(air$port[!air$port %in% c("baro_rbr", "liner")])
#ports <- unique(air$port[!air$port %in% c("baro_rbr1", "baro_rbr2", "liner")])

# assign custom colour to baro, all other ports follow viridis
# make sure to match viridis # to # of ports for each well (16, 21, 14, 20)
# viridis(#) = # of colours needed in the colour palette
custom_colors <- c("baro_rbr" = "#ee8326", "liner" = "#a42c27", setNames(viridis(14), ports))

# ELR1-R1 baro s/n: 213655, ELR2-R1 baro s/n: 213650
#custom_colors <- c("baro_rbr2" = "#ee8326", "baro_rbr1" = "#a90261", "liner" = "#a42c27", setNames(viridis(14), ports))

# alternate option
# have it set to auto calculate # of colours based on # of ports
# don't do this! no good
#custom_colors <- c("baro_rbr" = "#ee8326", setNames(viridis(20)[1:length(ports)], ports))

# depending on how long period is, may need to subset data
# [as.numeric(datetime) %% 10 == 0]
p_air <- plot_ly(air[as.numeric(datetime) %% 10 == 0],
                 x = ~datetime,
                 y = ~value_adj, #or value, value_adj, value_m, etc
                 color = ~port,
                 colors = custom_colors,
                 name = ~portloc,
                 type = "scatter", mode = "lines") %>%
  layout(
    title = list(text = "ELR2-R1: Air Monitoring", # Air, Open Hole
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Δ Pressure (dbar)"), # Δ Pressure (dbar), Pressure (dbar)
    legend = list(traceorder = "reversed")
  )

# depending on how long period is, may need to subset data
# [as.numeric(datetime) %% 10 == 0]
p_wl <- plot_ly(wl[as.numeric(datetime) %% 10 == 0],
                x = ~datetime,
                y = ~head_masl, #or value, value_adj, value_m, head_masl, etc
                color = ~port,
                colors = viridis(14),
                name = ~portloc,
                type = "scatter", mode = "lines")

# plot baro
p_baro <- plot_ly(wl[as.numeric(datetime) %% 10 == 0],
                  x = ~datetime,
                  y = ~baro_m,
                  line = list(color = "#ee8326"),
                  name = "Baro - 0.58 mbtoc",
                  type = "scatter", mode = "lines")

# plot liner
p_liner <- plot_ly(wl[as.numeric(datetime) %% 10 == 0],
                   x = ~datetime,
                   y = ~liner_m,
                   line = list(color = "#a42c27"),
                   name = "Liner - 15.00 mbtoc",
                   type = "scatter", mode = "lines")

# plot liner and baro together
p_baro_liner <- add_trace(p_liner,
                          x = ~datetime,
                          y = ~baro_m,
                          line = list(color = "#ee8326"),
                          name = "Baro - 0.58 mbtoc",
                          type = "scatter", mode = "lines")

# combine plots
s1 <- subplot(p_wl, p_baro_liner, shareX = TRUE, nrows = 2, heights = c(0.8, 0.2))%>%
  layout(
    title = list(text = "ELR2-R1: Open Hole Monitoring", # Air, Open Hole
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Hydraulic Head (masl)"), # Δ Pressure (dbar), Pressure (dbar), (m H20)
    yaxis2 = list(title = "Pressure (m H20)"), # Δ Pressure (dbar), Pressure (dbar), (m H20)
    legend = list(traceorder = "reversed")
  )









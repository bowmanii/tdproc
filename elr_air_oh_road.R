# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR1-R1, ELR1-R2
# Author: Isabella Bowman
# Created: Feb 05, 2025
# Last updated: Feb 19, 2025
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
#elev2 <- 402.013 + 0.600

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

# interval for air average
# take ~10 min where late time is stable, average baro
airavg_start_well1 <- as.POSIXct("2024-04-02 14:20:00", tz = "UTC") # R2-R1
airavg_end_well1 <- as.POSIXct("2024-04-02 14:30:00", tz = "UTC") # R2-R1
# baro average (Apr 2 14:20 - 14:30)
bavg1 <- 9.697238

# for blended calibration
# manual wl Apr 04 @ 17:15 = 17.722 mbtoc, 384.979 masl
manual_well1 <- as.POSIXct("2024-04-04 17:15:00", tz = "UTC") # R2-R1
manual_wl1 <- 384.979
# take +/- 1 min on either side of manual dtw, average
blendavg_start_well1 <- as.POSIXct("2024-04-04 17:14:00", tz = "UTC") # R2-R1
blendavg_end_well1 <- as.POSIXct("2024-04-04 17:16:00", tz = "UTC") # R2-R1

# set where data files are located
file_dir <- "data/"

# assign loc variable to excel file that has port depths, file names, s/n's, etc
# ensure NA in file is read as na in R
loc <- read_xlsx("./metadata/transducer_locations.xlsx", na = "NA")
# create a data.table using the metadata file we just read in
setDT(loc)
# only include entries that read either well name from the well column
#loc <- loc[well == "ELR2-R1" | port == "baro_rbr"]
#loc <- loc[well == "ELR2-R2" | serial == "213650"] # road baro
loc <- loc[well == "ELR2-R1" | serial == "213655"] # trail baro
#loc <- loc[well == "ELR2-R1"]
#loc <- loc[well %in% c("ELR2-R1", "ELR2-R2")]
#loc <- loc[serial %in% c("213655", "213650", "203042", "82210")]
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
pr <- rsk::read_rsk(fn[c(1:2, 4:18)], #excluding road baro
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

#### air monitoring period ####

# dt to process all ports together (keep baro,liner as rows)
# create a new dt to perform further manipulations
#air <- pr
air <- pr[datetime %between% c(airavg_start_well1, airavg_end_well1)]
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

#### manipulations for air correction factor ####

# find baro average
#vavg <- mean(pr$value)
#ans <- air[, .(mean(value)), by = port]

# find averages 
air[, avg := mean(value), by = port]

# calculate correction factor (cf) for each transducer from baro avg
#air[, cf := bavg - avg] # dbar
air[, cf := (bavg1 - avg) * dbar_to_m]
air_sub <- air[, !c("well", "screen_top", "screen_bottom", "value")]
air_short <- unique(air_sub, by = "port")
write.csv(air_short, "out/ELR2-R1_air_cf.csv")

#### open hole period ####

# dt to proccess ports separately from baro,liner (pull in as cols)
# create baro dt from pr subset using condition when port is equal to baro_rbr
baro <- pr[port == "baro_rbr"]
# create liner dt from pr subset
liner <- pr[port == "liner"]
# new dt
wl <- pr[!port %in% c("baro_rbr", "liner")]
# subset
wl <- wl[datetime %between% c(blendavg_start_well1, blendavg_end_well1)]

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

#### manipulations for manual correction factor ####

# get average for each port
#ans <- wl[, .(mean(value)), by = port] # one way to do it
wl[, avg := mean(value), by = port] # better way to keep result in dt as col
wl[, avg_baro := mean(baro), by = port]
wl[, avg_liner := mean(liner), by = port]
wl[, avg_m := avg *dbar_to_m]
wl[, avg_head := sensor_elev + (avg_m - avg_baro)]

# calculate correction factor (cf) for each transducer from manual wl
wl[, cf := manual_wl1 - avg_head]
# get smaller dt to export
wl_sub <- wl[, list(file_name, serial, port, monitoring_location, datetime,
                    avg, avg_m, avg_head, cf, avg_baro, avg_liner)]
wl_short <- unique(wl_sub, by = "port")
write.csv(wl_short, "out/ELR2-R1_blend_cf.csv")

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
p_air <- plot_ly(air,
                 x = ~datetime,
                 y = ~value, #or value, value_adj, value_m, value_cf
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
    yaxis = list(title = "Pressure (dbar)"), # Δ Pressure (dbar), Pressure (dbar)
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
    title = list(text = "ELR2-R1: Open Hole Monitoring", # Air Monitoring, Open Hole Monitoring
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Hydraulic Head (masl)"), # Δ Pressure (dbar), Pressure (dbar), (m H20)
    yaxis2 = list(title = "Pressure (m H20)"), # Δ Pressure (dbar), Pressure (dbar), (m H20)
    legend = list(traceorder = "reversed")
  )

# compare liners, baros
custom_colors <- c("213650" = "#ee8326", "213655" = "#a42c27", "82215" = "#3fb195", "82209" = "#953eb1")

# ELR1-R1 baro s/n: 213655, ELR2-R1 baro s/n: 213650
# ELR1-R1 liner s/n: 82215, ELR1-R2 liner s/n: 82209
# ELR2-R1 liner s/n: 203042, ELR2-R2 liner s/n: 82210

# [as.numeric(datetime) %% 10 == 0]
p_bl <- plot_ly(air[as.numeric(datetime) %% 10 == 0],
                x = ~datetime,
                y = ~value_adj, #or value, value_adj, value_m, etc
                color = ~serial,
                colors = custom_colors,
                name = ~serial,
                type = "scatter", mode = "lines") %>%
  layout(
    title = list(text = "Road", # Air, Open Hole
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Δ Pressure (dbar)"), # Δ Pressure (dbar), Pressure (dbar)
    legend = list(traceorder = "reversed")
  )







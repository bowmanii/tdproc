# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR1-R1, ELR1-R2
# Author: Isabella Bowman
# Created: Jun 18 2025
# Last updated: Jul 11 2025
# Description: Processing temporary deployment data from 2024 on trail wells - ELR1-R1

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
# make sure to clear workspace and restart R first
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
#elev2 <- 379.612 + 0.530

# air calibration
air_start_well1 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC") # R1-R1
air_end_well1 <- as.POSIXct("2024-04-02 16:01:00", tz = "UTC") # R1-R1
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

# sealed hole - first deployment
seal_start_well1 <- as.POSIXct("2024-04-05 18:33:00", tz = "UTC")
seal_end_well1 <- as.POSIXct("2024-06-25 19:28:00", tz = "UTC")
#seal_start_well2 <- as.POSIXct("2024-04-05 15:43:00", tz = "UTC")
#seal_end_well2 <- as.POSIXct("2024-06-24 14:35:00", tz = "UTC")

# sealed hole - second deployment
seal_start_well3 <- as.POSIXct("2024-07-09 17:32:00", tz = "UTC")
seal_end_well3 <- as.POSIXct("2024-07-29 15:03:00", tz = "UTC")
#seal_start_well4 <- as.POSIXct("2024-06-25 21:49:00", tz = "UTC")
#seal_end_well4 <- as.POSIXct("2024-10-18 15:44:00", tz = "UTC")

# pumping data for sealed holes
# 2023 data. Waiting for 2024 data
cw_pump_start1 <- as.POSIXct("2024-04-05 18:00:00", tz = "UTC")
cw_pump_end1 <- as.POSIXct("2024-06-25 20:00:00", tz = "UTC")
#cw_pump_start2 <- as.POSIXct("2024-04-05 15:00:00", tz = "UTC")
#cw_pump_end2 <- as.POSIXct("2024-06-24 15:00:00", tz = "UTC")
cw_pump_start3 <- as.POSIXct("2024-07-09 17:00:00", tz = "UTC")
cw_pump_end3 <- as.POSIXct("2024-07-29 15:00:00", tz = "UTC")
#cw_pump_start4 <- as.POSIXct("2024-06-25 21:00:00", tz = "UTC")
#cw_pump_end4 <- as.POSIXct("2024-10-18 16:00:00", tz = "UTC")

# climate data
cw_rain_start1 <- as.POSIXct("2024-04-05 18:00:00", tz = "UTC")
cw_rain_end1 <- as.POSIXct("2024-06-25 20:00:00", tz = "UTC")
#cw_rain_start2 <- as.POSIXct("2024-04-05 15:00:00", tz = "UTC")
#cw_rain_end2 <- as.POSIXct("2024-06-24 15:00:00", tz = "UTC")
cw_rain_start3 <- as.POSIXct("2024-07-09 17:00:00", tz = "UTC")
cw_rain_end3 <- as.POSIXct("2024-07-29 15:00:00", tz = "UTC")
#cw_rain_start4 <- as.POSIXct("2024-06-25 21:00:00", tz = "UTC")
#cw_rain_end4 <- as.POSIXct("2024-10-18 16:00:00", tz = "UTC")

# flute liner installs/removals for ELR1-R1
#flute_start_well1 <- as.POSIXct("2024-04-05 17:46:00", tz = "UTC") # install
#flute_end_well1 <- as.POSIXct("2024-04-05 18:33:00", tz = "UTC") # install
#flute_start_well3 <- as.POSIXct("2024-06-25 19:28:00", tz = "UTC") # removal
#flute_end_well3 <- as.POSIXct("2024-06-25 20:30:00", tz = "UTC") # removal
#flute_start_well5 <- as.POSIXct("2024-07-08 14:30:00", tz = "UTC") # removal
#flute_end_well5 <- as.POSIXct("2024-07-08 16:08:00", tz = "UTC") # removal
#flute_start_well7 <- as.POSIXct("2024-07-09 16:39:00", tz = "UTC") # install
#flute_end_well7 <- as.POSIXct("2024-07-09 17:32:00", tz = "UTC") # install
#flute_start_well9 <- as.POSIXct("2024-07-29 15:03:00", tz = "UTC") # removal
#flute_end_well9 <- as.POSIXct("2024-07-29 16:32:00", tz = "UTC") # removal

# flute liner installs/removals for ELR1-R2
#flute_start_well2 <- as.POSIXct("2024-04-05 14:49:00", tz = "UTC") # install
#flute_end_well2 <- as.POSIXct("2024-04-05 15:47:00", tz = "UTC") # install
#flute_start_well4 <- as.POSIXct("2024-06-24 14:35:00", tz = "UTC") # removal
#flute_end_well4 <- as.POSIXct("2024-06-24 15:26:00", tz = "UTC") # removal
#flute_start_well6 <- as.POSIXct("2024-06-25 17:58:00", tz = "UTC") # removal
#flute_end_well6 <- as.POSIXct("2024-06-25 18:41:00", tz = "UTC") # removal
#flute_start_well8 <- as.POSIXct("2024-06-25 21:18:00", tz = "UTC") # install
#flute_end_well8 <- as.POSIXct("2024-06-25 21:49:00", tz = "UTC") # install
#flute_start_well10 <- as.POSIXct("2024-10-18 15:44:00", tz = "UTC") # removal
#flute_end_well10 <- as.POSIXct("2024-10-18 16:37:00", tz = "UTC") # removal

# t-profile
# for well1: estimated times, no notes taken?
#tprof_start_well1 <- as.POSIXct("2024-06-26 14:41:00", tz = "UTC")
#tprof_end_well1 <- as.POSIXct("2024-06-26 14:44:00", tz = "UTC")
tprof_start_well2 <- as.POSIXct("2024-06-25 17:04:00", tz = "UTC")
tprof_end_well2 <- as.POSIXct("2024-06-25 17:10:00", tz = "UTC")

# Packer Testing
# don't know when it ended
#pack_start_well1 <- as.POSIXct("2024-08-09 18:20:00", tz = "UTC")
#pack_end_well1 <- as.POSIXct("2024-08-23 04:00:00", tz = "UTC")

###############################################################################
#### Data Manipulation ####

# adjust for -34 min/+20 min before/after
#tprof_s <- as.POSIXct("2024-06-25 16:30:00", tz = "UTC")
#tprof_e <- as.POSIXct("2024-06-25 17:30:00", tz = "UTC")
#tprof_wt <- as.POSIXct("2024-06-25 16:37:00", tz = "UTC")

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

# read in centre wellington data (9 sheets), specify sheet, rows to skip
cw_e4 <- read_xlsx("./data/cw_wells.xlsx", sheet = "Well E4", skip = 2)
setDT(cw_e4)
# assign column headers to dt
colnames(cw_e4) <- c("time", "flow", "drawdown", "waterlevel", "comments")
# take difference between hourly flow rate (they are cumulative daily), reset every 24hrs
cw_e4[, flow_hrly_avg := ifelse((as.numeric(time) %% 86400) == 0, flow, flow - lag(flow))]
# overwrite timezone to EST/EDT (read_xlsx auto assumes UTC, this is wrong in this case)
cw_e4[, datetime := force_tz(time, tzone = "America/Toronto")]
# convert EDT/EST time zones to UTC, will auto adjust for time shifts
cw_e4[, datetime_utc := with_tz(datetime, tzone = "UTC")]
# clean up dt - remove unnecessary columns
cw_e4[, c("comments", "datetime") := NULL]
# remove outliers
z_score <- scale(cw_e4$drawdown) # standardizing the data
outliers <- cw_e4$drawdown[abs(z_score) > 3]
upper_limit <- 45.0000
lower_limit <- 8.0000
cw_e4_outliers_dd <- subset(cw_e4, drawdown <= upper_limit & drawdown >= lower_limit)
# subset data (for memory and performance), keep desired cols and pump data by desired times
cw_e4_sub <- cw_e4_outliers_dd[, .(datetime_utc, flow_hrly_avg, drawdown)][datetime_utc %between% c(cw_pump_start1, cw_pump_end3)]

cw_e3 <- read_xlsx("./data/cw_wells.xlsx", sheet = "Well E3", skip = 2)
setDT(cw_e3)
colnames(cw_e3) <- c("time", "flow", "drawdown", "waterlevel", "comments")
cw_e3[, flow_hrly_avg := ifelse((as.numeric(time) %% 86400) == 0, flow, flow - lag(flow))]
cw_e3[, datetime := force_tz(time, tzone = "America/Toronto")]
cw_e3[, datetime_utc := with_tz(datetime, tzone = "UTC")]
cw_e3[, c("comments", "datetime") := NULL]
cw_e3_sub <- cw_e3[, .(datetime_utc, flow_hrly_avg, drawdown)][datetime_utc %between% c(cw_pump_start1, cw_pump_end3)]

cw_e1 <- read_xlsx("./data/cw_wells.xlsx", sheet = "Well E1", skip = 2)
setDT(cw_e1)
colnames(cw_e1) <- c("time", "flow", "drawdown", "waterlevel", "comments")
cw_e1[, flow_hrly_avg := ifelse((as.numeric(time) %% 86400) == 0, flow, flow - lag(flow))]
cw_e1[, datetime := force_tz(time, tzone = "America/Toronto")]
cw_e1[, datetime_utc := with_tz(datetime, tzone = "UTC")]
cw_e1[, c("comments", "datetime") := NULL]
# do this. above didnt work.
z_score <- scale(cw_e1$flow_hrly_avg) # standardizing the data
outliers <- cw_e1$flow_hrly_avg[abs(z_score) > 2]
upper_limit <- 114.0000
lower_limit <- 0.0000
cw_e1_outliers <- subset(cw_e1, flow_hrly_avg <= upper_limit & flow_hrly_avg >= lower_limit)
cw_e1_sub <- cw_e1_outliers[, .(datetime_utc, flow_hrly_avg, drawdown)][datetime_utc %between% c(cw_pump_start1, cw_pump_end3)]

###############################################################################
# this was first attempt, manually changing, good ref but found quicker way above!

# define EDT vs EST time zones
# assuming CW data is in EDT/EST tz
# Note: these are 4 hours behind the "true" local time
# have to do this because read_xlsx sets default tz to UTC so dt thinks its working in UTC
# ex. edt1_start_true <- as.POSIXct("2023-03-12 02:00:00")
# ex. edt1_end_true <- as.POSIXct("2023-11-05 01:00:00")

#edt1_start <- as.POSIXct("2023-03-11 22:00:00")
#edt1_end <- as.POSIXct("2023-11-04 21:00:00")

#est1_start <- as.POSIXct("2023-11-04 22:00:00")
#est1_end <- as.POSIXct("2024-03-09 21:00:00")

#edt2_start <- as.POSIXct("2024-03-09 22:00:00")
#edt2_end <- as.POSIXct("2024-11-02 21:00:00")

# make a new col where time is in UTC and corrected for EDT
# 60sec*60min*4hr/60sec*60min*5hr
#cw_e4[time %between% c(edt1_start, edt1_end), time_utc := time + 3600*4]
#cw_e4[time %between% c(est1_start, est1_end), time_utc := time + 3600*5]
#cw_e4[time %between% c(edt2_start, edt2_end), time_utc := time + 3600*4]

# check what type of data working with
#cl <- class(cw_e4$time_utc)
#tz <- attr(cw_e4$time_utc, "tzone")
#cl2 <- class(wl_sub$datetime)
###############################################################################

# precipitation data - monthly files - Elora RCS
# read in files (file paths) using the data dir and subsetting by csv files only
fp <- list.files(file_dir, full.names = TRUE, pattern = "*.csv")
# can also subset by characters in file name (if other csv's present)
#fp2 <- list.files(file_dir, full.names = TRUE, pattern = "Elora_RCS")
# read data (individually)
rd <- lapply(fp, fread, fill = TRUE)
# combine data into one data table
rcs <- rbindlist(rd)
# convert datetime column from char to POSIxct class type
rcs[, datetime := as.POSIXct(`Date/Time (UTC)`, format = "%Y-%m-%d %H:%M", tz = "UTC")]
# clean up dt - remove empty cols
rcs[, c("Temp Flag", "Dew Point Temp Flag", "Rel Hum Flag", "Precip. Amount Flag", 
        "Wind Dir Flag", "Wind Spd Flag", "Visibility (km)", "Visibility Flag", 
        "Stn Press Flag", "Hmdx Flag", "Wind Chill Flag") := NULL]
# subset data by cols and times
rcs_sub <- rcs[, .(datetime, `Precip. Amount (mm)`)][datetime %between% c(cw_rain_start1, cw_rain_end3)]
# clean up memory by setting rd, rcs to null
rd <- NULL
rcs <- NULL

# get correction factors into dt
cf_air1 <- read.csv("./out/ELR1-R1_air_cf_td1_use.csv") # air correction factors
setDT(cf_air1)
cf_air2 <- read.csv("./out/ELR1-R1_air_cf_td2_use.csv") # air correction factors
setDT(cf_air2)
cf_man1 <- read.csv("./out/ELR1-R1_blend_cf_td1_use.csv") # manual wl correction factors
setDT(cf_man1)
cf_man2 <- read.csv("./out/ELR1-R1_blend_cf_td2_use.csv") # manual wl correction factors
setDT(cf_man2)
# merge each cf type into one dt
cf_a <- list(cf_air1, cf_air2)
cf_air <- rbindlist(cf_a)
cf_m <- list(cf_man1, cf_man2)
cf_man <- rbindlist(cf_m)
cf_a <- NULL
cf_air1 <- NULL
cf_air2 <- NULL
cf_m <- NULL
cf_man1 <- NULL
cf_man2 <- NULL

# clean up memory after - garbage collection
gc()

# list all file names from "data" folder, return full file path, only .rsk files
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.rsk")
# cross reference the data files to the data table file_name column
fn <- fn[basename(fn) %in% loc$file_name]
# grep = matches to an argument 
#fn_sub <- fn[grep("20240626", fn)]

# using Kennels rsk package, read 1 transducer file from our fn variable to get the pressure data
# returns the stored data as a data.table, includes the file name
# simplify names uses "pressure_compensated" values when they exist (new RBR's record this), 
# if not, use the "pressure" value instead. TRUE = do this command
# raw, keep_raw is about what data it retains
pr <- rsk::read_rsk(fn[c(1:36)],
#pr <- rsk::read_rsk(fn_sub[c(1:2,10)],
#pr <- rsk::read_rsk(fn[c(1:6)],
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
pr <- pr[datetime %between% c(seal_start_well1, seal_end_well3)] #seal1-1, seal 3-3, seal 1-3

# bring in the loc DT to pr (13 cols), match data on file_name col
pr <- loc[pr, on = "file_name"]

# create baro dt from pr subset using condition when port is equal to baro_rbr
baro <- pr[port == "baro_rbr"]
# create liner dt from pr subset
liner <- pr[port == "liner"]
# create wl dt from pr subset using condition that excludes all ports equal to baros or liners
wl <- pr[!port %in% c("baro_rbr", "liner")]
#wl <- pr[!port %in% c("baro_rbr", "liner", "rbr_diver")]

# clean up memory - dt's no longer using,, unneeded cols
baro[, c("well", "serial", "port", "screen_top", "screen_bottom", "monitoring_location") := NULL]
liner[, c("well", "serial", "port", "screen_top", "screen_bottom", "monitoring_location") := NULL]
#loc <- NULL
#pr <- NULL

# using baro dt, use datetime to match columns between both dts to the wl dt, create new column baro that has the baro value, if no match, no value
# nomatch=0 = drops rows w/out a match
# nomatch=NA = leftjoin, keeps all rows from left table, fills missing with NA
# nomatch=-1 = returns error when no match, join error
wl <- baro[, .(datetime, baro = value)][wl, on = "datetime", nomatch = NA]
# add liner pressure to wl dt
wl <- liner[, .(datetime, liner = value)][wl, on = "datetime", nomatch = NA]
# bring in correction factors
wl <- cf_air[, .(file_name, cf_air = cf)][wl, on = "file_name"]
wl <- cf_man[, .(file_name, cf_man = cf)][wl, on = "file_name"]
# check it worked
#ans <- wl[port == "01" & datetime == seal_start_well3]

# clean up memory, dts no longer needed
#baro <- NULL
#liner <- NULL

# add port name to monitoring location
wl[, portloc := paste(paste(port, monitoring_location, sep = " - "), "mbtoc")]

# calculate elevation of transducer monitoring point
wl[, sensor_elev := elev1 - monitoring_location]

# convert all pressures to m H20
wl[, baro_m := baro * dbar_to_m]
wl[, liner_m := liner * dbar_to_m]
wl[, value_m := value * dbar_to_m]

# make new col in dt, calculation is pressure - the first pressure entry (2024-04-05 18:33:00)
wl[, value_adj := value_m - value_m[1], by = port]
# calculate water height above transducer from pressure, baro pr, port depth (make new col called "head")
wl[, head_masl := sensor_elev + (value_m - baro_m)]
# correction factors
wl[, head_masl_cf_air := head_masl + cf_air]
wl[, head_masl_cf_man := head_masl + cf_man]

# clean up wl dt
wl[, c("liner", "baro", "well", "serial", "screen_top", "screen_bottom", 
       "monitoring_location", "value") := NULL]

# sorts wl data table by date time (ascending order)
setkey(wl, datetime)
#setkey(wl, file_name)

###############################################################################
#### Data Subsets ####

######## subset data earlier...decide if I like workflow or want this##########
# create new dt so don't have to change the code below :)
wl_sub <- wl

# subset the wl dt by desired times
#wl_sub <- wl[datetime %between% c(seal_start_well1, seal_end_well3)]
#wl_sub <- wl[datetime %between% c(tprof_s, tprof_e)]
#write.csv(wl_sub, "out/ELR1-R1_20240405_20240625.csv") # this takes like 1 hr, wont open in excel

# subset pumping data by desired times
#cw_e4_sub <- cw_e4[datetime_utc %between% c(cw_pump_start1, cw_pump_end3)]

# subset precipitation data by desired times
#rcs_sub <- rcs[datetime %between% c(cw_rain_start1, cw_rain_end3)]

###############################################################################
#### Plots ####

# show subset in a plot
# set 300 entries to 0, means looking at every 5 min data bc we record at 1sec
# p1 <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0]
p_wl <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~head_masl_cf_man, #or head_masl, or value_m, value_adj, 
                              #head_masl_cf_air, head_masl_cf_man, etc
              color = ~port,
              colors = viridis(16),
              name = ~portloc,
              type = "scatter", mode = "lines")

# plot baro
p_baro <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
                  x = ~datetime,
                  y = ~baro_m,
                  line = list(color = "#ee8326"),
                  name = "Baro",
                  type = "scatter", mode = "lines")

# plot liner
p_liner <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~liner_m,
              line = list(color = "#a42c27"),
              name = "Liner",
              type = "scatter", mode = "lines")

# plot precipitation
p_rain <- plot_ly(rcs_sub,
                  x = ~datetime,
                  y = ~`Precip. Amount (mm)`,
                  marker = list(color = "#cc72b0"),
                  name = "Precipitation",
                  type = "bar")

# plot flow
p_cw_e4 <- plot_ly(cw_e4_sub,
                   x = ~datetime_utc,
                   y = ~flow_hrly_avg,
                   line = list(color = "#37bac8"),
                   name = "E4 Pumping",
                   type = "scatter", mode = "lines")

p_cw_e3 <- plot_ly(cw_e3_sub,
                   x = ~datetime_utc,
                   y = ~flow_hrly_avg,
                   line = list(color = "#8e37c8"),
                   name = "E3 Pumping",
                   type = "scatter", mode = "lines")

p_cw_e1 <- plot_ly(cw_e1_sub,
                   x = ~datetime_utc,
                   y = ~flow_hrly_avg,
                   line = list(color = "#c83771"),
                   name = "E1 Pumping",
                   type = "scatter", mode = "lines")

# plot drawdown
p_cw_e4_dd <- plot_ly(cw_e4_sub,
                   x = ~datetime_utc,
                   y = ~drawdown,
                   line = list(color = "#37bac8"),
                   name = "E4 Drawdown",
                   type = "scatter", mode = "lines")

p_cw_e3_dd <- plot_ly(cw_e3_sub,
                   x = ~datetime_utc,
                   y = ~drawdown,
                   line = list(color = "#8e37c8"),
                   name = "E3 Drawdown",
                   type = "scatter", mode = "lines")

p_cw_e1_dd <- plot_ly(cw_e1_sub,
                   x = ~datetime_utc,
                   y = ~drawdown,
                   line = list(color = "#c83771"),
                   name = "E1 Drawdown",
                   type = "scatter", mode = "lines")

# merging baro and liner plots together on one
p_baro_liner <- add_trace(p_liner, 
                          x = ~datetime, 
                          y = ~baro_m, 
                          name = "Baro",
                          type = "scatter", mode = "lines") %>%
  layout(
    title = "Liner Vs. Baro Response", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)"),
    legend = list(trace0 = "Liner", trace1 = "Baro")
  )

# find the 7/8 observations that plotly ignored (warning message)
missing_obs <- rcs_sub[is.na(`Precip. Amount (mm)`), ]

###############################################################################
# t-profile plot layout

# p1 <- plot_ly(wl_sub,
#               x = ~datetime,
#               y = ~value_adj, #or head_masl, or value_m, value_adj, etc
#               color = ~port,
#               colors = viridis(16),
#               name = ~portloc,
#               type = "scatter", mode = "lines")%>%
#   layout(
#     title = "ELR1-R1: Tprofile Response from ELR1-R2", 
#     xaxis = list(title = "Date and time (1 sec)",
#                  nticks = 20,
#                  tickangle = -45),
#     yaxis = list(title = "ΔPressure (m H20)"), 
#     legend = list(traceorder = "reversed"),
#     shapes = list(
#       list(
#         type = "line",
#         x0 = tprof_start_well2,
#         x1 = tprof_start_well2,
#         y0 = -0.1, # change according to data range
#         y1 = 0.7, # change according to data range
#         line = list(
#           color = "red",
#           width = 2
#         )
#       ),
#       list(
#         type = "line",
#         x0 = tprof_end_well2,
#         x1 = tprof_end_well2,
#         y0 = -0.1,
#         y1 = 0.7,
#         line = list(
#           color = "red",
#           width = 2
#         )
#       ),
#       list(
#         type = "line",
#         x0 = tprof_wt,
#         x1 = tprof_wt,
#         y0 = -0.1,
#         y1 = 0.7,
#         line = list(
#           color = "red",
#           width = 2
#         )
#       )
#     ),
#     annotations = list(
#       list(
#         x = tprof_wt,   # X coordinate for the annotation
#         y = 0.7,  # Y coordinate for the annotation
#         text = "Liner @ WT (16:34)", # Text for the annotation
#         showarrow = TRUE, # Whether to show an arrow
#         arrowhead = 2, # Style of the arrow
#         ax = 0, # X offset for the text
#         ay = -30 # Y offset for the text (- = point down, + = point up)
#       ),
#       list(
#         x = tprof_start_well2,
#         y = 0.7,
#         text = "Start (17:04)",
#         showarrow = TRUE,
#         arrowhead = 2,
#         ax = -20,
#         ay = -30
#       ),
#       list(
#         x = tprof_end_well2,
#         y = 0.7,
#         text = "End (17:10)",
#         showarrow = TRUE,
#         arrowhead = 2,
#         ax = 20,
#         ay = -30
#       )
#     )
#   )
# y0 = min(wl_sub$head_masl),
# y1 = max(wl_sub$head_masl),
# y0 = 367.7,
# y1 = 369.5,
# xref = "x2"
###############################################################################

###############################################################################
#### Subplots ####

# display numerous plots in single view, customize plot features (axis titles, etc) using layout
# to make axis values reverse: yaxis=list(autorange="reversed")
# or do range in opposite order (c(100,50) etc)
# custom axis range: range=list(1.5,4.5)
# minor=list(nticks=50)
# minor = list(nticks = 140, showgrid = TRUE, gridcolor = "lightgrey", tickmode = "linear")
# shapes = list(line(tprof_start_well2))
s0 <- subplot(p_wl, p_baro, shareX = TRUE, nrows = 2)%>%
  layout(
    title = "ELR1-R1: Temporary Deployment", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Head (m asl)", 
                 range = c(367, 373.5)), # Δ Pressure (m H20)
    yaxis2 = list(title = "Pressure (m H20)"),
    legend = list(traceorder = "reversed")
  )

# plot baro, liner, wl together
s1 <- subplot(p_wl, p_baro, p_liner, shareX = TRUE, nrows = 3, heights = c(0.7, 0.15, 0.15))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment", 
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Head (m asl)", 
                 range = c(367, 373.5)), # Δ Pressure (m H20)
    yaxis2 = list(title = "Pressure (m H20)"),
    yaxis3 = list(range = c(14.4, 15.5)),
    legend = list(traceorder = "reversed")
  )

# plot wl, baro, liner, pump together
s2 <- subplot(s1, p_cw, shareX = FALSE, nrows = 2, heights = c(0.75, 0.25))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis2 = list(title = "Date and time"),
    yaxis3 = list(title = "Head (m asl)"), # Δ Pressure (m H20)
    yaxis2 = list(title = "Pressure (m H20)"),
    yaxis4 = list(title = "Avg Flow (m3/hr)"),
    legend = list(traceorder = "reversed")
  )

# plot wl, baro, liner, pump, rain together
s3 <- subplot(s1, p_rain, p_cw, shareX = FALSE, nrows = 3, heights = c(0.5, 0.25, 0.25))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis3 = list(title = "Date and time"),
    yaxis3 = list(title = "Head (m asl)"), # Δ Pressure (m H20)
    yaxis = list(title = "Pressure (m H20)"),
    yaxis4 = list(title = "Precip (mm/hr)"),
    yaxis5 = list(title = "Avg Flow (m3/hr)"),
    legend = list(traceorder = "reversed")
  )

# plot wl, baro, liner, rain together
s4 <- subplot(p_wl, sba, p_rain, shareX = TRUE, nrows = 3, heights = c(0.7, 0.1, 0.2))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment - No Corr", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Head (m asl)", # Δ Pressure (m H20)
                  range = c(367, 373.5)),
    yaxis2 = list(title = "Pressure (m H20)"),
    yaxis4 = list(title = "Precip (mm)"),
    legend = list(traceorder = "reversed")
  )

sba <- subplot(p_baro, p_liner, shareX = TRUE, nrows = 2, heights = c(0.5, 0.5))%>%
  layout(
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)"),
    yaxis2 = list(title = "Pressure (m H20)",
                  range = c(14.4, 15.4))
  )

scw <- subplot(p_cw_e4, p_cw_e3, p_cw_e1, shareX = TRUE, nrows = 3, heights = c(0.33, 0.33, 0.33))%>%
  layout(
    title = list(text = "Centre Wellington Elora Well Cluster (E4, E3, E1)", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Avg Flow (m3/hr)"),
    yaxis2 = list(title = "Avg Flow (m3/hr)"),
    yaxis3 = list(title = "Avg Flow (m3/hr)")
  )

s6 <- subplot(p_wl, p_baro, p_liner, p_rain, shareX = TRUE, nrows = 4, heights = c(0.55, 0.1, 0.1, 0.25))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment - No Corr", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Δ Pressure (m H20)", # Δ Pressure (m H20)
                 range = c(-4, 2)),
                 #autorange ="reversed"),
    yaxis2 = list(title = "Pressure (m H20)"),
    yaxis3 = list(range = c(14.4, 15.5)),
    yaxis4 = list(title = "Precip (mm)"),
    legend = list(traceorder = "reversed")
  )

# plot wl, baro, liner, pump, rain together
s5 <- subplot(s4, scw, shareX = TRUE, nrows = 2, heights = c(0.7, 0.3))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis2 = list(title = "Date and time"),
    yaxis4 = list(title = "Head (m asl)"), # Δ Pressure (m H20)
    yaxis3 = list(title = "Pressure<br>(m H20)"),
    yaxis = list(title = "Precip<br>(mm/hr)"),
    yaxis6 = list(title = "Avg Flow (m3/hr)"),
    legend = list(traceorder = "reversed")
  )

# plot wl, baro, liner, rain together
s7 <- subplot(p_wl, p_baro, p_liner, p_rain, p_cw, shareX = TRUE, nrows = 5, heights = c(0.5, 0.1, 0.1, 0.15, 0.15))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment - Manual Corr", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Head (m asl)", # Δ Pressure (m H20)
                 range = c(367, 373.5)),
    yaxis2 = list(title = "Pressure (m H20)"),
    yaxis3 = list(range = c(14.4, 15.5)),
    yaxis4 = list(title = "Precip (mm)"),
    yaxis5 = list(title = "Avg Flow (m3/hr)"),
    legend = list(traceorder = "reversed")
  )


s8 <- subplot(p_wl, p_cw_e4, p_cw_e3, p_cw_e1, shareX = TRUE, nrows = 4, heights = c(0.55, 0.15, 0.15, 0.15))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45,
                 showgrid = FALSE),
    yaxis = list(title = "Head (m asl)", # Δ Pressure (m H20)
                 range = c(367, 373.5)),
    yaxis3 = list(title = "Avg Flow (m3/hr)"),
    legend = list(traceorder = "reversed")
  )

s9 <- subplot(p_wl, p_cw_e4_dd, p_cw_e3_dd, p_cw_e1_dd, shareX = TRUE, nrows = 4, heights = c(0.55, 0.15, 0.15, 0.15))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45,
                 showgrid = FALSE),
    yaxis = list(title = "Head (m asl)", # Δ Pressure (m H20)
                 range = c(367, 373.5)),
    yaxis3 = list(title = "Drawdown (m)"),
    legend = list(traceorder = "reversed")
  )

scw_flow <- subplot(p_cw_e4, p_cw_e3, p_cw_e1, shareX = TRUE, nrows = 3, heights = c(0.33, 0.33, 0.33))%>%
  layout(
    title = list(text = "Centre Wellington Elora Well Cluster (E4, E3, E1)", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Avg Flow (m3/hr)"),
    yaxis2 = list(title = "Avg Flow (m3/hr)"),
    yaxis3 = list(title = "Avg Flow (m3/hr)")
  )

scw_dd <- subplot(p_cw_e4_dd, p_cw_e3_dd, p_cw_e1_dd, shareX = TRUE, nrows = 3, heights = c(0.33, 0.33, 0.33))%>%
  layout(
    title = list(text = "Centre Wellington Elora Well Cluster (E4, E3, E1)", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Drawdown (mbtoc)"),
    yaxis2 = list(title = "Drawdown (mbtoc)"),
    yaxis3 = list(title = "Drawdown (mbtoc)")
  )

s10 <- subplot(p_wl, scw_flow, scw_dd, shareX = TRUE, nrows = 3, heights = c(0.4, 0.3, 0.3))%>%
  layout(
    title = list(text = "ELR1-R1: Temporary Deployment", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45,
                 showgrid = FALSE),
    yaxis = list(title = "Head (m asl)", # Δ Pressure (m H20)
                 range = c(367, 373.5)),
    yaxis3 = list(title = "Avg Flow (m3/hr)"),
    yaxis6 = list(title = "Drawdown (m)"),
    legend = list(traceorder = "reversed")
  )




###############################################################################
#### Extract Processed Data ####

# create DT for vertical head profiles
#vhp <- wl[datetime %in% as.POSIXct(c("2024-05-12 8:45:00", "2024-05-18 12:25:00"), tz = "UTC")] #old choice
vhp <- wl_sub[datetime %in% as.POSIXct(c("2024-04-05 18:35:00", "2024-05-12 12:45:00", 
                                         "2024-05-18 18:35:00", "2024-05-31 18:15:00", 
                                         "2024-06-03 18:05:00", "2024-07-25 14:00:00", 
                                         "2024-07-25 19:00:00", "2024-07-25 21:45:00"), tz = "UTC")]
#write.csv(vhp, "ELR1-R1_vhp_Hamid.csv")
# shorten table
vhp <- vhp[, list(datetime, port, head_masl_cf_man, head_masl_cf_air, head_masl)]
write.csv(vhp, "out/ELR1-R1_vhp_v5.csv")

vhp2 <- wl_sub[datetime %in% as.POSIXct(c("2024-07-09 17:35:00"), tz = "UTC")]
vhp2 <- vhp2[, list(datetime, port, head_masl_cf_man, head_masl_cf_air, head_masl)]
write.csv(vhp2, "out/ELR1-R1_vhp_v6.csv")

###############################################################################
#### Data Table Manipulations ####

# shorten the number of cols in wl_sub to only these 4
wl_sub <- wl_sub[,list(datetime, value, baro, port)]

# new dt using wl_sub with three cols
wl_wide <- data.table::dcast(wl_sub, datetime+baro~port)
# using wl_wide dt, replace 1,2,etc with "port_01", etc
setnames(wl_wide, c("1","2"), c("port_01", "port_02"), skip_absent = TRUE)


###############################################################################
#### Barometric Efficiency ####

hydrorecipes::be_visual(wl_sub, dep = "value_m",
                        ind = "baro_m", time = "datetime",
                        be_tests = seq(0, 1.0, 0.5), inverse = FALSE)


###############################################################################
#### Barometric Deconvolution Example ####

# creating a formula
# add port 1 and 2, as a function of baro and datetime
# datetime+baro = LHS = rows
# ~port = RHS = cols
# y variable = LHS, explanation on RHS
frm <- formula(port_01 + port_02~baro + datetime)
# ?? ~.??
# . = "everything else"
# 2 response variable = port 1,2 = y
# everything else as explanatory variables, "regressors", "x's"
frm2 <- formula(port_01 + port_02~.)
# recipe, takes formula, data from wl_wide
rec <- hydrorecipes::recipe(frm, data = wl_wide) |>
  # log_logs_arma(n, max_lag)
  step_distributed_lag(baro, knots = log_lags_arma(15, 86400)) |> #dont know what this does
  # step_harmonic(datetime, frequency = c(1,2), cycle_size = 86400) |>
  # df = degrees of freedom
  step_spline_b(datetime, df = 70) |>
  step_intercept() |>
  step_drop_columns(datetime) |>
  step_drop_columns(baro) |>
  step_ols(frm2) |>
  prep() |>
  bake()

# combine wl_wide with above recipe results? I think?
tmp <- cbind(wl_wide, rec$get_predict_data(type = "dt"))

# design plots
p3 <- plot_ly(tmp[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~port_01_step_distributed_lag_baro,
              name = "port_01_step_dist_lag_baro",
              type = "scatter", mode = "lines")
p4 <- plot_ly(tmp[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~baro,
              name = "baro",
              type = "scatter", mode = "lines")
p5 <- plot_ly(tmp[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~port_01_step_spline_b_datetime,
              name = "port_01_step_spline_b",
              type = "scatter", mode = "lines")
p6 <- plot_ly(tmp[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~port_01,
              name = "port_01",
              type = "scatter", mode = "lines")
# display all plots together
subplot(p3, p4, p5, p6, shareX = TRUE, nrows = 4)

# ?
frm3 <- formula(port_01~baro)
rec <- hydrorecipes::recipe(frm3, data = wl_wide) |>
  step_fft_transfer_experimental(c(port_01, baro), n_groups = 100) |>
  prep() |>
  bake()
# ?
a <- collapse::qDT(rec$get_step_data("fft_result")[[1]])

# plot it in a log plot
plot(Mod(fft_transfer_experimental)~frequency, a[1:10000], type = "l", log = "x", ylim = c(0,1))

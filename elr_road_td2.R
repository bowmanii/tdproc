# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR2-R1, ELR2-R2
# Author: Isabella Bowman
# Created: Dec 20, 2024
# Last updated: Mar 19, 2024
# Description: Processing temporary deployment data from 2024 on road wells (ELR2-R2)

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
well2 <- "ELR2-R2"

# well elevation (m asl)
#elev1 <- 402.491 + 0.210
elev2 <- 402.013 + 0.600

# air calibration
#air_start_well1 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC")
#air_end_well1 <- as.POSIXct("2024-04-02 18:36:00", tz = "UTC")
air_start_well2 <- as.POSIXct("2024-03-31 12:00:00", tz = "UTC")
air_end_well2 <- as.POSIXct("2024-04-02 18:13:00", tz = "UTC")

# OH monitoring
#blend_start_well1 <- as.POSIXct("2024-04-02 18:50:00", tz = "UTC")
#blend_end_well1 <- as.POSIXct("2024-04-04 17:45:00", tz = "UTC")
blend_start_well2 <- as.POSIXct("2024-04-02 18:26:00", tz = "UTC")
blend_end_well2 <- as.POSIXct("2024-04-04 15:05:00", tz = "UTC")

# sealed hole
#seal_start_well1 <- as.POSIXct("2024-04-04 19:22:00", tz = "UTC")
#seal_end_well1 <- as.POSIXct("2024-10-17 16:16:00", tz = "UTC")
seal_start_well2 <- as.POSIXct("2024-04-04 15:53:00", tz = "UTC")
seal_end_well2 <- as.POSIXct("2024-10-17 14:14:00", tz = "UTC")

# trim sealed period
sealtrim_start_well2 <- as.POSIXct("2024-04-05 20:00:00", tz = "UTC") # pumped water out, adjusting
sealtrim_end_well2 <- as.POSIXct("2024-08-21 14:05:00", tz = "UTC") #p20 last good entry 2024-08-21 14:05:00
#sealtrim_end_well2 <- as.POSIXct("2024-09-30 19:20:00", tz = "UTC") #p07 last good entry 2024-09-30 19:20:00

# pumping data for sealed holes
# 2023 data. Waiting for 2024 data
#cw_pump_start1 <- as.POSIXct("2024-04-04 19:00:00", tz = "UTC")
#cw_pump_end1 <- as.POSIXct("2024-10-17 17:00:00", tz = "UTC")
#cw_rain_end1 <- as.POSIXct("2024-10-03 19:00:00", tz = "UTC")
#cw_pump_start2 <- as.POSIXct("2024-04-04 15:00:00", tz = "UTC")
cw_pump_start2 <- as.POSIXct("2024-04-05 20:00:00", tz = "UTC")
#cw_pump_end2 <- as.POSIXct("2024-10-17 15:00:00", tz = "UTC")
cw_pump_end2 <- as.POSIXct("2024-08-21 15:00:00", tz = "UTC")

# climate data
#cw_rain_start1 <- as.POSIXct("2024-04-04 19:00:00", tz = "UTC")
#cw_rain_end1 <- as.POSIXct("2024-10-17 17:00:00", tz = "UTC")
#cw_rain_start2 <- as.POSIXct("2024-04-04 15:00:00", tz = "UTC")
cw_rain_start2 <- as.POSIXct("2024-04-05 20:00:00", tz = "UTC")
#cw_rain_end2 <- as.POSIXct("2024-10-17 15:00:00", tz = "UTC")
cw_rain_end2 <- as.POSIXct("2024-08-21 15:00:00", tz = "UTC")

# flute liner installs/removals for ELR2-R1
#flute_start_well1 <- as.POSIXct("2024-04-04 17:45:00", tz = "UTC") # install
#flute_end_well1 <- as.POSIXct("2024-04-04 19:22:00", tz = "UTC") # install
#flute_start_well3 <- as.POSIXct("2024-10-17 16:16:00", tz = "UTC") # removal
#flute_end_well3 <- as.POSIXct("2024-10-17 18:20:00", tz = "UTC") # removal

# flute liner installs/removals for ELR2-R2
#flute_start_well2 <- as.POSIXct("2024-04-04 15:05:00", tz = "UTC") # install
#flute_end_well2 <- as.POSIXct("2024-04-04 15:53:00", tz = "UTC") # install
#flute_start_well4 <- as.POSIXct("2024-10-17 14:14:00", tz = "UTC") # removal
#flute_end_well4 <- as.POSIXct("2024-10-17 15:13:00", tz = "UTC") # removal

###############################################################################
#### Data Manipulation ####

# adjust for -15 min/+8 min before/after
#flute_s <- as.POSIXct("2024-04-04 17:30:00", tz = "UTC")
#flute_e <- as.POSIXct("2024-04-04 19:30:00", tz = "UTC")
#flute_half <- as.POSIXct("2024-04-04 18:04:00", tz = "UTC")

# set where data files are located
file_dir <- "data/"

# assign loc variable to excel file that has port depths, file names, s/n's, etc
# ensure NA in file is read as na in R
loc <- read_xlsx("./metadata/transducer_locations.xlsx", na = "NA")
# create a data.table using the metadata file we just read in
setDT(loc)
# only include entries that read either well name from the well column
loc <- loc[well == "ELR2-R2" | serial == "213655"]
#loc <- loc[well == "ELR2-R2"]
#loc <- loc[well %in% c("ELR2-R2", "ELR2-R1")]
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
# subset data (for memory and performance), keep desired cols and pump data by desired times
cw_e4_sub <- cw_e4[, .(datetime_utc, flow_hrly_avg)][datetime_utc %between% c(cw_pump_start2, cw_pump_end2)]
# remove larger dt
cw_e4 <- NULL # clean up memory

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
rcs_sub <- rcs[, .(datetime, `Precip. Amount (mm)`)][datetime %between% c(cw_rain_start2, cw_rain_end2)]
# clean up memory by setting rd, rcs to null
rd <- NULL
rcs <- NULL
# clean up memory after - garbage collection
gc()

# get correction factors into dt
cf_air <- read.csv("./out/ELR2-R2_air_cf_use.csv") # air correction factors
setDT(cf_air)
cf_man <- read.csv("./out/ELR2-R2_blend_cf_use.csv") # manual wl correction factors
setDT(cf_man)

# list all file names from "data" folder, return full file path, only .rsk files
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.rsk")
# cross reference the data files to the data table file_name column
fn <- fn[basename(fn) %in% loc$file_name]

# using Kennels rsk package, read transducer files from our fn variable to get the pressure data
# returns the stored data as a data.table, includes the file name
# simplify names uses "pressure_compensated" values when they exist (new RBR's record this), 
# if not, use the "pressure" value instead. TRUE = do this command
# raw, keep_raw is about what data it retains
pr <- rsk::read_rsk(fn[c(1:10, 12:23)], # exclude port 08
                    return_data_table = TRUE,
                    include_params = c('file_name'),
                    simplify_names = TRUE,
                    keep_raw = FALSE,
                    raw = TRUE)

# subset pr to only include this exact string (match) in the variable column
pr <- pr[variable %in% c("pressure")]

# clean up filename col by removing file paths
pr[, file_name := basename(file_name)]

# make loc, pr dt smaller before merging
loc[, c("site", "is_baro", "use") := NULL]
pr[, c("variable") := NULL]
# make dt smaller - subset by seal time
pr <- pr[datetime %between% c(sealtrim_start_well2, sealtrim_end_well2)]

# bring in the loc DT to pr (9 cols), match data on file_name col
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
loc <- NULL
pr <- NULL

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
baro <- NULL
liner <- NULL

# add port name to monitoring location
wl[, portloc := paste(paste(port, monitoring_location, sep = " - "), "mbtoc")]

# calculate elevation of transducer monitoring point
wl[, sensor_elev := elev2 - monitoring_location]

# convert all pressures to m H20
wl[, baro_m := baro * dbar_to_m]
wl[, liner_m := liner * dbar_to_m]
wl[, value_m := value * dbar_to_m]

# make new col in dt, calculation is pressure - the first pressure entry (2024-0-0 ::00)
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

###############################################################################
#### Data Subsets ####

# subset the wl dt by desired times
#wl_sub <- wl[datetime %between% c(seal_start_well1, seal_end_well1)]
# create new dt so don't have to change the code below :)
wl_sub <- wl
#wl_sub <- wl[datetime %between% c(sealtrim_start_well2, sealtrim_end_well2)]
#test1 <- as.POSIXct("2024-04-04 12:00:00", tz = "UTC")
#test2 <- as.POSIXct("2024-04-05 12:00:00", tz = "UTC")
#wl_sub <- wl_sub[datetime %between% c(test1, test2)]

###### temp fix. this data is not baro corrected past Sept 26
#wl_sub[, head_masl := sensor_elev + (value_m - coalesce(baro_m, 0))]
#wl <- NULL
#wl_sub <- wl[datetime %between% c(tprof_s, tprof_e)]

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
              colors = viridis(20), #19?
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

# plot E4 flow rate
p_cw <- plot_ly(cw_e4_sub,
                x = ~datetime_utc,
                y = ~flow_hrly_avg,
                line = list(color = "#37bac8"),
                name = "E4 Pumping",
                type = "scatter", mode = "lines")


# find the 7/8 observations that plotly ignored (warning message)
missing_obs <- rcs_sub[is.na(`Precip. Amount (mm)`), ]

###############################################################################
# t-profile plot layout

# p1 <- plot_ly(wl_sub,
#               x = ~datetime,
#               y = ~value_adj, #or head_masl, or value_m, value_adj, etc
#               color = ~port,
#               colors = viridis(20),
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
# custom axis range: range=list(1.5,4.5)
# minor=list(nticks=50)
# minor = list(nticks = 140, showgrid = TRUE, gridcolor = "lightgrey", tickmode = "linear")
# shapes = list(line(tprof_start_well2))
s0 <- subplot(p_wl, p_baro, shareX = TRUE, nrows = 2)%>%
  layout(
    title = "ELR2-R2: Temporary Deployment", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Head (m asl)"), # Δ Pressure (m H20)
    yaxis2 = list(title = "Pressure (m H20)"),
    legend = list(traceorder = "reversed")
  )

# plot baro, liner, wl together
s1 <- subplot(p_wl, p_baro, p_liner, shareX = TRUE, nrows = 3, heights = c(0.7, 0.15, 0.15))%>%
  layout(
    title = list(text = "ELR2-R2: Temporary Deployment",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Head (m asl)"), # Δ Pressure (m H20)
    yaxis2 = list(title = "Pressure (m H20)"),
    legend = list(traceorder = "reversed")
  )

# plot wl, baro, liner, rain together
s4 <- subplot(p_wl, p_baro, p_liner, p_rain, shareX = TRUE, nrows = 4, heights = c(0.55, 0.1, 0.1, 0.25))%>%
  layout(
    title = list(text = "ELR2-R2: Temporary Deployment - Manual Corr", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Head (m asl)"), # Δ Pressure (m H20)
                 #range = c(364, 375.5)),
    yaxis2 = list(title = "Pressure (m H20)"),
    #yaxis3 = list(range = c(16, 17)),
    yaxis4 = list(title = "Precip (mm)"),
    legend = list(traceorder = "reversed")
  )

s6 <- subplot(p_wl, p_baro, p_liner, p_rain, shareX = TRUE, nrows = 4, heights = c(0.55, 0.1, 0.1, 0.25))%>%
  layout(
    title = list(text = "ELR2-R2: Temporary Deployment - No Corr", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)", # Δ Pressure (m H20)
                 #range = c(-4, 2)),
                 autorange ="reversed"),
    yaxis2 = list(title = "Pressure (m H20)"),
    #yaxis3 = list(range = c(9.7, 10.1)),
    yaxis4 = list(title = "Precip (mm)"),
    legend = list(traceorder = "reversed")
  )

# plot wl, baro, liner, pump, rain together
s5 <- subplot(s4, p_cw, shareX = FALSE, nrows = 2, heights = c(0.8, 0.2))%>%
  layout(
    title = list(text = "ELR2-R2: Temporary Deployment",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis2 = list(title = "Date and time"),
    yaxis4 = list(title = "Head (m asl)"), # Δ Pressure (m H20)
    yaxis3 = list(title = "Pressure (m H20)"),
    yaxis = list(title = "Precip (mm/hr)"),
    yaxis5 = list(title = "Avg Flow (m3/hr)"),
    legend = list(traceorder = "reversed")
  )

# plot wl, baro, liner, rain together
s7 <- subplot(p_wl, p_baro, p_liner, p_rain, p_cw, shareX = TRUE, nrows = 5, heights = c(0.5, 0.1, 0.1, 0.15, 0.15))%>%
  layout(
    title = list(text = "ELR2-R2: Temporary Deployment - Manual Corr", #Air Corr, Manual Corr, No Corr
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Head (m asl)"), # Δ Pressure (m H20)
    yaxis2 = list(title = "Pressure (m H20)"),
    #yaxis3 = list(range = c(9.7, 10.1)),
    yaxis4 = list(title = "Precip (mm)"),
    yaxis5 = list(title = "Avg Flow (m3/hr)"),
    legend = list(traceorder = "reversed")
  )

###############################################################################
#### Extract Processed Data ####

# create DT for vertical head profiles, use wl_sub (smaller dt)
#vhp <- wl[datetime %in% as.POSIXct(c("2024-05-12 12:45:00", "2024-05-18 18:35:00"), tz = "UTC")]
vhp <- wl_sub[datetime %in% as.POSIXct(c("2024-05-12 12:45:00", "2024-05-18 18:35:00"), tz = "UTC")]
#write.csv(vhp, "ELR1-R2_vhp_Hamid.csv")
# shorten table further if desired
vhp <- vhp[, list(datetime, well, port, monitoring_location, head_masl)]
write.csv(vhp, "out/ELR2-R2_vhp.csv")

vhp <- wl_sub[datetime %in% as.POSIXct(c("2024-04-06 10:10:00", "2024-05-12 12:45:00", 
                                         "2024-06-19 16:40:00", "2024-07-25 14:00:00", 
                                         "2024-08-09 12:50:00", "2024-08-09 4:00:00"), tz = "UTC")]
vhp <- vhp[, list(datetime, port, head_masl_cf_man, head_masl_cf_air, head_masl)]
write.csv(vhp, "out/ELR2-R2_vhp_v1.csv")

###############################################################################
#### Data Table Manipulations ####

# shorten the number of cols in wl_sub to only these 4
wl_sub <- wl_sub[,list(datetime, value, baro, port)]

# new dt using wl_sub with three cols
wl_wide <- data.table::dcast(wl_sub, datetime+baro~port)
# using wl_wide dt, replace 1,2,etc with "port_01", etc
setnames(wl_wide, c("1","2"), c("port_01", "port_02"), skip_absent = TRUE)

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

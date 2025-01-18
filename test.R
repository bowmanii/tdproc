# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR1-R1, ELR1-R2
# Author: Isabella Bowman
# Created: July 18 2024
# Last updated: Jan 17, 2025
# Description: Processing temporary deployment data from 2024 on trail wells (ELR1-R1)

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
# trim the interval (excludes data from transporting)
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

# clean up wl dt
wl[, c("liner", "baro", "well", "serial", "screen_top", "screen_bottom", 
       "monitoring_location", "value") := NULL]
# too many deleted: use this instead
wl[, c("well", "serial", "screen_top", "screen_bottom", 
       "monitoring_location") := NULL]

# sorts wl data table by date time (ascending order)
setkey(wl, datetime)

###############################################################################
#### Data Subsets ####

######## subset data earlier...decide if I like workflow or want this##########
# create new dt so don't have to change the code below :)
wl_sub <- wl

# air
pr_sub <-pr

###############################################################################
#### Plots ####

# show subset in a plot
# set 300 entries to 0, means looking at every 5 min data bc we record at 1sec
# p1 <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0]
p_wl <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
                x = ~datetime,
                y = ~head_masl, #or head_masl, or value_m, value_adj, etc
                color = ~port,
                colors = viridis(20),
                name = ~portloc,
                type = "scatter", mode = "lines")

# plot baro
p_baro <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
                  x = ~datetime,
                  y = ~baro_m,
                  line = list(color = "#ee8326"),
                  name = "Baro",
                  type = "scatter", mode = "lines")

#######################################################################################################
# for air monitoring plot

# make a special colour for baro

p_wl1 <- plot_ly(wl_sub,
                 x = ~datetime,
                 y = ~value, #or head_masl, or value_m, value_adj, etc
                 color = ~port,
                 colors = viridis(20),
                 name = ~portloc,
                 type = "scatter", mode = "lines") 

p_wl2 <- add_trace(p_wl1, 
                   x = baro$datetime, 
                   y = baro$value, 
                   type = "scatter", 
                   mode = "lines", 
                   name = "Baro") %>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Pressure (dbar)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )

# plot liner
p_liner <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
                   x = ~datetime,
                   y = ~liner_m,
                   line = list(color = "#a42c27"),
                   name = "Liner",
                   type = "scatter", mode = "lines")

# merging baro and liner plots together on one
p_baro_liner <- add_trace(p_liner, x = ~datetime, y = ~baro_m, type = "scatter", mode = "lines", name = "Baro") %>%
  layout(
    title = "Liner Vs. Baro Response", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)"),
    legend = list(trace0 = "Liner", trace1 = "Baro")
  )
####
p_wl1 <- plot_ly(wl_sub,
                 x = ~datetime,
                 y = ~value, #or head_masl, or value_m, value_adj, etc
                 color = ~port,
                 colors = viridis(20),
                 name = ~portloc,
                 type = "scatter", mode = "lines") %>%
  add_lines (
    x = unique(wl_sub$datetime),
    y = ~baro_m,
    line = list(color = "#ee8326"),
    name = "Baro"
  ) %>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Pressure (dbar)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )


custom_colors <- c("baro_rbr" = "#ee8326", setNames(viridis(20), unique(pr_sub$port[pr_sub$port != "baro_rbr"])))

p_wl_air <- plot_ly(pr_sub,
                    x = ~datetime,
                    y = ~value, #or head_masl, or value_m, value_adj, etc
                    color = ~port,
                    colors = custom_colors,
                    #colors = c(viridis(20), "baro_rbr" = "#ee8326"),
                    name = ~port,
                    type = "scatter", mode = "lines")

p_wl_air <- plot_ly(pr_sub,
                    x = ~datetime,
                    y = ~value, #or head_masl, or value_m, value_adj, etc
                    color = ~port,
                    colors = viridis(20),
                    name = ~port,
                    type = "scatter", mode = "lines")

# for air monitoring plot
p_baro_air <- plot_ly(wl_sub,
                      x = ~datetime,
                      y = ~baro_m,
                      line = list(color = "#ee8326"),
                      name = "Baro",
                      type = "scatter", mode = "lines")

p_air <- add_trace(wl_sub, x = ~datetime, y = ~value_m, color = ~port, colors = viridis(20), name = ~portloc, type = "scatter", mode = "lines")%>%
  add_trace(wl_sub, x = ~datetime, y = ~baro_m, line = list(color = "#ee8326"), type = "scatter", mode = "lines")%>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H2O)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )

s0 <- subplot(p_wl_air, p_baro_air, ncols = 1)%>%
  layout(
    title = "ELR1-R1: Temporary Deployment", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H2O)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )
#####################################################################################################################

# plot liner
p_liner <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
                   x = ~datetime,
                   y = ~liner_m,
                   line = list(color = "#a42c27"),
                   name = "Liner",
                   type = "scatter", mode = "lines")

# merging baro and liner plots together on one
p_baro_liner <- add_trace(p_liner, x = ~datetime, y = ~baro_m, type = "scatter", mode = "lines", name = "Baro") %>%
  layout(
    title = "Liner Vs. Baro Response", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)"),
    legend = list(trace0 = "Liner", trace1 = "Baro")
  )


################## attempts: trail.R
#line 307
# too many deleted: use this instead
wl[, c("well", "serial", "screen_top", "screen_bottom", 
       "monitoring_location") := NULL]
#line 320
# air
pr_sub <-pr

#######################################################################################################
# for air monitoring plot

# make a special colour for baro

p_wl1 <- plot_ly(wl_sub,
                 x = ~datetime,
                 y = ~value, #or head_masl, or value_m, value_adj, etc
                 color = ~port,
                 colors = viridis(20),
                 name = ~portloc,
                 type = "scatter", mode = "lines") %>%
  add_lines (
    x = ~datetime,
    y = ~baro,
    line = list(color = "#ee8326"),
    name = "Baro"
  ) %>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Pressure (dbar)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )

p_wl1 <- plot_ly(,
                 x = ~datetime,
                 y = ~value, #or head_masl, or value_m, value_adj, etc
                 color = ~port,
                 colors = viridis(20),
                 name = ~portloc,
                 type = "scatter", mode = "lines") %>%
  add_lines (
    x = ~datetime,
    y = ~baro,
    line = list(color = "#ee8326"),
    name = "Baro"
  ) %>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Pressure (dbar)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )


custom_colors <- c("baro_rbr" = "#ee8326", setNames(viridis(20), unique(pr_sub$port[pr_sub$port != "baro_rbr"])))

p_wl_air <- plot_ly(pr_sub,
                    x = ~datetime,
                    y = ~value, #or head_masl, or value_m, value_adj, etc
                    color = ~port,
                    colors = custom_colors,
                    #colors = c(viridis(20), "baro_rbr" = "#ee8326"),
                    name = ~port,
                    type = "scatter", mode = "lines")

p_wl_air <- plot_ly(pr_sub,
                    x = ~datetime,
                    y = ~value, #or head_masl, or value_m, value_adj, etc
                    color = ~port,
                    colors = viridis(20),
                    name = ~port,
                    type = "scatter", mode = "lines")

# for air monitoring plot
p_baro_air <- plot_ly(wl_sub,
                      x = ~datetime,
                      y = ~baro_m,
                      line = list(color = "#ee8326"),
                      name = "Baro",
                      type = "scatter", mode = "lines")

p_air <- add_trace(wl_sub, x = ~datetime, y = ~value_m, color = ~port, colors = viridis(20), name = ~portloc, type = "scatter", mode = "lines")%>%
  add_trace(wl_sub, x = ~datetime, y = ~baro_m, line = list(color = "#ee8326"), type = "scatter", mode = "lines")%>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H2O)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )

s0 <- subplot(p_wl_air, p_baro_air, ncols = 1)%>%
  layout(
    title = "ELR1-R1: Temporary Deployment", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H2O)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )
#####################################################################################################################
################## attempts: air.R

###############################################################################
#### Plots ####

custom_colors <- c("baro_rbr" = "#ee8326", setNames(viridis(20), unique(air$port[air$port != "baro_rbr"])))


p_air <- plot_ly(air,
                 x = ~datetime,
                 y = ~value_adj, #or head_masl, or value_m, value_adj, etc
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
    yaxis = list(title = "Value_adj (-)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )


p_air <- plot_ly(air,
                 x = ~datetime,
                 y = ~value_adj, #or head_masl, or value_m, value_adj, etc
                 color = ~port,
                 colors = ifelse(~port == "baro_rbr", "#ee8326", viridis(20)),
                 name = ~portloc,
                 type = "scatter", mode = "lines") %>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Value_adj (-)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )

p_air <- plot_ly(air,
                 x = ~datetime,
                 y = ~value_adj, #or head_masl, or value_m, value_adj, etc
                 color = ~port,
                 colors = c("baro_rbr" = "#ee8326", "01" = "blue"),
                 name = ~portloc,
                 type = "scatter", mode = "lines") %>%
  layout(
    title = list(text = "ELR1-R1: Air Monitoring",
                 y = 0.98,
                 font = list(size = 18)),
    xaxis = list(title = "Date and time",
                 nticks = 20, #~24hrsx3 = 72/20 = 3.6 -> ticks every 3 hrs
                 tickangle = -45),
    yaxis = list(title = "Value_adj (-)"), # Δ Pressure (m H20)
    legend = list(traceorder = "reversed")
  )


#Title: Elora TD Processing
# Site: ELR
# SiteID: ELORA
# Author: Isabella Bowman
# Created: July 17 2025
# Last updated: July 17, 2025
# Description: Processing pumping wells

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

# pumping data for each hole
cw_pump_start1 <- as.POSIXct("2024-04-05 18:00:00", tz = "UTC") # R1-R1
cw_pump_end1 <- as.POSIXct("2024-07-29 15:00:00", tz = "UTC") # R1-R1
cw_pump_start2 <- as.POSIXct("2024-04-05 15:00:00", tz = "UTC") # R1-R2
cw_pump_end2 <- as.POSIXct("2024-10-18 16:00:00", tz = "UTC") # R1-R2
cw_pump_start3 <- as.POSIXct("2024-04-04 19:00:00", tz = "UTC") # R2-R1
cw_pump_end3 <- as.POSIXct("2024-10-03 19:00:00", tz = "UTC") # R2-R1
cw_pump_start4 <- as.POSIXct("2024-04-05 20:00:00", tz = "UTC") # R2-R2
cw_pump_end4 <- as.POSIXct("2024-08-21 15:00:00", tz = "UTC") # R2-R2

# set where data files are located
file_dir <- "data/"

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
cw_e4_sub <- cw_e4[, .(datetime_utc, flow_hrly_avg)][datetime_utc %between% c(cw_pump_start3, cw_pump_end3)]

cw_e3 <- read_xlsx("./data/cw_wells.xlsx", sheet = "Well E3", skip = 2)
setDT(cw_e3)
colnames(cw_e3) <- c("time", "flow", "drawdown", "waterlevel", "comments")
cw_e3[, flow_hrly_avg := ifelse((as.numeric(time) %% 86400) == 0, flow, flow - lag(flow))]
cw_e3[, datetime := force_tz(time, tzone = "America/Toronto")]
cw_e3[, datetime_utc := with_tz(datetime, tzone = "UTC")]
cw_e3[, c("comments", "datetime") := NULL]
cw_e3_sub <- cw_e3[, .(datetime_utc, flow_hrly_avg)][datetime_utc %between% c(cw_pump_start3, cw_pump_end3)]

cw_e1 <- read_xlsx("./data/cw_wells.xlsx", sheet = "Well E1", skip = 2)
setDT(cw_e1)
colnames(cw_e1) <- c("time", "flow", "drawdown", "waterlevel", "comments")
cw_e1[, flow_hrly_avg := ifelse((as.numeric(time) %% 86400) == 0, flow, flow - lag(flow))]
cw_e1[, datetime := force_tz(time, tzone = "America/Toronto")]
cw_e1[, datetime_utc := with_tz(datetime, tzone = "UTC")]
cw_e1[, c("comments", "datetime") := NULL]
# QA data
cw_e1[flow_hrly_avg < 0.0, flow := 0.0, by = time]
cw_e1[flow_hrly_avg > quantile(flow_hrly_avg, 0.99, na.rm = TRUE), flow_hrly_avg := quantile(flow_hrly_avg, 0.99, na.rm = TRUE)]
cw_e1[flow_hrly_avg < 0.0, flow_hrly_avg := 0.0, by = time]
# do this. above didnt work.
z_score <- scale(cw_e1$flow_hrly_avg) # standardizing the data
outliers <- cw_e1$flow_hrly_avg[abs(z_score) > 2]
upper_limit <- 114.0000
lower_limit <- 0.0000
cw_e1_sub_outliers <- subset(cw_e1, flow_hrly_avg <= upper_limit & flow_hrly_avg >= lower_limit)
cw_e1_sub <- cw_e1_sub_outliers[, .(datetime_utc, flow_hrly_avg)][datetime_utc %between% c(cw_pump_start3, cw_pump_end3)]
# fill in missing datettimes/make regular timestamps
rng <- range(cw_e1$time)
dts <- data.table(time = seq.POSIXt(rng[1], rng[2], by = "hour"))
cw_e1 <- cw_e1[, .SD[dts, on = "time"]]
unique(cw_e1[, diff(time)])
cw_e1[is.na(flow), flow_hrly_avg := 0]
cw_e1_sub <- cw_e1[, .(datetime_utc, flow_hrly_avg)][datetime_utc %between% c(cw_pump_start3, cw_pump_end3)]

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

s1 <- subplot(p_cw_e4, p_cw_e3, p_cw_e1, shareX = TRUE, nrows = 3, heights = c(0.33, 0.33, 0.33))%>%
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


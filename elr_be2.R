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

# subset for data every 5 minutes
wl_baro <- wl_baro[minute(datetime) %% 5 == 0 & second(datetime) == 0]

# remove liner trend from data
# remove from transducer
time_index <- 1:length(wl_baro$value)
linear_model <- lm(wl_baro$value ~ time_index)
trend <- predict(linear_model)
wl_baro[, detrend := value - trend]
plot(wl_baro$value, type = "l", main = "Original Data with Trend")
lines(time_index, trend, col = "red") # Plot the fitted trend
plot(wl_baro$detrend, type = "l", main = "Detrended Data")

# remove from baro
time_index_b <- 1:length(wl_baro$baro)
linear_model_b <- lm(wl_baro$baro ~ time_index_b)
trend_b <- predict(linear_model_b)
wl_baro[, detrend_b := baro - trend_b]
plot(wl_baro$baro, type = "l", main = "Original Data with Trend")
lines(time_index_b, trend_b, col = "red") # Plot the fitted trend
plot(wl_baro$detrend_b, type = "l", main = "Detrended Data")

#%[as.numeric(datetime) %% 300 == 0]
# plot detrended data against detrended baro
p_baro <- plot_ly(wl_baro,
                  x = ~datetime,
                  y = ~detrend_b,
                  name = "baro",
                  type = "scatter", mode = "lines")

p_wl_0 <- plot_ly(wl_baro,
                  x = ~datetime,
                  y = ~detrend,
                  name = "pressure",
                  type = "scatter", mode = "lines")

# plot detrended data against detrended baro
p_baro_wl <- add_trace(p_wl_0, 
                         x = ~datetime, 
                         y = ~detrend_b, 
                         name = "baro",
                         type = "scatter", mode = "lines") %>%
  layout(
    title = "Detrended Pressure Vs. Baro Response", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)")
  )

# now start comparing the loading efficiences
wl_baro[, LE.1 := detrend - 0.1*detrend_b]
wl_baro[, LE.2 := detrend - 0.2*detrend_b]
wl_baro[, LE.3 := detrend - 0.3*detrend_b]
wl_baro[, LE.4 := detrend - 0.4*detrend_b]
wl_baro[, LE.5 := detrend - 0.5*detrend_b]
wl_baro[, LE.6 := detrend - 0.6*detrend_b]
wl_baro[, LE.7 := detrend - 0.7*detrend_b]
wl_baro[, LE.8 := detrend - 0.8*detrend_b]
wl_baro[, LE.9 := detrend - 0.9*detrend_b]
wl_baro[, LE1.0 := detrend - 1.0*detrend_b]

p_le <- add_trace(p_baro_wl, 
                       x = ~datetime, 
                       y = ~LE.1, 
                       name = "LE - 0.1",
                       type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~LE.2, 
    name = "LE - 0.2",
    type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~LE.3, 
    name = "LE - 0.3",
    type = "scatter", mode = "lines") %>% 
  add_trace(
    x = ~datetime, 
    y = ~LE.4, 
    name = "LE - 0.4",
    type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~LE.5, 
    name = "LE - 0.5",
    type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~LE.6, 
    name = "LE - 0.6",
    type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~LE.7, 
    name = "LE - 0.7",
    type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~LE.8, 
    name = "LE - 0.8",
    type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~LE.9, 
    name = "LE - 0.9",
    type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~LE1.0, 
    name = "LE - 1.0",
    type = "scatter", mode = "lines")



# visualize the data
# dependent variable = wl pressure
# independent variable = baro pressure
# range is 0 to 1, with increments of 0.05, 0.01, etc
hydrorecipes::be_visual(wl_baro,
                        dep = "value", ind = "baro", time = "datetime",
                        be_tests = seq(0, 1.0, 0.01), inverse = FALSE)

################################################################################
# attempts - incorrect

# combine dt's while also calculating the mean value of each
wl_baro2 <- baro[,.(datetime, baro = value)][wl[, .(datetime, value = value)], on = "datetime", nomatch = 0]
wl_baro2 <- wl_baro2[between(datetime, as.POSIXct("2024-04-17", tz = "UTC"), as.POSIXct("2024-04-24", tz = "UTC"))]


# this isnt the right way to do this, use detrended data instead. formula good
wl_baro[, t.1 := value - 0.1*baro]
wl_baro[, t.9 := value - 0.9*baro]

# dont do this..very wrong. detrended/averaging twice.
#wl_baro[, LE := detrend - baro]
#wl_baro[, LE2 := detrend - detrend_b]

p_wl_0 <- plot_ly(wl_baro,
                  x = ~datetime,
                  y = ~detrend,
                  name = "wl_0",
                  type = "scatter", mode = "lines")
p_wl_1 <- plot_ly(wl_baro,
                  x = ~datetime,
                  y = ~t.1,
                  name = "wl_0.1",
                  type = "scatter", mode = "lines")
p_wl_9 <- plot_ly(wl_baro,
                  x = ~datetime,
                  y = ~t.9,
                  name = "wl_0.9",
                  type = "scatter", mode = "lines")

s1 <- subplot(p_wl_0, p_wl_1, p_wl_9, shareX = TRUE, nrows = 3)%>%
  layout(
    yaxis = list(title = "pres0"),
    yaxis2 = list(title = "pres0.1"),
    yaxis3 = list(title = "pres0.9")
  )

# more attempts at plots
p_wl <- plot_ly(wl_baro2,
                x = ~datetime,
                y = ~value,
                type = "scatter", mode = "lines")

p_baro <- plot_ly(wl_baro2,
                  x = ~datetime,
                  y = ~baro,
                  line = list(color = "#ee8326"),
                  name = "Baro",
                  type = "scatter", mode = "lines")

p_baro_wl <- add_trace(p_wl, 
                       x = ~datetime, 
                       y = ~baro, 
                       name = "baro",
                       type = "scatter", mode = "lines") %>%
  layout(
    title = "wl Vs. Baro Response", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)"),
    legend = list(trace0 = "wl", trace1 = "Baro")
  )

p_baro_wl_1 <- add_trace(p_wl_0, 
                         x = ~datetime, 
                         y = ~detrend_b, 
                         name = "baro",
                         type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~datetime, 
    y = ~t.1, 
    name = "baro",
    type = "scatter", mode = "lines") %>%
  layout(
    title = "wl Vs. Baro Response", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)"),
    legend = list(trace0 = "wl", trace1 = "Baro")
  )



s0 <- subplot(p_wl, p_baro, shareX = TRUE, nrows = 2)%>%
  layout(
    yaxis = list(title = "pres", 
                 range = c(129.8, 130.2)),
    yaxis2 = list(title = "baro",
                  range = c(9.6,10)),
    legend = list(traceorder = "reversed")
  )

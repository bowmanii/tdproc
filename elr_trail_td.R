# Title: Elora TD Processing
# Site: ELR
# SiteID: ELR1-R1, ELR1-R2
# Author: Isabella Bowman
# Created: July 18 2024
# Last updated: Dec 04, 2024
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
library(dplyr)

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
## what does on= do for dt manipulation? direct sub "on" the same values?
## follow up, by=??
## "paste" fn is so slow, better way to do this?
## plotly, no minor axis exists? have to use loops/ifs?
## line, annotations list for plot...condense it?

#####################################################################
#### For future use after trial run w Jennie ####

#well1 <- "ELR1-R1"
#well2 <- "ELR1-R2"

# well elevation (m amsl)
elev1 <- 377.540 + 0.580
elev2 <- 379.612 + 0.530

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

# t-profile
# for well1: estimated times, no notes taken?
#tprof_start_well1 <- as.POSIXct("2024-06-26 14:41:00", tz = "UTC")
#tprof_end_well1 <- as.POSIXct("2024-06-26 14:44:00", tz = "UTC")
tprof_start_well2 <- as.POSIXct("2024-06-25 17:04:00", tz = "UTC")
tprof_end_well2 <- as.POSIXct("2024-06-25 17:10:00", tz = "UTC")

#####################################################################

# adjust for +34 min/-20 min before/after
tprof_s <- as.POSIXct("2024-06-25 16:30:00", tz = "UTC")
tprof_e <- as.POSIXct("2024-06-25 17:30:00", tz = "UTC")
tprof_wt <- as.POSIXct("2024-06-25 16:37:00", tz = "UTC")

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

# read in centre wellington data (9 sheets), specify sheet, rows to skip
cw_e4 <- read_xlsx("./data/cw_wells.xlsx", sheet = "Well E4", skip = 2)
setDT(cw_e4)
# assign column headers to dt
colnames(cw_e4) <- c("time", "flow", "drawdown", "waterlevel", "comments")

# need to take difference between entries bc they are cumulative. need to reset every 24hours
# come back to this because this is isnt
cw_e4[, flow2 := (Diff = lead(flow) - flow)]
#cw_e4 %>%
#  mutate(Diff = lead(flow) - flow) %>%
#  fill(Diff)
for (i in 1:nrow(cw_e4)) {
  
}

cw_e4[, {
  flow3 := (Diff = lead(flow) - flow)
}]

cw_e4[, {
  if (cw_e4[time %in% c("00:00:00")]) {
    flow4 = flow
  } else {
    flow4 := (Diff = lead(flow) - flow)
  }
}]

cw_e4[, flow4 := ifelse(cw_e4[time %in% c("00:00:00")], flow, (diff = lead(flow) - flow))]

cw_e4[, flow5 := ifelse(grepl("00:00:00", cw_e4$time), flow, (diff = lead(flow) - flow))]

cw_e4[, flow6 := ifelse((as.numeric(time) %% 86400) == 0, flow, lead(flow) - flow)]

cw_e4[, flow7 := ifelse((as.numeric(time) %% 86400) == 0, flow, lag(flow) - flow)]

cw_e4[, flow8 := ifelse((as.numeric(time) %% 86400) == 0, flow, (lag(flow) - flow)*-1)]

cw_e4[, flow9 := ifelse((as.numeric(time) %% 86400) == 0, flow, flow - lag(flow))]

# list all file names from "data" folder, return full file path, only .rsk files
fn <- list.files(file_dir, full.names = TRUE, pattern = "*.rsk")
# cross reference the data files to the data table file_name column
fn <- fn[basename(fn) %in% loc$file_name]

# using Kennels rsk package, read 1 transducer file from our fn variable to get the pressure data
# returns the stored data as a data.table, includes the file name
# simplify names uses "pressure_compensated" values when they exist (new RBR's record this), 
# if not, use the "pressure" value instead. TRUE = do this command
# raw, keep_raw is about what data it retains
pr <- rsk::read_rsk(fn[c(1:18)],
                    return_data_table = TRUE,
                    include_params = c('file_name'),
                    simplify_names = TRUE,
                    keep_raw = FALSE,
                    raw = TRUE)

# subset pr to only include this exact string (match) in the variable column
pr <- pr[variable %in% c("pressure")]

# ignore rows (no manipulation), in cols, beside the file_name col, add the following substitution:
# basename wasn't working, text replacement, replace w empty string, looking in file_name
# using it to clean up file name column!
pr[, file_name := gsub('data/', '', file_name, fixed = TRUE)]

# bring in the loc DT to pr (13 cols), match data on file_name col
pr <- loc[pr, on = "file_name"]

# create baro dt from pr subset using condition when port is equal to baro_rbr
baro <- pr[port == "baro_rbr"]
# create liner dt from pr subset
liner <- pr[port == "liner"]
# create wl dt from pr subset using condition that excludes all ports equal to baros or liners
wl <- pr[!port %in% c("baro_rbr", "liner")]
#wl <- pr[!port %in% c("baro_rbr", "liner", "rbr_diver")]
# using baro dt, use datetime to match columns between both dts to the wl dt, create new column baro that has the baro value, if no match, no value
wl <- baro[, list(datetime, baro = value)][wl, on = "datetime", nomatch = 0]
# add liner pressure to wl dt
wl <- liner[, list(datetime, liner = value)][wl, on = "datetime", nomatch = 0]

# add port name to monitoring location
################# can this get speed up? better way? ############################################################
wl[, portloc := paste(port, monitoring_location, sep = " - ")]
wl[, portloc := paste(portloc, "mbTOC")]

# calculate elevation of transducer monitoring point
wl[, sensor_elev := elev1 - monitoring_location]

# convert all pressures to m H20
wl[, baro_m := baro * dbar_to_m]
wl[, liner_m := liner * dbar_to_m]
wl[, value_m := value * dbar_to_m]

# calculate water height above transducer from pressure, baro pr, port depth (make new col called "head")
wl[, head_masl := sensor_elev + (value_m - baro_m)]

# sorts wl data table by date time (ascending order)
setkey(wl, datetime)

# subset the wl dt by desired times
wl_sub <- wl[datetime %between% c(seal_start_well1, seal_end_well1)]
#wl_sub <- wl[datetime %between% c(tprof_s, tprof_e)]

# make new col in dt, calculation is pressure - the first pressure entry (2024-04-05 18:33:00)
wl_sub[, value_adj := value_m - value_m[1], by = port]

# show subset in a plot
# set 300 entries to 0, means looking at every 5 min data bc we record at 1sec
# p1 <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0]
p1 <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~value_adj, #or head_masl, or value_m, value_adj, etc
              color = ~port,
              colors = viridis(20),
              name = ~portloc,
              type = "scatter", mode = "lines")

# plot baro
p2 <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~baro_m,
              name = "Baro",
              type = "scatter", mode = "lines")

# plot liner
p3 <- plot_ly(wl_sub[as.numeric(datetime) %% 300 == 0],
              x = ~datetime,
              y = ~liner_m,
              name = "Liner",
              type = "scatter", mode = "lines")

# merging baro and liner plots together on one
p4 <- add_trace(p3, x = ~datetime, y = ~baro_m, type = "scatter", mode = "lines", name = "Baro") %>%
  layout(
    title = "Liner Vs. Baro Response", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Pressure (m H20)"),
    legend = list(trace0 = "Liner", trace1 = "Baro")
  )

# plot E4 flow rate
p5 <- plot_ly(cw_e4,
              x = ~time,
              y = ~flow2,
              name = "E4 - Flow",
              type = "scatter", mode = "lines")

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

# display numerous plots in single view, customize plot features (axis titles, etc) using layout
# to make axis values reverse: yaxis=list(autorange="reversed")
# custom axis range: range=list(1.5,4.5)
# minor=list(nticks=50)
# minor = list(nticks = 140, showgrid = TRUE, gridcolor = "lightgrey", tickmode = "linear")
# shapes = list(line(tprof_start_well2))
subplot(p1, p2, shareX = TRUE, nrows = 2)%>%
  layout(
    title = "ELR1-R1: Temporary Deployment", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Δ Pressure (m H20)"), # Δ Pressure (m H20)
    yaxis2 = list(title = "Pressure (m H20)"),
    legend = list(traceorder = "reversed")
  )

# plot baro, liner, wl together
subplot(p1, p2, p3, shareX = TRUE, nrows = 3, heights = c(0.7, 0.15, 0.15))%>%
  layout(
    title = "ELR1-R1: Temporary Deployment", 
    xaxis = list(title = "Date and time",
                 nticks = 20,
                 tickangle = -45),
    yaxis = list(title = "Δ Pressure (m H20)"), # Δ Pressure (m H20)
    yaxis2 = list(title = "Pressure (m H20)"),
    legend = list(traceorder = "reversed")
  )

# create DT for vertical head profiles
#vhp <- wl[datetime %in% as.POSIXct(c("2024-05-12 8:45:00", "2024-05-18 12:25:00"), tz = "UTC")] #old choice
vhp <- wl_sub[datetime %in% as.POSIXct(c("2024-05-12 12:45:00", "2024-05-18 18:35:00"), tz = "UTC")]
#write.csv(vhp, "ELR1-R1_vhp_Hamid.csv")
# shorten table
vhp <- vhp[, list(datetime, well, port, monitoring_location, head_masl)]
write.csv(vhp, "out/ELR1-R1_vhp_v2.csv")

# shorten the number of cols in wl_sub to only these 4
wl_sub <- wl_sub[,list(datetime, value, baro, port)]

# new dt using wl_sub with three cols
wl_wide <- data.table::dcast(wl_sub, datetime+baro~port)
# using wl_wide dt, replace 1,2,etc with "port_01", etc
setnames(wl_wide, c("1","2"), c("port_01", "port_02"), skip_absent = TRUE)

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

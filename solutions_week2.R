
### EXCERCISE 2 ###

# Preparations
# https://github.com/hugwes/excercise_week2 (github-link)

##########################################################
# Load in libraries
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

##########################################################
# Import data
wildschwein <- read_delim("wildschwein_BE_2056.csv",",")
wildschwein <- st_as_sf(wildschwein, coords=c("E","N"), crs=2056, remove=FALSE)

##########################################################
# Task 1: Getting an overview
wildschwein <- group_by(wildschwein, TierID) %>%
  mutate(timelag_sec = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="secs"))) %>%
  mutate(timelag_min=as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="mins")))

# Plot 1
ggplot(wildschwein, aes(DatetimeUTC, TierID)) +
  geom_point()

# Plot 2
ggplot(wildschwein, aes(timelag_sec)) +
  geom_histogram(binwidth = 50) +
  lims(x = c(0,15000)) +
  scale_y_log10()

# Plot 3
ggplot(wildschwein, aes(DatetimeUTC, timelag_min, colour=TierID)) +
  geom_point(size=0.7) +
  geom_line()

# How many individuals were tracked?                              3
# For how long were the individual tracked? Are there gaps?       for one year, starting in september 2014  
# Were all individuals tracked concurrently or sequentially?      mostly cocurrently  
# What is the temporal sampling interval between the locations?

##########################################################
# Task 2: Deriving movement parameters I: Speed

# euclidean distance (steplength) = sqrt((E1-E2)^2+(N1-N2)^2))

# E       = current location E
# lead(E) = consecutive location E
# N       = current location N
# lead(N) = consecutive location N

wildschwein <- wildschwein %>%
  mutate(
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
    speed = steplength/timelag_sec)

##########################################################
# Task 3: Cross-scale movement analysis
caro <- read_delim("caro60.csv",",")
caro <- st_as_sf(caro, coords=c("E","N"), crs=2056, remove=FALSE)

# Reduce the granularity by selecting every 3rd, 6th and 9th position 
caro_3 <- caro %>%
  seq(from = 1, to = 200, by = 3))


# Calculating the timelab, steplength and speed
caro <- caro %>%
  mutate(
    timelag_sec = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="secs")),
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
    speed = steplength/timelag_sec)

caro_3 <- caro_3 %>%
  mutate(
    timelag_sec = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="secs")),
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
    speed = steplength/timelag_sec)

caro_6 <- caro_6 %>%
  mutate(
    timelag_sec = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="secs")),
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
    speed = steplength/timelag_sec)

caro_9 <- caro_9 %>%
  mutate(
    timelag_sec = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="secs")),
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
    speed = steplength/timelag_sec)

##########################################################
# Task 4: Deriving movement parameters II: Rolling window functions



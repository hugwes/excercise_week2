
### EXCERCISE 2 ###

##########################################################
# Preparations
# https://github.com/hugwes/excercise_week2 (github-link)

##########################################################
# Load in Libraries
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # to handle raster data
library(lubridate)    # to handle dates and times
library(zoo)          # moving window functions

# Import data
wildschwein <- read_delim("wildschwein_BE_2056.csv",",")

# Convert into sf-object
wildschwein <- st_as_sf(wildschwein, coords=c("E","N"), crs=2056, remove=FALSE)

##########################################################
# Task 1: Getting an Overview
wildschwein <- group_by(wildschwein, TierID) %>%
  mutate(timelag_sec = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="secs"))) %>%
  mutate(timelag_min=as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="mins"))) %>%
  mutate(timelag_hours=as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="hours")))

# Plot 1
ggplot(wildschwein, aes(DatetimeUTC, TierID)) +
  geom_point()

# Plot 2
ggplot(wildschwein, aes(timelag_min)) +
  geom_histogram(binwidth = 1) +             # Histogramm
  lims(x = c(0,250)) +                       # X-Achsenbereich wählen
  scale_y_log10()                            # Y-Achse logarithieren

# Plot 3
ggplot(wildschwein, aes(DatetimeUTC, timelag_min, colour=TierID)) +
  geom_point(size=0.7) +
  geom_line()

##########################################################
# Task 2: Deriving Movement Parameters I: Speed

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
# Task 3: Cross-scale Movement Analysis

# Import Data 
caro <- read_delim("caro60.csv",",")

# Convert into sf-object
caro <- st_as_sf(caro, coords=c("E","N"), crs=2056, remove=FALSE)

# Reducing Granularity by Selecting every 3rd, 6th and 9th Position 
caro_3 <- caro %>%
  slice(seq(from=1, to=200, by = 3))

caro_6 <- caro %>%
  slice(seq(from=1, to=200, by = 6))

caro_9 <- caro %>%
  slice(seq(from=1, to=200, by = 9))

# Calculating timelab, steplength and speed
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

# Plot 1
ggplot() +
  geom_point(data=caro, aes(E,N), colour="black") + 
  geom_path(data=caro, aes(E,N), colour="black") +
  geom_point(data=caro_3, aes(E,N), colour="blue") +
  geom_path(data=caro_3, aes(E,N), colour="blue")
  
# Plot 2
ggplot() +
  geom_point(data=caro, aes(E,N), colour="black") + 
  geom_path(data=caro, aes(E,N), colour="black") +
  geom_point(data=caro_6, aes(E,N), colour="green") +
  geom_path(data=caro_6, aes(E,N), colour="green")

# Plot 3
ggplot() +
  geom_point(data=caro, aes(E,N), colour="black") + 
  geom_path(data=caro, aes(E,N), colour="black") +
  geom_point(data=caro_9, aes(E,N), colour="purple") +
  geom_path(data=caro_9, aes(E,N), colour="purple")

# Plot 4
ggplot() +
  geom_line(data=caro, aes(DatetimeUTC, speed), colour="black") +
  geom_line(data=caro_3, aes(DatetimeUTC, speed), colour="blue") +
  geom_line(data=caro_6, aes(DatetimeUTC, speed), colour="green") +
  geom_line(data=caro_9, aes(DatetimeUTC, speed), colour="purple")

##########################################################
# Task 4: Deriving Movement Parameters II: Rolling Window Functions

# Rolling Window Function
caro_RWF <- caro %>%
  mutate(speed_5 = rollmean(caro$speed, k=5, fill=NA, align="left")) %>%
  mutate(speed_15 = rollmean(caro$speed, k=15, fill=NA, align="left")) %>%
  mutate(speed_30 = rollmean(caro$speed, k=30, fill=NA, align="left"))

# Plot
ggplot(caro_RWF) +
  geom_line(aes(x=DatetimeUTC, y=speed), colour="black") +
  geom_line(aes(x=DatetimeUTC, y=speed_5), colour="blue") +
  geom_line(aes(x=DatetimeUTC, y=speed_15), colour="green") +
  geom_line(aes(x=DatetimeUTC, y=speed_30), colour="purple") +
  labs(x="Time", y="Speed (m/s)", title="Comparing speed at different sampling intervals") +
  scale_y_continuous(limits=c(0,1)) +
  theme_classic()




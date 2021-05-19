
### EXCERCISE 2 ###

##########################################################
# Preparations
# https://github.com/hugwes/excercise_week2 (github-link)

##########################################################
# Load in Libraries
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data (sf=shape file)
library(terra)        # to handle raster data
library(lubridate)    # to handle dates and times
library(zoo)          # moving window functions

# Import data
wildschwein <- read_delim("wildschwein_BE_2056.csv",",")

# Convert into sf-object (shape file)
wildschwein <- st_as_sf(wildschwein, coords=c("E","N"), crs=2056, remove=FALSE)

##########################################################
# Task 1: Getting an Overview
wildschwein <- group_by(wildschwein, TierID) %>%
  mutate(timelag_sec = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="secs"))) %>%
  mutate(timelag_min=as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="mins"))) %>%
  mutate(timelag_hours=as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="hours")))

# Plot 1
ggplot(wildschwein, aes(DatetimeUTC, TierID)) +
  geom_line()

# Plot 2
ggplot(wildschwein, aes(timelag_min)) +
  geom_histogram(binwidth = 1) +             # Histogramm
  lims(x = c(0,250)) +                       # X-Achsenbereich wählen
  scale_y_log10()                            # Y-Achse logarithieren

# Plot 3
ggplot(wildschwein, aes(DatetimeUTC, timelag_min, colour=TierID)) +
  geom_point(size=0.7) +
  geom_line()

wildschwein %>%
  filter(year(DatetimeUTC)==2014) %>%
  ggplot(aes(DatetimeUTC, timelag_min, colour=TierID)) +
  geom_line() +
  geom_point(size=0.7)

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

# Plot 1 (Wesley)
ggplot() +
  geom_point(data=caro, aes(E,N), alpha=0.2) + 
  geom_path(data=caro, aes(E,N), alpha=0.2) +
  geom_point(data=caro_3, aes(E,N)) +
  geom_path(data=caro_3, aes(E,N)) +
  labs(color="Trajectory", title="Comparing original with 3 minutes-resampled data")

# Plot 1 (Lösung)
ggplot() +
  geom_point(data=caro, aes(E, N, colour="1 minute"), alpha = 0.2) +
  geom_path(data=caro, aes(E, N, colour="1 minute"), alpha = 0.2) +
  geom_point(data=caro_3, aes(E, N, colour="3 minutes")) +
  geom_path(data=caro_3, aes(E, N, colour="3 minutes")) +
  labs(color="Trajectory", title="Comparing original with 3 minutes-resampled data")  +
  theme_minimal()

# Plot 2 (Wesley)
ggplot() +
  geom_point(data=caro, aes(E,N), alpha=0.2) + 
  geom_path(data=caro, aes(E,N), alpha=0.2) +
  geom_point(data=caro_6, aes(E,N)) +
  geom_path(data=caro_6, aes(E,N)) +
  labs(color="Trajectory", title="Comparing original with 6 minutes-resampled data")

# Plot 2 (Lösung)
ggplot() +
  geom_point(data=caro, aes(E, N, colour="1 minute"), alpha = 0.2) +
  geom_path(data=caro, aes(E, N, colour="1 minute"), alpha = 0.2) +
  geom_point(data=caro_3, aes(E, N, colour="6 minutes")) +
  geom_path(data=caro_3, aes(E, N, colour="6 minutes")) +
  labs(color="Trajectory", title="Comparing original with 6 minutes-resampled data")  +
  theme_minimal()

# Plot 3 (Wesley)
ggplot() +
  geom_point(data=caro, aes(E,N), alpha=0.2) + 
  geom_path(data=caro, aes(E,N), alpha=0.2) +
  geom_point(data=caro_9, aes(E,N)) +
  geom_path(data=caro_9, aes(E,N)) +
  labs(color="Trajectory", title="Comparing original with 9 minutes-resampled data")

# Plot 3 (Lösung)
ggplot() +
  geom_point(data=caro, aes(E, N, colour="1 minute"), alpha = 0.2) +
  geom_path(data=caro, aes(E, N, colour="1 minute"), alpha = 0.2) +
  geom_point(data=caro_9, aes(E, N, colour="9 minutes")) +
  geom_path(data=caro_9, aes(E, N, colour="9 minutes")) +
  labs(color="Trajectory", title="Comparing original with 9 minutes-resampled data")  +
  theme_minimal()

# Plot 4 (Wesley)
ggplot() +
  geom_line(data=caro, aes(DatetimeUTC, speed), colour="black") +
  geom_line(data=caro_3, aes(DatetimeUTC, speed), colour="blue") +
  geom_line(data=caro_6, aes(DatetimeUTC, speed), colour="green") +
  geom_line(data=caro_9, aes(DatetimeUTC, speed), colour="purple")

# Plot 4 (Lösung)
ggplot() +
  geom_line(data=caro, aes(DatetimeUTC,speed, colour = "1 minute")) +
  geom_line(data=caro_3, aes(DatetimeUTC,speed, colour = "3 minutes")) +
  geom_line(data=caro_6, aes(DatetimeUTC,speed, colour = "6 minutes")) +
  geom_line(data=caro_9, aes(DatetimeUTC,speed, colour = "9 minutes")) +
  labs(x = "Time",y = "Speed (m/s)", title = "Comparing derived speed at different sampling intervals") +
  theme_minimal()

##########################################################
# Task 4: Deriving Movement Parameters II: Rolling Window Functions (Wesley)

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

# Task 4: Deriving Movement Parameters II: Rolling Window Functions (Lösung)
example <- rnorm(10)
rollmean(example, k=3, fill=NA, align="left")
rollmean(example, k=4, fill=NA, align="left")

caro_x <- caro %>%
  mutate(
    speed3=rollmean(speed, 3, NA, align="left"),
    speed6=rollmean(speed, 6, NA, align="left"),
    speed9=rollmean(speed, 9, NA, align="left"))

caro_x %>%
  ggplot() +
  geom_line(aes(DatetimeUTC, speed), colour="#E41A1C") +
  geom_line(aes(DatetimeUTC, speed3), colour="#377EB8") +
  geom_line(aes(DatetimeUTC, speed6), colour="#4DAF4A") +
  geom_line(aes(DatetimeUTC, speed9), colour="#984EA3")



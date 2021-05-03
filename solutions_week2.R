
### EXCERCISE 2 ###

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
wildschwein <- group_by(wildschwein, TierID)
wildschwein <- mutate(wildschwein, timelag_sec=as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="secs")))
wildschwein <- mutate(wildschwein, timelag_min=as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units="mins")))
head(wildschwein)

# Plot 1
ggplot(wildschwein, aes(DatetimeUTC, TierID)) +
  geom_point()

# Plot 2
wildschwein_count <- wildschwein %>%
  group_by(timelag_sec) %>%
  summarise(count = n())

ggplot(wildschwein_count, aes(timelag_sec, count)) +
  geom_bar()

# Plot 3
ggplot(wildschwein, aes(DatetimeUTC, timelag_sec, colour=TierID)) +
  geom_point(size=0.7)

##########################################################
# Task 2: Deriving movement parameters I: Speed
E1 <- wildschwein$E                                # current location E
E2 <- lead(wildschwein$E)                          # consecutive location E
N1 <- wildschwein$N                                # current location N
N2 <- lead(wildschwein$N)                          # consecutive location N
euclidean_distance <- sqrt((E1-E2)^2+(N1-N2)^2)    # euclidean distance

wildschwein <- wildschwein %>%
  mutate(steplength = sqrt((E1-E2)^2+(N1-N2)^2))

###
wildschwein <- wildschwein %>%
  mutate(
    steplength = sqrt((E-lead(E))^2+(N-lead(N))^2),
    speed = steplength/timelag_sec)

##########################################################
# Task 3: Cross-scale movement analysis
caro <- read_delim("caro60.csv",",")
caro <- st_as_sf(caro, coords=c("E","N"), crs=2056, remove=FALSE)


##########################################################
# Task 4: Deriving movement parameters II: Rolling window functions





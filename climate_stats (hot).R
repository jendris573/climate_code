###Code to determine absolute highs 
###written by Joe Endris

## unless stated, all dates are since 1900

library(readxl)
library(writexl)
library(fitdistrplus)
library(lubridate)
library(MuMIn)
library(tidyverse)
library(pracma)
library(multcomp)
library(ggplot2)
library(ggfortify)
library(gridExtra)

# # # # # # # # # # # #
# Data Preparation ----
# # # # # # # # # # # #

#Load NOAA Climate Data Online data
tenn_clim<-read.csv("data/Tennessee_climate.csv")

#create column for year
tenn_clim <- mutate(tenn_clim, year=year(tenn_clim$DATE))

#create column for month
tenn_clim <- mutate(tenn_clim, month=month(tenn_clim$DATE))

## create column for julian date##
tenn_clim$julian_date <- yday(tenn_clim$DATE)

#omit NA in precipitation recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,6]),]
#omit NA in TMAX/TMIN recordings 
tenn_clim<-tenn_clim[complete.cases(tenn_clim[,9]),]

#filter for 1980-present
tenn1980 <- tenn_clim %>%
  filter(year>1979)

# # # # # # # # # # # # # # # # # # # # # #
# Calculate Absolute High Temperatures ----
# # # # # # # # # # # # # # # # # # # # # #

#Determine absolute hottest day by year
tenn_clim$DATE <- as.Date(tenn_clim$DATE)
class(tenn_clim$DATE)

#record high temperature since 1900
record_TMAX <- tenn_clim %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for May
may_TMAX <- tenn_clim %>%
  filter(month==5) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June
june_TMAX <- tenn_clim %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for July
july_TMAX <- tenn_clim %>%
  filter(month==7) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for August
aug_TMAX <- tenn_clim %>%
  filter(month==8) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for September
sep_TMAX <- tenn_clim %>%
  filter(month==9) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June 2022
june2022_TMAX <- tenn_clim %>%
  filter(year==2022) %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for May 2023
may2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==5) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for June 2023
june2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==6) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for July 2022
july2022_TMAX <- tenn_clim %>%
  filter(year==2022) %>%
  filter(month==7) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for July 2023
july2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==7) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for August 2023
aug2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==8) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for September 2022
sep2022_TMAX <- tenn_clim %>%
  filter(year==2022) %>%
  filter(month==9) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for September 2023
sep2023_TMAX <- tenn_clim %>%
  filter(year==2023) %>%
  filter(month==9) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for 2023
tmax2023 <- tenn_clim %>%
  filter(year==2023) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#record high temp for 2022
tmax2022 <- tenn_clim %>%
  filter(year==2022) %>%
  summarise(temp = max(TMAX, na.rm = TRUE))

#Mean precip since 1980
annual_precip <- tenn1980 %>%
  group_by(year) %>%
  summarise(rain = sum(PRCP, na.rm = TRUE))

mean_precip <- mean(annual_precip$rain)

#mean precip in 2022
precip_2022 <- tenn1980 %>%
  filter(year==2022) %>%
  summarise(rain = sum(PRCP, na.rm = TRUE))

#mean precip in 2023
precip_2023 <- tenn1980 %>%
  filter(year==2023) %>%
  summarise(rain = sum(PRCP, na.rm = TRUE))

june22_mean <- tenn1980 %>%
  filter(year==2022) %>%
  filter(month==6) %>%
  summarise(mean_temp = mean(TMAX, na.rm = TRUE))

june23_mean <- tenn1980 %>%
  filter(year==2023) %>%
  filter(month==6) %>%
  summarise(mean_temp = mean(TMAX, na.rm = TRUE))

july22_mean <- tenn1980 %>%
  filter(year==2022) %>%
  filter(month==7) %>%
  summarise(mean_temp = mean(TMAX, na.rm = TRUE))

july23_mean <- tenn1980 %>%
  filter(year==2023) %>%
  filter(month==7) %>%
  summarise(mean_temp = mean(TMAX, na.rm = TRUE))


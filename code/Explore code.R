# Aleutian GKC sample size and power review
# K. Palof ADF&G 
# 5-23-16

#####Load Packages ---------------------------------
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(extrafont)
library(ggthemes)

#####Load Data --------------------------------------
dat <- read.csv("C:/Users/kjpalof/Documents/R projects/Aleutian-GKC-sample-size/data/AIGKC Erla N 2015 for Katie.csv")

head(dat)
glimpse(dat)

# calculate a soak time so that I don't have to carry the date and times through.

# reduce data set to what's needed. using total crab so size, sex info not neccessary
# need: string, pot.size, subsample.rate, pot, soaktime - not worried about location right now.

dat %>%
  group_by(string, pot, pot.size) %>%
  summarize(total_crab = sum(subsample.rate))

##### soak time calculations ------------------------------
# creates a vector of time in and time out that are together in one column 
dat %>%
  mutate(time_in = paste(datein, timein, sep = " "), time_out = paste(dateout, timeout, sep = " ")) -> dat1
# Changes these from being stored as characters to being stored in the date and time format
#trial <- as.POSIXlt(strptime(dat1$time_in, format = "%m/%d/%Y %H:%M"))
dat1$start_in <- as.POSIXlt(strptime(dat1$time_in, format = "%m/%d/%Y %H:%M"))
dat1$end_out <- as.POSIXlt(strptime(dat1$time_out, format = "%m/%d/%Y %H:%M"))

# difference in hours between the two times
dat1$soak <- difftime(dat1$end_out, dat1$start_in, units = "hours")
# end result is soak vector which is stores in the number of hours 
# want to add soak back to dat
dat$soak <- dat1$soak
#####

##### reduce data ---------------------------------------
dat %>%
  group_by(string, pot, pot.size, soak) %>%
  summarize(total_crab = sum(subsample.rate)) -> GKC

#CPUE by string
GKC %>%
  
 


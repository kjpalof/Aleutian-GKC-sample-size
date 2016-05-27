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
## note: Ben suggested to use lubridate in the future
#####

# remove string 19 due to no catch 
strings_used <- c(1:18)
dat %>%
  filter(string %in% strings_used) -> dat

##### reduce data ---------------------------------------
#total crab
dat %>%
  group_by(string, pot, pot.size, soak) %>%
  summarize(total_crab = sum(subsample.rate)) %>%
  mutate(soaktime = as.numeric (soak))  -> GKC
# Legal 1 and 2
target <- c(1,2)
dat %>%
  filter(legal %in% target) %>%
  group_by(string, pot, pot.size, soak) %>%
  summarize(legal_crab = sum(subsample.rate)) %>%
  mutate(soaktime = as.numeric (soak)) -> GKC_legal


##### Standardize CPUE by soak time in hours ----------------------------------
GKC %>%
  mutate(std_CPUE = total_crab/ soaktime) -> GKC
GKC_legal %>%
  mutate(std_CPUE = legal_crab/ soaktime) -> GKC_legal

##### CPUE calcs -------------------------------------------
# use std_CPUE which standardizes to the hour of soak time
#CPUE by string, standard error calculation assumes 5 pots per string
### Total crab
GKC %>% 
  group_by(string) %>%
  summarize(CPUEbar = mean(std_CPUE), Csd = sd(std_CPUE)) %>%
  mutate(Cse =  Csd/ sqrt(5), CV = (100*(Csd/CPUEbar)), crab_day = CPUEbar*24) -> string_CPUE
#calculate SD, SE, and CV
#CV is the % sd to the average.  

ggplot(string_CPUE, aes(x = string, y = CPUEbar)) + geom_errorbar(aes(ymin=CPUEbar - Cse, ymax = CPUEbar+Cse), 
                                                                  width =0.1) + geom_point()
#CPUE by region
# this is the population mean 
string_CPUE %>%
  summarize(CPUE_region = mean(CPUEbar), sd_region = sd(CPUEbar)) %>%
  mutate(Cse =  sd_region/ sqrt(18), CV = (100*(sd_region/CPUE_region)), crab_day = CPUE_region*24)

mean(string_CPUE$Csd)

### Legal Crab
GKC_legal %>% 
  group_by(string) %>%
  summarize(CPUEbar = mean(std_CPUE), Csd = sd(std_CPUE)) %>%
  mutate(Cse =  Csd/ sqrt(5), CV = (100*(Csd/CPUEbar)), crab_day = CPUEbar*24) -> string_CPUE_legal
#calculate SD, SE, and CV
#CV is the % sd to the average.  

ggplot(string_CPUE_legal, aes(x = string, y = CPUEbar)) + geom_errorbar(aes(ymin=CPUEbar - Cse, ymax = CPUEbar+Cse), 
                                                                  width =0.1) + geom_point()
#CPUE by region
# this is the population mean 
string_CPUE_legal %>%
  summarize(CPUE_region = mean(CPUEbar), sd_region = sd(CPUEbar)) %>%
  mutate(Cse =  sd_region/ sqrt(18), CV = (100*(sd_region/CPUE_region)), crab_day = CPUE_region*24)

mean(string_CPUE_legal$Csd)

##### Summary of legal crab 
string_CPUE_legal

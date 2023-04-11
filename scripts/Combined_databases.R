# Combined Final Database
# author: Rachelle Tallman
# date started: 2022-05-16

# looking at summary statistics of numbers of fish tagged for Klamath project 2022.

setwd("C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project/raw")

library(tidyverse)
library(lubridate)

# read in each computer's data file
Flame = read.csv("FishJSATSTags_20220331_Flame.csv")
JSATS = read.csv("FishJSATSTags_20220331_JSATS.csv")
Rachelle = read.csv("FishJSATSTags_20220331_Rachelle.csv")
Rubber = read.csv("FishJSATSTags_20220331_Rubber.csv")
Arnold = read.csv("FishJSATSTags_20220331_Arnold.csv")
Flame1 = read.csv("FishJSATSTags_20220328_Flame.csv")


# Fix Date/time data on Flame 1

Flame1$date <- as.POSIXct("1899-12-30", format= "%Y-%m-%d") # this is not the correct date but I'm keeping the data standardized with the rest of the other dataframes where Excel automatically put 12-30-1899 as the date. This will later on be filtered out in the Merge_acoustic_PIT.R script.

Flame1$TimeInAnes_PST <- paste(Flame1$date, Flame1$TimeInAnes_PST)
Flame1$TimeOutAnes <- paste(Flame1$date, Flame1$TimeOutAnes)
Flame1$TimeOutSurgery <- paste(Flame1$date, Flame1$TimeOutSurgery)
Flame1$TimeRecovered <- paste(Flame1$date, Flame1$TimeRecovered)

# remove the date column that I made
Flame1 <- Flame1[-17] 

# convert date data

Flame$DateRecorded <- as.Date(as.POSIXct(Flame$DateRecorded, format = "%Y-%m-%d"))
JSATS$DateRecorded <- as.Date(as.POSIXct(JSATS$DateRecorded, format = "%Y-%m-%d"))
Rachelle$DateRecorded <- as.Date(as.POSIXct(Rachelle$DateRecorded, format = "%m/%d/%Y"))
Rubber$DateRecorded <- as.Date(as.POSIXct(Rubber$DateRecorded, format = "%Y-%m-%d"))
Arnold$DateRecorded <- as.Date(as.POSIXct(Arnold$DateRecorded, format = "%m/%d/%Y"))
Flame1$DateRecorded <- as.Date(as.POSIXct(Flame1$DateRecorded, format = "%Y-%m-%d"))



# Arnold recorded the last 6 characters of the PIT number but it's easier if we subset it to 5 since the other records have the last four digits

Arnold$PIT <- str_sub(Arnold$PIT, start= -5)



# Change Flame 1 column Names so they are the same as the other databases

Flame1 <-setNames(Flame1, names(Flame))





# Bind all the databases together

D = rbind(Flame, JSATS, Rachelle, Rubber, Arnold, Flame1)



write.csv(D,"C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project/data_output\\2022_Full_Database.csv", row.names=F)

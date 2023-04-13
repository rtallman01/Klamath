# 2023 Klamath Combined Databases 

library(tidyverse)
library(lubridate)
library(stringi)
# set working directory


setwd("C:/Users/Rachelle/Documents/RProjects/Klamath/data/2023_Klamath_Data")



# Read in database csv files ----------------------------------------------

t1 <- read_csv("FishJSATSTags_Team1_20230406.csv")

t2 <- read_csv("FishJSATSTags_Team2_20230406.csv")

t3 <- read_csv("FishJSATSTags_Team3_20230406.csv")

t4 <- read_csv("FishJSATSTags_Team4_20230405.csv")

lw <- read_csv("2023_live_well_scanner.csv", skip = 6)


# Format Individual Databases ---------------------------------------------


# t2

# format t3 database so only the last 5 digits of the PIT tag number are recorded. This will match the format of the other databases

t2$PIT <-stri_sub(t2$PIT, -5,-1)

# remove sacrificed fish from 61 in t2
t2 = t2[-61,]


# t3

# convert t3 times to characters. Later on I will convert them to POSIXct objects
t3$TimeInAnes <- as.character(t3$TimeOutAnes, format = "%m/%d/%Y %H:%M:%S")
t3$TimeOutAnes <- as.character(t3$TimeOutAnes, format = "%m/%d/%Y %H:%M:%S")
t3$TimeOutSurgery <- as.character(t3$TimeOutSurgery, format = "%m/%d/%Y %H:%M:%S")
t3$TimeRecovered <- as.character(t3$TimeRecovered, format = "%m/%d/%Y %H:%M:%S")


# combine databases together

t <- rbind(t1, t2, t3, t4)



# Format Date and Time ----------------------------------------------------

# only select the time stamps (remove the pre-determined date Excel generated)

t$TimeInAnes <- sub('.*12/30/1899', '', t$TimeInAnes)
t$TimeOutAnes <- sub('.*12/30/1899', '', t$TimeOutAnes)
t$TimeOutSurgery <- sub('.*12/30/1899', '', t$TimeOutSurgery)
t$TimeRecovered <- sub('.*12/30/1899', '', t$TimeRecovered)

# only select the date time stamp (remove the pre-determined time Excel generated)
t$DateRecorded <- stri_sub(t$DateRecorded, 1,8) 

# combine the correct date with the correct time

t$TimeInAnes <- paste(t$DateRecorded, t$TimeInAnes)
t$TimeOutAnes <- paste(t$DateRecorded, t$TimeOutAnes)
t$TimeOutSurgery <- paste(t$DateRecorded, t$TimeOutSurgery)
t$TimeRecovered <- paste(t$DateRecorded, t$TimeRecovered)

# Convert to POSIXct object

t$TimeInAnes<- as.POSIXct(t$TimeInAnes,format='%m/%d/%Y %H:%M:%S', TZ= "ETc/GMT+8")
t$TimeOutAnes<- as.POSIXct(t$TimeOutAnes,format='%m/%d/%Y %H:%M:%S', TZ= "ETc/GMT+8")
t$TimeOutSurgery <- as.POSIXct(t$TimeOutSurgery,format='%m/%d/%Y %H:%M:%S', TZ= "ETc/GMT+8")
t$TimeRecovered <- as.POSIXct(t$TimeRecovered,format='%m/%d/%Y %H:%M:%S', TZ= "ETc/GMT+8")

# Remove DateRecorded column (no longer need since all time stamps have date and time included)

t <- t%>% 
  select(-c(DateRecorded))



# Errors ------------------------------------------------------------------


# look for duplicated JSATS ids

t$JSATS[duplicated(t$JSATS)]

# only JSATS duplicate value is for 35D8

# team 1 record number 228 should be 35DB

t[226, 3] = "35DB"  

# no more duplicate JSATs ids


# look for duplicated PIT tag ids

t$PIT[duplicated(t$PIT)]

# D6820 -- > 2nd record should be D5BA4
t[477, 2] = "D5BA4"  

# AB738 -->  2nd record should be D5D5B
t[702, 2] = "D5D5B"

# DB3B0- typo should be D5EB0
t[479, 2] = "D5EB0"

# no more duplicate PIT tag #s



# Format PIT tags with full identification code ---------------------------

# identify the tag series

lw$StartPIT <- stri_sub(lw$Tag_ID, 1,8) 
unique(lw$StartPIT)

# there are two PIT tag series

#3DD.003D

l1<- lw %>% 
  filter(str_detect(Tag_ID, '3DD.003D')) %>% 
  mutate(End_PIT= str_sub(Tag_ID, -5, -1)) %>% # select last 5 characters
  select(Tag_ID,Release_location,End_PIT)

#3DD.003E


l2<- lw %>% 
  filter(str_detect(Tag_ID, '3DD.003E')) %>% 
  mutate(End_PIT= str_sub(Tag_ID, -5, -1)) %>%  # select last 5 characters
  select(Tag_ID,Release_location,End_PIT)

full_PIT<- rbind(l1,l2)

# Join full_PIT to t using left join


t$End_PIT <- t$PIT # create the same column in t data frame as the column in the full_PIT data frame

joined_dat1 <- t %>% 
  left_join(l1, by = c('End_PIT'))

joined_full_dat <- t %>% 
  left_join(full_PIT, by=('End_PIT'))

# Double check for duplicated PIT tag numbers
#joined_full_dat$FullPIT[duplicated(joined_full_dat$FullPIT)]

# Re-format Database with Appropriate Column Names ------------------------

Dat <- joined_full_dat %>% 
  select(RecNum, Tag_ID, JSATS, TimeInAnes, TimeOutAnes, Mass, ForkLength, TimeOutSurgery, TagEffects, TimeRecovered, Notes, Bleeding, Tagger, Recorder)





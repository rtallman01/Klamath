# 2023 Klamath Combined Databases 

library(tidyverse)
library(lubridate)
# set working directory


setwd("C:/Users/Rachelle/Documents/RProjects/Klamath/data/2023_Klamath_Data")



# Read in database csv files ----------------------------------------------

t1 <- read_csv("FishJSATSTags_Team1_20230406.csv")

t2 <- read_csv("FishJSATSTags_Team2_20230406.csv")

t3 <- read_csv("FishJSATSTags_Team3_20230406.csv")

t4 <- read_csv("FishJSATSTags_Team4_20230405.csv")

lw <- read_csv("2023_live_well_scanner.csv", skip = 6)


# combine databases together

t <- rbind(t1, t2, t3, t4)


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

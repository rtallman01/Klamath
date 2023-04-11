# Filter NOAA Data

install.packages("lubridate")
install.packages("tidyverse")

# open packages
library(tidyverse)
library(lubridate)

# set working directory

setwd("C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project")

# read in csv

dat <- read_csv("UC Davis Tallman Tag Detects_05JUL2022.csv")





# format date data

dat$DATETIME <- as.POSIXct(dat$DATETIME, format= "%d%B%Y:%H:%M:%S", tz= "ETC/GMT+8")



# Filter

CleanFish = dat %>% 
  group_by(TAGID2, detLocation) %>%  # group data by Tag ID and Location
  arrange(TAGID2, DATETIME) %>%  # show me a unique HexID in chronological order
  mutate(LagS = as.numeric(difftime(DATETIME, lag(DATETIME), unit = "sec"))) %>%  # create new column called LagS which is the difference in time in seconds between DateTime column for that observation and the time before
  mutate(Flag = ifelse(LagS > 120, 1, 0)) %>%  # create new column called Flag, which is when LagS is greater than 120 seconds
  filter(LagS > 0.3) %>% # remove any lags > 0.3
  ungroup() %>% 
  arrange(TAGID2, DATETIME) %>%  # arrange data by Tag ID and DateTIme
  mutate(PresNum = cumsum(Flag)) %>% # sum of the cumulative sum of flags
  group_by(PresNum) %>% 
  mutate(NumDets = n()) %>% # new columns is the total amount of unique values
  group_by(TAGID2) %>% 
  filter(NumDets > 4) # need at least 4 detections within 120 seconds to call it a valid detection







#########  End of Code ##################
length(unique(CleanFish$TAGID2)) # does this mean I have 2 valid fish detections below Iron Gate Dam?

unique(CleanFish$detLocation)
unique(CleanFish$detLocation=="KLA82_Mouth_2DSGate_US")


Locations <- CleanFish %>% 
  filter(DATETIME==2022-04-13)


ODFW_filter <- CleanFish %>% 
  select(DATETIME,TAGID2, detLocation) %>% 
  group_by(TAGID2,DATETIME) %>% 
  arrange(TAGID2,DATETIME)

summary_stats <- ODFW_filter %>% 
  mutate(Count)

Tag_8AC6 <- CleanFish %>% 
  filter(TAGID2=="8AC6")

write.csv(Tag_8AC6, "C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project/data_output\\Tag_8AC6.csv", row.names= F)

unique(Tag_8AC6$detLocation)

Tag_67E4 <- CleanFish %>% 
  filter(TAGID2=="67E4")

# write to csv

write.csv(CleanFish,"C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project/data_output\\NOAA_filtered.csv", row.names = F)

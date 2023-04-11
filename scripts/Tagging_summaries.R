##### Klamath Tagging Check
# author: Derrick Alcott
# date started: 2022-03-29

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
Flame1 = read.csv("FishJSATSTags_20220331_Flame.csv")

#Arnold = Arnold %>%
 # select(-DrugTime, -SurgTime, -CF) %>%
  # mutate(FullPIT = NA)    

# code was hashtagged because Derrick already correctly formatted the data

D = rbind(Flame, JSATS, Rachelle, Rubber, Arnold, Flame1)








# Pull the full JSASTs tag list



df[!(is.na(df$start_pc) | df$start_pc==""), ]
Final_JSAT<- D[!(is.na(D$JSATS) | D$JSATS==""), ]

tag_list <- Final_JSAT$JSATS

# write csv

write.csv(tag_list,"C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project/data_output\\Final_tag_list.csv", row.names = FALSE)







# Change Date and Time Data



# change the format for Date/Time for Rachelle File

Rachelle$TimeInAnes <- format(as.POSIXct(Rachelle$TimeInAnes,format="%d/%m/%y %H:%M:%S"),format='%H:%M:%S')

as.POSIXct(Rachelle$TimeInAnes, format = "%m/%d/%Y %H:%M:%S")







D$TimeInAnes <- format(as.POSIXct(D$TimeInAnes,format="%Y-%m-%d %H:%M:%S"),format='%H:%M:%S')

D$DateRecorded <- as.POSIXct(D$DateRecorded, format="%Y-%m-%d")

D = D %>%
  filter(!is.na(JSATS)) %>%
  filter(JSATS != "") %>%
  mutate(Format = ifelse(substr(DateRecorded, 5, 5) == "-", 1, 0)) %>%
  mutate(Date = ifelse(Format == 1,
                       as.Date(DateRecorded, format = "%Y-%m-%d %H:%M:%S"),
                       as.Date(DateRecorded, format = "%m/%d/%Y %H:%M:%S"))
  )


FishPerTagger = D %>%
  group_by(Tagger) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

sum(FishPerTagger$Count)

FishByDay = D %>%
  group_by(Tagger, Date) %>%
  summarise(Count = n()) %>%
  arrange(Date, Tagger)

#### Look at surgery time
D = D %>%
  mutate(TimeInAnes = as.POSIXct(substr(TimeInAnes, 12, 19), format = "%H:%M:%S"),
         TimeOutAnes = as.POSIXct(substr(TimeOutAnes, 12, 19), format = "%H:%M:%S"),
         TimeOutSurgery = as.POSIXct(substr(TimeOutSurgery, 12, 19), format = "%H:%M:%S"),
         DrugTimeMins = difftime(TimeOutAnes, TimeInAnes, units = "mins"),
         SurgTimeMins = difftime(TimeOutSurgery, TimeOutAnes, units = "mins"))


Times = D %>%
  group_by(Tagger) %>%
  summarise(TagTimeMins = median(SurgTimeMins, na.rm = T)) %>%
  arrange(TagTimeMins)


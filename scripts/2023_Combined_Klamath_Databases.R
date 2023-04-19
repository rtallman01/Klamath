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

# format t2 database so only the last 5 digits of the PIT tag number are recorded. This will match the format of the other databases

t2$PIT <-stri_sub(t2$PIT, -5,-1)

# remove sacrificed fish from t2
t2 <- t2 %>% 
  filter(!(is.na(PIT) & is.na(JSATS)))


# fix the data recorder for team 1 on the last day
t1 <-t1 %>%
  mutate(Recorder = case_when(DateRecorded == '4/6/2023 0:00:00' ~ 'CB',
                              TRUE ~ "CE"))



# Date-time Data ----------------------------------------------------------

# t1 and t2 are in the same format
t_1_2 <- rbind(t1,t2)
t_1_2$TimeInAnes <- sub('.*12/30/1899 ', '', t_1_2$TimeInAnes)
t_1_2$TimeOutAnes <- sub('.*12/30/1899 ', '', t_1_2$TimeOutAnes)
t_1_2$TimeOutSurgery <- sub('.*12/30/1899 ', '', t_1_2$TimeOutSurgery)
t_1_2$TimeRecovered <- sub('.*12/30/1899 ', '', t_1_2$TimeRecovered)


# Fix Date Recorded
t_1_2$DateRecorded <- stri_sub(t_1_2$DateRecorded, 1,8)
t_1_2$DateRecorded <- mdy(t_1_2$DateRecorded)
t_1_2$DateRecorded <- as.character(t_1_2$DateRecorded, format = "%m/%d/%Y")




# t3 and t4 are in the same format
t_3_4 <-rbind(t3, t4)
t_3_4$TimeInAnes <- sub('.*1899-12-30 ', '', t_3_4$TimeInAnes)
t_3_4$TimeOutAnes <- sub('.*1899-12-30 ', '', t_3_4$TimeOutAnes)
t_3_4$TimeOutSurgery <- sub('.*1899-12-30 ', '', t_3_4$TimeOutSurgery)
t_3_4$TimeRecovered <- sub('.*1899-12-30 ', '', t_3_4$TimeRecovered)

# Fix Date Recorded
t_3_4$DateRecorded <- stri_sub(t_3_4$DateRecorded, 1,8) # select the first 8 characters
t_3_4$DateRecorded <- ymd(t_3_4$DateRecorded)
t_3_4$DateRecorded <- as.character(t_3_4$DateRecorded, format = "%m/%d/%Y")



# combine databases together

t <- rbind(t_1_2, t_3_4)



# Format Date and Time ----------------------------------------------------


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



# Duplicate IDs ------------------------------------------------------------------


# look for duplicated JSATS ids

t$JSATS[duplicated(t$JSATS)]

# only JSATS duplicate value is for 35D8

# team 1 record 35D8 should be 35DB

t <- t %>% 
  mutate(JSATS = case_when(JSATS == "35D8" & PIT =="D63D8" ~ "35DB",
                           TRUE ~ JSATS))

# no more duplicate JSATs ids


# look for duplicated PIT tag ids

t$PIT[duplicated(t$PIT)]

# D6820 and AB378

# D6820 -- > 2nd record should be D5BA4

# AB738 -->  2nd record should be D5D5B

t <- t %>% 
  mutate(PIT = case_when(JSATS == "7A36" & PIT =="D6820" ~ "D5BA4",
                         JSATS == "341A" & PIT == "AB738" ~ "D5D5B",
                           TRUE ~ PIT))



# no more duplicate PIT tag #s


# Incorrectly entered PIT tag numbers




#	D609A should be D690A

# A9E7 should be AA9E7

# D64B should be D64BC

# 9DOF should be B9D0F

# D5EFS should be D5EF5

# D53B0 should be D5EB0 

# AB370 should be AB379

# AB25D should be AB26D


t <- t %>% 
  mutate(PIT = case_when(JSATS == "31A8" & PIT == "D609A" ~ "D690A",
                         JSATS == "815A" & PIT == "A9E7" ~ "AA9E7",
                         JSATS == "80AA" & PIT == "D64B" ~ "D64BC",
                         JSATS == "31DA" & PIT == "9D0F" ~ "B9D0F",
                         JSATS == "7B50" & PIT == "D5EFS" ~ "D5EF5",
                         JSATS == "7934" & PIT == "D53B0" ~ "D5EB0",
                         JSATS == "7A6D" & PIT == "AB370" ~ "AB379",
                         JSATS == "3259" & PIT == "AB25D" ~ "AB26D",
                         TRUE ~ PIT))


# Format PIT tags with full identification code ---------------------------

# identify the tag series

lw$StartPIT <- stri_sub(lw$Tag_ID, 1,9) 
unique(lw$StartPIT)

# there are three PIT tag series

#3DD.003D6

l1<- lw %>% 
  filter(str_detect(Tag_ID, '3DD.003D6')) %>% 
  mutate(End_PIT= str_sub(Tag_ID, -5, -1)) %>% # select last 5 characters of the PIT tag number
  select(Tag_ID, Release_location, StartPIT, End_PIT)

#3DD.003E1

l2<- lw %>% 
  filter(str_detect(Tag_ID, '3DD.003E1')) %>% 
  mutate(End_PIT= str_sub(Tag_ID, -5, -1)) %>%
  select(Tag_ID, Release_location, StartPIT, End_PIT)

#3DD.003D8
l3<- lw %>% 
  filter(str_detect(Tag_ID, '3DD.003D8')) %>% 
  mutate(End_PIT= str_sub(Tag_ID, -5, -1)) %>%  # select last 5 characters
  select(Tag_ID, Release_location, StartPIT, End_PIT)

#3DD.003D8

full_PIT<- rbind(l1,l2,l3)

# Join full_PIT to t using left join


t$End_PIT <- t$PIT # create the same column in t data frame as the column in the full_PIT data frame

# Join the full_pit with our complete database t using left_join by joining End_PIT columns in both dataframes

joined_full_dat <- t %>% 
  left_join(full_PIT, by=('End_PIT'))


# Double check for duplicated PIT tag numbers in the full list

# 1st check
joined_full_dat$Tag_ID[duplicated(joined_full_dat$Tag_ID)]

# Recheck JSATS tagIDs for duplicates
joined_full_dat$JSATS[duplicated(joined_full_dat$JSATS)]

# no duplicates

# Re-format Database with Appropriate Column Names ------------------------

Dat <- joined_full_dat %>% 
  select(RecNum, 
         HexID_PIT_ID=Tag_ID, 
         JSATS, 
         TimeInAnes, 
         TimeOutAnes, 
         Mass, 
         ForkLength, 
         TimeOutSurgery, 
         TagEffects, 
         TimeRecovered, 
         Notes, 
         Bleeding, 
         Tagger, 
         Recorder,
         Release_location)


# Check Tag Effects values. There should only be 0 L or D. L for live tag. D for dead tag

unique(Dat$TagEffects) # only values are 0, D, L

# Check release location only has three values: Williamson, Wood, OSU
unique(Dat$Release_location)

# Williamson was spelled wrong several times
# values:  "williamson"  "wood"        "osu"         "willismson"  "wiilliamson"

Dat2 <-Dat %>%
  mutate(Release_location = case_when(Release_location == 'willismson' ~ 'Williamson',
                                      Release_location == 'wiilliamson' ~ 'Williamson',
                                      Release_location == 'osu' ~ "OSU",
                                      Release_location == 'wood' ~ "Wood",
                                      Release_location == 'williamson' ~ "Williamson"))


# double check release location values
unique(Dat2$Release_location)


# double check data recorders used a consistent format
unique(Dat2$Recorder)

# check for surgeons (there should only be 4 values)
unique(Dat2$Tagger)


# Identify which PIT ID number was scanned at the livewells that I do not have a record for in the databases

lw$Tag_ID[!lw$Tag_ID %in% Dat2$HexID_PIT_ID]

# "3DD.003D6D6616"
# Have no record of it and all JSATS fish have an associated JSATS tag

# reorder record number
Dat2$RecNum<-1:nrow(Dat2)



# Number of Fish Released -------------------------------------------------

table(Dat2['Release_location'])


# Release_location
# OSU   Williamson    Wood 
# 31        350        352




# Check for additional human errors by order columns in ascending and descending values

d <- Dat2 %>% 
  arrange(ForkLength)

e <- Dat2 %>% 
  arrange(Mass)


# errors with Record # 239, JSATS = 342E, mass and fork length a way too small to be correct. Replace with NA value

# errors with Record # 236, JSATS= 34BB. Fork length is too small to be correct. Replace with NA value.

# errors with Record number 237, JSATS = 3488. Mass is too high.

Dat2 <- Dat2 %>% 
  mutate(ForkLength = case_when(JSATS == "342E" ~ NA_real_,
                                JSATS == "34BB" ~ NA_real_, 
                                TRUE ~ ForkLength),
         Mass = case_when(JSATS == "342E" ~ NA_real_,
                          JSATS == "3488" ~ NA_real_,
                          TRUE ~ Mass))







# write a csv file to save for my own records and a txt file for Mark

write_csv(Dat2, "C:/Users/Rachelle/Documents/RProjects/Klamath/data_output\\2023_Full_Database.csv")

# For ODFW
write.table(Dat2, "C:/Users/Rachelle/Documents/RProjects/Klamath/data_output\\2023_Full_Database.txt", sep=",", row.names = F)


# Kaplan Meier Curve Script


# data comes from script: Battery_life_filtered

setwd("E:/Klamath_2022_downloads/2022_downloads/R_projects/data")


install.packages("ggsurvfit")
library(ggsurvfit)
library(survival)
library(tidyverse)
library(ggfortify)


# Identify Tag Effects Code -----------------------------------------------

te_dat <- read_csv("2022_Full_Database.csv")

te_dat <- te_dat %>% 
  filter(TagEffects ==1) %>% 
  select(JSATS)

# I now have my tag effects tag list


# Read in compressed tank_dat

c <- read_csv("CompressedDetections.csv") %>% 
  filter(File == "Rec_200245_formatted.csv") # filter for tag effects receiver file

# Make tag code letters upper case so they match TE tag list

c$HexID <- toupper(c$HexID)

# Filter tank data for tag effects codes
c2 <- c %>%
  filter(HexID %in% te_dat$JSATS) %>% 
  group_by(HexID) %>% 
  slice(n()) # keep the last record for that tag number

unique(c2$HexID)

# only have 108 tag codes when the database had 114 tag effects codes

# Link tag codes with battery type
# I know from previous code there is only two battery types in the tag effects tank


small_bat1<- read_csv("Tag codes for 142  Upper Klamath River 2022.csv") %>% 
  mutate(Batt_type ="Small") %>% 
  select("TagHex Proposed", "Batt_type") %>% 
  rename(HexID = "TagHex Proposed")

small_bat2 <-read_csv("UpperKlamath_TagCodes_118tags_NOAA_Arcata.csv") %>% 
  mutate(Batt_type ="Small") %>% 
  select("TagHex Proposed", "Batt_type") %>% 
  rename(HexID= "TagHex Proposed")

# combine small battery data into one dataframe
small_bat <- rbind(small_bat1, small_bat2)

rm(small_bat1, small_bat2)

largebat <- read_csv("Tag Inventory SO 104679 for 1068 steelhead tags.csv") %>% 
  mutate(Batt_type = "Large") %>% 
  select(HexCode, Batt_type) %>% 
  rename(HexID = HexCode) 

batt <- rbind(small_bat, largebat)

rm(small_bat, largebat)

# Join battery capacity data with tag effects data using a left join through HexID
c3 <- c2 %>% 
  left_join(batt, by= "HexID" ) %>% 
  select(HexID, DecID, StartTime, EndTime,Batt_type)


# Link data with tag_activation data so that we can calculate duration from the time the tag was activated to when it was last detected

act_dat <- read_csv("2022_Klamath_Tag_activation.csv")

act_dat$Date_activation <- paste(act_dat$Date, act_dat$`Activation Time`)
act_dat$`Tag ID/Code` <- str_replace(act_dat$`Tag ID/Code`, "O", "0") # replace O with 0. Someone accidentally typed in O.

act_dat <- act_dat %>% 
  filter(!Date_activation== "NA NA") # none of my tag effects were these tags

act_dat$Date_activation <- as.POSIXct(act_dat$Date_activation, format = "%m/%d/%Y %H:%M:%S", TZ= "ETc/GMT+8")

a <- act_dat %>% 
  select(HexID=`Tag ID/Code`, Date_activation) 

# Join tag effects tank data with tag activation data
c4 <- c3 %>% 
  left_join(a, by= "HexID" ) %>% 
  select(HexID, DecID, StartTime, EndTime, Date_activation, Batt_type) 

# Calculate the difference in time for duration
c4$Duration <- difftime(c4$EndTime, c4$Date_activation, units = "days")

# convert day metric to a number  
c4$Time_days <- as.numeric(c4$Duration)


c5 <- c4 %>% 
  select(HexID, EndTime, Date_activation, Batt_type, Time_days)


# assign event numbers to the tag codes

# 1 means battery died
# 0 means battery is still alive

# I use 09-01-22 as the cut off for a tag to be considered alive at the end of the study it needed to ping after this date.

kp_dat <- c5 %>% 
  mutate(Event = case_when(Batt_type == "Small" ~ 1,
                           Batt_type == "Large" & EndTime <"2022-08-31" ~1,
                           Batt_type == "Large" & EndTime >"2022-08-31" ~ 0))


# Work with kp_dat --------------------------------------------------------

# make Batt_type a factor

kp_dat$Batt_type <- as.factor(kp_dat$Batt_type)

# Surv function creates survival object which is the response variable
Y = Surv(kp_dat$Time_days, kp_dat$Event == 1)

# Stratify by Battery Type variable:
kmfit = survfit(Y ~ kp_dat$Batt_type)


summary(kmfit, times = c(seq(0, 200, by = 5)))




# Plotting Survival Curves ------------------------------------------------

autoplot(kmfit) +
  xlab("Battery Life (Days)") +
  ylab("Percent Detected (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  

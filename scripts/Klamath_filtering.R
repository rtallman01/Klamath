# Klamath Filtering Script

#############    Coarse False Positive Filter    ###############
# Filter to keep only our tag codes


####   Prep   #####
setwd("C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project/NOAA_Update_Data") # set WD to folder where this script is stored
options(digits.secs=5) # include 5 digits of subseconds
options(digits = 12) # prevent longer numbers from getting cut off

# Load necessary packages
install.load <- function(package.name) # function to install a package, if not already installed, otherwise just load.
{
  if (!require(package.name, character.only=TRUE, quietly=TRUE, warn.conflicts=TRUE)) install.packages(package.name, quiet=TRUE)
  library(package.name, character.only=TRUE, quietly=TRUE, warn.conflicts=TRUE)
}
install.load('tidyverse') # for general data manipulation (mostly dplyr & lubridate)
install.load('broman') # this is needed to convert hexidecimal to decimal and vice versa 
install.load('lubridate') # use lubridate (from tidyverse) instead of base R for date times
install.load('readxl') # for reading Excel files. readxl and tidyverse are not core tidyverse packages and thus are not loaded when tidyverse is loaded, but they are installed with tidyverse
install.load('ggplot2')



#####   Read in TagList   #####
TagList = read_csv("Final_tag_list.csv")

TagList$Dec = hex2dec(TagList$x)

# raw_200256 <- read.csv("200256.csv", header=F)
# head(raw_200256)

# Tells us which receiver had which detections
Rec_200207 = read_csv("Rec_200207.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200207 = filter(Rec_200207, TagID %in% TagList$Dec)
head(Rec_200207) # double checking data

Rec_200250 = read_csv("Rec_200250.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200250 = filter(Rec_200250, TagID %in% TagList$Dec
)

Rec_200253 = read_csv("Rec_200253.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200253 = filter(Rec_200253, TagID %in% TagList$Dec)

Rec_200255 = read_csv("Rec_200255.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200255 = filter(Rec_200255, TagID %in% TagList$Dec)





Rec_200207$SiteName = "Rec_200207"
Rec_200250$SiteName = "Rec_200250"
Rec_200253$SiteName = "Rec_200253"
Rec_200255$SiteName = "Rec_200255"

Data = rbind(Rec_200207, Rec_200250, Rec_200253, Rec_200255)

Data = Data %>% 
  mutate(HexID=dec2hex(TagID)) %>%  # created another column called HexID where values are the hex value of our TagID list
  rename(DecID=TagID)  # changed TagID name to DecID because data was stored in a decimal format



Data = Data %>% 
  mutate(DateTimeSec = DateTime*24*60*60, # convert date units from days to seconds since origin
         DateTime = DateTimeSec + SubSec,    # combine subseconds with the rest of DateTime
         DateTime = as_datetime(DateTime, origin = "1899-12-30 00:00:00"), # origin time according to R for Excel and Lotek numeric date times
         DateTime = force_tz(DateTime, tz = "Etc/GMT+8")) # DO NOT try to do this in previous line. Set time zone (GMT+8 = PST)
#%>% 
#mutate(TagType = TagList$TagType[match(x, TagList$x)])



rm(Rec_200207, Rec_200250, Rec_200253, Rec_200255)
head(Data)

Rec_200207 = Data %>% 
  filter(SiteName == "Rec_200207") %>% 
  #mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)
# keep only the 10th record of each fish. If there are 9 or fewer records then it gets thrown out
head(Rec_200207)


Rec_200250 = Data %>% 
  filter(SiteName == "Rec_200250") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)

Rec_200253 = Data %>% 
  filter(SiteName == "Rec_200253") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


Rec_200255 = Data %>% 
  filter(SiteName == "Rec_200255") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


CleanFish = Data %>% 
  # filter(TagType == "Fish") %>% 
  # group_by(TagID) %>% 
  group_by(HexID, SiteName) %>% 
  arrange(HexID, DateTime) %>%  # show me a unique HexID in chronological order
  mutate(LagS = as.numeric(difftime(DateTime, lag(DateTime), unit = "sec"))) %>% 
  mutate(Flag = ifelse(LagS > 120, 1, 0)) %>% 
  filter(LagS > 0.3) %>%
  ungroup() %>% 
  arrange(HexID, DateTime) %>% 
  mutate(PresNum = cumsum(Flag)) %>%
  group_by(PresNum) %>% 
  mutate(NumDets = n()) %>% # new columns is the total amount of unique values
  group_by(HexID) %>% 
  filter(NumDets > 4) 

head(CleanFish)
n_distinct(CleanFish$HexID)

tags <- unique(CleanFish$HexID[CleanFish$HexID %in% CleanFish2$HexID ])
tags_notdet <- unique(CleanFish2$HexID[!CleanFish2$HexID %in% CleanFish$HexID])
tags_notdet
Missing <- pit_dat$Tag_ID[!(pit_dat$Tag_ID %in% f5$Tag_ID)]

a = CleanFish %>%
  filter(SiteName == "Rec_200207") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1) # 1 record per fish tag code


b= CleanFish %>%
  filter(SiteName == "Rec_200250") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

c= CleanFish %>%
  filter(SiteName == "Rec_200253") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

d= CleanFish %>%
  filter(SiteName == "Rec_200255") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

class(C)






# Downstream Dam Receivers ------------------------------------------------




TagList = read_csv("Final_tag_list.csv")

TagList$Dec = hex2dec(TagList$x)

# raw_200256 <- read.csv("200256.csv", header=F)
# head(raw_200256)

# Tells us which receiver had which detections
Rec_200250 = read_csv("Rec_200250.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200250 = filter(Rec_200250, TagID %in% TagList$Dec)
head(Rec_200250) # double checking data

Rec_200253 = read_csv("Rec_200253.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200253 = filter(Rec_200253, TagID %in% TagList$Dec
)

Rec_200207 = read_csv("Rec_200207.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200207 = filter(Rec_200207, TagID %in% TagList$Dec)

Rec_200255 = read_csv("Rec_200255.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200255 = filter(Rec_200255, TagID %in% TagList$Dec)





Rec_200250$SiteName = "Rec_200250"
Rec_200253$SiteName = "Rec_200253"
Rec_200207$SiteName = "Rec_200207"
Rec_200255$SiteName = "Rec_200255"

Data = rbind(Rec_200250, Rec_200253, Rec_200207, Rec_200255)

Data = Data %>% 
  mutate(HexID=dec2hex(TagID)) %>%  # created another column called HexID where values are the hex value of our TagID list
  rename(DecID=TagID)  # changed TagID name to DecID because data was stored in a decimal format



Data = Data %>% 
  mutate(DateTimeSec = DateTime*24*60*60, # convert date units from days to seconds since origin
         DateTime = DateTimeSec + SubSec,    # combine subseconds with the rest of DateTime
         DateTime = as_datetime(DateTime, origin = "1899-12-30 00:00:00"), # origin time according to R for Excel and Lotek numeric date times
         DateTime = force_tz(DateTime, tz = "Etc/GMT+8")) # DO NOT try to do this in previous line. Set time zone (GMT+8 = PST)
#%>% 
#mutate(TagType = TagList$TagType[match(x, TagList$x)])



rm(Rec_200250, Rec_200253, Rec_200207, Rec_200255)
head(Data)

Rec_200250 = Data %>% 
  filter(SiteName == "Rec_200250") %>% 
  #mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)
# keep only the 10th record of each fish. If there are 9 or fewer records then it gets thrown out
head(Rec_200250)


Rec_200253 = Data %>% 
  filter(SiteName == "Rec_200253") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)

Rec_200207 = Data %>% 
  filter(SiteName == "Rec_200207") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


Rec_200255 = Data %>% 
  filter(SiteName == "Rec_200255") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


CleanFish = Data %>% 
  # filter(TagType == "Fish") %>% 
  # group_by(TagID) %>% 
  group_by(HexID, SiteName) %>% 
  arrange(HexID, DateTime) %>%  # show me a unique HexID in chronological order
  mutate(LagS = as.numeric(difftime(DateTime, lag(DateTime), unit = "sec"))) %>% 
  mutate(Flag = ifelse(LagS > 120, 1, 0)) %>% 
  filter(LagS > 0.3) %>%
  ungroup() %>% 
  arrange(HexID, DateTime) %>% 
  mutate(PresNum = cumsum(Flag)) %>%
  group_by(PresNum) %>% 
  mutate(NumDets = n()) %>% # new columns is the total amount of unique values
  group_by(HexID) %>% 
  filter(NumDets > 4) 

head(CleanFish)


a = CleanFish %>%
  filter(SiteName == "Rec_200205") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1) # 1 record per fish tag code


b= CleanFish %>%
  filter(SiteName == "Rec_200216") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

c= CleanFish %>%
  filter(SiteName == "Rec_200249") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

d= CleanFish %>%
  filter(SiteName == "Rec_200256") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

class(C)


# fish counts
a %>% filter(!HexID_ID %in% b$Hex_ID)

dif <-b %>% filter(!HexID %in% d$HexID)










# Downstream Williamson ---------------------------------------------------




TagList = read_csv("Final_tag_list.csv")

TagList$Dec = hex2dec(TagList$x)

# raw_200256 <- read.csv("200256.csv", header=F)
# head(raw_200256)

# Tells us which receiver had which detections
Rec_200204 = read_csv("Download_200204.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200204 = filter(Rec_200204, TagID %in% TagList$Dec)
head(Rec_200204) # double checking data

Rec_200258 = read_csv("Download_200258.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200258 = filter(Rec_200258, TagID %in% TagList$Dec
)

Rec_200248 = read_csv("Download_200248.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200248 = filter(Rec_200248, TagID %in% TagList$Dec)

Rec_200203 = read_csv("Download_200203.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200203 = filter(Rec_200203, TagID %in% TagList$Dec)





Rec_200203$SiteName = "Rec_200203"
Rec_200204$SiteName = "Rec_200204"
Rec_200248$SiteName = "Rec_200248"
Rec_200258$SiteName = "Rec_200258"

Data = rbind(Rec_200203, Rec_200204, Rec_200248, Rec_200258)

Data = Data %>% 
  mutate(HexID=dec2hex(TagID)) %>%  # created another column called HexID where values are the hex value of our TagID list
  rename(DecID=TagID)  # changed TagID name to DecID because data was stored in a decimal format



Data = Data %>% 
  mutate(DateTimeSec = DateTime*24*60*60, # convert date units from days to seconds since origin
         DateTime = DateTimeSec + SubSec,    # combine subseconds with the rest of DateTime
         DateTime = as_datetime(DateTime, origin = "1899-12-30 00:00:00"), # origin time according to R for Excel and Lotek numeric date times
         DateTime = force_tz(DateTime, tz = "Etc/GMT+8")) # DO NOT try to do this in previous line. Set time zone (GMT+8 = PST)
#%>% 
#mutate(TagType = TagList$TagType[match(x, TagList$x)])



rm(Rec_200193, Rec_200208, Rec_200246, Rec_200251)
head(Data)

Rec_200193 = Data %>% 
  filter(SiteName == "Rec_200203") %>% 
  #mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)
# keep only the 10th record of each fish. If there are 9 or fewer records then it gets thrown out
head(Rec_200193)


Rec_200208 = Data %>% 
  filter(SiteName == "Rec_200204") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)

Rec_200246 = Data %>% 
  filter(SiteName == "Rec_200248") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


Rec_200251 = Data %>% 
  filter(SiteName == "Rec_200258") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


CleanFish = Data %>% 
  # filter(TagType == "Fish") %>% 
  # group_by(TagID) %>% 
  group_by(HexID, SiteName) %>% 
  arrange(HexID, DateTime) %>%  # show me a unique HexID in chronological order
  mutate(LagS = as.numeric(difftime(DateTime, lag(DateTime), unit = "sec"))) %>% 
  mutate(Flag = ifelse(LagS > 120, 1, 0)) %>% 
  filter(LagS > 0.3) %>%
  ungroup() %>% 
  arrange(HexID, DateTime) %>% 
  mutate(PresNum = cumsum(Flag)) %>%
  group_by(PresNum) %>% 
  mutate(NumDets = n()) %>% # new columns is the total amount of unique values
  group_by(HexID) %>% 
  filter(NumDets > 4) 

n_distinct(CleanFish$HexID)

head(CleanFish)


a = CleanFish %>%
  filter(SiteName == "Rec_200193") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1) # 1 record per fish tag code


b= CleanFish %>%
  filter(SiteName == "Rec_200208") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

c= CleanFish %>%
  filter(SiteName == "Rec_200246") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

d= CleanFish %>%
  filter(SiteName == "Rec_200251") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

class(C)


# fish counts
a %>% filter(!HexID_ID %in% b$Hex_ID)

dif <-a %>% filter(!HexID %in% c$HexID)



# Downstream Wood ---------------------------------------------------------




TagList = read_csv("Final_tag_list.csv")

TagList$Dec = hex2dec(TagList$x)

# raw_200256 <- read.csv("200256.csv", header=F)
# head(raw_200256)

# Tells us which receiver had which detections
Rec_200204 = read_csv("Download_200204.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200204 = filter(Rec_200204, TagID %in% TagList$Dec)
head(Rec_200204) # double checking data

Rec_200258 = read_csv("Download_200258.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200258 = filter(Rec_200258, TagID %in% TagList$Dec
)

Rec_200248 = read_csv("Download_200248.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200248 = filter(Rec_200193, TagID %in% TagList$Dec)

Rec_200203 = read_csv("Download_200203.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200203 = filter(Rec_200203, TagID %in% TagList$Dec)





Rec_200203$SiteName = "Rec_200203"
Rec_200204$SiteName = "Rec_200204"
Rec_200248$SiteName = "Rec_200248"
Rec_200258$SiteName = "Rec_200258"

Data = rbind(Rec_200203, Rec_200204, Rec_200248, Rec_200258)

Data = Data %>% 
  mutate(HexID=dec2hex(TagID)) %>%  # created another column called HexID where values are the hex value of our TagID list
  rename(DecID=TagID)  # changed TagID name to DecID because data was stored in a decimal format



Data = Data %>% 
  mutate(DateTimeSec = DateTime*24*60*60, # convert date units from days to seconds since origin
         DateTime = DateTimeSec + SubSec,    # combine subseconds with the rest of DateTime
         DateTime = as_datetime(DateTime, origin = "1899-12-30 00:00:00"), # origin time according to R for Excel and Lotek numeric date times
         DateTime = force_tz(DateTime, tz = "Etc/GMT+8")) # DO NOT try to do this in previous line. Set time zone (GMT+8 = PST)
#%>% 
#mutate(TagType = TagList$TagType[match(x, TagList$x)])



rm(Rec_200203, Rec_200204, Rec_200248, Rec_200258)
head(Data)

Rec_200203 = Data %>% 
  filter(SiteName == "Rec_200203") %>% 
  #mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)
# keep only the 10th record of each fish. If there are 9 or fewer records then it gets thrown out
head(Rec_200203)


Rec_200204 = Data %>% 
  filter(SiteName == "Rec_200204") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)

Rec_200248 = Data %>% 
  filter(SiteName == "Rec_200248") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


Rec_200258 = Data %>% 
  filter(SiteName == "Rec_200258") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


CleanFish = Data %>% 
  # filter(TagType == "Fish") %>% 
  # group_by(TagID) %>% 
  group_by(HexID, SiteName) %>% 
  arrange(HexID, DateTime) %>%  # show me a unique HexID in chronological order
  mutate(LagS = as.numeric(difftime(DateTime, lag(DateTime), unit = "sec"))) %>% 
  mutate(Flag = ifelse(LagS > 120, 1, 0)) %>% 
  filter(LagS > 0.3) %>%
  ungroup() %>% 
  arrange(HexID, DateTime) %>% 
  mutate(PresNum = cumsum(Flag)) %>%
  group_by(PresNum) %>% 
  mutate(NumDets = n()) %>% # new columns is the total amount of unique values
  group_by(HexID) %>% 
  filter(NumDets > 4) 

head(CleanFish)


a = CleanFish %>%
  filter(SiteName == "Rec_200193") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1) # 1 record per fish tag code


b= CleanFish %>%
  filter(SiteName == "Rec_200208") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

c= CleanFish %>%
  filter(SiteName == "Rec_200246") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

d= CleanFish %>%
  filter(SiteName == "Rec_200251") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

class(C)






# Mouth of the Wood River -------------------------------------------------



TagList = read_csv("Final_tag_list.csv")

TagList$Dec = hex2dec(TagList$x)

# raw_200256 <- read.csv("200256.csv", header=F)
# head(raw_200256)

# Tells us which receiver had which detections
Rec_200204 = read_csv("Download_200204.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200204 = filter(Rec_200204, TagID %in% TagList$Dec)
head(Rec_200204) # double checking data

Rec_200258 = read_csv("Download_200258.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200258 = filter(Rec_200258, TagID %in% TagList$Dec
)

Rec_200248 = read_csv("Download_200248.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200248 = filter(Rec_200193, TagID %in% TagList$Dec)

Rec_200203 = read_csv("Download_200203.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
Rec_200203 = filter(Rec_200203, TagID %in% TagList$Dec)





Rec_200203$SiteName = "Rec_200203"
Rec_200204$SiteName = "Rec_200204"
Rec_200248$SiteName = "Rec_200248"
Rec_200258$SiteName = "Rec_200258"

Data = rbind(Rec_200203, Rec_200204, Rec_200248, Rec_200258)

Data = Data %>% 
  mutate(HexID=dec2hex(TagID)) %>%  # created another column called HexID where values are the hex value of our TagID list
  rename(DecID=TagID)  # changed TagID name to DecID because data was stored in a decimal format



Data = Data %>% 
  mutate(DateTimeSec = DateTime*24*60*60, # convert date units from days to seconds since origin
         DateTime = DateTimeSec + SubSec,    # combine subseconds with the rest of DateTime
         DateTime = as_datetime(DateTime, origin = "1899-12-30 00:00:00"), # origin time according to R for Excel and Lotek numeric date times
         DateTime = force_tz(DateTime, tz = "Etc/GMT+8")) # DO NOT try to do this in previous line. Set time zone (GMT+8 = PST)
#%>% 
#mutate(TagType = TagList$TagType[match(x, TagList$x)])



rm(Rec_200203, Rec_200204, Rec_200248, Rec_200258)
head(Data)

Rec_200203 = Data %>% 
  filter(SiteName == "Rec_200203") %>% 
  #mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)
# keep only the 10th record of each fish. If there are 9 or fewer records then it gets thrown out
head(Rec_200203)


Rec_200204 = Data %>% 
  filter(SiteName == "Rec_200204") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)

Rec_200248 = Data %>% 
  filter(SiteName == "Rec_200248") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


Rec_200258 = Data %>% 
  filter(SiteName == "Rec_200258") %>% 
  # mutate(TagType = TagList$TagType[match(ShortDecID, TagList$ShortDecID)]) %>% 
  #filter(TagType == "Fish") %>% 
  group_by(HexID) %>% 
  slice(10)


CleanFish = Data %>% 
  # filter(TagType == "Fish") %>% 
  # group_by(TagID) %>% 
  group_by(HexID, SiteName) %>% 
  arrange(HexID, DateTime) %>%  # show me a unique HexID in chronological order
  mutate(LagS = as.numeric(difftime(DateTime, lag(DateTime), unit = "sec"))) %>% 
  mutate(Flag = ifelse(LagS > 120, 1, 0)) %>% 
  filter(LagS > 0.3) %>%
  ungroup() %>% 
  arrange(HexID, DateTime) %>% 
  mutate(PresNum = cumsum(Flag)) %>%
  group_by(PresNum) %>% 
  mutate(NumDets = n()) %>% # new columns is the total amount of unique values
  group_by(HexID) %>% 
  filter(NumDets > 4) 

head(CleanFish)


a = CleanFish %>%
  filter(SiteName == "Rec_200193") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1) # 1 record per fish tag code


b= CleanFish %>%
  filter(SiteName == "Rec_200208") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

c= CleanFish %>%
  filter(SiteName == "Rec_200246") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

d= CleanFish %>%
  filter(SiteName == "Rec_200251") %>% # only keep records from this site name
  group_by(HexID)%>%
  slice(1)

class(C)


# fish counts
a %>% filter(!HexID_ID %in% b$Hex_ID)

dif <-a %>% filter(!HexID %in% c$HexID)



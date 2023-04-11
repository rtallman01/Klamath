# Klamath Filtering Script

#############    Coarse False Positive Filter    ###############
# Filter to keep only our tag codes


####   Prep   #####
setwd("E:/Klamath_2022_downloads/2022_downloads/2022-09-18-Downloads/200195") # set WD to folder where this script is stored
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



##### Read in csv #####
# Tells us which receiver had which detections
Rec_200195 = read_csv("Rec_200195.csv", col_names = c("DateTime", "SubSec", "TagID", "SigStr"))
head(Rec_200195) # double checking data


# Remove first row
Rec_200195 = Rec_200193[-1,]




Rec_200195$SiteName = "Rec_200195"


Data = Rec_200195

Data = Data %>% 
 # mutate(HexID=dec2hex(TagID)) %>%  # created another column called HexID where values are the hex value of our TagID list
  rename(DecID=TagID)  # changed TagID name to DecID because data was stored in a decimal format

Data$DateTime <- as.numeric(Data$DateTime)
Data$SubSec <- as.numeric(Data$SubSec)

Data = Data %>% 
  mutate(DateTimeSec = DateTime*24*60*60, # convert date units from days to seconds since origin
         DateTime = DateTimeSec + SubSec,    # combine subseconds with the rest of DateTime
         DateTime = as_datetime(DateTime, origin = "1899-12-30 00:00:00"), # origin time according to R for Excel and Lotek numeric date times
         DateTime = force_tz(DateTime, tz = "Etc/GMT+8")) # DO NOT try to do this in previous line. Set time zone (GMT+8 = PST)
#%>% 
#mutate(TagType = TagList$TagType[match(x, TagList$x)])



Data$DecID <- str_pad(Data$DecID, width = 5, side = "left", pad ="0")
Data$HexID <- str_pad(Data$SigStr, width = 4, side = "left", pad ="0")


Formatted_dat <- Data %>% 
  select(DateTime,
         SubSec,
         DecID,
         HexID,
         "SigStr" = X5,
         "Receiver_ID" = SiteName)

Formatted_dat$SiteName <- "Wood_River_10rkm_LooselyRd" 

Formatted_dat$FileName <- "Rec_200195"

write.csv(Formatted_dat,"E:/Klamath_2022_downloads/2022_downloads/Final_Downloads/Formatted_csv_files\\Rec_200195_formatted.csv", row.names = F)

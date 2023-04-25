# Percent cumulative detections by site

# this code is used to generate cumulative percentages at mouth of the Wood, mouth of the Williamson, US of Link River Dam, and DS of Link River Dam

library(tidyverse)
library(lubridate)

# Read in compressed detections data file

setwd("E:/Klamath_2022_downloads/2022_downloads/R_projects/data")

det_dat <- read_csv("CompressedDetections.csv")


# Read in tagging metadata
tag_dat <- read_csv("2020_Klamath_Tagging_Merged_Full_Database.csv") %>% 
  filter(is.na(TagEffects)|TagEffects == 0) %>% 
  select(HexID=JSATS, Release_location)




# Filter condensed data for specific receiver sites -----------------------



# filter for mouth of Wood River Detections

# receivers at Wood River mouth

# Rec 200203
# Rec 200248
# Rec 200258
# Rec 200204

wd_mouth <- det_dat %>% 
  filter(File =="Rec_200203_formatted.csv"|
           File == "Rec_200248_formatted.csv"|
           File == "Rec_200258_formatted.csv"|
           File == "Rec_200204_formatted.csv") %>% 
  mutate(Site_name = "Wood_mouth") %>% 
  select(HexID, StartTime, EndTime, Site_name, File)


# receivers at Williamson River mouth

# Rec 200246
# Rec 200251
# Rec 200193
# Rec 200208 - > no allowable tag codes were detected at this receiver based on filtered script. Will not include in detection plots 

wm_mouth <- det_dat %>% 
  filter(File =="Rec_200246_formatted.csv"|
           File == "Rec_200251_formatted.csv"|
           File == "Rec_200193_formatted.csv") %>% 
  mutate(Site_name = "Williamson_mouth") %>% 
  select(HexID, StartTime, EndTime, Site_name, File)



# receivers upstream of Link River Dam

# Rec 200250
# Rec 200253
# Rec 200207 _> not valid detections at this receiver. Tag codes were rejected because they were not allowable tag codes, echos, and had too few hits.
# Rec 200218
# Rec 200255


us_link <- det_dat %>% 
  filter(File =="Rec_200250_formatted.csv"|
           File == "Rec_200253_formatted.csv"|
           File == "Rec_200218_formatted.csv"|
           File == "Rec_200255_formatted.csv"
           ) %>% 
  mutate(Site_name = "US_Link") %>% 
  select(HexID, StartTime, EndTime, Site_name, File)



# receivers downstream of Link River Dam
# Rec 200249
# Rec 200205
# Rec 200217
# Rec 200256
# Rec 200216

ds_link <- det_dat %>%
  filter(File =="Rec_200249_formatted.csv"|
           File == "Rec_200205_formatted.csv"|
           File == "Rec_200217_formatted.csv"|
           File == "Rec_200256_formatted.csv"|
           File == "Rec_200216_formatted.csv"
  ) %>% 
  mutate(Site_name = "DS_Link") %>% 
  select(HexID, StartTime, EndTime, Site_name, File)

# bind data backtogether

d <- rbind(wd_mouth, wm_mouth, us_link, ds_link)

# Reformat HexID codes so that the letters are all upper case

d$HexID <- toupper(d$HexID)

# filter out tag effects codes from detections file
# filter out any "detections that occurred prior to fish release 04/04/22

d2 <- d %>% 
  filter(HexID %in% tag_dat$HexID == 1) %>% 
  filter(StartTime > "2022-04-03")


# left join tagging data with detection data so that the detection data also has release location associated with it

d3 <- d2 %>% 
  left_join(tag_dat, by = "HexID") %>% 
  group_by(HexID, Site_name)



# Organize Data for Cumulative Sums ---------------------------------------


d4 <- d3 %>% 
  group_by(HexID, Site_name) %>% 
  arrange(EndTime) %>% 
  slice(n()) %>%  # take the last detection at that site for that tag code
  add_count(Site_name) %>%  # tally of the tag code at that site. It should be once. 
  ungroup() %>% 
  mutate(Date = as.Date(EndTime)) # remove time stamp of detection and keep the date
  


d4 <- d3 %>% 
  group_by(HexID, Site_name) %>% 
  arrange(StartTime) %>% 
  slice(1) %>%  # take the last detection at that site for that tag code
  mutate(Date = as.Date(StartTime)) %>%  # remove time stamp of detection and keep the date)
  ungroup() %>% 
  group_by(HexID, Site_name, Date) %>% 
  add_count(Site_name) %>%  # tally of the tag code at that site. It should be once. 
  ungroup() %>% 
  select(HexID,Site_name, Release_location, Date , n)
  

# Calculating Cumulative Sums based on Site Name and Date

det_cum_sum <- d4 %>% 
  group_by(Site_name) %>% 
  arrange(Date) %>% 
  mutate(cum_sum = cumsum(n))


# Plot the cumulative sums for the different sites

ggplot(det_cum_sum,
       aes(x= Date, y=cum_sum, group= Site_name, color= Site_name))+
  geom_step()+
  theme_bw() +
  theme(legend.position = c(.85, 0.2),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
   
  




  






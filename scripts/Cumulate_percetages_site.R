# Percent cumulative detections by site

# this code is used to generate cumulative percentages at mouth of the Wood, mouth of the Williamson, US of Link River Dam, and DS of Link River Dam

library(tidyverse)
library(lubridate)
library(RColorBrewer)

# Read in compressed detections data file

setwd("E:/Klamath_2022_downloads/2022_downloads/R_projects/data")

det_dat <- read_csv("CompressedDetections.csv")


# Read in tagging metadata
tag_dat <- read_csv("2020_Klamath_Tagging_Merged_Full_Database.csv") %>% 
  filter(is.na(TagEffects)|TagEffects == 0) %>%  # select tag codes that are not tag effects codes
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
  select(HexID,StartTime, EndTime, Site_name, File)


# bind data back together

d <- rbind(wd_mouth, wm_mouth, us_link, ds_link)

# Reformat HexID codes so that the letters are all upper case

d$HexID <- toupper(d$HexID)

# filter out tag effects codes from detections file
# filter out any "detections that occurred prior to fish release 04/04/22

d2 <- d %>% 
  filter(HexID %in% tag_dat$HexID) %>% 
  filter(StartTime > "2022-04-03")


# left join tagging data with detection data so that the detection data also has release location associated with it

d3 <- d2 %>% 
  left_join(tag_dat, by = "HexID") %>% 
  group_by(HexID, Site_name)


# Determine Total # of detentions per site after filtering for start time and removing tag effects code ids.

wood <- d3 %>% 
  filter(Site_name == "Wood_mouth")
length(unique(wood$HexID)) # count the number of total detections at this site
# 399 tag codes

will <- d3 %>% 
  filter(Site_name == "Williamson_mouth")
length(unique(will$HexID))
# 373 tag codes

link_us <- d3 %>% 
  filter(Site_name =="US_Link")
length(unique(link_us$HexID))
#54 tag codes

link_ds <- d3 %>% 
  filter(Site_name == "DS_Link")
length(unique(link_ds$HexID))

# 48 tag codes


# Use case when statements to update dataframe with correct number of total detections at a given site. 

d4 <-d3 %>% 
  mutate(Det_num = case_when(Site_name == "Wood_mouth"~ 399,
                             Site_name == "Williamson_mouth" ~ 373,
                             Site_name == "US_Link" ~ 54,
                             Site_name == "DS_Link" ~ 48))


# Organize Data for Cumulative Sums ---------------------------------------


#d4 <- d3 %>% 
 # group_by(HexID, Site_name) %>% 
  # arrange(StartTime) %>% 
  # slice(n()) %>%  # take the last detection at that site for that tag code
  # add_count(Site_name) %>%  # tally of the tag code at that site. It should be once. 
  # ungroup() %>% 
  # mutate(Date = as.Date(EndTime)) # remove time stamp of detection and keep the date
  
# Add in the release date

d4$ReleaseDate <- as.Date("2022-04-04")
d4$Days <- difftime(d4$StartTime, d4$ReleaseDate, units = "days")

d4$Days <- as.numeric(d4$Days)

d5 <- d4 %>% 
  group_by(HexID, Site_name) %>% 
  arrange(StartTime) %>% 
  slice(1) %>%  # take the first detection at that site for that tag code
  mutate(Date = as.Date(StartTime)) %>%  # remove time stamp of detection and keep the date)
  ungroup() %>% 
  group_by(HexID, Site_name, Date) %>% 
  add_count(Site_name) %>%  # tally of the tag code at that site. It should be once. 
  ungroup() %>% 
  select(HexID, Det_num, Site_name, Release_location, Date , n, Days)
  

# Calculating Cumulative Sums percentages based on Site Name and Date

det_cum_sum <- d5 %>% 
  group_by(Site_name) %>% 
  arrange(Date) %>% 
  mutate(cum_per = (cumsum(n)/Det_num))

# look up cumulative percent (per)/ (total unique detections)

# Plot the cumulative sums for the different sites

# reorder site names

det_cum_sum$Site_name <- as.factor(det_cum_sum$Site_name)

det_cum_sum$Site_name <- factor(det_cum_sum$Site_name, levels = c("Wood_mouth","Williamson_mouth", "US_Link", "DS_Link")) 

# remove all other dataframes except the det_cum_sum

rm(list=setdiff(ls(), "det_cum_sum"))


# Individual Site Graphs --------------------------------------------------


# create new facet labels before plotting

det_cum_sum2 <- det_cum_sum %>% 
  mutate(Site_name = recode(Site_name,
                             "Wood_mouth"  = "Wood River Mouth",
                             "Williamson_mouth" = "Williamson River Mouth",
                             "US_Link" = "Upstream Link River Dam",
                             "DS_Link" = "Downstream Link River Dam"))




perc_plot <-ggplot(det_cum_sum2,
       aes(x= Days, y=cum_per, group= Site_name, color= Site_name))+
  geom_step(size= 1.8)+
  facet_wrap(~Site_name) + # create four separate plots based on site name
  ylab("Cummulative Number of Tags Detected") +
  theme_bw() +
  labs(color = "Site") +
  scale_color_discrete(labels=c('Wood River', 'Williamson River', 'US Link River', 'DS Link River'))+
  theme(legend.position = "none",
        axis.text=element_text(size= 14),
        axis.title = element_text(size=16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"), # change spacing between facets
        strip.text.x = element_text(size = 15))  # change face label size




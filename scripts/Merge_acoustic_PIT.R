




write.csv(f8,"C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project/data_output\\Merged_Full_Database.csv", row.names = F)

write.csv(f7,"C:/Users/Rachelle/Documents/RProjects/2022_Klamath_Project/data_output\\2020_Klamath_Tagging_Merged_Full_Database.csv", row.names = F)


# Double Checking Duplicates ----------------------------------------------



# Double Check that JSATs and PIT Tag Numbers are not repeated
check <- f7 %>% 
  group_by(JSATS )%>% 
  summarize(n=n()) %>% 
  filter(n>1)

  
check_2 <- f7 %>% 
  group_by(Tag_ID )%>% 
  summarize(n=n()) %>% 
  filter(n>1)  


# Number of Fish per release site
sum(f7$Release_location == "Williamson")  
sum(f7$Release_location == "Wood") 
table(f7$TagEffects)


mean(f7$Mass)
mean(f7$ForkLength)

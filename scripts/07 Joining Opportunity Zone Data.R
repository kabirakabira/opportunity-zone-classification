# Joining Opportunity Zone classification data to all levels of data

## Setup
library(dplyr)

## Read in dependencies
oppzone.data <- read.csv("data/raw/OZ_CensusTracts.csv")
level1.data <- read.csv("data/processed/Level1Data_Clean.csv")
level2.data <- read.csv("data/processed/Level2Data_Clean.csv")
level3.data <- read.csv("data/processed/Level3Data_Clean.csv")

## Clean Opportunity Zone data
oppzone.data <- oppzone.data %>%
  select(Tract_GEOID,
         Tract_Category) %>%
  mutate(Opp.Zone = 1)

## Join to each of the data files
level1.data <- level1.data %>% 
  left_join(oppzone.data,
            by = c("GEOID" = "Tract_GEOID")) %>%
  mutate(Opp.Zone = ifelse(is.na(Opp.Zone), 0, Opp.Zone),
         Tract_Category = ifelse(is.na(Tract_Category), "", Tract_Category))
level2.data <- level2.data %>% 
  left_join(oppzone.data,
            by = c("GEOID" = "Tract_GEOID")) %>%
  mutate(Opp.Zone = ifelse(is.na(Opp.Zone), 0, Opp.Zone),
         Tract_Category = ifelse(is.na(Tract_Category), "", Tract_Category))
level3.data <- level3.data %>% 
  left_join(oppzone.data,
            by = c("GEOID" = "Tract_GEOID")) %>%
  mutate(Opp.Zone = ifelse(is.na(Opp.Zone), 0, Opp.Zone),
         Tract_Category = ifelse(is.na(Tract_Category), "Not an Opportunity Zone", Tract_Category))

## Write out data
write.csv(level1.data,
          "data/processed/Level1Data_Ready.csv",
          row.names = F)
write.csv(level2.data,
          "data/processed/Level2Data_Ready.csv",
          row.names = F)
write.csv(level3.data,
          "data/processed/Level3Data_Ready.csv",
          row.names = F)
# Creating Level 3 Data - Level 2 data + within-State comparison of MHHI

## Setup
library(dplyr)

## Read in dependencies
level1.data <- read.csv("data/processed/Level1Data.csv") %>%
  select(GEOID, B19013_001, MHHI_State) %>%
  `colnames<-`(c("GEOID", "Median.HH.Income", "MHHI_State"))
level2.data <- read.csv("data/processed/Level2Data.csv")

## Join the MHHI_State data to Level 2 data
level3.data <- left_join(
  level2.data,
  level1.data,
  by = "GEOID"
)

## Write out
write.csv(level3.data,
          "data/processed/Level3Data.csv",
          row.names = F)
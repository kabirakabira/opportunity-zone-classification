# Pulling ACS data for all census tracts for all States for 2022
# This script only pull data needed for the second level of data (comparable to SVI).

## Setup
library(dplyr)
library(tidyr)
library(tidycensus)
census_api_key(readLines("data/reference/censuskey.txt"))

## Read Dependencies
### List of all US State abbreviations
state.list <- read.csv('data/reference/US_states.csv')$Abbreviation
### List of codes and variables for required data
varlist <- read.csv('data/reference/VarList_Level2.csv')

## Pulling Census Tract-level estimates
level2.data <- get_acs(
  geography = "tract",
  state = state.list[1],
  variables = varlist$Code,
  survey = "acs5",
  year = 2015
) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable,
              values_from = estimate)

for (i in 2:length(state.list)) {
  level2.data <- rbind(
    level2.data,
    get_acs(
      geography = "tract",
      state = state.list[i],
      variables = varlist$Code,
      survey = "acs5",
      year = 2015
    ) %>%
      select(-moe) %>%
      pivot_wider(names_from = variable,
                  values_from = estimate)
  )
}

## Write out data
write.csv(level2.data,
          "data/processed/Level2Data.csv",
          row.names = F)

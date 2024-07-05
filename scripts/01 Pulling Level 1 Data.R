# Pulling ACS data for all census tracts for all States for 2022
# This script only pull data needed for the first level of data (income and population).

## Setup
library(dplyr)
library(tidyr)
library(tidycensus)
census_api_key(readLines("data/reference/censuskey.txt"))

## Read Dependencies
### List of all US State abbreviations
state.list <- read.csv('data/reference/US_states.csv')$Abbreviation
### List of codes and variables for required data
varlist <- read.csv('data/reference/VarList_Level1.csv')

## Pulling Census Tract-level estimates
level1.data <- get_acs(
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
  level1.data <- rbind(
    level1.data,
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

## Pulling State-level estimates
state.data <- get_acs(geography = "state",
                      state = state.list[1],
                      variables = c("B19013_001E"),
                      survey = "acs5",
                      year = 2015) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  `colnames<-`(c("State.GEOID","State","MHHI_State"))

for (i in 2:length(state.list)) {
  state.data <- rbind(
    state.data,
    get_acs(geography = "state",
            state = state.list[i],
            variables = c("B19013_001E"),
            survey = "acs5",
            year = 2015) %>%
      select(-moe) %>%
      pivot_wider(names_from = variable,
                  values_from = estimate) %>%
      `colnames<-`(c("State.GEOID","State","MHHI_State"))
  )
}

## Joining State-level estimates to Census Tract-level estimates
level1.data <- level1.data %>%
  mutate(State.GEOID = substr(GEOID, 1,2))
level1.data <- level1.data %>%
  left_join(state.data, by = "State.GEOID")

## Write out Data
write.csv(level1.data,
          "data/processed/Level1Data.csv",
          row.names = F)
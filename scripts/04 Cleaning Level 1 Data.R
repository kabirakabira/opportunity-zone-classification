# Cleaning up Level 1 Data

## Setup
library(dplyr)
library(tidyr)

## Read in dependencies
### Level 1 Data
level1.data <- read.csv("data/processed/Level1Data.csv")
### Variable List
varlist <- read.csv("data/reference/VarList_Level1.csv")

## Replacing column names with variable names
column.names <- colnames(level1.data)

for (i in 1:length(column.names)) {
  current.column <- paste0(column.names[i], "E")
  if (current.column %in% varlist$Code) {
    column.names[i] <- varlist$Label[match(current.column, varlist$Code)]
  }
}

colnames(level1.data) <- column.names

## Calculate new columns where required
level1.data <- level1.data %>%
  mutate(
    ## Calculating percent of population with income:povert ratio less than 1.5
    Poverty.Below.150Percent = (IncomePovertyRatio_0.5 + IncomePovertyRatio_0.99 +
                                  IncomePovertyRatio_1.5) / IncomePovertyRatio_All,
    ## Calculating (Census Tract MHHI - State MHHI)
    MHHI.TractMinusState = (Median.HH.Income - MHHI_State)
  )

## Clean up by only keeping necessary columns
level1.data <- level1.data %>%
  select(GEOID, Poverty.Below.150Percent, MHHI.TractMinusState)

## Drop rows with NAs
## Note: 1,517 rows dropped due to incomplete data
level1.data <- level1.data[complete.cases(level1.data),]

## Write out Data
write.csv(level1.data,
          "data/processed/Level1Data_Clean.csv",
          row.names = F)


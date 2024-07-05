# Cleaning up Level 2 Data

## Setup
library(dplyr)
library(tidyr)

## Read in dependencies
### Level 2 Data
level2.data <- read.csv("data/processed/Level2Data.csv")
### Variable List
varlist <- read.csv("data/reference/VarList_Level2.csv")

## Replacing column names with variable names
column.names <- colnames(level2.data)

for (i in 1:length(column.names)) {
  current.column <- paste0(column.names[i], "E")
  if (current.column %in% varlist$Code) {
    column.names[i] <- varlist$Label[match(current.column, varlist$Code)]
  }
}

colnames(level2.data) <- column.names

## Calculating new columns where required
level2.data <- level2.data %>%
  mutate(
    
    ## Calculating percent of population with income:povert ratio less than 1.5
    Poverty.Below.150Percent = (IncomePovertyRatio_0.5 + IncomePovertyRatio_0.99 +
                                  IncomePovertyRatio_1.5) / IncomePovertyRatio_All,
    
    ## Unemployment Rate
    Unemployment.Rate = (Workers_Unemployed / Workers_LaborForce),
    
    ## Housing Cost Burdened
    Percent.CostBurdened = (OwnerOccupied_IncBelow20_CostOver30Percent +
                              OwnerOccupied_IncBelow35_CostOver30Percent +
                              OwnerOccupied_IncBelow50_CostOver30Percent +
                              OwnerOccupied_IncBelow75_CostOver30Percent +
                              RenterOccupied_IncBelow20_CostOver30Percent +
                              RenterOccupied_IncBelow35_CostOver30Percent +
                              RenterOccupied_IncBelow50_CostOver30Percent +
                              RenterOccupied_IncBelow75_CostOver30Percent) /
      OccupiedHousingUnits,
    
    ## Percent of population without high school diploma
    Percent.NoHSD = Education_NoHSDP / Education_Total,
    
    ## Percent of population with No Health Insurance
    Percent.NoInsurance = Insurance_NoCoverage / Insurance_Total,
    
    ## Percent of population over the age of 65
    Percent.AgeAbove65 = Population_Over65 / Population,
    
    ## Percent of population under the age of 18
    Percent.AgeUnder18 = Population_Under18 / Population,
    
    ## Percent of population with Disability
    Percent.Disabled = (Population_Disability_Under19 + 
                          Population_Disability_19to64 + 
                          Population_Disability_Over64) / 
      Population,
    
    ## Percent of households that are single-parent households
    Percent.SPH = (Households_SPH_Female + Households_SPH_Male) / 
      Households,
    
    ## Percent of households that are limited-english speaking households
    Percent.LimitedEnglishHH = (LimitedEnglishHouseholds_Spanish +
                                  LimitedEnglishHouseholds_IndoEuropean +
                                  LimitedEnglishHouseholds_AAPI +
                                  LimitedEnglishHouseholds_Other) / 
      Households,
    
    ## Percent of population belonging to minortiy groups (basically, not "White, Not Hispanic")
    Percent.Minority = 1 - (Minority_WhiteNotHispanic / Population),
    
    ## Percent of housing units that are in units with 10+ units (multi-structure)
    Percent.MSU = (MultiUnit_10to19 + MultiUnit_20to49 + 
                     MultiUnit_50plus) / MultiUnit_Total,
    
    ## Percent of housing units that are mobile homes
    Percent.MobileHome = MobileHomes / OccupiedHousingUnits,
    
    ## Percent of housing units with more than 1.0 occupant per room
    Percent.Overcrowded = (Overcrowding_OwnerOccupied_1to1.5Occupants + 
                             Overcrowding_OwnerOccupied_1.5to2Occupants + 
                             Overcrowding_OwnerOccupied_2plusOccupants +
                             Overcrowding_RenterOccupied_1to1.5Occupants + 
                             Overcrowding_RenterOccupied_1.5to2Occupants + 
                             Overcrowding_RenterOccupied_2plusOccupants) / 
      Overcrowding_Total,
    
    ## Percent of households with no access to vehicles
    Percent.NoVehicle = Vehicles_None / Vehicles_Total,
    
    ## Percent of population living in group quarters
    Percent.GroupQuarters = GroupQuartersPopulation / Population
 
  )

## Clean up by only keeping necessary columns
level2.data <- level2.data %>%
  select(
    GEOID,
    
    Poverty.Below.150Percent,
    Unemployment.Rate,
    Percent.CostBurdened,
    Percent.NoHSD,
    Percent.NoInsurance,
    
    Percent.AgeAbove65,
    Percent.AgeUnder18,
    Percent.Disabled,
    Percent.SPH,
    Percent.LimitedEnglishHH,
    
    Percent.Minority,
    
    Percent.MSU,
    Percent.MobileHome,
    Percent.Overcrowded,
    Percent.NoVehicle,
    Percent.GroupQuarters
  )

## Drop rows with NAs
## Note: 1,082 rows dropped due to incomplete data
level2.data <- level2.data[complete.cases(level2.data),]

## Write out Data
write.csv(level2.data,
          "data/processed/Level2Data_Clean.csv",
          row.names = F)
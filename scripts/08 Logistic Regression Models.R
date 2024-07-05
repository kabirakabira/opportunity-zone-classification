# Logistic Regressions on all levels of Data
## This script doesn't write out any data
## It will be loaded into the RMD to show the confusion matrices.

## Setup
set.seed(42)
library(dplyr)
library(caret)

## Read in dependencies
level1.data <- read.csv("data/processed/Level1Data_Ready.csv")
level2.data <- read.csv("data/processed/Level2Data_Ready.csv")
level3.data <- read.csv("data/processed/Level3Data_Ready.csv")

## 70:30-train:test split
### Level 1 Data
level1.trainsize <- floor(0.70 * nrow(level1.data))
level1.traindummy <- sample(seq_len(nrow(level1.data)), size = level1.trainsize)
level1.training <- level1.data[level1.traindummy,]
level1.testing <- level1.data[-level1.traindummy,]
### Level 2 Data
level2.trainsize <- floor(0.70 * nrow(level2.data))
level2.traindummy <- sample(seq_len(nrow(level2.data)), size = level2.trainsize)
level2.training <- level2.data[level2.traindummy,]
level2.testing <- level2.data[-level2.traindummy,]
### Level 3 Data
level3.trainsize <- floor(0.70 * nrow(level3.data))
level3.traindummy <- sample(seq_len(nrow(level3.data)), size = level3.trainsize)
level3.training <- level3.data[level3.traindummy,]
level3.testing <- level3.data[-level3.traindummy,]

## Logistic Regression Models
### Level 1 Data
level1.logreg.model <- glm(
  Opp.Zone ~
    Poverty.Below.150Percent +
    MHHI.TractMinusState,
  data = level1.training,
  family = "binomial"(link = "logit")
)
level1.logreg.predicted <- predict(
  level1.logreg.model,
  newdata = level1.testing,
  type = "response"
)
level1.logreg.confusionmatrix <- confusionMatrix(
  data = as.factor(as.numeric(level1.logreg.predicted>0.5)),
  reference = as.factor(level1.testing$Opp.Zone)
)

### Level 2 Data
level2.logreg.model <- glm(
  Opp.Zone ~
    Poverty.Below.150Percent +
    Unemployment.Rate +
    Percent.CostBurdened + 
    Percent.NoHSD +
    Percent.NoInsurance +
    Percent.AgeAbove65 + 
    Percent.AgeUnder18 +
    Percent.Disabled + 
    Percent.SPH +
    Percent.LimitedEnglishHH +
    Percent.Minority +
    Percent.MSU +
    Percent.MobileHome +
    Percent.Overcrowded +
    Percent.NoVehicle +
    Percent.GroupQuarters,
  data = level2.training,
  family = "binomial"(link = "logit")
)
level2.logreg.predicted <- predict(
  level2.logreg.model,
  newdata = level2.testing,
  type = "response"
)
level2.logreg.confusionmatrix <- confusionMatrix(
  data = as.factor(as.numeric(level2.logreg.predicted>0.5)),
  reference = as.factor(level2.testing$Opp.Zone)
)

### Level 3 Data
level3.logreg.model <- glm(
  Opp.Zone ~
    Poverty.Below.150Percent +
    Unemployment.Rate +
    Percent.CostBurdened + 
    Percent.NoHSD +
    Percent.NoInsurance +
    Percent.AgeAbove65 + 
    Percent.AgeUnder18 +
    Percent.Disabled + 
    Percent.SPH +
    Percent.LimitedEnglishHH +
    Percent.Minority +
    Percent.MSU +
    Percent.MobileHome +
    Percent.Overcrowded +
    Percent.NoVehicle +
    Percent.GroupQuarters +
    MHHI.TractMinusState,
  data = level3.training,
  family = "binomial"(link = "logit")
)
level3.logreg.predicted <- predict(
  level3.logreg.model,
  newdata = level3.testing,
  type = "response"
)
level3.logreg.confusionmatrix <- confusionMatrix(
  data = as.factor(as.numeric(level3.logreg.predicted>0.5)),
  reference = as.factor(level3.testing$Opp.Zone)
)

## Save models
saveRDS(level1.logreg.model,
        "data/models/Level1LogRegModel.rds")
saveRDS(level2.logreg.model,
        "data/models/Level2LogRegModel.rds")
saveRDS(level3.logreg.model,
        "data/models/Level3LogRegModel.rds")

## Save confusion matrices
write.csv(
  as.matrix(level1.logreg.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level1LogRegCF.csv",
  row.names = F
)
write.csv(
  as.matrix(level2.logreg.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level2LogRegCF.csv",
  row.names = F
)
write.csv(
  as.matrix(level3.logreg.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level3LogRegCF.csv",
  row.names = F
)
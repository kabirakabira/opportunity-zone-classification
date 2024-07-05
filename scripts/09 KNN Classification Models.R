# KNN Classification on all levels of Data
## This script doesn't write out any data
## It will be loaded into the RMD to show the confusion matrices.

## Setup
set.seed(42)
library(dplyr)
library(caret)

## Read in Dependencies
level1.data <- read.csv("data/processed/Level1Data_Ready.csv") %>%
  mutate(Opp.Zone = as.factor(Opp.Zone))
level2.data <- read.csv("data/processed/Level2Data_Ready.csv") %>%
  mutate(Opp.Zone = as.factor(Opp.Zone))
level3.data <- read.csv("data/processed/Level3Data_Ready.csv") %>%
  mutate(Opp.Zone = as.factor(Opp.Zone))

## Scale data and do a 70:30-train:test split
### Level 1 Data
level1.data[,2:3] <- scale(level1.data[,2:3])
level1.trainsize <- floor(0.70 * nrow(level1.data))
level1.traindummy <- sample(seq_len(nrow(level1.data)), size = level1.trainsize)
level1.training <- level1.data[level1.traindummy,]
level1.testing <- level1.data[-level1.traindummy,]
### Level 2 Data
level2.data[,2:17] <- scale(level2.data[,2:17])
level2.trainsize <- floor(0.70 * nrow(level2.data))
level2.traindummy <- sample(seq_len(nrow(level2.data)), size = level2.trainsize)
level2.training <- level2.data[level2.traindummy,]
level2.testing <- level2.data[-level2.traindummy,]
### Level 3 Data
level3.data[,2:18] <- scale(level3.data[,2:18])
level3.trainsize <- floor(0.70 * nrow(level3.data))
level3.traindummy <- sample(seq_len(nrow(level3.data)), size = level3.trainsize)
level3.training <- level3.data[level3.traindummy,]
level3.testing <- level3.data[-level3.traindummy,]

## Choosing K values and performing KNN Classification for each data level
### Specify 10-fold cross-validation with three repetitions
trainCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
### Set up k-grid up to 20
grid <- expand.grid(k=seq(1,20,by=1))

### Level 1 Data
level1.knn.model <- train(
  Opp.Zone ~ 
    Poverty.Below.150Percent + 
    MHHI.TractMinusState,
  data = level1.training, 
  method = "knn", 
  metric = "Accuracy", 
  tuneGrid = grid, 
  trControl = trainCtrl
)
level1.knn.predicted <- predict(
  level1.knn.model,
  newdata = level1.testing
)
level1.knn.confusionmatrix <- confusionMatrix(
  level1.knn.predicted,
  level1.testing$Opp.Zone
)

### Level 2 Data
level2.knn.model <- train(
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
  method = "knn", 
  metric = "Accuracy", 
  tuneGrid = grid, 
  trControl = trainCtrl
)
level2.knn.predicted <- predict(
  level2.knn.model,
  newdata = level2.testing
)
level2.knn.confusionmatrix <- confusionMatrix(
  level2.knn.predicted,
  level2.testing$Opp.Zone
)

### Level 3 Data
level3.knn.model <- train(
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
  method = "knn", 
  metric = "Accuracy", 
  tuneGrid = grid, 
  trControl = trainCtrl
)
level3.knn.predicted <- predict(
  level3.knn.model,
  newdata = level3.testing
)
level3.knn.confusionmatrix <- confusionMatrix(
  level3.knn.predicted,
  level3.testing$Opp.Zone
)

## Save models
saveRDS(level1.knn.model,
        "data/models/Level1KNNModel.rds")
saveRDS(level2.knn.model,
        "data/models/Level2KNNModel.rds")
saveRDS(level3.knn.model,
        "data/models/Level3KNNModel.rds")

## Save confusion matrices
write.csv(
  as.matrix(level1.knn.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level1KNNCF.csv",
  row.names = F
)
write.csv(
  as.matrix(level2.knn.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level2KNNCF.csv",
  row.names = F
)
write.csv(
  as.matrix(level3.knn.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level3KNNCF.csv",
  row.names = F
)
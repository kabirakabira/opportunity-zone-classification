# Random Forest classification on all levels of Data
## This script doesn't write out any data
## It will be loaded into the RMD to show the confusion matrices.

## Setup
set.seed(42)
library(dplyr)
library(caret)
library(randomForest)

## Read in Dependencies
level1.data <- read.csv("data/processed/Level1Data_Ready.csv") %>%
  mutate(Opp.Zone = as.factor(Opp.Zone))
level2.data <- read.csv("data/processed/Level2Data_Ready.csv") %>%
  mutate(Opp.Zone = as.factor(Opp.Zone))
level3.data <- read.csv("data/processed/Level3Data_Ready.csv") %>%
  mutate(Opp.Zone = as.factor(Opp.Zone))

## Scale data and do a 70:30-train:test split
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

## Random Forest classification for each data level
### Note that here, ntree is set to 500 and mtry is set to 3 (default for classification)
### In the readme / report, the optimal ntree and mtry params will be investigated
### Level 1 Data
level1.rf.model <- randomForest(
  y = level1.training$Opp.Zone,
  x = level1.training[,2:3],
  ntree = 500,
  mtry = 2
)
level1.rf.predicted <- predict(
  level1.rf.model,
  level1.testing
)
level1.rf.confusionmatrix <- confusionMatrix(
  level1.rf.predicted,
  level1.testing$Opp.Zone
)

### Level 2 Data
level2.rf.model <- randomForest(
  y = level2.training$Opp.Zone,
  x = level2.training[,2:17],
  ntree = 500,
  mtry = 3
)
level2.rf.predicted <- predict(
  level2.rf.model,
  level2.testing
)
level2.rf.confusionmatrix <- confusionMatrix(
  level2.rf.predicted,
  level2.testing$Opp.Zone
)

### Level 3 Data
level3.rf.model <- randomForest(
  y = level3.training$Opp.Zone,
  x = level3.training[,2:18],
  ntree = 500,
  mtry = 3
)
level3.rf.predicted <- predict(
  level3.rf.model,
  level3.testing
)
level3.rf.confusionmatrix <- confusionMatrix(
  level3.rf.predicted,
  level3.testing$Opp.Zone
)

## Save models
saveRDS(level1.rf.model,
        "data/models/Level1RFModel.rds")
saveRDS(level2.rf.model,
        "data/models/Level2RFModel.rds")
saveRDS(level3.rf.model,
        "data/models/Level3RFModel.rds")

## Save confusion matrices
write.csv(
  as.matrix(level1.rf.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level1RFCF.csv",
  row.names = F
)
write.csv(
  as.matrix(level2.rf.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level2RFCF.csv",
  row.names = F
)
write.csv(
  as.matrix(level3.rf.confusionmatrix, what = "classes"),
  "data/models/confusionMatrices/Level3RFCF.csv",
  row.names = F
)
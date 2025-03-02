library(caret)
library(randomForest)

# Load dataset
data <- read.csv('data.csv')

## Do EDA and report you findings.

# Split dataset
set.seed(42)
trainIndex <- createDataPartition(data$y_variable, p=0.8, list=FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Train and evaluate KNN
knn_model <- train(y_variable ~ ., data=train, method='knn', tuneLength=5) # Fit KNN model
pred_knn <- predict(knn_model, test)
print(confusionMatrix(pred_knn, test$y_variable))

# Train and evaluate Decision Tree
dt_model <- train(y_variable ~ ., data=train, method='rpart') # Fit Decision Tree Model
pred_dt <- predict(dt_model, test)
print(confusionMatrix(pred_dt, test$y_variable))

## Proceed with evaluation and interpretation of both models.
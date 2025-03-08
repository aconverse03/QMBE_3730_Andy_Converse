# load libraries
library(caret)
library(randomForest)
library(tidyverse)
library(caTools)

# load dataset
dataset <- read.csv('/Users/andyconverse/Documents/Hamline Acedmics/Hamline 2024-25/Hamline Spring 25/QMBE 3730/QMBE_3730_Andy_Converse/Untitled/Classification and comparing models/loan_default_data_set.csv'
                    )
view(dataset)

# 1. EDA
dim(dataset)


## 1. In this dataset there are 20000 observation with 21 variables.

# 2. 
sum(is.na(dataset))
colSums(is.na(dataset))
colSums(is.na(dataset)) / sum(is.na(dataset))

dataset_clean <- na.omit(dataset)

## 2. There are a total of 3517 missing values. They only come from 2 variables, pct_card_over_50_uti and rep_income.
##    55% of missing values are from pct_card_over_50_uti and 44.32% are from rep_income.
##    To deal with the missing values I cleaned the data set by removing all observations with a missing value.

## 3. 
sum(duplicated(dataset_clean))
str(dataset)

## 3. There are no duplicates in the cleaned data set. 
##    Of the 21 variables they either a number or integer data type. However, there is one characters variable.

## 4. I'd handle duplicates by deleting all except the first observation. 
##    As for mis-classified variables I would change entire columns to the correct data type to ensure that the column is correct.

## 5. 
ggplot(data = dataset_clean,
       aes(x = tot_balance,
           y = credit_age)) +
  geom_point() +
  labs(title = "Total Available Balance vs Age of First Credit Product",
       x = "Total Available Balance of all Credit Products ($)",
       y = "Age of First Credit Product (in Months)",
       theme_dark()
       )

## 5. The scatter plot create compares total available credit against the age of the observations first credit product in months.

## 6. 
table(dataset_clean$rep_education)/16653

## 6. Graduates are under represented in this dataset as they only make up 12.1% of the data set.

## 7.
table(dataset_clean$Def_ind)/16653

## 7. "def_ind" is an unbalanced variable. To fix this we could create a sample and under sample the majority (not defaulted).
##    We could also over sample the minority (defaulted). By doing either we would even the balance out.

## 8. 
summary(dataset_clean$rep_income)
hist(dataset_clean$rep_income,
     main = "Distrubtion of Annual Income",
     xlab = "Annual Income ($)",
     col = "green",
     border = "black")

## 8. I would describe "rep_income"'s distribution as approximately normal.

## 9. 
group_default.by.edu <- dataset_clean %>%
  group_by(rep_education) %>%
  summarise(total_count = n(),
            default_count = sum(Def_ind == 1),
            default_rate = mean(Def_ind == 1) * 100) %>%
  arrange(desc(default_rate))

group_default.by.edu

## 9. High school education applicants are most likely to default at 11.8%.

## 10. Nothing else stands out to me.

### Separate training and test sets
set.seed(2)
split <- sample.split(dataset_clean$Def_ind,
                      SplitRatio = 0.8)
train_data <- subset(dataset_clean,
                     split == TRUE)
test_data <- subset(dataset_clean,
                    split == FALSE)
dim(train_data)
dim(test_data)

### Fit model
log_model <- glm(Def_ind ~ .,
                 data = dataset_clean,
                 family = binomial()
                 )
summary(log_model)

pred_probs <- predict(log_model,
                      test_data,
                      type = "response")
pred_default <- ifelse(pred_probs > 0.5,
                       1,
                       0)
head(pred_probs)
head(pred_default)

### Confusion matrix
conf_matrix <- table(Predicted = pred_default,
                     Actual = test_data$Def_ind)
conf_matrix

### Metrics
TN <- conf_matrix[1, 1]
FP <- conf_matrix[1, 2]
FN <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]

TN
FP
FN
TP

accuracy <- (TP + TN) / (TP + TN + FP + FN)
accuracy

precision <- TP / (TP + FP)
precision

recall <- TP / (TP + FN)
recall

### ROC curve and AUC value
library(pROC)

roc_curve <- roc(test_data$Def_ind,
                 pred_probs)
plot(roc_curve,
     main = "ROC Curve",
     col = "dodgerblue")

auc_val <- auc(roc_curve)
print(paste("AUC:", 
            auc_val))

### KNN model
train_data$Def_ind <- as.factor(train_data$Def_ind)
test_data$Def_ind <- as.factor(test_data$Def_ind)

knn_model <- train(Def_ind ~.,
                   data = train_data,
                   method = 'knn',
                   tuneLength = 5)
pred_knn <- predict(knn_model,
                    test_data)
print(confusionMatrix(pred_knn, test_data$Def_ind))

### DT model
dt_model <- train(Def_ind ~ .,
                  data= train_data,
                  method = 'rpart')
pred_dt <- predict(dt_model,
                   test_data)
print(confusionMatrix(pred_dt, 
                      test_data$Def_ind))

#### 1. The accuracy, precision, and recall came back as 90%, 15%, and 65%, respectful.
####    Since the accuracy is so high we know the model is 90% accurate.
####    Since the precision is so low that tells us that the model makes false postive errors
####    The model correctly identified 65%, which isn't great.

#### 2. The confusion matrix has 2963 true positives, 286 false positives, 28 false negatives, and 53 true positives.

#### 3. See above in "ROC curve and AUC value" section

#### 4. "uti_open_card" has the highest coefficient in the regression model meaning it has great importance to "Def_ind."

#### 5. The decision tree model performed the best as it had the highest accuracy at 90.81%.






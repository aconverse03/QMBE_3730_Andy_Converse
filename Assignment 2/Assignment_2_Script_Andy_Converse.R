# load libraries
library(tidyverse)
library(caTools)

# load data
admit_data <- read.csv('/Users/andyconverse/Documents/Hamline Acedmics/Hamline 2024-25/Hamline Spring 25/QMBE 3730/QMBE_3730_Andy_Converse/Untitled/Assignment 2/admit.csv'
         )

# view data
view(admit_data)

# EDA

dim(admit_data)

## Shape: There are 400 observations (Rows) and 4 different variables (Columns)

str(admit_data)

## Datatypes: GPA is the only variable that is numerical in this dataset. 
###           The other three are intergers. 

sum(is.na(admit_data))
sum(duplicated(admit_data))

## There are no missing values and there are 5 duplicates in this dataset.

table(admit_data$admit)
table(admit_data$admit)/400

#### 1. 31.75% of applicants were admitted or 127 people. 
#####   68.25% of applicants were not admitted or 273 people.

summary(admit_data$gre)
hist(admit_data$gre,
     main = "Distrubtion of GRE Scores",
     xlab = "GRE Scores",
     col = "dodgerblue",
     border = "black")

#### 2. GRE scores are left skewed on a histogram. 
#####   The median score is 580 and the mean score is 587.7.

# Split data into training and test data
set.seed(1)

split <- sample.split(admit_data$admit,
                      SplitRatio = 0.7)
train_data <- subset(admit_data, 
                    split == TRUE)
test_data <- subset(admit_data,
                    split == FALSE)
dim(train_data)
dim(test_data)

# Fit Logistical regression model
log_model <- glm(admit ~ 
                   gpa + 
                   gre + 
                   rank,
                 data = 
                   admit_data,
                 family = 
                   binomial)
summary(log_model)

pred_probs <- predict(log_model,
                      test_data,
                      type = 
                        "response")
pred_admit <- ifelse(pred_probs > 0.5, 
                     1, 
                     0)

head(pred_probs)
head(pred_admit)

do.call(rbind,
        Map(data.frame,
            predicted_classes=pred_admit,
            admit=test_data$admit))

### A higher GPA will have a more positive influence compared to GRE scores.

#Confusion Matrix
conf_matrix <- table(Predicted = pred_admit, 
                     Actual = test_data$admit)
conf_matrix

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

print(conf_matrix)
print(paste("Accuracy:",
            round(accuracy,
                  4)))
### Becasue there are a high number of true values this model is performing well.
### This model is performing at a 71.67% accuracy.

#Visualize prediction vs. actual
ggplot(test_data,
       aes(x = gpa,
           y = as.numeric(as.character(admit)),
           color = as.factor(pred_admit))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admission",
       x = "GPA",
       y = "Admitted or Not (0 = Not Admitted, 1 = Admitted)") +
  scale_color_manual(values = c("red", 
                                "blue"),
                     name = "Prediction")

### GPA is one of the most important variable becasue of it's coeffiecent in the model.
### That is why i choose to visualize it with a plot.
  
  
  
  
  



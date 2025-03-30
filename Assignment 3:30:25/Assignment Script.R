# Libraries
library(ggplot2)
library(lmtest)
library(sandwich)
library(dplyr)
library(readxl)

# Load data
wages <- read_xlsx('/Users/andyconverse/Documents/Hamline Acedmics/Hamline 2024-25/Hamline Spring 25/QMBE 3730/QMBE_3730_Andy_Converse/Untitled/Assignment 3:30:25/wages.xlsx')
annarbor <- read_xlsx('/Users/andyconverse/Documents/Hamline Acedmics/Hamline 2024-25/Hamline Spring 25/QMBE 3730/QMBE_3730_Andy_Converse/Untitled/Assignment 3:30:25/AnnArbor.xlsx')

# 1. Wages Dataset
## a. Scatterplot
ggplot(data = wages, 
       aes(x = Age, 
           y = Wage)) +
  geom_point() +
  labs(title = "Wages vs Age Scatterplot", 
       x = "Age", 
       y = "Wages")

## b. Linear Model
lm.wages <- lm(Wage ~ Age + Educ, 
               data = wages)
summary(lm.wages)

## c. Quadratic Model
wages$Age_sq <- wages$Age^2
qm.wages <- lm(Wage ~ Age + Age_sq + Educ, 
               data = wages)
summary(qm.wages)

# Compare residuals
par(mfrow = c(1,2))
plot(lm.wages$residuals, main = "Residuals of Linear Model", 
     ylab = "Residuals", 
     xlab = "Index")
plot(qm.wages$residuals, main = "Residuals of Quadratic Model", 
     ylab = "Residuals", 
     xlab = "Index")

# Compare goodness of fit
AIC(lm.wages, 
    qm.wages)
BIC(lm.wages, 
    qm.wages)

## d. Predict wages for Age 30, 50, 70 with 16 years of education
wages.d <- data.frame(Age = c(30, 50, 70), 
                      Age_sq = c(30^2, 50^2, 70^2), 
                      Educ = rep(16, 3))
predicted_wages <- predict(qm.wages, 
                           newdata = wages.d)
data.frame(Age = wages.d$Age, 
           Predicted_Wages = predicted_wages)

## e. Find the age at which wage is maximized
coefficients <- coef(qm.wages)
max_wages.16 <- -coefficients["Age"] / (2 * coefficients["Age_sq"])
max_wages.16  


#2. 
ggplot(data = annarbor,
       aes(x = Beds,
           y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rent vs Number of Bedrooms",
       x = "Number of Bedrooms",
       y = "Rent")
ggplot(data = annarbor,
       aes(x = Baths,
           y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rent vs Number of Bathrooms",
       x = "Number of Bathrooms",
       y = "Rent")
ggplot(data = annarbor,
       aes(x = Sqft,
           y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rent vs Square Footage",
       x = "Square Footage",
       y = "Rent")

annarbor$logRent <- log(annarbor$Rent)
annarbor$logSqft <- log(annarbor$Sqft)

ggplot(data = annarbor,
       aes(x = logSqft,
           y = logRent)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Rent (Log) vs Square Footage (Log)",
       x = "Square Footage (Log)",
       y = "Rent (Log)")

lm.annarbor <- lm(logRent ~ logSqft + Beds + Baths,
                  data = annarbor)
summary(lm.annarbor)

predicted_annarbor <- predict(lm.annarbor,
                              newdata = data.frame(logSqft = log(1600),
                                                   Beds = 3,
                                                   Baths = 2)
                              )
predicted_rent <- exp(predicted_annarbor)
predicted_rent












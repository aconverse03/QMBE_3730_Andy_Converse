# libraries
library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(dplyr)

# load data
data(mtcars)

plot(density(mtcars$mpg))
plot(density(log(mtcars$mpg)))
View(mtcars)

# convert categorical variables
mtcars$am <- as.factor(mtcars$am)
mtcars$cyl <_ as.factor(mtcars$cyl)

# linear regression model
linear_model <- lm(mpg ~ wt + hp + am, 
                   data = mtcars)
summary(linear_model)

#1, normality of residuals (shapiro-wilk test & qq plot)
shapiro.test(residuals(linear_model))
qqnorm(residuals(linear_model))
qqline(residuals(linear_model), col ="red")

#2. hetroscedasticity test (brtudensch-page test)
bptest(linear_model)

#3. linearity check (residuals vs fitted)
plot(fitted(linear_model), 
     residuals(linear_model),
     main="Residuals vs Fitted",
     xlab="Fitted values",
     ylab="Residuals")
abline(h=0,
       col="red")

#4. Independance of Errors (Durin-Watson test)
dwtest(linear_model)

linear_model <- lm(mpg ~ wt + hp + am,
                   data = mtcars)
summary(linear_model)

#alt models

#1. log model
log_model <- lm(log(mpg) ~ log(wt) + log(hp) + am,
                data = mtcars)
summary(log_model)

#2. polynomial regession model (2nd degree)
poly_model <- lm(mpg ~ poly(wt, 2) + poly(hp, 2) + am,
                 data = mtcars)
summary(poly_model)  

#3. model with categorical variables
cat_models <- lm(mpg ~ wt + hp + am + cyl,
                 data = mtcars)
summary(cat_models)  
  


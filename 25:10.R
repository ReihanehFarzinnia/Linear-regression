# SESSION AIM ----
# 1. Further practice on vizualising linear models of the gapminder data
# 2. Fitting and interpreting regression models that use predictors with 
#   discrete, nominal, i.e. non-continuous, distributions 

# Tell R we want to use these libraries
library(gapminder)
library(tidyverse)
library(ggplot2)


# Questions
# 1.1 For all countries in the gapminder dataset, What is the rate of linear change 
#     in years of average life expectancy over time? Use a linear model to calculate
#     this.
# 1.2. What is the value of the model intercept, and what does this represent?
# 1.3. According to the model, how much did global life expectancy change in the 
#     latter half of the 20th century, i.e. between 1950 and 2000?
# 1.4. What is the proportion of variance in the life expectancy variable that
#     is accounted for by the linear model?
# 1.5. Why are there vertical "bands" in the ggplot of the relationship?
# 2.1 Fit a linear model with lifeExp as the outcome and "continent" as the 
#      predictor. What is the intercept and what does it represent?
# 2.2. What is the b_1 coefficient for "continentAmericas" and what does it represent?


# Answers  ----------------------------------------------------------------
#1.1
lm_model <- lm(formula = lifeExp ~ year, data = gapminder)

summary(lm_model)

gapminder  %>%
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

#1.2

#1.3
#Life Expectancy = Intercept + (Rate of Change * Year)
#Change in Life Expectancy = (Rate of Change * (2000 - 1950))
#Change in Life Expectancy = (Rate of Change * 50 years)

change <- 0.3259 * 50 
print(change)

#1.4
#To find the proportion of variance in the life expectancy variable accounted 
#for by the linear model, you can calculate the coefficient of determination, 
#often denoted as R-squared (R²). This statistic tells you how well the model 
#explains the variance in the dependent variable (in this case, life expectancy).

summary(lm_model)$r.squared

#1.5
#Vertical "bands" or stripes in a ggplot of a relationship can be an indicator 
#of overplotting or data density issues. This occurs when multiple data points 
#have the same or very close values, causing them to be plotted on top of each 
#other, which can create these bands. These bands can make it difficult to 
#discern individual data points or trends in the data.

#2.1
fit_linear <- gapminder  %>%
  lm(formula = lifeExp ~ continent)

summary(fit_linear)

gapminder  %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

#2.2
#15.79  
#The "b_1" coefficient for the predictor variable "continentAmericas" in a 
#linear regression model represents the change in the dependent variable 
#(in this case, average life expectancy) associated with a one-unit change in 
#the predictor variable while holding all other variables constant. So, in the 
#context of your analysis, if you have fitted a linear regression model to 
#predict average life expectancy based on different variables, and 
#"continentAmericas" is one of the predictor variables, then the "b_1" 
#coefficient for "continentAmericas" represents the change in average life 
#expectancy when moving from one continent to another (in this case, from 
#non-American continents to the Americas), assuming all other factors are constant.

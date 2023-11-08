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






# SESSION AIM ------------------ Week 4 practical answers 
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
#
# 2.1 Fit a linear model with lifeExp as the outcome and "continent" as the 
#      predictor. What is the intercept and what does it represent?
# 2.2. What is the b_1 coefficient for "continentAmericas" and what does it represent?

# Answers

#  1.1 
#  lifeExp_i = b_0 + year_i*b_1 + e_i
# 
fit_linear_year <- gapminder  %>%
  lm(formula = lifeExp ~ year)

# View the OLS-estimated parameters and r-square
summary(fit_linear_year) 

#  The b_1 coefficient is 0.326 years. This means that the rate of linear
#     change in life expectancy is an average increase of about a third of a year
#    per year for the years between 1952 and 2007.
#
# 1.2. The b_0 coefficient, the intercept, is -585.65. This represents the model
#     prediction for y when x is zero. x is year, so year = 0 equals year zero,
#     which is 2,023 years ago. (There isn't actually a year = zero in the 
#     common-era/Christian calendar, so this corresponds to the life-expectancy
#     in the year 1 BC!) The model is using "year=0" as the reference 
#     point of the line, which is fine as far as the mathematics of the model 
#     goes - it's just meaningless from a substantive interpretation. 
#   This is what our model would predict for life expectancy to be in year zero 
#    if we were to extrapolate the rate of change in life expectancy between 1952
#    and 2007 back in time to year zero. 
#
# 1.3. We can simplify this calculation by noting that, in the difference between
#    the models where x=1950 and x=2000, the intercept occurs on both sides of 
#    subtraction (i.e. diff_y = (b_0 + 2000*b_1) - (b_0 + 1950*b_1), so we can drop it.
#    This just gives us diff_y = 2000*b_1 - 1950*b_1, which simplifies to 
#    b_1*(2000-1950), or b_1*50.
#    b_1 * 50 = 0.3259*50 = 16.295.
#    Interpretation: In the latter half of the twentieth century, the linear 
#    trend in global life expectancy resulted in an increase of 16.3 years. 

# 1.4. The R-squared for this model is 0.1898. About 19% of the variance in life
#     expectancy is accounted for by the liner rate of change over time.
#
# 1.5.

gapminder  %>%
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

#  The "bands" are the datapoints. They are clustered on particular years, because
#   data was only collected on these particular years. The year variable has a
#   "discrete" distribution, in that only 12 specific years are represented in 
#   the dataset. However, in principle, year could take on any value, and is 
#   therefore a "continuous" variable.

#  2.1
fit_linear_continent <- gapminder  %>%
  lm(formula = lifeExp ~ continent)

# View the OLS-estimated parameters and r-square
summary(fit_linear_continent) 

# The Intercept is 48.87 years. This represents the average life expectancy
#  for Africa across all the years 1952 to 2007. Why Africa? Look at the 
#  continents shown in the table - all are there except Africa. Africa is 
#  being treated as x=0, and so Africa is the "reference". 

# 2.2. The b_1 for continentAmericas is 15.79 years. This is the difference
#      in mean life expectancy across 1952 and 2007 between countries in the
#      Americas versus countries in the "reference" continent - Africa.


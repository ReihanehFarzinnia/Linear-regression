
# SESSION AIM ----
# 1. Vizualise linear and quadratic models of the gapminder data
# 2. Fit regression models to these data and evaluate R-square

# Tell R we want to use these libraries
library(gapminder)
library(tidyverse)
library(ggplot2)


# PLOTS OF DATA ----

# 1. Let's use a straight line to model Asian countries in 2002

gapminder  %>%
  filter(continent == "Asia" & year ==  2002) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x)

# 2. The grey band around the straight line is called the "Standard Error". 
#    It is used to represent statistical uncertainty when we use samples
#    of data to represent whole populations - different samples would
#    be expected to give us slightly different estimates of the same
#    quantity in the full population. 
#    We can hide this Standard Error from the plot if we want:  

gapminder  %>%
  filter(continent == "Asia" & year ==  2002) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE)  

# 3. The straight line is a hypothesis! It is a simple model that assumes the 
#     relationship betweeen gdp and life expectancy is the same across the
#     whole range of gdp, i.e. the difference in life expectancy between 
#     $1,000 to $2,000 is the same as between $40,000 and $41,000. 
#    Is this reasonable? Wouldn't we expect the effect of increasing gdp on
#     life expectancy to get smaller as gdp becomes higher?
#    An alternative hypothesis is "diminishing returns", whereby the slope of 
#     the line gets smaller as gdp become higher. We can model this using a
#     quadratic line.

# Let's add a quadratic line
gapminder  %>%
  filter(continent == "Asia" & year ==  2002) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x,          se = F) +   # straight line
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)     # quadratic line

# We specified the quadratic line by adding the square of the x variable to the 
#  formula. Another way to ask for the same thing is:

gapminder  %>%
  filter(continent == "Asia" & year ==  2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, se = F) +         # straight line
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F)  # polynomial line

# a quadratic is another way of saying a polynomial where x is raised to the 
#  second power, i.e. squared.



# LINEAR MODEL ---- 

## Now lets see what these lines look like when we use our linear model:
#
#  y_i = b_0 + x_i*b_1 + e_i
# 
#  where: 
#         y_i = lifeExp,  x_i = gdpPercap 

# Step 1: Fit the model 
fit_linear <- gapminder  %>%
  filter(continent == "Asia" & year ==  2002) %>%
  lm(formula = lifeExp ~ gdpPercap)

# View the OLS-estimated parameters and r-square
summary(fit_linear) 

# Note the R-square. Our straight-line model reduces the 
#  variance of the e_i term by 0.5087, or 50.87%

# The "Estimate" for the b_0 parameter, the "(Intercept)", is 63.87 years. 
# This tells us the expected value of the outcome, life expectancy, when
# the x variable = 0, i.e. if gdp per capita was zero.
# Gdp per capita = 0 does not occur in the dataset - b_0 (the intercept) is just  
# a reference point, like the line of zero longitude that goes through London. 

# The b1 parameter is 0.0005357 years. This is a very small number.
# This is because gdp is measured in dollars. Life expectancy
#  differs by only 0.0005357 years per dollar. Let's change the x_i units
#  to 1,000s of dollars and see what this does:

fit_linear_gdp1000 <- gapminder  %>%
  filter(continent == "Asia" & year ==  2002) %>%
  mutate(gdpPercap_1000 = gdpPercap / 1000) %>%     # divide gdpPercap by 1,000
  lm(formula = lifeExp ~ gdpPercap_1000)

# View the OLS-estimated parameters and r-square
summary(fit_linear_gdp1000) 

# The b_1 parameter is now approximately 0.537 years per 1,000 dollars, 
#  i.e. countries that differ by $1,000 in their GDP, on average differ
#  by about half a year in average life expectancy.

# What has happended to Intercept? That's right, it's the same: 63.78 years.
# The value of zero dollars is the same in both units, dollars and 1,000s of dollars, 
#  so "x = zero" has the same exact meaning in both units. 

# What has happened to the R-square? That's right, nothing. Changing the units
#  does not change it because it's aproportion (of variance accounted for by 
#  the model). e.g. 0.5 km is half of 1 km, just as 500 metres is half of 
#  1,000 metres, they are just in different units. The proportion is the same 
#  whatever the units. 


# Quadratic model ----
# we need to create a new paramter, equal to the square of the predictor, 
#  to represent the quadratic component of the model.

fit_quadratic <- gapminder  %>%
  filter(continent == "Asia" & year ==  2002) %>%
  mutate(gdpPercap_1000 = gdpPercap / 1000)   %>%        # gdp in $1,000
  mutate(gdpPercap_1000_sq = gdpPercap_1000^2) %>%       # square of the above
  lm(formula = lifeExp ~ gdpPercap_1000 + gdpPercap_1000_sq)

summary(fit_quadratic) 
# R-squared = 0.5664  
# This has gone up! The quadratic model has a higher R-square. How much higher?

# We can ask R to compute the difference in R-square between the linear and 
#  quadratic model. 
# The data objects "fit_..." contain the r-square as a variable: "r.squared" 
# To get the difference we just subtract them:

summary(fit_quadratic)$r.squared - summary(fit_linear_gdp1000)$r.squared

#  = 0.058, i.e. about 6%. 
#
#  The quadratic model reduces the size of the residuals by 6% MORE than
#   the linear model. This is modest evidence that the relationship between
#   gdp and life expectancy is weaker at high levels of gdp compared to lower
#   levels, i.e. "diminishing returns". 

# What is the b_1 parameter "gdpPercap_1000" telling us?
# This is the linear component of the quadratic curve. It tells us how many
# units of difference in the outcome we would expect for a one unit difference
# in the predictor, WHEN THE PREDICTOR IS ZERO. It is the slope of the line at
# x = zero.

# What is the new parameter "gdpPercap_1000_sq" telling us? (Let's call it b_2)
# This is the quadratic component of the quadratic curve. It tells us how much
#  the slope of the line changes per unit difference of this variable. 
#  Remember, this variable is measured in $1,000^2, i.e. squared thousands of 
#  dollars! This can make it tricky to interpret. But note that its sign is 
#  negative, i.e. as it gets larger, the slope is going to go down.

# Questions:

#  1. according to the linear model, what is the "predicted" mean life-expectancy
#      for a country with a GDP per capita of $20,000?

#  2. according to the quadratic model, what is the "predicted" mean life-expectancy
#      for a country with a GDP per capita of $20,000?





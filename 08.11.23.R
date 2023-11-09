
# TITLE: Using DAGs to select model spefication ----

# EXERCISE 
#  Use data simulated from a known DAG to explore the problems of using 
#   linear models without making causal assumptions

# AIMS
# 1. To use the "dagitty" library to simulate data from DAGs.
# 2. To fit correctly- and incorrectly-specified models to the simulated
#     data.

# SUMMARY
# We will be taking a DAG and then simulating a dataset from it using
#  the rules of structural causal modelling. 
# You will then be shown the DAG. The research question you must answer
#  is always "What is the total causal effect of x on y?"
# Using your knowledge of causal modelling on DAGs, you will be able to 
#  use the DAG to specify a model that includes a sufficient set of third-
#  variables ("control variables") to block all spurious, non-causal paths 
#  and give you the correct estimate of the size of the total causal effect 
#  variable x in predicting variable y. 


# YOUR TASKS ----

# Install the library "dagitty"
install.packages("dagitty")

# Activate daggity and ggplot2
library(dagitty)
library(ggplot2)


############## Your TASK 1: Simulate three new datsets
#
# Run the code below - it will create three new datasets in
#  the "Environment" tab, called "data_dag_n" where n is 1,2,or3.

dag_1 <- dagitty('dag{z -> x [beta=.6] z -> y [beta=.6] }')
set.seed(837123)  
data_dag_1 <- simulateSEM(dag_1) 

dag_2 <- dagitty('dag{x -> z [beta=.6] z -> y [beta=.6] }')
set.seed(837123)  # set.seed(637112)
data_dag_2 <- simulateSEM(dag_2) 

dag_3 <- dagitty('dag{a -> c [beta=.3] a -> y [beta=.3] b -> c [beta=.3] 
     b -> x [beta=.3] b -> d [beta=.3] c -> y [beta=.3] d -> e [beta=.3] 
     d -> y [beta=.3] e -> y [beta=.3] f -> y [beta=.3] x -> e [beta=.3] 
     x -> f [beta=.3] }')
set.seed(837123)                     
data_dag_3 <- simulateSEM(dag_3)
#
# You have created three datasets

############### The DAGs
#
# The datasets above (data_dag_n) have been simulated from a population
#  where the variables are causally related because of the relationships
#  given by a specific DAG. In particular, data_dag_1 was simulated from
#  dag_1, data_dag_2 was simulated from dag_2, and so on.
# Here are the DAGS:

# DAG_1:   x <- z -> y

# DAG_2:   x -> z -> y

# DAG_3:   create a plot of DAG_3 using the plot() command:
plot(dag_3)

# By "simulated" data, I mean that the datasets were created by taking 
#  n=500 hypothetical random units from the population given by the
#  DAG. So, each dataset is N=500 rows. For the purposes of simulating
#  the data, we assumed that that mean of each variable is zero and the
#  standard deviation is one. These values will be approximate in the 
#  dataset, because the data were simulated including sampling "noise"
#  i.e. each unit has an e ("error") coefficient 
#
# Let's have a look at some data! 
ggplot(aes(x = x, y = y), data=data_dag_1)  + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se=F)

# in data_dag_1, x and y are correlated - the line has a non-zero slope. 
#  However, some or even all of this correlation may be spurious, non-causal.
# The size of the causal effect is given by the DAG, and we have to specify
#  our linear models correctly if we want to estimate it. 

#### YOUR TASK
# 
# Your general research question is, for each pair of DAG and data, 
#
#   "What is the size of the total causal effect of x on y?", 
#
#   (assuming that the true causal effect is that implied by the DAG.)


############################### Exercise 1: DAG_1  ----

# DAG_1:   x <- z -> y (z is a confounder which means it is a non-collider on a backdoor path)

# QUESTION 1  ----
# 1. Fit linear models to data_dag_1.   
#     Fit models such that:
# 1.1. The model shows the "naive" relationship between x and y, with no 
#        "third" variables in the model. 
fit_dag_1_1 <- lm(y ~ x, data = data_dag_1)
summary(fit_dag_1_1)
#        What is the b_1 coefficient for x?
# 1.2. The model includes sufficient third variables to correctly 
#       recover the total causal effect of x -> y
fit_dag_1_2 <- lm(y ~ x + z, data = data_dag_1)
summary(fit_dag_1_2)
#        What is the b_1 coefficient for x?

# 1.3. The model has the highest r-square of all possible models.

#        What is the b_1 coefficient for x?
# 1.4  What is the theoretical "true" value for the b_1 parameter for x?


############################### Exercise 2: DAG_2  ----
#
# DAG_2:   x -> z -> y
#
# QUESTION 2  ----
# 2. Fit linear models to data_dag_2. 
#     Fit models such that:
# 2.1. The model shows the "naive" relationship between x and y, with no 
#        "third" variables in the model. 
fit_dag_2_1 <- lm(y ~ x, data = data_dag_2)
summary(fit_dag_2_1)
#        What is the b_1 coefficient for x?
# 2.2. The model includes sufficient third variables to correctly 
#       recover the total causal effect of x -> y
fit_dag_2_2 <- lm(y ~ x + z, data = data_dag_2)
summary(fit_dag_2_2)
#        What is the b_1 coefficient for x?
# 2.3. The model has the highest r-square of all possible models.
#        What is the b_1 coefficient for x?
# 2.4  What is the theoretical "true" value for the b_1 parameter for x?



############################### Exercise 3: DAG_3 ----
#
#  DAG_3:
plot(dag_3) 
#
#
# QUESTION 3  ----
# 3. Fit linear models to data_dag_3.   
#     Fit models such that:
# 3.1. The model shows the "naive" relationship between x and y, with no 
#        "third" variables in the model. 
#        What is the b_1 coefficient for x?
# 3.2. The model includes sufficient third variables to correctly 
#       recover the total causal effect of x -> y
fit_dag_3_2 <- lm(y ~ x + b, data = data_dag_3)
summary(fit_dag_3_2)
#        What is the b_1 coefficient for x?
# 3.3. The model has the highest r-square of all possible models.
#        What is the b_1 coefficient for x?
# 3.4  What is the theoretical "true" value for the b_1 parameter for x?


##################### ANSWERS

### Exercise 1 - answers ----
#
# z is a confounder (cause of both) x and y, which lies on a backdoor path
# between them. There are no causal paths between x and y, therefore the
# size of the true causal effect is zero. To recover this true causal effect
# we must block the backdoor path by controlling for confounder z. 

## 1.1. Naive model, not controlling for anything

fit_dag_1_11 <- lm(data = data_dag_1, formula = y ~ x)
summary(fit_dag_1_11)

# b_1 for x is 0.359. It should be around zero.  
# From this model we would incorrectly conclude that x is having a 
# strong effect on y, when in reality, it has no effect.

## 1.2 Correct model, controlling for the confounder
fit_dag_1_12 <- lm(data = data_dag_1, formula = y ~ x + z)
summary(fit_dag_1_12)
# b_1 for x is now about right, i.e. small (-0.078), close to zero. 
# The fact that it is not exactly zero is because this is data that has
# been simulated to include sampling "noise", i.e. e values.

## 1.3 In this case, the correct model, 1.2, maximizes the r-square (44.1%)

## 1.4. As there were no causal paths between x and y in the DAG, the size 
#        of the causal effect must be zero in the population.

# In this case we saw that, even controlling correctly for the confounder,
#  the sample estimate was quite different to the population value (-0.08 vs
#  0, respectively), but notice that the 95% Confidence Intervals for the two 
#  estimates of b_0 for x overlap.)



### Exercise 2 - answers ----
#
#  z is a mediator on the causal path between x and y. There are no backdoor
#  paths between x and y, therefore there is no need to control for any variable
#  to block all backdoor paths. 

## 2.1. A model not controlling for anything - correct
fit_dag_2_21 <- lm(data = data_dag_2, formula = y ~ x)
summary(fit_dag_2_21)
# b_1 for x = 0.35 

## 2.2. Incorrect model - controlling for the mediator - overcontrol bias
fit_dag_2_22 <- lm(data = data_dag_2, formula = y ~ x + z)
summary(fit_dag_2_22)
# The b_1 for x is now -0.088

## 2.3  Did you notice the " [beta=.6] " parts in the syntax we used to
#       simulate the data? These tell R the size of the b-coefficients 
#       for the paths between the variables in the model, in the population
#       from which the data were simulated from. On a causal path, we can
#       simply multiply these values along the path to get the expected
#       size of the causal effect (if the variables have been "standardized"
#       first, i.e. mean-centred then divided by their standard deviation. This
#       gives the variable a mean of zero and a standard deviation of one.)
#      The beta coefficients (beta = standardized b) are all 0.6. There are
#       two parts to the causal path, x->z and z->y, both of which are 0.6.
#
#      Multiplying 0.6*0.6 = 0.36. This is the size of the causal path between
#       x and y in the population.  
#
#      The sample estimate from the correct model is very close to the 
#       population value of 0.36. The estimate from the other model is very
#       wrong, not even the same sign (which is a "Simpson's Paradox" - where 
#       the sign of the relationship between two variables is flipped by 
#       controlling for a third variable)
#
## 2.4 The R-square is 44% for the wrong model and 12.8% in the correct model.
#     You cannot rely on R-square for model selection.



### Exercise 3 - Answers.  ---- 
#
# This example is drawn from Figure 1 in: 
#   Elwert, F. (2013). "Chapter 13: Graphical Causal Models."
#    In Handbook of Causal Analysis for Social Research, 
#    edited by Stephen Morgan, Springer.  
# This is on blackboard.
#

## 3.1. A naive model, with no control for backdoor paths
fit_naive <- lm(data = data_dag_3, formula = y ~ x)
summary(fit_naive)
# b_1 for x = 0.275

# 3.2.1 A model with the minimum theoretically-adequate adjustment (just variable b)
fit_minimal <- lm(data = data_dag_3, formula = y ~ x + b)
summary(fit_minimal)
# b_1 for t = 0.219 (should be 0.18)

# 3.2.2 A model with the maximal theoretically-adequate adjustment (variables a, b, c, d)
fit_maximal <- lm(data = data_dag_3, formula = y ~ x + a + b + c + d)
summary(fit_maximal)
# b_1 for x = 0.18.9 (should be 0.18)
#
# Note that, because of the presence of random variation and a small sample,
#  the maximal adjustment set (a, b, c, d) gives better control of spurious,
#  non-causal relationships than the minimal set.

# 3.3 A model with the maximal theoretically-adequate adjustment (variables a, b, c, d)
# r-square is 65%, vs. 50% for the "best" model above.

# 3.4  What is the theoretical "true" value for the b_1 parameter for x?
# The size of causal effect = 
#    x->f->y + x->e->y = 
#   (0.3*0.3)+(0.3*0.3) = 0.18
#
#  (0.3 values are the "betas" when simulating the data)
#
#  The "best" model (using all suitable adjustment variables) delivered
#   a very close sample estimate of the population parameter, even though
#   it did not have the highest r-square of all possible models.

# Conclusion:
# Making incorrect causal assumptions can lehad to very wrong statistical 
#  estimates of the size of causal effects .
#
# The r-square statistic doesn't help us diagnose this problem! I.e. the r-square
#  for the wrong model might be higher than that for the correct model.

#### END ----



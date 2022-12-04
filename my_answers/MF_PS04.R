library(dplyr)
library(car)
data(Prestige)
help(Prestige)

##############
# Question 1 #
##############

# create new variable 'professional' by recoding 'type'
# so that professionals = 1 and blue & white collar = 0

professional <- ifelse(Prestige$type == "prof", 1, 0)
Data <- cbind(Prestige, professional)

# run a linear model with prestige as outcome and income, professional, and the
# interaction of the two as predictors

mod <- lm(Data$prestige ~ Data$income + Data$professional + 
            Data$income:Data$professional)
print(summary(mod))

# Prediction Equation

Y = 21.1422589 + 0.0031709(income) + 37.7812800(professional) + -0.0023257(income * professional)

# Part D: Interpret Coefficient for Income

# There is a small, positive, and statistically reliable relationship between income and occupational prestige, such that a one dollar increase in income is associated with an average increase of 0.003 points in  occupational prestige score.

# Part E: Interpret Coefficient for Professional

# There is a positive and statistically reliable relationship between having a Professional job type and occupational prestige, such that having a Professional job type is associated with an average increase of 37.78 points in  occupational prestige score.


# part f

base <- 21.1422589 + 0.0031709*(0) + 37.7812800 * (1) + (-0.0023257 * (0))
raise <- 21.1422589 + 0.0031709*(1000) + 37.7812800 * (1) + (-0.0023257 * (1000 * 1))
raise - base
# we would expect that a $1000 raise would lead to an 0.8452 point increase in prestige score


# part g
prof <- 21.1422589 + 0.0031709*(6000) + 37.7812800 * (1) + (-0.0023257 * (6000 * 1))
nonprof <- 21.1422589 + 0.0031709*(6000) + 37.7812800 * (0) + (-0.0023257 * (6000 * 0))
prof - nonprof

# for an individual making $6000, we would expect that moving from 
# nonprofessional to professional would lead to a 23.82 point increase in 
# prestige 


##############
# Question 2 #
##############

xbar <- 0.042
mu <- 0.302

# part a

# Ho: assigned = constant
# Ha: assigned /= constant (two tailed test)
# SE = 0.016


# Step 2: Calculate the test statistic
Z <- (0.042 - 0.302)/0.016 # Z = -16.5
# Calculate the p-value
pvalue <- 2*pnorm(Z, lower.tail = F)

# alpha (.05) < pvalue = 2 : fail to reject the Null

# part b

# Ho: adjacent = constant
# Ha: adjacent /= constant (two tailed test)
# alpha = 0.05

# SE = 0.013

# Step 2: Calculate the test statistic
z <- (0.042 - 0.302)/0.013 # z = -20
# Step 3: Caluclate p value
pvalue <- 2*pnorm(z, lower.tail = F)

# alpha (.05) < pvalue (2) : Fail to reject the Null

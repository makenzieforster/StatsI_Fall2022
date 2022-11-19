library(tidyverse)
data <- read.csv("~/Documents/GitHub/StatsI_Fall2022/datasets/incumbents_subset.csv")

### Question 1 ###

# regression with outcome variable Y = voteshare and 
# explanatory variable X = difflog

Q1 <- lm(data$voteshare ~ data$difflog, data=data)
summary(Q1)

# make scatterplot and add regression line
plot(data$voteshare ~ data$difflog, data=data)
abline(Q1)

#save residuals as separate object
resQ1 <- Q1$residuals

# prediction equation
# Y = 0.579031 + 0.041666(X)


### Question 2 ###

# regression with outcome variable Y = presvote and 
# explanatory variable X = difflog

Q2 <- lm(data$presvote ~ data$difflog, data=data)
summary(Q2)

# make scatterplot and add regression line
plot(data$presvote ~ data$difflog, data=data)
abline(Q2)

#save residuals as separate object
resQ2 <- Q2$residuals

# prediction equation
# Y = 0.507583 + 0.023837(X)


### Question 3 ###

# regression with outcome variable Y = voteshare and 
# explanatory variable X = presvote

Q3 <- lm(data$voteshare ~ data$presvote, data=data)
summary(Q3)

# make scatterplot and add regression line
plot(data$voteshare ~ data$presvote, data=data)
abline(Q3)

# prediction equation
# Y = 0.441330 + 0.388018(X)


### Question 4 ###

# regression with outcome variable Y = resQ1 and 
# explanatory variable X = resQ2

Q4 <- lm(resQ1 ~ resQ2)
summary(Q4)

# make scatterplot and add regression line
plot(resQ1 ~ resQ2)
abline(Q4)

# prediction equation
# Y = -1.674e-18 + 2.569e-01(X)

### Question 5 ###

# multivariate regression with outcome variable Y = voteshare and 
# explanatory variables X = presvote and difflog

Q5 <- lm(data$voteshare ~ data$difflog + data$presvote, data=data)
summary(Q5)

# prediction equation
# Y = 0.4486442 + 0.0355431(X1) + 0.2568770(X2)




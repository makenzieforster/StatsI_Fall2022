## Question 1 ##

pchisq(3.801, df = 2, lower.tail = FALSE)

## Question 2 ##

dat <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# regression line
lm(dat$reserved ~ dat$water, 
   data = dat)

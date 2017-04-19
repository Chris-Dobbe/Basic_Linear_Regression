############################### Linear Regression Starter Script ############################

# Uses the lm() linear regressions modeling and ggplot to calculate & visualize correlation

# Begin by loading the data set and then choosing 2 or 3 variables to plug into the model

# Project Name:
# Data source:
# Correlation Variables:

############################################################################################

# begin by setting the working directory for the files
setwd("C:/Users/*********/Documents/R")

#Load the packages and libraries, or d/l any using install.packages("???")
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(DescTools)
library(RDCOMClient)  # For importing specific cells of Excel files
library(gdata)  # for importing Excel files
library(GGally)
library(scales)
library(caret)

### Begin by loading in the dataset using the appropriate method, uncomment chosen method

#data1 <- file.choose() # Allows for manual selection from folder location, paired with below
  #data <- read_csv(data1)  # 
#data1 <- read_csv("file_name.csv") #loads in the CSV to a tibble, easier to work with
#data1 <- read.csv("file_name.csv")
#data1 <- read.csv("file_name.tsv", sep = "\t", header = TRUE)
#data1 <- read.xls("file_name.xlsx")
#data1 <- XLGetRange(sheet = "sheet1", range = "A1:B21", header = TRUE) #only used when Excel workbook is open

#View dataset
View(data1)

### View summary info on the dataset that was loaded in
names(data1)
head(data1)
str(data1)
summary(data1)

### For modeling, you will want to create a training and test data set
set.seed(1)      # setting the seed will allow you to adjust the random dataset in training, or keep the same data by changing seed #
in_train = createDataPartition(y = data1$var_1, p = 0.75, list = FALSE)    # Puts 75% of observations in the training dataset
head(in_train)    # row indices of observations in the training dataset

train = data1[in_train, ]
test = data1[-in_train, ]

dim(train)
dim(test)

### create the linear regression model, choosing 2 variables to examine

model <- lm(var_1 ~ var_2, data = data1)    #produces the linear model between the two var.
attributes(model)                           # returns list of all things inside the model, handy for calling specific elements later
summary(model)                              # gives you the residuals and coefficients of the linear model
ggplot(data1, aes(x = var_1, y = var_2)) +  # creates scatter plot
  geom_smooth(method = 'lm') +
  geom_point(size = 1, alpha = 0.60) +
  #facet_grid(season ~. ) +                 # provides additional graphs breakdown by additional dimension
  theme_minimal()

# Residual section contains summary of the distribution of the errors (distance between line and individual points)
# Coefficients contains estimated coefficients, standard errors, t & p values for variables in model
#   The (intercept) estimate is going to be the average value for the 1st variable
#   2nd variable estimate is the pos/neg correlation, for every increase
#     of 1 in the 2nd variable, the 1st variables goes up by the 2nd var. estimate
#   we want to see a large t value >= 2.5, and a tiny p value to indicate correlation

### This plot includes a 3rd variable in the plot, performing multivariate regression
# produces same result as first model, but includes additional variable correlated with var. 1

model2 <- lm(var_1 ~ var_2 + var_3, data = data1)
summary(model2)             # Output will show how each var_2 and var_3 correlate to var_1, using same metrics

### below chart plots two variables, and adds size and color of point for a 3rd variable

ggplot(data1, aes(y = var_4, x = var_5)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black') + 
  geom_point(aes(color = var_1, size = var_1), alpha = 0.70) +
  #facet_grid(season ~. ) + 
  scale_colour_gradient(limits = c(2, 28540), low = 'blue', high = 'yellow') +
  scale_size(range = c(0, 15)) + 
  theme_minimal()

### explore the residuals further to understand their distribution and trend of linear model
residuals(model)  # lists the distance from the mean for each observation in the model
summary(residuals(model))
plot(fitted(model), residuals(model))
abline(h = 0)  #plots the residual values around the mean line of 0
qqnorm(residuals(model))
qqline(residuals(model)) # additional residual plot 

### Confidence Intervals for a linear model to predict values

plot(data1$Var_1, data1$var_2)
ci <- confint(model, level=0.95) #creates the confidence interval, using a specified sign. level
ci                               
lower_est = ci[1] * 9 + ci[2]  #creates lowest estimate for the x/y value based on where the *x is
lower_est                       #estimating x or y depends on where the *x goes, on 1sr or 2nd ci[]
upper_est = ci[3] * 9 + ci[4]  #creates upper estimate for x/y value 
upper_est

#resulting outputs are the upper and lower bounds based on foncidence intervals from linear model

### predicting values with a generalized linear function, aka declaring multiple values to determine 1

gen_model <- glm(var_1 ~ var_2 + var_3, data=data1, family=poisson) #family is a type of general linear model
params <- data.frame(var_2=110, var_3=160)   #creates the predicting parameters based on the model created
predict(gen_model, params, type="response")  

# resulting output is the estimated number of cylinders (var_1) based on historical data in model

############################### Linear Regression Starter Script ############################

# Uses the lm() linear regressions modeling and ggplot to calculate & visualize correlation between
# two independent, quantitative variables

# Begin by loading the data set and then choosing 2 or 3 variables to plug into the model

# Project Name:
# Data source:
# Correlation Variables:

############################################################################################

# begin by setting the working directory for the files
setwd("C:/Users/cdobbe/Documents/R")

#Load the packages and libraries, or d/l any using install.packages("???")
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(ggplot2)
library(lattice) #still need to isntall this


### Begin by loading in the dataset using the appropriate method

#data1 <- file.choose() # Allows for manual selection from folder location
data1 <- read.csv("pizza_data_frame3.csv", header=TRUE) #read in standard CSV
#data1 <- read_csv("file_name.tsv", sep = "\t", header = TRUE) #read in TSV file
#data1 <- read.delim("file_name.txt", sep = "\t", header = TRUE) #read in .txt files
#data1 <- read.xls("file_name.xlsx") #read in Excel file
#data1 <- XLGetRange(sheet = "sheet1", range = "A1:B21", header = TRUE) # used with Excel file being open

### View summary info on the dataset that was loaded in
View(data1)
names(data)
head(data1)
str(data1)
summary(data1)

### First perform any data cleaning steps necessary to preapre the data set for modeling

### Remove any rows that have an N/A value within a specific column
data1 <- data1 %>% drop_na(delivery_min)  #removes rows with NA in specific column
data1 <- data1 %>% drop_na(temperature)

### Before creating the linear model, varify the 3 Correlation Conditions visually
# 1. Are you using quantitiative Variables?
# 2. Is the scatter plot linear (use your best judgement)
# 3. Are there any outliers or extreme points to consider?

### First check linearity using a scatter plot, make x the predictor and y the response var.
ggplot(data1, aes(x = data1$delivery_min, y = data1$temperature), color="black") + # creates scatter plot
  geom_point(size = 1, alpha = 0.30)

### Next check for outliers using boxplots of your 2 variables
par(mfrow=c(1, 2))
boxplot(data1$delivery_min, main = "Delivery Speed", sub=paste("Outlier rows: ", boxplot.stats(data1$delivery_min)$out))
boxplot(data1$temperature, main="Pizza Temperatue", sub=paste("Outlier rows: ", boxplot.stats(data1$temperature)$out))

# If your two variables meet the 3 conditions, that move forward

### We can check correlatoin between the two vairables first
cor(data1$delivery_min, data1$temperature)
#close to 1 or -1 is very strong correlation, close to 0 is no correlation

### create the linear model, choosing 2 variables to examine, with predictor (x) value on the left, response on the right

model <- lm(data1$delivery_min ~ data1$temperature, data = data1) #produces the linear model between the two var.
print(model) # provides linear equation values, y-intercept and slope

qplot(data1$delivery_min, data1$temperature, data = data1) #quick plot of variables
attributes(model) # returns list of all things inside the model, handy for calling specific elements later
summary(model) # gives you the residuals and coefficients of the linear model
#   Least Squares line equation values are in the first "estimates" column (mathces above 'print' line)
#     (Intercept) value shows the y-intercept in the linear model
#     Value below that, right of your response (y) value is the slope of the line
#   R-squared value closer to 1 means the variability of the y value is strongly explained by the model (and x value)
#   For stat. significance, we want both Pr(>|t|) values to be <0.05, and 3 stars ***
#  High T value (>1.96) indicates the null hypothese (coefficients = 0) can be rejected for atl. hypothese (not = 0)
#  Aka a high T value indicates there exists a reltionship between the predictor & response var.

# requires lattice, but should be a residual plot for x-values
xyplot(resid(model) ~ fitted(model),
       xlab = "Delivery Minutes",
       ylab = "Pizza Temperature",
       main = "Delivery time, pizza temp. Association",
       panel = function(x,y, ...)
         {
         pane.grid(h = -1, v = -1)
         panel.abline(h = 0)
         panel.xyplot(x, y, ...)
       })

# Creates the scatter plot with the linear model 
linregplot <- ggplot(data1, aes(x = data1$delivery_min, y = data1$temperature)) +
  geom_smooth(method = 'lm', color= "red") +
  geom_point(size = 1, alpha = 0.40)
print(linregplot)

### This should provide you with all of the information and visual needed for your linear model

### Once completing your code, export a CSV to a specified location
write.csv(data1, "/Users/cdobbe/Documents/R/output csv files/X_linear_model_data.csv")


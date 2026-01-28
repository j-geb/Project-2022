# Importing Packages 
library(readxl)
library(tidyverse)
library(broom)
library(sjPlot)
library(ggplot2)
library(ggstatsplot)
library(jtools)
library(ggstance)
library(car)
library(coefplot)

# Reading in Data 
concrete = read_excel("~/Desktop/UNITS/DATA2002/Assignment 2/concrete+compressive+strength/Concrete_Data.xls")

# Cleaning Data 
old_names = colnames(concrete)
new_names = c("cement", "furnace_slag", "fly_ash", "water", "superplasticizer",
              "coarse_agg", "fine_agg", "age", "concrete_strength")
colnames(concrete) = new_names
name_combo = bind_cols(New=new_names, Old=old_names)
name_combo |> gt::gt()
data = subset(concrete, select = -c(water))

# Backward Step-wise Model
M1 = lm(data$concrete_strength ~ ., data = data)
step.back.aic = step(M1, direction = "backward", trace = FALSE)
round(summary(step.back.aic)$coef,3)
step.back.aic |>
  broom::glance() |>
  round(2) |> t()

# Visualizing the predictions our model would make

summ(M1)

plot_summs(M1, plot.distributions = TRUE, inner_ci_level = .95)

# This plots the regression coefficients of each predictor variable.
# The horizontal line indicates the confidence interval at a 95% level.
# The distributions for each coefficient are plotted above them. 
# Here we say that most variables used in the model have a small variance at the 95% level.
# Most coefficients have minimal variance, indicating they are reliable.
# Furthermore, the normality assumption of the variable is upheld in most cases.
# This is important later as it is a pre-requisite to deriving p values.
# Finally, they are all to the left of the dotted line, indicating the relationship is positive.

summ(M1)









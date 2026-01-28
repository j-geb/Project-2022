# Importing Packages 

library(readxl)
library(tidyverse)
library(broom)
library(sjPlot)

# Reading in Data 

concrete <- read_excel("~/Desktop/UNITS/DATA2002/Assignment 2/concrete+compressive+strength/Concrete_Data.xls")

# Cleaning Data 

old_names = colnames(concrete)
new_names = c("cement", "furnace_slag", "fly_ash", "water", "superplasticizer",
              "coarse_agg", "fine_agg", "age", "concrete_strength")

colnames(concrete) = new_names

name_combo = bind_cols(New=new_names, Old=old_names)
name_combo |> gt::gt()

data = subset(concrete, select = -c(water))

# Creating Linear Model

M1 = lm(data$concrete_strength ~ ., data = data)

# Backward Stepwise Model

step.back.aic = step(M1, direction = "backward", trace = FALSE)
round(summary(step.back.aic)$coef,3)
step.back.aic |>
  broom::glance() |>
  round(2) |> t()

# Plotting Fitted vs Residual Values, you can run any 

res <- resid(step.back.aic)

plot(fitted(step.back.aic), res, main = "Residual against Fitted Value", xlab = "Fitted", ylab = "Residual")
abline(0,0, col = 'red')

qqnorm(res)
qqline(res) 

plot(density(res))

# Reading in Data
library(readxl)
Concrete_Data <- read_excel("~/Desktop/UNITS/DATA2002/Assignment 2/concrete+compressive+strength/Concrete_Data.xls")

# Cleaning Data 
new_names = c("Cement","Blast_Furnace_Slag","Fly_Ash","Water",
              "Superplasticizer","Coarse_Aggregate","Fine_Aggregate","Age", "Concrete_Compressive_Strength")
colnames(Concrete_Data) = new_names

# Visualizing Entire Data Set
library(GGally)
install.packages("GGally")
GGally::ggpairs(Concrete_Data) + theme_bw(base_size = 12)

# Creating Multiple Regression
model = lm(Concrete_Compressive_Strength ~ Cement + Blast_Furnace_Slag + Fly_Ash + Water + Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Age, Concrete_Data)

#Creating Summary of Regression Model
summary(model)$coefficients |> round(3)

# QQ-Plot
qqnorm(res)
qqline(res)

#Residual Plot
res <- resid(model)
plot(fitted(model), res)
abline(0,0)

# CHECKING MODEL ASSUMPTION 1: LINEARITY

plot(Concrete_Data$Cement, Concrete_Data$Concrete_Compressive_Strength, main = "Cement vs Concrete Compressive Strength",
     xlab = "Cement (kg in a m^3 mixture)", ylab = "Concrete compressive strength (megapascals) ",
     pch = 19, frame = FALSE)
abline(lm(Concrete_Data$Concrete_Compressive_Strength ~ Concrete_Data$Cement, data = Concrete_Data), col = "red")


plot(Concrete_Data$Blast_Furnace_Slag, Concrete_Data$Concrete_Compressive_Strength, main = "Blast Furnace Slag vs Concrete Compressive Strength",
     xlab = "Blast Furnace Slag (kg in a m^3 mixture)", ylab = "Concrete compressive strength (megapascals) ",
     pch = 19, frame = FALSE)
abline(lm(Concrete_Data$Concrete_Compressive_Strength ~ Concrete_Data$Blast_Furnace_Slag, data = Concrete_Data), col = "red")


plot(Concrete_Data$Fly_Ash, Concrete_Data$Concrete_Compressive_Strength, main = "Fly Ash vs Concrete Compressive Strength",
     xlab = "Fly Ash (kg in a m^3 mixture)", ylab = "Concrete compressive strength (megapascals) ",
     pch = 19, frame = FALSE)
abline(lm(Concrete_Data$Concrete_Compressive_Strength ~ Concrete_Data$Fly_Ash, data = Concrete_Data), col = "red")


plot(Concrete_Data$Water, Concrete_Data$Concrete_Compressive_Strength, main = "Water vs Concrete Compressive Strength",
     xlab = "Water (kg in a m^3 mixture)", ylab = "Concrete compressive strength (megapascals) ",
     pch = 19, frame = FALSE)
abline(lm(Concrete_Data$Concrete_Compressive_Strength ~ Concrete_Data$Water, data = Concrete_Data), col = "red")


plot(Concrete_Data$Superplasticizer, Concrete_Data$Concrete_Compressive_Strength, main = "Superplasticizer vs Concrete Compressive Strength",
     xlab = "Superplasticizer (kg in a m^3 mixture)", ylab = "Concrete compressive strength (megapascals) ",
     pch = 19, frame = FALSE)
abline(lm(Concrete_Data$Concrete_Compressive_Strength ~ Concrete_Data$Superplasticizer, data = Concrete_Data), col = "red")


plot(Concrete_Data$Coarse_Aggregate, Concrete_Data$Concrete_Compressive_Strength, main = "Coarse Aggregate vs Concrete Compressive Strength",
     xlab = "Coarse Aggregate (kg in a m^3 mixture)", ylab = "Concrete compressive strength (megapascals) ",
     pch = 19, frame = FALSE)
abline(lm(Concrete_Data$Concrete_Compressive_Strength ~ Concrete_Data$Coarse_Aggregate, data = Concrete_Data), col = "red")


plot(Concrete_Data$Fine_Aggregate, Concrete_Data$Concrete_Compressive_Strength, main = "Fine Aggregate vs Concrete Compressive Strength",
     xlab = "Fine Aggregate (kg in a m^3 mixture)", ylab = "Concrete compressive strength (megapascals) ",
     pch = 19, frame = FALSE)
abline(lm(Concrete_Data$Concrete_Compressive_Strength ~ Concrete_Data$Fine_Aggregate, data = Concrete_Data), col = "red")


plot(Concrete_Data$Age, Concrete_Data$Concrete_Compressive_Strength, main = "Age vs Concrete Compressive Strength",
     xlab = "Age (kg in a m^3 mixture)", ylab = "Concrete compressive strength (megapascals) ",
     pch = 19, frame = FALSE)
abline(lm(Concrete_Data$Concrete_Compressive_Strength ~ Concrete_Data$Age, data = Concrete_Data), col = "red")

# CHECKING ASSUMPTION 2: INDEPENDENCE/ NO-MULTICOLINEARITY

install.packages("caTools")    
install.packages('car')

library(caTools)
library(car)

vif_values <- vif(model)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

#CHECK MODEL ASSUMPTION 3: HOMOSKEDASTICITY









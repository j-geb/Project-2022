library(readxl)
library(tidyverse)
Concrete_Data <- read_excel("~/Documents/DATA2002/Assignment/group project/Concrete_Data.xls")


new_names = c("Cement","Blast_Furnace_Slag","Fly_Ash","Water",
                          "Superplasticizer","Coarse_Aggregate","Fine_Aggregate","Age", "Concrete_Compressive_Strength")

colnames(Concrete_Data) = new_names


# Dependent variable = Concrete Strength

p = Concrete_Data %>% drop_na() %>% ggplot() + 
  aes(y = Concrete_Compressive_Strength) + 
  geom_boxplot() + 
  ylab("Concrete Compressive Strength")


#Concrete Compressive Strength vs other variables

p1 = Concrete_Data %>% drop_na() %>% ggplot() + aes(x = Cement, y = Concrete_Compressive_Strength) +
  geom_point(size = 3, alpha = 0.6) + scale_y_log10()

p1 + geom_smooth(method = "lm", se = FALSE) #  linear

p2 = Concrete_Data %>% drop_na() %>% ggplot() + aes(x = Age, y = Concrete_Compressive_Strength) +
  geom_point(size = 3, alpha = 0.6)

p2 + geom_smooth(method = "lm", se = FALSE) #

p3 = Concrete_Data %>% drop_na() %>% ggplot() + aes(x = Fly_Ash, y = Concrete_Compressive_Strength) +
  geom_point(size = 3, alpha = 0.6)

p3 + geom_smooth(method = "lm", se = FALSE) #not 


p4 = Concrete_Data %>% drop_na() %>% ggplot() + aes(x = Superplasticizer, y = Concrete_Compressive_Strength) +
  geom_point(size = 3, alpha = 0.6) #not linear

p4 + geom_smooth(method = "lm", se = FALSE)


p5 = Concrete_Data %>% drop_na() %>% ggplot() + aes(x = Fine_Aggregate, y = Concrete_Compressive_Strength) +
  geom_point(size = 3, alpha = 0.6) + scale_y_log10()

p5 + geom_smooth(method = "lm", se = FALSE)

p6 = Concrete_Data %>% drop_na() %>% ggplot() + aes(x = Blast_Furnace_Slag, y = Concrete_Compressive_Strength) +
  geom_point(size = 3, alpha = 0.6) #not linear

p6 + geom_smooth(method = "lm", se = FALSE)

p7 = Concrete_Data %>% drop_na() %>% ggplot() + aes(x = Water, y = Concrete_Compressive_Strength) +
  geom_point(size = 3, alpha = 0.6) #not linear

p7 + geom_smooth(method = "lm", se = FALSE)

p8 = Concrete_Data %>% drop_na() %>% ggplot() + aes(x = Coarse_Aggregate, y = Concrete_Compressive_Strength) +
  geom_point(size = 3, alpha = 0.6) + scale_y_log10()

p8 + geom_smooth(method = "lm", se = FALSE)

#As the abstract suggests, through purely visualsing vi to be  a non-linear relationship
#between concrete compression strength.





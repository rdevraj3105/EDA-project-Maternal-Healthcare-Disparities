library(tidyverse)
maternal <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/maternal.csv")

# maternal age is associated with prenatal visits. Higher maternal age with result in more prenatal visits.  

age_visits_relation <- maternal %>% select(AverageMotherAge, AverageNumberPrenatalVisits)

age_visits_relation %>%  
  ggplot(aes(x=AverageMotherAge, y=AverageNumberPrenatalVisits)) +
  geom_point(color="lightpink",size=3,alpha=0.5) +
  theme_minimal() 

age_visits_relation %>% 
  ggplot(aes(x = AverageMotherAge, y = AverageNumberPrenatalVisits)) +
  geom_point(color = "lightpink", size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", linewidth = 2, color="hotpink")

cor(age_visits_relation$AverageMotherAge, 
    age_visits_relation$AverageNumberPrenatalVisits, 
    use = "complete.obs")
# correlation coefficient suggests a weak yet positive relationship 


library(ggplot2)
library(dplyr)


# Bar plot for pre-pregnancy diabetes
maternal %>% 
  ggplot(aes(x = TobaccoUse, fill = PrePregnancyDiabetes)) +
  geom_bar() +
  labs(x = "Tobacco Use",
       y = "Proportion",
       fill = "Pre-pregnancy Diabetes")

# Bar plot for pre-pregnancy hypertension
maternal %>% 
ggplot(aes(x = TobaccoUse, fill = PrePregnancyHypertension)) +
  geom_bar() +
  labs(x = "Tobacco Use",
       y = "Proportion",
       fill="Pre-Pregnancy Hypertension")













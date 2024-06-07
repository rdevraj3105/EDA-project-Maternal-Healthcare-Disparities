

# Hypothesis 3: Is there an effect between tobacco use and the pre-pregnancy diabetes on a newborns birth rate?

#from sam :)
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
       fill = "Pre-pregnancy Hypertension")
# 3. Hypotheses/Questions
# Hypothesis 1: Does maternal age correlate with birth weight?
# Hypothesis 2: Are there differences in the average number of prenatal visits among different states?
# Hypothesis 3: Is there a relationship between tobacco use and pre-pregnancy conditions like diabetes and hypertension?

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
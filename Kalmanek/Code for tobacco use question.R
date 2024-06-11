
library(tidyverse)

maternal <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/maternal.csv")


subset_data <- maternal %>%
  select(TobaccoUse, PrePregnancyDiabetes, AverageBirthWeight)


subset_data <- subset_data %>%
  mutate(
    TobaccoUse = factor(TobaccoUse),
    PrePregnancyDiabetes = factor(PrePregnancyDiabetes)
  )

ggplot(subset_data, aes(x = TobaccoUse, y = AverageBirthWeight, fill = PrePregnancyDiabetes)) +
  geom_boxplot() +
  labs(
    title = "Effect of Tobacco Use and Pre-Pregnancy Diabetes on Newborns' Birth Weight",
    x = "Tobacco Use",
    y = "Average Birth Weight",
    fill = "Pre-Pregnancy Diabetes"
  ) +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "Hotpink", "Unknown" = "lightgrey")) +
  theme_light()



subset_data <- subset_data %>%
  mutate(
    TobaccoUse = factor(TobaccoUse),
    PrePregnancyDiabetes = factor(PrePregnancyDiabetes)
  )

cluster_data <-
  select(TobaccoUse, PrePregnancyDiabetes, AverageBirthWeight)
  mutate_if(is.factor, as.numeric)
  scale() %>%
  kmeans(centers = 3)






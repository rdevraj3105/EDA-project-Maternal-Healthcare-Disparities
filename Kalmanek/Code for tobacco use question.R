
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
  )

tobacco_colors <- c("No" = "Lightblue", "Yes" = "Orange", "Unknown" = "Hotpink")
scale_fill_manual(values = tobacco_colors)





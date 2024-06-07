library(tidyverse)
maternal <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/maternal.csv")
maternal
View(maternal)

#1: Maternal tobacco use is associated with lower average birthweights
#2: Does maternal age correlate with birthweight?
#3: How do number of births in that state with a defined combination of the previous four conditions (PriorBirthsNowDeceased, TobaccoUse, PrePregnancyDiabetes, PrePregnancyHypertension) vary between each state

maternal |> 
  filter(TobaccoUse %in% c("Yes"), PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("Yes")) |> 
  select(c("State","Births")) |> 
  mutate(State=factor(State),
         State = fct_reorder(State, Births)) |> 
  ggplot() +
  geom_segment( aes(x=State, xend=State, y=0, yend=Births)) +
  coord_flip()+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x =  
  ) +
  geom_point(aes(x=State,y=Births),size=3, color="red", fill=alpha("orange", 0.3), alpha=0.6, shape=21, stroke=2)+
  labs(
     x = "State",
     y = "Number of births in each state with TobaccoUse, PrePregnancyDiabetes, and PrePregnancyHypertension" 
  )

tobacco_no = maternal |> 
  filter(TobaccoUse %in% c("No"), PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("Yes"))

tobacco_yes = maternal |> 
  filter(TobaccoUse %in% c("Yes"), 
         PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("Yes"))

tobacco_equals = rbind(tobacco_no, tobacco_yes)
tobacco_equals |> 
  select(c("State","Births","TobaccoUse")) |> 
  mutate(State=factor(State),
         State = fct_reorder(State, Births)) |> 
  ggplot(aes(group=State)) +
  geom_segment( aes(x=State, xend=State, y=0, yend=Births)) +
  geom_point(aes(x=State,y=Births, fill = TobaccoUse, color = TobaccoUse),size=3, 
             alpha=0.6, shape=21 )+
  coord_flip()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    x = "State",
    y = "Number of births in each state with TobaccoUse, PrePregnancyDiabetes, and PrePregnancyHypertension" 
  )


#marginal difference with the TobaccoUse
#first filter then add another filter 
#rbind f












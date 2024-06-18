library(tidyverse)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
maternal <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/maternal.csv")


View(maternal)

#How does the number of births in each state with Tobacco Use, Pre Pregnancy Diabetes, and Pre Pregnancy Hypertension differ with the marginal difference of not using tobacco.
theme_set(theme_bw())

library(ggthemes)
library(gganimate)
#1: Maternal tobacco use is associated with lower average birthweights
#2: Does maternal age correlate with birthweight?
#3: How do number of births in that state with a defined combination of the previous four conditions (PriorBirthsNowDeceased, TobaccoUse, PrePregnancyDiabetes, PrePregnancyHypertension) vary between each state

# maternal |> 
#   filter(TobaccoUse %in% c("Yes"), PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("Yes")) |> 
#   select(c("State","Births")) |> 
#   mutate(State=factor(State),
#          State = fct_reorder(State, Births)) |> 
#   ggplot() +
#   geom_segment( aes(x=State, xend=State, y=0, yend=Births)) +
#   coord_flip()+
#   #theme_fivethirtyeight() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.text.x =  
#   ) +
#   geom_point(aes(x=State,y=Births),size=3, color="red", fill=alpha("orange", 0.3), alpha=0.6, shape=21, stroke=2)+
#   labs(
#      x = "State",
#      y = "Number of births in each state with TobaccoUse, PrePregnancyDiabetes, and PrePregnancyHypertension" 
#   )

tobacco_no = maternal |> 
  filter(TobaccoUse %in% c("No"), PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("Yes"))
tobacco_yes = maternal |> 
  filter(TobaccoUse %in% c("Yes"), 
         PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("Yes"))
tobacco_equals = rbind(tobacco_no, tobacco_yes)
tobacco_equals <- tobacco_equals |> 
  select(c("State","Births","TobaccoUse")) |> 
  mutate(State=factor(State),
         State = fct_reorder(State, Births)) |> 
  ggplot(aes(group=State)) +
  geom_segment( aes(x=State, xend=State, y=0, yend=Births)) +
  geom_point(aes(x=State,y=Births, fill = TobaccoUse),size=5, 
             alpha=0.67, shape=21)+
  scale_y_continuous(breaks=seq(0,600,50))+
  coord_flip()+
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 13, hjust = 0.5, vjust = 0.5, face = "bold", 
                              margin = margin(b = 0.2, unit = "cm")),
    axis.text.x = element_text(face="bold", color = "black"),
    axis.text.y = element_text(face="bold", color="black"),
    axis.title.y = element_text(face = "bold", color = "darkred"),
    axis.title.x = element_text(face = "bold", color = "darkred",vjust = -0.85)
  ) +
  scale_fill_manual(values = c("No"="brown","Yes"="black"))+
  labs(
    #title = "Number of Births in each State: Tobacco Use vs. Non-Use, Considering Diabetes and Hypertension",
    x = "State",
    y = "Births",
    fill = "Tobacco Use"
  )
tobacco_equals
 
#first filter then add another filter 
#rbind f




#Diabetes
diabetes_no = maternal |> 
  filter(TobaccoUse %in% c("Yes"), PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes %in% c("No"), PrePregnancyHypertension %in% c("Yes"))
diabetes_yes = maternal |> 
  filter(TobaccoUse %in% c("Yes"), 
         PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("Yes"))

diabetes_equals = rbind(diabetes_no,diabetes_yes)
diabetes <- diabetes_equals |> 
  select(c("State","Births","PrePregnancyDiabetes")) |> 
  mutate(State=factor(State),
         State = fct_reorder(State, Births)) |> 
  ggplot(aes(group=State)) +
  geom_segment( aes(x=State, xend=State, y=0, yend=Births)) +
  geom_point(aes(x=State,y=Births, fill = PrePregnancyDiabetes),size=5, 
             alpha=0.68, shape=21)+
  scale_y_continuous(breaks=seq(0,600,50))+
  coord_flip()+
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 13, hjust = 0.5, vjust = 0.5, face = "bold", 
                             margin = margin(b = 0.2, unit = "cm")),
    axis.text.x = element_text(face="bold", color = "black"),
    axis.text.y = element_text(face="bold", color="black"),
    axis.title.y = element_text(face = "bold", color = "darkred"),
    axis.title.x = element_text(face = "bold", color = "darkred",vjust = -0.85)
  ) +
  scale_fill_manual(values = c("No"="brown","Yes"="black"))+
  labs(
    #title = "Number of Births in each State: With or Without Diabetes, Considering Tobacco Use and Hypertension",
    x = "State",
    y = "Births",
    fill = "Diabetes"
  )
diabetes


#Hypertension

hypertension_no = maternal |> 
  filter(TobaccoUse %in% c("Yes"), PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("No"))
hypertension_yes = maternal |> 
  filter(TobaccoUse %in% c("Yes"), 
         PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes %in% c("Yes"), PrePregnancyHypertension %in% c("Yes"))

hypertension_equals = rbind(hypertension_no,hypertension_yes)
hypertension <- hypertension_equals |> 
  select(c("State","Births","PrePregnancyHypertension")) |> 
  mutate(State=factor(State),
         State = fct_reorder(State, Births)) |> 
  ggplot(aes(group=State)) +
  geom_segment( aes(x=State, xend=State, y=0, yend=Births)) +
  geom_point(aes(x=State,y=Births, fill = PrePregnancyHypertension),size=5, 
             alpha=0.68, shape=21)+
  scale_y_continuous(breaks=seq(0,600,50))+
  coord_flip()+
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 13, hjust = 0.5, vjust = 0.5, face = "bold", 
                              margin = margin(b = 0.2, unit = "cm")),
    axis.text.x = element_text(face="bold", color = "black"),
    axis.text.y = element_text(face="bold", color="black"),
    axis.title.y = element_text(face = "bold", color = "darkred"),
    axis.title.x = element_text(face = "bold", color = "darkred",vjust = -0.85)
  ) +
  scale_fill_manual(values = c("No"="brown","Yes"="black"))+
  labs(
    #title = "Number of Births in each State: With or Without Hypertension, Considering Tobacco Use and Pre-Pregnancy Diabetes",
    x = "State",
    y = "Births",
    fill = "Hypertension"
  )
hypertension


# 
# View(maternal)
# maternal |> 
#   count(TobaccoUse)
# 
# library(cowplot)
# plot_grid(tobacco_equals,hypertension,diabetes) 
# 
# install.packages("gtsummary")
# library(gtsummary)
# maternal |>
#   select(TobaccoUse,State,Births) |>
#   #filter(State%in%c("California","Texas","New York","Missouri")) |>
#   group_by(TobaccoUse, State) |>
#   summarize(Births=sum(Births, na.rm=TRUE)) |>
#   ungroup() |> mutate(TobaccoUse = factor(TobaccoUse), State =factor(State)) |>
#   tbl_summary(by = State)
# 
# View(maternal)
# View(Statematernal)
  



library(sf)
library(RColorBrewer)

install.packages("usmap")
library(usmap)
plot_usmap(regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())

maternal |> 
  filter(State %in% c("California", "New York", "Florida")) |>
  ggplot(aes(x=AverageBirthWeight,y=Births, fill = State))+
  geom_area()


maternal |> 
  filter(State %in% c("California", "New York", "Florida","Alaska","Illinois")) |> 
  filter(TobaccoUse == "No", 
         PriorBirthsNowDeceased == '0', 
         PrePregnancyDiabetes == "No", 
         PrePregnancyHypertension == "No") |> 
  ggplot(aes(x = State, y = Births)) +
  scale_y_continuous(breaks=seq(0,600000,50000))+
  geom_bar(stat = "identity") +
  labs(title = "Number of Births in select states with none of the conditions",
       x = "State",
       y = "Number of Births")





  
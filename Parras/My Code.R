library(tidyverse)
maternal <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/maternal.csv")

# maternal age is associated with prenatal visits. Higher maternal age with result in more prenatal visits.  

maternal %>%  
  ggplot(aes(x=AverageMotherAge, y=AverageNumberPrenatalVisits)) +
  geom_point(color="lightpink",size=3,alpha=0.5) +
  theme_light()+
  labs(title="Relation between Prenatal Visits and Age", x="Average Mother Age", y="Average Prenatal Visits")
# adding a linear regression line 
maternal %>% 
  ggplot(aes(x = AverageMotherAge, y = AverageNumberPrenatalVisits)) +
  geom_point(color = "lightpink", size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", linewidth = 2, color="hotpink") +
  theme_light()+ 
  labs(title="Relation between Prenatal Visits and Age", x="Average Mother Age", y="Average Prenatal Visits")
maternal %>%  
  ggplot(aes(x=AverageMotherAge, y=AverageNumberPrenatalVisits, color=TobaccoUse)) +
  geom_point(size=3,alpha=0.5) +
  theme_light()+
  labs(title="Relation between Prenatal Visits and Age with Tobacco Use", x="Average Mother Age", y="Average Prenatal Visits")


cor(maternal$AverageMotherAge, 
    maternal$AverageNumberPrenatalVisits, 
    use = "complete.obs")
# correlation coefficient suggests a weak yet positive relationship 

       

# The clusters are visually distinguishable, indicating that AverageMotherAge
# and AverageNumberPrenatalVisits play significant roles in cluster formation.

# Cluster 1: Likely represents younger mothers who might have more prenatal visits on average.
# Cluster 2: Possibly an older group of mothers with fewer prenatal visits.
# Cluster 3: An intermediate group or one with specific 
# characteristics not entirely clear from age and visit numbers alone.

# new code 
clean_data <- maternal %>%  
  select(AverageMotherAge, AverageNumberPrenatalVisits) %>%
  na.omit()

init_kmeans <- clean_data %>%  
  kmeans(centers = 3, nstart = 1, algorithm = "Lloyd")

clean_data <- clean_data %>%
  mutate(clusters = as.factor(init_kmeans$cluster)) 

clean_data %>%
  ggplot(aes(x = AverageMotherAge, y = AverageNumberPrenatalVisits, color = clusters)) +
  geom_point() +
  labs(title = "Clustering",
       x = "Average Mother Age",
       y = "Average Number of Prenatal Visits") +
  coord_fixed()


# box plots for clusters 
ggplot(clean_data, aes(x = clusters, y = AverageNumberPrenatalVisits, color = clusters)) +
  geom_boxplot() +
  labs(title= "Average Number of Prenatal Visits in the Clusters",x = "Cluster", y = "Average Prenantal Visits") +
  theme_light()

ggplot(clean_data, aes(x = clusters, y = AverageMotherAge, color = clusters)) +
  geom_boxplot() +
  labs(title= "Average Mother Age in the Clusters",x = "Cluster", y = "Average Mother Age") +
  theme_light()

#please work 


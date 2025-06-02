edutables<-pull_ACS_yrs("B06009_005",c(2013,2015,2017,2019,2021,2023),metro_trimmed,nmetro_trimmed)
metro_with_edu <- edutables[[1]]
nmetro_with_edu <- edutables[[2]]
sum(metro_with_edu$estimate_2013,na.rm=TRUE)/sum(metro_with_edu$population_2013,na.rm=TRUE)
sum(nmetro_with_edu$estimate_2013,na.rm=TRUE)/sum(nmetro_with_edu$population_2013,na.rm=TRUE)

edurates<- summarize_rate(metro_with_edu,nmetro_with_edu,c(2013,2015,2017,2019,2021,2023))
head(metro)
library(tidyverse)

# Convert to long format
edu_long <- edurates %>%
  pivot_longer(cols = c(Metro, Nonmetro), names_to = "Category", values_to = "EduRate")

# Plot
ggplot(edu_long, aes(x = Year, y = EduRate, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.18)) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 2)) +  
  scale_color_manual(
    values = c(
      "Metro" = "darkgreen",
      "Nonmetro" = "lightblue"
    )
  ) +
  labs(
    title = "College Education Over Time by County Type",
    x = "Year",
    y = "Percentage with Bachelor's Degree",
    color = "Region Type"
  ) +
  theme_minimal()

x=quantile(medum$population_2013,.01,na.rm = TRUE)

medunm75 <- medunm %>%
  filter(rate_2013 <= quantile(rate_2013, 0.25, na.rm = TRUE),population_2013>x) %>%
  mutate(Quartile = "Q4")

medunm25 <- medunm %>%
  filter(rate_2013 >= quantile(rate_2013, 0.75, na.rm = TRUE),population_2013>x) %>%
  mutate(Quartile = "Q1")

medunm50 <- medunm %>%
  filter(rate_2013 > quantile(rate_2013, 0.25, na.rm = TRUE), rate_2013 < quantile(rate_2013, 0.75, na.rm = TRUE),population_2013>x) %>%
  mutate(Quartile = "Q2-Q3")
combined <- bind_rows(medum25, medum50, medum75)

median(medum75$`Land Area (square miles), 2010`)

library(tidyverse)

rate_summary <- combined %>%
  pivot_longer(
    cols = matches("estimate_\\d{4}|population_\\d{4}"),
    names_to = c(".value", "Year"),
    names_pattern = "(estimate|population)_(\\d{4})"
  ) %>%
  group_by(Quartile, Year) %>%
  summarize(
    total_estimate = sum(estimate, na.rm = TRUE),
    total_population = sum(population, na.rm = TRUE),
    agg_rate = total_estimate / total_population,
    .groups = "drop"
  )
ggplot(rate_summary, aes(x = as.integer(Year), y = 1-agg_rate, color = Quartile)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 2))+
  labs(
    title = "Metro Male Higher Ed Enrollment Rate by 2013 County Quartile",
    subtitle = "Q1 (Bottom 25%), Q2-Q3 (Middle 50%), Q4 (Top 25%)",
    x = "Year",
    y = "Higher Ed Enrollment Rate",
    color = "Quartile Group"
  ) +
  scale_color_manual(
    values = c("Q1" = "skyblue", "Q2-Q3" = "gray60", "Q4" = "firebrick")
  ) +
  theme_minimal()

1-(9774724/15469750)
1-(8033594/14837891)

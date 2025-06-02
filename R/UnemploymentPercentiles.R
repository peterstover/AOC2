# Add Region labels
unmml_q <- summarize_unemployment_quantiles(unmml) %>%
  mutate(Region = "Nonmetro")

umtml_q <- summarize_unemployment_quantiles(umtml) %>%
  mutate(Region = "Metro")

# Combine both
all_quantiles <- bind_rows(unmml_q, umtml_q)

# Prepare long format for plotting
line_data <- all_quantiles %>%
  pivot_longer(cols = c(Q1, Median, Q3), names_to = "LineType", values_to = "Value") %>%
  mutate(
    Label = paste(Region, LineType)
  )

# Plot the six lines
ggplot(line_data, aes(x = Year, y = Value, color = Label, linetype = LineType)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    name = "Region & Statistic",
    values = c(
      "Metro Q1" = "steelblue",
      "Metro Median" = "blue4",
      "Metro Q3" = "steelblue",
      "Nonmetro Q1" = "darkred",
      "Nonmetro Median" = "firebrick",
      "Nonmetro Q3" = "darkred"
    )
  ) +
  scale_linetype_manual(
    name = "Percentile",
    values = c("Q1" = "dashed", "Median" = "solid", "Q3" = "dashed")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous() +
  labs(
    title = "Unemployment Rate Distribution (Male, Ages 16–64)",
    subtitle = "Metro vs. Nonmetro Tracts: 25th, Median, and 75th Percentiles",
    x = "Year",
    y = "Unemployment Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


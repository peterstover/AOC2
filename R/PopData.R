x<-50

metro_trimmed <- RUCA4_m_tracts %>%
  filter(
    `Land Area (square miles), 2010` > quantile(`Land Area (square miles), 2010`, 0.25) - x * IQR(`Land Area (square miles), 2010`),
    `Land Area (square miles), 2010` < quantile(`Land Area (square miles), 2010`, 0.75) + x * IQR(`Land Area (square miles), 2010`),
    `Tract Population, 2010`> quantile(`Tract Population, 2010`,.25)-x*IQR(`Tract Population, 2010`),
    `Tract Population, 2010`<quantile(`Tract Population, 2010`,.75)+x*IQR(`Tract Population, 2010`)
  )

nmetro_trimmed <- RUCA4_nm_tracts %>%
  filter(
    `Land Area (square miles), 2010` > quantile(`Land Area (square miles), 2010`, 0.25) - x * IQR(`Land Area (square miles), 2010`),
    `Land Area (square miles), 2010` < quantile(`Land Area (square miles), 2010`, 0.75) + x * IQR(`Land Area (square miles), 2010`),
    `Tract Population, 2010`> quantile(`Tract Population, 2010`,.25)-x*IQR(`Tract Population, 2010`),
    `Tract Population, 2010`<quantile(`Tract Population, 2010`,.75)+x*IQR(`Tract Population, 2010`)
)
quantile(metro_trimmed$`Land Area (square miles), 2010`,1)

df <- tribble(
  ~Category,     ~Metric,              ~Value,
  "Metro",       "Population (millions)",         sum(metro_trimmed$`Tract Population, 2010`)/1e6,
  "Nonmetro",    "Population (millions)",         sum(nmetro_trimmed$`Tract Population, 2010`)/1e6,
  "Metro",       "Land Area (million mi^2)",      sum(metro_trimmed$`Land Area (square miles), 2010`)/1e6,
  "Nonmetro",    "Land Area (million mi^2)",      sum(nmetro_trimmed$`Land Area (square miles), 2010`)/1e6,
  "Metro",       "Population Density (people/mi^2)",   sum(metro_trimmed$`Tract Population, 2010`)/sum(metro_trimmed$`Land Area (square miles), 2010`),
  "Nonmetro",    "Population Density (people/mi^2)",  sum(nmetro_trimmed$`Tract Population, 2010`)/sum(nmetro_trimmed$`Land Area (square miles), 2010`)
)

# Plot
ggplot(df, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Metric, scales = "free_y") +
  scale_fill_manual(values = c("Metro" = "darkgreen", "Nonmetro" = "lightblue")) +
  labs(title = "Metro vs Nonmetro Population Comparison (2010)",
       x = "County Type", y = NULL) +
  theme_classic() +
  theme(legend.position = "none")

library(dplyr)
library(tidyr)
library(ggplot2)

# Function to calculate jobs per capita
get_percap <- function(df, label) {
  df %>%
    summarize(
      `2013` = sum(estimate_2013_total, na.rm = TRUE) / sum(population_2013.x, na.rm = TRUE),
      `2018` = sum(estimate_2018_total, na.rm = TRUE) / sum(population_2018.x, na.rm = TRUE),
      `2023` = sum(estimate_2023_total, na.rm = TRUE) / sum(population_2023.x, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = "JobsPerCapita") %>%
    mutate(
      Year = as.integer(Year),
      Category = label
    )
}

# Split into percentile zones
mcmpercentiles <- filter_by_percentiles(mcmf_combined, "estimate_2013")
mcmq25 <- mcmpercentiles$below_25
mcmq50 <- mcmpercentiles$middle_50
mcmq75 <- mcmpercentiles$above_75

nmcmpercentiles <- filter_by_percentiles(nmcmf_combined, "estimate_2013")
nmcmq25 <- nmcmpercentiles$below_25
nmcmq50 <- nmcmpercentiles$middle_50
nmcmq75 <- nmcmpercentiles$above_75

# Get per capita job counts per group
percap_metro <- bind_rows(
  get_percap(mcmq25, "Bottom Quartile"),
  get_percap(mcmq50, "Middle 50%"),
  get_percap(mcmq75, "Top Quartile")
)

percap_nonmetro <- bind_rows(
  get_percap(nmcmq25, "Bottom Quartile"),
  get_percap(nmcmq50, "Middle 50%"),
  get_percap(nmcmq75, "Top Quartile")
)

# Optional: remove infinite or NA values
percap_metro <- percap_metro %>% filter(is.finite(JobsPerCapita))
percap_nonmetro <- percap_nonmetro %>% filter(is.finite(JobsPerCapita))

# Plot Metro
ggplot(percap_metro, aes(x = Year, y = 1000 * JobsPerCapita, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Metro Computer and Math Jobs Per Thousand People",
    x = "Year",
    y = "Jobs Per Thousand",
    color = "2013 Percentile Group"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_minimal()

# Plot Nonmetro
ggplot(percap_nonmetro, aes(x = Year, y = 1000 * JobsPerCapita, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Nonmetro Computer and Math Jobs Per Thousand People",
    x = "Year",
    y = "Jobs Per Thousand",
    color = "Percentile Group"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_minimal()

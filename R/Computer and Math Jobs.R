cmtable<-pull_ACS_yrs_fast(variable_code ="C24010_008",years = c(2013,2018,2023) )
mcm<-cmtable[[1]]
nmcm<-cmtable[[2]]
mcm<-mcm%>%drop_na(estimate_2023)
nmcm<-nmcm%>%drop_na(estimate_2023)

sum(mcm$estimate_2018,na.rm = T)+sum(fmcm$estimate_2018,na.rm=T)+sum(nmcm$estimate_2018,na.rm = T)+sum(fnmcm$estimate_2018,na.rm=T)
sum(mcm$estimate_2023,na.rm=T)+sum(fmcm$estimate_2023,na.rm=T)+sum(nmcm$estimate_2023,na.rm=T)+sum(fnmcm$estimate_2023,na.rm=T)



library(tidyverse)

mcmpercentiles<-filter_by_percentiles(mcmf_combined,"estimate_2013_total")
mcmq25<-mcmpercentiles$below_25
mcmq50<-mcmpercentiles$middle_50
mcmq75<-mcmpercentiles$above_75
nmcmpercentiles <- filter_by_percentiles(nmcmf_combined, "estimate_2013_total")
nmcmq25 <- nmcmpercentiles$below_25
nmcmq50 <- nmcmpercentiles$middle_50
nmcmq75 <- nmcmpercentiles$above_75

get_index <- function(df, label) {
  df %>%
    summarize(
      `2013` = sum(estimate_2013_total, na.rm = TRUE),
      `2018` = sum(estimate_2018_total, na.rm = TRUE),
      `2023` = sum(estimate_2023_total, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Total") %>%
    mutate(
      Year = as.integer(Year),
      Index = Total / Total[Year == 2013],
      Category = label
    )
}
index_metro <- bind_rows(
  get_index(mcmq25, "Metro <25th"),
  get_index(mcmq50, "Metro 25th–75th"),
  get_index(mcmq75, "Metro >75th")
)

index_nonmetro <- bind_rows(
  get_index(nmcmq25, "Nonmetro <25th"),
  get_index(nmcmq50, "Nonmetro 25th–75th"),
  get_index(nmcmq75, "Nonmetro >75th")
)

# Step 6: Plot Metro
ggplot(index_metro, aes(x = Year, y = Index, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Indexed Estimate of Metro Male Computer and Math Jobs by Percentile",
    x = "Year",
    y = "Index (Base = 2013 = 1)",
    color = "Percentile Group"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_minimal()

# Step 7: Plot Nonmetro
ggplot(index_nonmetro, aes(x = Year, y = Index, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Indexed Estimate of Nonmetro Male Computer and Math Jobs by Percentile",
    x = "Year",
    y = "Index (Base = 2013 = 1)",
    color = "Percentile Group"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_minimal()





# Step 1: Summarize totals by year for metro
index_metro <- mcm %>%
  summarize(
    `2013` = sum(estimate_2013, na.rm = TRUE),
    `2018` = sum(estimate_2018, na.rm = TRUE),
    `2023` = sum(estimate_2023, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Total") %>%
  mutate(
    Year = as.integer(Year),
    Index = Total / Total[Year == 2013],
    Category = "Metro"
  )

# Step 2: Repeat for nonmetro
index_nonmetro <- nmcm %>%
  summarize(
    `2013` = sum(estimate_2013, na.rm = TRUE),
    `2018` = sum(estimate_2018, na.rm = TRUE),
    `2023` = sum(estimate_2023, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Total") %>%
  mutate(
    Year = as.integer(Year),
    Index = Total / Total[Year == 2013],
    Category = "Nonmetro"
  )

# Step 3: Combine and plot
combined_index <- bind_rows(index_metro, index_nonmetro)

ggplot(combined_index, aes(x = Year, y = Index, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Index of Male Computer and Math Jobs by Year",
    x = "Year",
    y = "Index (Base = 2013 = 1)",
    color = "Category"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_minimal()


library(tidyverse)

# Step 1: Summarize per capita for metro
percap_metro <- mcm %>%
  summarize(
    `2013` = sum(estimate_2013, na.rm = TRUE) / sum(population_2013, na.rm = TRUE),
    `2018` = sum(estimate_2018, na.rm = TRUE) / sum(population_2018, na.rm = TRUE),
    `2023` = sum(estimate_2023, na.rm = TRUE) / sum(population_2023, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "JobsPerCapita") %>%
  mutate(Year = as.integer(Year), Category = "Metro")

# Step 2: Repeat for nonmetro
percap_nonmetro <- nmcm %>%
  summarize(
    `2013` = sum(estimate_2013, na.rm = TRUE) / sum(population_2013, na.rm = TRUE),
    `2018` = sum(estimate_2018, na.rm = TRUE) / sum(population_2018, na.rm = TRUE),
    `2023` = sum(estimate_2023, na.rm = TRUE) / sum(population_2023, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "JobsPerCapita") %>%
  mutate(Year = as.integer(Year), Category = "Nonmetro")

# Step 3: Combine and plot
percap_combined <- bind_rows(percap_metro, percap_nonmetro)

ggplot(percap_combined, aes(x = Year, y = 1000*JobsPerCapita, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Male Computer and Math Jobs Per Thousand People",
    x = "Year",
    y = "Jobs Per Thousand People",
    color = "Category"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_minimal()


###WOMEN

fcmtable<-pull_ACS_yrs_fast(variable_code ="C24010_044",years = c(2013,2018,2023) )

# Extract female metro/nonmetro tables
fmcm <- fcmtable[[1]] %>% drop_na(estimate_2023)
fnmcm <- fcmtable[[2]] %>% drop_na(estimate_2023)

# ==== INDEX PLOT ====

# Metro
index_f_metro <- fmcm %>%
  summarize(
    `2013` = sum(estimate_2013, na.rm = TRUE),
    `2018` = sum(estimate_2018, na.rm = TRUE),
    `2023` = sum(estimate_2023, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Total") %>%
  mutate(
    Year = as.integer(Year),
    Index = Total / Total[Year == 2013],
    Category = "Metro"
  )

# Nonmetro
index_f_nonmetro <- fnmcm %>%
  summarize(
    `2013` = sum(estimate_2013, na.rm = TRUE),
    `2018` = sum(estimate_2018, na.rm = TRUE),
    `2023` = sum(estimate_2023, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Total") %>%
  mutate(
    Year = as.integer(Year),
    Index = Total / Total[Year == 2013],
    Category = "Nonmetro"
  )

# Combine and plot
combined_f_index <- bind_rows(index_f_metro, index_f_nonmetro)

ggplot(combined_f_index, aes(x = Year, y = Index, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Index of Female Computer and Math Jobs by Year",
    x = "Year",
    y = "Index (Base = 2013 = 1)",
    color = "Category"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_minimal()


# ==== PER CAPITA PLOT ====

# Metro
percap_f_metro <- fmcm %>%
  summarize(
    `2013` = sum(estimate_2013, na.rm = TRUE) / sum(population_2013, na.rm = TRUE),
    `2018` = sum(estimate_2018, na.rm = TRUE) / sum(population_2018, na.rm = TRUE),
    `2023` = sum(estimate_2023, na.rm = TRUE) / sum(population_2023, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "JobsPerCapita") %>%
  mutate(Year = as.integer(Year), Category = "Metro")

# Nonmetro
percap_f_nonmetro <- fnmcm %>%
  summarize(
    `2013` = sum(estimate_2013, na.rm = TRUE) / sum(population_2013, na.rm = TRUE),
    `2018` = sum(estimate_2018, na.rm = TRUE) / sum(population_2018, na.rm = TRUE),
    `2023` = sum(estimate_2023, na.rm = TRUE) / sum(population_2023, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Year", values_to = "JobsPerCapita") %>%
  mutate(Year = as.integer(Year), Category = "Nonmetro")

# Combine and plot
percap_f_combined <- bind_rows(percap_f_metro, percap_f_nonmetro)

ggplot(percap_f_combined, aes(x = Year, y = 1000 * JobsPerCapita, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Female Computer and Math Jobs Per Thousand People",
    x = "Year",
    y = "Jobs Per Thousand People",
    color = "Category"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_minimal()

###SUM
# ---- Summed Columns for Metro ----
mcm <- mcm %>% drop_na(estimate_2023)
fmcm <- fcmtable[[1]] %>% drop_na(estimate_2023)

mcmf_combined <- full_join(mcm, fmcm, by = "State-County-Tract FIPS Code") %>%
  mutate(
    estimate_2013_total = estimate_2013.x + estimate_2013.y,
    estimate_2018_total = estimate_2018.x + estimate_2018.y,
    estimate_2023_total = estimate_2023.x + estimate_2023.y,
  )

# ---- Summed Columns for Nonmetro ----
nmcm <- nmcm %>% drop_na(estimate_2023)
fnmcm <- fcmtable[[2]] %>% drop_na(estimate_2023)

nmcmf_combined <- full_join(nmcm, fnmcm, by = "State-County-Tract FIPS Code") %>%
  mutate(
    estimate_2013_total = estimate_2013.x + estimate_2013.y,
    estimate_2018_total = estimate_2018.x + estimate_2018.y,
    estimate_2023_total = estimate_2023.x + estimate_2023.y,
  )

# Summarize totals and compute index
index_metro <- mcmf_combined %>%
  summarize(
    `2013` = sum(estimate_2013_total, na.rm = TRUE),
    `2018` = sum(estimate_2018_total, na.rm = TRUE),
    `2023` = sum(estimate_2023_total, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Year", values_to = "Total") %>%
  mutate(
    Year = as.integer(Year),
    Index = Total / Total[Year == 2013],
    Category = "Metro"
  )

index_nonmetro <- nmcmf_combined %>%
  summarize(
    `2013` = sum(estimate_2013_total, na.rm = TRUE),
    `2018` = sum(estimate_2018_total, na.rm = TRUE),
    `2023` = sum(estimate_2023_total, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Year", values_to = "Total") %>%
  mutate(
    Year = as.integer(Year),
    Index = Total / Total[Year == 2013],
    Category = "Nonmetro"
  )

combined_index <- bind_rows(index_metro, index_nonmetro)

ggplot(combined_index, aes(x = Year, y = Index, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Index of Total Computer and Math Jobs by Year",
    x = "Year",
    y = "Index (Base = 2013 = 1)",
    color = "Category"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_minimal()

percap_metro <- mcmf_combined %>%
  summarize(
    `2013` = sum(estimate_2013_total, na.rm = TRUE) / sum(population_2013.x, na.rm = TRUE),
    `2018` = sum(estimate_2018_total, na.rm = TRUE) / sum(population_2018.x, na.rm = TRUE),
    `2023` = sum(estimate_2023_total, na.rm = TRUE) / sum(population_2023.x, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Year", values_to = "JobsPerCapita") %>%
  mutate(Year = as.integer(Year), Category = "Metro")

percap_nonmetro <- nmcmf_combined %>%
  summarize(
    `2013` = sum(estimate_2013_total, na.rm = TRUE) / sum(population_2013.x, na.rm = TRUE),
    `2018` = sum(estimate_2018_total, na.rm = TRUE) / sum(population_2018.x, na.rm = TRUE),
    `2023` = sum(estimate_2023_total, na.rm = TRUE) / sum(population_2023.x, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Year", values_to = "JobsPerCapita") %>%
  mutate(Year = as.integer(Year), Category = "Nonmetro")

percap_combined <- bind_rows(percap_metro, percap_nonmetro)

ggplot(percap_combined, aes(x = Year, y = 1000 * JobsPerCapita, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Computer and Math Jobs Per Thousand People (Total)",
    x = "Year",
    y = "Jobs Per Thousand People",
    color = "Category"
  ) +
  scale_x_continuous(breaks = c(2013, 2018, 2023)) +
  scale_y_continuous(labels = scales::number_format(accuracy = .1)) +
  theme_minimal()

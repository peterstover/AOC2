MtTotalUnemployment<-full_join(mtfl,mtml, by = "State-County-Tract FIPS Code")
MtTotalUnemployment<- MtTotalUnemployment%>%
  select(-starts_with("fciv_labor_"),-starts_with("femp_"),-starts_with("total_pop"),-starts_with("ftotal_pop"))%>%
  select(starts_with("labor_force"),starts_with("employed"),`State-County-Tract FIPS Code`)

years <- 2010:2023

for (year in years) {
  labor_col_male <- paste0("labor_force_", year, ".x")
  labor_col_female <- paste0("labor_force_", year, ".y")
  employed_col_male <- paste0("employed_", year, ".x")
  employed_col_female <- paste0("employed_", year, ".y")
  
  # Create new columns with aggregated totals
MtTotalUnemployment[[paste0("labor_force_", year, "_total")]] <-
  MtTotalUnemployment[[labor_col_male]] + MtTotalUnemployment[[labor_col_female]]
  
MtTotalUnemployment[[paste0("employed_", year, "_total")]] <-
  MtTotalUnemployment[[employed_col_male]] + MtTotalUnemployment[[employed_col_female]]

}

for (year in years){
  MtTotalUnemployment[[paste0("unemp_rate_", year)]] <-
    1 - MtTotalUnemployment[[paste0("employed_", year, "_total")]] /
    MtTotalUnemployment[[paste0("labor_force_", year, "_total")]]
}
all_cols <- colnames(MtTotalUnemployment)
keep_cols <- grep("(_total|^State-County|^unemp_rate)",all_cols,value = TRUE)
MtTotalUnemployment<-MtTotalUnemployment[,keep_cols]


NmTotalUnemployment<-full_join(nmfl,nmml, by = "State-County-Tract FIPS Code")
NmTotalUnemployment<- NmTotalUnemployment%>%
  select(-starts_with("fciv_labor_"),-starts_with("femp_"),-starts_with("total_pop"),-starts_with("ftotal_pop"))%>%
  select(starts_with("labor_force"),starts_with("employed"),`State-County-Tract FIPS Code`)

years <- 2010:2023

for (year in years) {
  nlabor_col_male <- paste0("labor_force_", year, ".x")
  nlabor_col_female <- paste0("labor_force_", year, ".y")
  nemployed_col_male <- paste0("employed_", year, ".x")
  nemployed_col_female <- paste0("employed_", year, ".y")
  
  # Create new columns with aggregated totals
  NmTotalUnemployment[[paste0("labor_force_", year, "_total")]] <-
    NmTotalUnemployment[[nlabor_col_male]] + NmTotalUnemployment[[nlabor_col_female]]
  
  NmTotalUnemployment[[paste0("employed_", year, "_total")]] <-
    NmTotalUnemployment[[nemployed_col_male]] + NmTotalUnemployment[[nemployed_col_female]]
  
}

for (year in years){
  NmTotalUnemployment[[paste0("unemp_rate_", year)]] <-
    1 - NmTotalUnemployment[[paste0("employed_", year, "_total")]] /
    NmTotalUnemployment[[paste0("labor_force_", year, "_total")]]
}
all_cols <- colnames(NmTotalUnemployment)
keep_cols <- grep("(_total|^State-County|^unemp_rate)",all_cols,value = TRUE)
NmTotalUnemployment<-NmTotalUnemployment[,keep_cols]


summarize_unemp_quantiles <- function(df, stat_prefix = "unemp_rate", probs = c(0.25,.5,0.75)) {
  all_cols <- names(df)
  years <- unique(stringr::str_extract(all_cols, "\\d{4}$"))
  years <- years[!is.na(years)]
  
  map_dfr(years, function(yr) {
    col_name <- paste0(stat_prefix, "_", yr)
    vals <- df[[col_name]]
    qs <- quantile(vals, probs = probs, na.rm = TRUE)
    tibble(
      Year = as.integer(yr),
      Q1 = qs[[1]],
      Median = qs[[2]],
      Q3 = qs[[3]]
    )
  })
}
# Add Region labels
MtUnempRate <- summarize_unemp_quantiles(MtTotalUnemployment) %>%
  mutate(Region = "Metro")
NmUnempRate <- summarize_unemp_quantiles(NmTotalUnemployment) %>%
  mutate(Region = "Nonmetro")


# Combine both
all_quantiles2 <- bind_rows(MtUnempRate,NmUnempRate)

# Prepare long format for plotting
line_data2 <- all_quantiles2 %>%
  pivot_longer(cols = c(Q1, Median, Q3), names_to = "LineType", values_to = "Value") %>%
  mutate(
    Label = paste(Region, LineType)
  )
line_data2 <- line_data2 %>%
  mutate(Region = ifelse(grepl("Metro", Label), "Metro", "Nonmetro"))

# Plot the six lines
ggplot(line_data2, aes(x = Year, y = Value, color = Region, linetype = LineType)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    name = "Region",
    values = c("Metro" = "steelblue", "Nonmetro" = "firebrick")
  ) +
  scale_linetype_manual(
    name = "Quartile",  # or remove it entirely from the legend
    values = c("Q1" = "dotted", "Median" = "solid", "Q3" = "dotted"),
    guide = "none"  # hides linetype legend
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Unemployment Rate Distribution (Ages 16–64)",
    subtitle = "Metro vs. Nonmetro Tracts: 25th, Median, and 75th Percentiles",
    x = "Year",
    y = "Unemployment Rate"
  ) +
  theme_minimal()

library(tidyverse)

# Helper to pivot and aggregate by summing labor force and employed
aggregate_unemp <- function(df, category_label) {
  years <- 2010:2023
  
  labor_long <- df %>%
    pivot_longer(cols = matches("^labor_force_\\d{4}_total$"),
                 names_to = "Year", names_pattern = "labor_force_(\\d{4})_total",
                 values_to = "LaborForce")
  
  employed_long <- df %>%
    pivot_longer(cols = matches("^employed_\\d{4}_total$"),
                 names_to = "Year", names_pattern = "employed_(\\d{4})_total",
                 values_to = "Employed")
  
  # Join and summarize
  full_join(labor_long, employed_long, by = c("Year", "State-County-Tract FIPS Code")) %>%
    group_by(Year) %>%
    summarize(
      TotalLabor = sum(LaborForce, na.rm = TRUE),
      TotalEmployed = sum(Employed, na.rm = TRUE),
      Category = category_label,
      .groups = "drop"
    ) %>%
    mutate(UnempRate = 1 - (TotalEmployed / TotalLabor),
           Year = as.integer(Year))
}

# Run for both metro and nonmetro
mt_unemp <- aggregate_unemp(MtTotalUnemployment, "Metro")
nm_unemp <- aggregate_unemp(NmTotalUnemployment, "Nonmetro")

# Combine for plotting
combined_unemp <- bind_rows(mt_unemp, nm_unemp)

# Plot
ggplot(combined_unemp, aes(x = Year, y = UnempRate, color = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Aggregate Unemployment Rate by Year (16-64)",
    x = "Year",
    y = "Unemployment Rate",
    color = "Category"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()


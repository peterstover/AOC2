housingTest<-pull_ACS_yrs_fast(variable_code ="B25001_001",years=c(2013,2018,2023))


housingm<-housingTest[[1]]
housingnm<-housingTest[[2]]
housingm<-housingm%>%
  mutate(popdex2013 = population_2013/population_2013,popdex2018 = population_2018/population_2013, popdex2023= population_2023/population_2018)%>%
  drop_na(popdex2023)
housingnm<-housingnm%>%
  mutate(popdex2013= population_2013/population_2013, popdex2018 = population_2018/population_2013, popdex2023= population_2023/population_2018)%>%
  drop_na(popdex2023)

hmpercentiles<-filter_by_percentiles(housingm,"population_2013")
hm25<-hmpercentiles$below_25
hm50<-hmpercentiles$middle_50
hm75<-hmpercentiles$above_75

hnmpercentiles <- filter_by_percentiles(housingnm, "population_2013")
hnm25 <- hnmpercentiles$below_25
hnm50 <- hnmpercentiles$middle_50
hnm75 <- hnmpercentiles$above_75


sum(housingnm$population_2023)/sum(housingnm$population_2013)
  library(tidyverse)
  
  # Metro total population and popdex
  popdex_metro <- housingmq25 %>%
    summarize(
      `2013` = sum(population_2013, na.rm = TRUE),
      `2018` = sum(population_2018, na.rm = TRUE),
      `2023` = sum(population_2023, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = "TotalPop") %>%
    mutate(
      Year = as.integer(Year),
      Popdex = TotalPop / TotalPop[Year == 2013],
      Category = "Metro"
    )
  
  # Nonmetro total population and popdex
  popdex_nonmetro <- housingnm %>%
    summarize(
      `2013` = sum(population_2013, na.rm = TRUE),
      `2018` = sum(population_2018, na.rm = TRUE),
      `2023` = sum(population_2023, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = "TotalPop") %>%
    mutate(
      Year = as.integer(Year),
      Popdex = TotalPop / TotalPop[Year == 2013],
      Category = "Nonmetro"
    )
  
  # Combine and plot
  popdex_combined <- bind_rows(popdex_metro, popdex_nonmetro)
  
  ggplot(popdex_combined, aes(x = Year, y = Popdex, color = Category)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Aggregate Population Index by Year",
      x = "Year",
      y = "Population Index (Base = 2013 = 1)",
      color = "Category"
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    scale_x_continuous(breaks = c(2013, 2018, 2023)) +
    theme_minimal()
               
  library(tidyverse)
  
  # Pivot to long format and tag category
  hm_long <- housingm %>%
    select(`State-County-Tract FIPS Code`, starts_with("popdex")) %>%
    pivot_longer(cols = starts_with("popdex"),
                 names_to = "Year", names_prefix = "popdex",
                 values_to = "Popdex") %>%
    mutate(Year = as.integer(Year), Category = "Metro")
  
  hnm_long <- housingnm %>%
    select(`State-County-Tract FIPS Code`, starts_with("popdex")) %>%
    pivot_longer(cols = starts_with("popdex"),
                 names_to = "Year", names_prefix = "popdex",
                 values_to = "Popdex") %>%
    mutate(Year = as.integer(Year), Category = "Nonmetro")
  
  # Combine and compute quantiles
  quantiles <- bind_rows(hm_long, hnm_long) %>%
    group_by(Year, Category) %>%
    summarize(
      q25 = quantile(Popdex, 0.25, na.rm = TRUE),
      q50 = quantile(Popdex, 0.50, na.rm = TRUE),
      q75 = quantile(Popdex, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Plot quantile bands and medians
  ggplot(quantiles, aes(x = Year, color = Category, fill = Category)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, color = NA) + 
    geom_line(aes(y = q50), size = 1.2) +
    geom_line(aes(y = q75), linetype= "dashed",size = 1.2)+
    geom_line(aes(y = q25),linetype= "dashed", size = 1.2)+
    geom_point(aes(y = q50), size = 2) +
    labs(
      title = "Tract-Level Median, 75th, and 25th, Percentile Population Indices by Year",
      x = "Year",
      y = "Population Index (Popdex)",
      color = "Category",
      fill = "Category"
    ) +
    scale_x_continuous(breaks = c(2013, 2018, 2023)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    theme_minimal()
  
  ###HOUSING
  
  
  library(tidyverse)
  
  # Step 1: Calculate housedex for each tract (metro + nonmetro)
  housingm <- housingm %>%
    mutate(
      housedex2013 = estimate_2013 / estimate_2013,
      housedex2018 = estimate_2018 / estimate_2013,
      housedex2023 = estimate_2023 / estimate_2018
    ) %>% drop_na()
  
  housingnm <- housingnm %>%
    mutate(
      housedex2013 = estimate_2013 / estimate_2013,
      housedex2018 = estimate_2018 / estimate_2013,
      housedex2023 = estimate_2023 / estimate_2018
    ) %>% drop_na()
  
  # Step 2: Pivot to long format and label
  hm_house_long <- housingm %>%
    select(`State-County-Tract FIPS Code`, starts_with("housedex")) %>%
    pivot_longer(cols = starts_with("housedex"),
                 names_to = "Year", names_prefix = "housedex",
                 values_to = "Housedex") %>%
    mutate(Year = as.integer(Year), Category = "Metro")
  
  hnm_house_long <- housingnm %>%
    select(`State-County-Tract FIPS Code`, starts_with("housedex")) %>%
    pivot_longer(cols = starts_with("housedex"),
                 names_to = "Year", names_prefix = "housedex",
                 values_to = "Housedex") %>%
    mutate(Year = as.integer(Year), Category = "Nonmetro")
  
  # Step 3: Compute quantiles
  housedex_quantiles <- bind_rows(hm_house_long, hnm_house_long) %>%
    group_by(Year, Category) %>%
    summarize(
      q05 = quantile(Housedex, 0.05, na.rm = TRUE),
      q25 = quantile(Housedex, 0.25, na.rm = TRUE),
      q50 = quantile(Housedex, 0.50, na.rm = TRUE),
      q75 = quantile(Housedex, 0.75, na.rm = TRUE),
      q95 = quantile(Housedex, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Step 4: Plot
  ggplot(housedex_quantiles, aes(x = Year, color = Category, fill = Category)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, color = NA) +
    geom_line(aes(y = q50), size = 1.2) +
    geom_line(aes(y = q75), linetype = "dashed", size = 1) +
    geom_line(aes(y = q25), linetype = "dashed", size = 1) +
    geom_point(aes(y = q50), size = 2) +
    labs(
      title = "Tract-Level Median, 25th, and 75th Percentile Housing Indices by Year",
      x = "Year",
      y = "Housing Index (Housedex)",
      color = "Category",
      fill = "Category"
    ) +
    scale_x_continuous(breaks = c(2013, 2018, 2023)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    theme_minimal()
  
  library(tidyverse)
  
  # Step 1: Sum total housing units by year for metro
  housedex_metro <- housingm %>%
    summarize(
      `2013` = sum(estimate_2013, na.rm = TRUE),
      `2018` = sum(estimate_2018, na.rm = TRUE),
      `2023` = sum(estimate_2023, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = "TotalHousing") %>%
    mutate(
      Year = as.integer(Year),
      Housedex = TotalHousing / TotalHousing[Year == 2013],
      Category = "Metro"
    )
  
  # Step 2: Repeat for nonmetro
  housedex_nonmetro <- housingnm %>%
    summarize(
      `2013` = sum(estimate_2013, na.rm = TRUE),
      `2018` = sum(estimate_2018, na.rm = TRUE),
      `2023` = sum(estimate_2023, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = "TotalHousing") %>%
    mutate(
      Year = as.integer(Year),
      Housedex = TotalHousing / TotalHousing[Year == 2013],
      Category = "Nonmetro"
    )
  
  # Step 3: Combine and plot
  housedex_total <- bind_rows(housedex_metro, housedex_nonmetro)
  
  ggplot(housedex_total, aes(x = Year, y = Housedex, color = Category)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Aggregate Housing Index by Year",
      x = "Year",
      y = "Housing Index (Base = 2013 = 1)",
      color = "Category"
    ) +
    scale_x_continuous(breaks = c(2013, 2018, 2023)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    theme_minimal()
  
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Function to calculate popdex for a given df and label
  get_popdex <- function(df, label) {
    df %>%
      summarize(
        `2013` = sum(population_2013, na.rm = TRUE),
        `2018` = sum(population_2018, na.rm = TRUE),
        `2023` = sum(population_2023, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Year", values_to = "TotalPop") %>%
      mutate(
        Year = as.integer(Year),
        Popdex = TotalPop / TotalPop[Year == 2013],
        Category = label
      )
  }
  
  # Calculate popdex for each group
  hm25_popdex <- get_popdex(hm25, "Bottom Quartile")
  hm50_popdex <- get_popdex(hm50, "Middle 50%")
  hm75_popdex <- get_popdex(hm75, "Top Quartile")
  
  hnm25_popdex <- get_popdex(hnm25, "Bottom Quartile")
  hnm50_popdex <- get_popdex(hnm50, "Middle 50%")
  hnm75_popdex <- get_popdex(hnm75, "Top Quartile")
  
  # Combine into two datasets
  popdex_metro <- bind_rows(hm25_popdex, hm50_popdex, hm75_popdex)
  popdex_nonmetro <- bind_rows(hnm25_popdex, hnm50_popdex, hnm75_popdex)
  
  # Plot: Metro
  ggplot(popdex_metro, aes(x = Year, y = Popdex, color = Category)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Metro Population Growth Index by 2013 Population",
      x = "Year",
      y = "Population Index (Base = 2013 = 1)",
      color = "Group"
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    scale_x_continuous(breaks = c(2013, 2018, 2023)) +
    theme_minimal()
  
  # Plot: Nonmetro
  ggplot(popdex_nonmetro, aes(x = Year, y = Popdex, color = Category)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Nonmetro Population Growth Index by Percentile Group",
      x = "Year",
      y = "Population Index (Base = 2013 = 1)",
      color = "Group"
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    scale_x_continuous(breaks = c(2013, 2018, 2023)) +
    theme_minimal()
  
  
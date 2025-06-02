pull_ACS_yrs_fast <- function(variable_code, population = "B01001_001", years, metro_trimmed = mt, nmetro_trimmed = nmt) {
  library(tidyverse)
  library(tidycensus)
  data("fips_codes")
  
  # Limit to U.S. states + DC
  states <- unique(fips_codes$state)[1:51]  # 50 states + DC
  
  for (yr in years) {
    message(paste("Pulling data for year", yr))
    
    # Pull both variables at once for each state
    acs_data <- map_dfr(states, function(st) {
      message(paste("Fetching data for", st))
      tryCatch({
        get_acs(
          geography = "tract",
          state = st,
          variables = c(main = variable_code, pop = population),
          year = yr,
          survey = "acs5",
          cache_table = TRUE
        ) %>%
          select(GEOID, variable, estimate)
      }, error = function(e) {
        warning(paste("Skipping", st, "due to error:", e$message))
        NULL
      })
    })
    
    # Pivot wide and compute rate
    merged_data <- acs_data %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      rename(
        !!paste0("estimate_", yr) := main,
        !!paste0("population_", yr) := pop
      ) %>%
      mutate(
        !!paste0("rate_", yr) := .data[[paste0("estimate_", yr)]] / .data[[paste0("population_", yr)]]
      )
    
    # Join with metro and nonmetro
    metro_trimmed <- left_join(metro_trimmed, merged_data, by = c("State-County-Tract FIPS Code" = "GEOID"))
    nmetro_trimmed <- left_join(nmetro_trimmed, merged_data, by = c("State-County-Tract FIPS Code" = "GEOID"))
  }
  
  return(list(metro = metro_trimmed, nonmetro = nmetro_trimmed))
}

pull_multiple_ACS_vars <- function(variable_codes, years, metro_trimmed = mt, nmetro_trimmed = nmt) {
  library(tidyverse)
  library(tidycensus)
  data("fips_codes")
  
  # Limit to U.S. states + DC
  states <- unique(fips_codes$state)[1:51]  # 50 states + DC
  
  for (yr in years) {
    message(paste("Pulling data for year", yr))
    
    # Pull all variables for each state and bind them
    acs_data <- map_dfr(states, function(st) {
      message(paste("Fetching data for", st))
      tryCatch({
        get_acs(
          geography = "tract",
          state = st,
          variables = variable_codes,
          year = yr,
          survey = "acs5",
          cache_table = TRUE
        ) %>%
          select(GEOID, variable, estimate)
      }, error = function(e) {
        warning(paste("Skipping", st, "due to error:", e$message))
        NULL
      })
    })
    
    # Pivot wider with each variable code getting its own estimate column
    merged_data <- acs_data %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      rename_with(.fn = ~ paste0(.x, "_", yr), .cols = all_of(variable_codes))
    
    # Join with metro/nonmetro
    metro_trimmed <- left_join(metro_trimmed, merged_data, by = c("State-County-Tract FIPS Code" = "GEOID"))
    nmetro_trimmed <- left_join(nmetro_trimmed, merged_data, by = c("State-County-Tract FIPS Code" = "GEOID"))
  }
  
  return(list(metro = metro_trimmed, nonmetro = nmetro_trimmed))
}

filter_by_percentiles <- function(df, column) {
  q25 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  q75 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  
  list(
    below_25 = df %>% filter(.data[[column]] <= q25),
    middle_50 = df %>% filter(.data[[column]] > q25 & .data[[column]] < q75),
    above_75 = df %>% filter(.data[[column]] >= q75)
  )
}


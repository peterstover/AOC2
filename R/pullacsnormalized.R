tpull_ACS_yrs_fast <- function(variable_code, population = "B01001_001", years,
                                         metro_trimmed = mt, nmetro_trimmed = nmt,
                                         crosswalk_path = "C:/Users/peter/Documents/ct20_ct10_xwalk.csv"
                                         ){
  library(tidyverse)
  library(tidycensus)
  data("fips_codes")
  
  # Load crosswalk
  crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE) %>%
    rename(
      GEOID_2020 = TRTID2020,
      GEOID_2010 = TRTID2010,
      weight = TR20_TO_TR10_WEIGHT
    ) %>%
    mutate(
      GEOID_2020 = as.character(GEOID_2020),
      GEOID_2010 = as.character(GEOID_2010)
    )
  
  # Limit to U.S. states + DC
  states <- unique(fips_codes$state)[1:51]
  
  for (yr in years) {
    message(paste("Pulling data for year", yr))
    
    # Pull ACS data
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
    
    # Pivot wide and prepare
    merged_data <- acs_data %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      rename(
        estimate = main,
        population = pop
      ) %>%
      mutate(GEOID = as.character(GEOID))
    
    # Normalize to 2010 tracts if year > 2020
    if (yr >= 2021) {
      merged_data <- merged_data %>%
        rename(GEOID_2020 = GEOID) %>%
        left_join(crosswalk, by = "GEOID_2020") %>%
        mutate(
          estimate_weighted = estimate * weight,
          population_weighted = population * weight
        ) %>%
        group_by(GEOID_2010) %>%
        summarise(
          !!paste0("estimate_", yr) := sum(estimate_weighted, na.rm = TRUE),
          !!paste0("population_", yr) := sum(population_weighted, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          !!paste0("rate_", yr) := .data[[paste0("estimate_", yr)]] / .data[[paste0("population_", yr)]]
        ) %>%
        rename(`State-County-Tract FIPS Code` = GEOID_2010)
    } else {
      # For 2010 or 2020 data, no normalization
      merged_data <- merged_data %>%
        rename(`State-County-Tract FIPS Code` = GEOID) %>%
        mutate(
          !!paste0("estimate_", yr) := estimate,
          !!paste0("population_", yr) := population,
          !!paste0("rate_", yr) := estimate / population
        ) %>%
        select(`State-County-Tract FIPS Code`, ends_with(as.character(yr)))
    }
    
    # Merge with metro and nonmetro data
    metro_trimmed <- left_join(metro_trimmed, merged_data, by = "State-County-Tract FIPS Code")
    nmetro_trimmed <- left_join(nmetro_trimmed, merged_data, by = "State-County-Tract FIPS Code")
  }
  
  return(list(metro = metro_trimmed, nonmetro = nmetro_trimmed))
}

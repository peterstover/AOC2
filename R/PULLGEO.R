pull_ACS_with_geometry <- function(variable_code,
                                   population = "B01001_001",
                                   years,
                                   metro_trimmed = mt,
                                   nmetro_trimmed = nmt) {
  library(tidycensus)
  library(tidyverse)
  data("fips_codes")
  
  states <- unique(fips_codes$state)[1:51]  # 50 states + DC
  
  for (yr in years) {
    message(paste("Pulling data for year", yr))
    
    acs_data <- map_dfr(states, function(st) {
      message(paste("  Fetching", st))
      tryCatch({
        get_acs(
          geography = "tract",
          state = st,
          variables = c(main = variable_code, pop = population),
          year = yr,
          survey = "acs5",
          geometry = TRUE
        ) %>%
          select(GEOID, variable, estimate, geometry)
      }, error = function(e) {
        warning(paste("    Skipped", st, "due to error:", e$message))
        NULL
      })
    })
    
    if (nrow(acs_data) == 0) {
      warning(paste("No data returned for year", yr, "— skipping."))
      next
    }
    
    # Pivot and rename manually
    merged_data <- acs_data %>%
      pivot_wider(names_from = variable, values_from = estimate)
    
    names(merged_data)[names(merged_data) == "main"] <- paste0("estimate_", yr)
    names(merged_data)[names(merged_data) == "pop"] <- paste0("population_", yr)
    
    # Merge with metro/nonmetro
    metro_trimmed <- left_join(metro_trimmed, merged_data, by = c("State-County-Tract FIPS Code" = "GEOID"))
    nmetro_trimmed <- left_join(nmetro_trimmed, merged_data, by = c("State-County-Tract FIPS Code" = "GEOID"))
  }
  
  return(list(metro = metro_trimmed, nonmetro = nmetro_trimmed))
}

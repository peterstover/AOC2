counties<- read_excel("C:/Users/peter/Downloads/2022-geography-reference-manual.xlsx")
RUCA <- read_excel("C:/Users/peter/Downloads/ruca2010revised (1).xlsx")

metro_trimmed<-mt
nm_trimmed<-nmt
print(class(metro_trimmed))

pull_all_tracts <- function(statistic, year) {
  library(tidycensus)
  library(tidyverse)
  
  data("fips_codes")  # Contains FIPS state and county codes
  
  # Get unique state abbreviations
  states <- unique(fips_codes$state)[1:2]  # Exclude territories
  
  # Pull data for each state and combine
  all_data <- map_dfr(states, function(st) {
    message(paste("Fetching data for", st))
    get_acs(
      geography = "tract",
      state = st,
      variables = statistic,
      year = year
    )
  })
  
}



pull_ACS_yrs <- function(variable_code,population = "B01001_001", years, metro_trimmed = mt , nmetro_trimmed = mt) {
  library(tidyverse)
  library(tidycensus)
  
  for (yr in years) {
    message(paste("Pulling data for year", yr))
    
    # Pull main variable
    main_data <- pull_all_tracts(variable_code, yr) %>%
      select(GEOID, estimate) %>%
      rename(!!paste0("estimate_", yr) := estimate)
    
    # Pull population variable
    pop_data <- pull_all_tracts(population, yr) %>%
      select(GEOID, estimate) %>%
      rename(!!paste0("population_", yr) := estimate)
    
    # Merge estimates and compute rate
    merged_data <- left_join(main_data, pop_data, by = "GEOID") %>%
      mutate(!!paste0("rate_", yr) := .data[[paste0("estimate_", yr)]] / .data[[paste0("population_", yr)]])
    
    
    # Join with input tables
    metro_trimmed <- left_join(metro_trimmed, merged_data, by = c("State-County-Tract FIPS Code" = "GEOID"))
    nmetro_trimmed <- left_join(nmetro_trimmed, merged_data, by = c("State-County-Tract FIPS Code" = "GEOID"))
  }
  
  return(list(metro = metro_trimmed, nonmetro = nmetro_trimmed))
}

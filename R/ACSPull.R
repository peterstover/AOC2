library(readxl)
library(dplyr)
library(ggplot2)
{#import data
  X2022_geography_reference_manual <- read_excel("C:/Users/peter/Downloads/2022-geography-reference-manual.xlsx")
  RUCA <- read_excel("C:/Users/peter/Downloads/ruca2010revised (1).xlsx")
  nonmetro_counties <- X2022_geography_reference_manual
  #filter data
  nonmetro_counties <- nonmetro_counties %>% filter(`GEO-TYPE`=='03')
  nonmetro_counties <- nonmetro_counties %>% 
    mutate(FIPS_State_County = paste0(`FIPS State Code`, `FIPS County Code`))
  nonmetro_tracts <- RUCA %>%
    semi_join(nonmetro_counties, by = c("State-County FIPS Code" = "FIPS_State_County"))
  metro_tracts <- RUCA %>%
    anti_join(nonmetro_counties, by = c("State-County FIPS Code" = "FIPS_State_County"))
  RUCA4_nm_tracts <- nonmetro_tracts %>%
    filter(`Primary RUCA Code 2010`>=4)
  RUCA4_m_tracts <- metro_tracts %>%
    filter(`Primary RUCA Code 2010`>=4)}

pull_all_tracts <- function(statistic, year) {
  library(tidycensus)
  library(tidyverse)
  
  data("fips_codes")  # Contains FIPS state and county codes
  
  # Get unique state abbreviations
  states <- unique(fips_codes$state)[1:51]  # Exclude territories
  
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
  
}
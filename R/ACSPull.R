library(readxl)
library(dplyr)
library(ggplot2)
#import data
 counties<- read_excel("C:/Users/peter/Downloads/2022-geography-reference-manual.xlsx")
  RUCA <- read_excel("C:/Users/peter/Downloads/ruca2010revised (1).xlsx")

#Filter only metro counties
metro_msa_codes <- counties %>%
  filter(`GEO-TYPE` =="08" & grepl("Metro Area", TITLE)) %>%
  pull(`MSA Code`) %>%
  unique()
metro_counties <- counties %>%
  filter(`GEO-TYPE` == "03", `MSA Code` %in% metro_msa_codes)
metro_counties <- metro_counties %>% 
    mutate(FIPS_State_County = paste0(`FIPS State Code`, `FIPS County Code`))
 
metro_tracts <- RUCA %>%
    semi_join(metro_counties, by = c("State-County FIPS Code" = "FIPS_State_County"))
nonmetro_tracts <- RUCA %>%
    anti_join(metro_counties, by = c("State-County FIPS Code" = "FIPS_State_County"))
nmt <- nonmetro_tracts %>%
    filter(`Primary RUCA Code 2010`==4)%>%
   filter({
    Q1 <- quantile(`Population Density (per square mile), 2010`, 0.25, na.rm = TRUE)
    Q3 <- quantile(`Population Density (per square mile), 2010`, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    `Population Density (per square mile), 2010` >= (Q1 - 2 * IQR_val) & `Population Density (per square mile), 2010` <= (Q3 + 2 * IQR_val)
  })
mt<- metro_tracts %>%
    filter(`Primary RUCA Code 2010`==4)%>%
   filter({
    Q1 <- quantile(`Population Density (per square mile), 2010`, 0.25, na.rm = TRUE)
    Q3 <- quantile(`Population Density (per square mile), 2010`, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    `Population Density (per square mile), 2010` >= (Q1 - 2 * IQR_val) & `Population Density (per square mile), 2010` <= (Q3 + 2 * IQR_val)
  })

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
  

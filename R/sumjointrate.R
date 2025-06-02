summarize_joined_rates <- function(metro_df, nonmetro_df, years = 2009:2023) {
  results <- tibble()
  
  for (yr in years) {
    est_col <- paste0("estimate_", yr, ".x")  # numerator
    pop_col <- paste0("estimate_", yr, ".y")  # denominator
    
    if (est_col %in% names(metro_df) && pop_col %in% names(metro_df)) {
      metro_rate <- sum(metro_df[[est_col]], na.rm = TRUE) / 
        sum(metro_df[[pop_col]], na.rm = TRUE)
    } else {
      metro_rate <- NA_real_
    }
    
    if (est_col %in% names(nonmetro_df) && pop_col %in% names(nonmetro_df)) {
      nonmetro_rate <- sum(nonmetro_df[[est_col]], na.rm = TRUE) / 
        sum(nonmetro_df[[pop_col]], na.rm = TRUE)
    } else {
      nonmetro_rate <- NA_real_
    }
    
    results <- bind_rows(results, tibble(
      Year = yr,
      Metro = metro_rate,
      Nonmetro = nonmetro_rate
    ))
  }
  
  return(results)
}

calculate_derivative_pricing <- function(
    df_main, df_contract_calendar, wind_limit, wave_limit, 
    strike_percents, avg_profit_day, risk_free_rate
) {
  
  df_fdi <- df_main |>
    dplyr::mutate(
      fdi = as.integer(max_wind_ms <= wind_limit & max_swh_m <= wave_limit),
      month_day = format(as.Date(date), "%m-%d")
    )
  
  # Align the "Mean FDI" calculation to the Python method
  # Filter raw data to only include days within the contract window
  contract_month_days <- df_contract_calendar$month_day
  
  annual_contract_fdi <- df_fdi |>
    dplyr::filter(month_day %in% contract_month_days) |>
    dplyr::group_by(year) |>
    dplyr::summarise(contract_fdi = sum(fdi, na.rm = TRUE), .groups = "drop")
  
  # This is the 'mean_contract_fdi' from your Python code
  mean_contract_fdi <- mean(annual_contract_fdi$contract_fdi, na.rm = TRUE)
  
  # Setup the Probability Vector (p) for Poisson-Binomial
  p_vector <- df_contract_calendar$p_fishable
  n_days <- length(p_vector)
  contract_years <- n_days / 365
  
  # Build the Pricing Table
  pricing_table <- dplyr::tibble(
    wind_threshold = wind_limit,  # Added metadata
    wave_threshold = wave_limit,   # Added metadata
    strike_percent = strike_percents
  ) |>
    dplyr::mutate(
      # Matches Python: int(round(mean * strike))
      strike_days = as.integer(round(mean_contract_fdi * strike_percent)),
      
      # Loss amount: max(mean - strike, 0) * profit
      loss_amount = pmax(mean_contract_fdi - strike_days, 0) * avg_profit_day,
      
      # Trigger Probability P(N < K) 
      # ppoisbinom(k, p) is P(N <= k). For P(N < K), we use strike_days - 1
      trigger_prob = round(
        ifelse(strike_days <= 0, 0, 
               poisbinom::ppoisbinom(strike_days - 1, p_vector)), 
        4),
      
      discount_factor = exp(-risk_free_rate * contract_years),
      weather_derivative_price = round(
        discount_factor * loss_amount * trigger_prob,
        2)
    )
  
  return(pricing_table)
}

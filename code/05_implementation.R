# --- PARAMETERS ---
contract_start <- "01-01"
contract_end   <- "12-31"
strike_percents <- seq(0.80, 0.95, by = 0.05)
avg_daily_profit <- 330
risk_free_rate <- 0.05

# Generate sequences based on the actual observed range
# We round to 1 decimal place to keep the table readable
wind_thresholds <- c(8.5, 9.5, 10.5)
wave_thresholds <- c(1.5, 1.75, 2)

source(here::here("code", "03_contract_probability_prep.R"))
source(here::here("code", "04_pricing_engine.R"))

# Example of Prep Calendar
df_contract_calendar <- prep_contract_calendar(
  df_main, 
  wind_thresholds[1],
  wave_thresholds[2],
  contract_start, 
  contract_end
)

# Example of Calculate Pricing
df_pricing <- calculate_derivative_pricing(
  df_main,
  df_contract_calendar, 
  wind_thresholds[1],
  wave_thresholds[2],
  strike_percents, 
  avg_daily_profit, 
  risk_free_rate
)


# --- VISUALIZATION ---
# Plotting Daily Probability
ggplot2::ggplot(df_contract_calendar, ggplot2::aes(x = contract_day_index, y = p_fishable)) +
  ggplot2::geom_line(color = "steelblue", linewidth = 1) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::labs(title = "Daily Probability of Fishable Conditions",
       x = "Day of Contract", y = "Probability (Pj)") +
  ggplot2::theme_minimal()

# Plotting Price vs Strike
ggplot2::ggplot(df_pricing, ggplot2::aes(x = strike_percent, y = weather_derivative_price)) +
  ggplot2::geom_line(group = 1, color = "darkred") +
  ggplot2::geom_point(size = 3, color = "darkred") +
  ggplot2::scale_x_continuous(labels = scales::percent) +
  ggplot2::labs(title = "Weather Derivative Pricing",
       x = "Strike (% of Mean)", y = "Price ($)") +
  ggplot2::theme_minimal()



# Setup Scenario Grid (The 9 Combinations)
scenarios <- tidyr::crossing(
  wnd = wind_thresholds, 
  wav = wave_thresholds
)

# Run Batch Processing
dashboard_data <- purrr::map2_dfr(
  scenarios$wnd,  # These are now exactly 9 elements
  scenarios$wav,  # These are now exactly 9 elements
  ~{
    # A. Generate the daily probabilities (Function 1)
    df_cal <- prep_contract_calendar(
      df_main, 
      .x, 
      .y, 
      contract_start, 
      contract_end
    )
    
    # B. Generate the pricing table (Function 2)
    df_price <- calculate_derivative_pricing(
      df_main,
      df_cal, 
      .x, 
      .y,
      strike_percents, 
      avg_daily_profit, 
      risk_free_rate
    )
    
    # C. Combine them
    # We join so every 'day' has the 'pricing' data attached
    df_cal |>
      dplyr::left_join(
        df_price, 
        by = c("wind_threshold", "wave_threshold"),
        relationship = "many-to-many")
  }
)

# Save to Processed Folder
saveRDS(dashboard_data, here::here("data", "processed", "dashboard_data.rds"))
readr::write_csv(dashboard_data, here::here("dashboard", "dashboard_data.csv"))

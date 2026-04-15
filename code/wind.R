# Define the file names for the wind dataset
file_wind <- "st_croix_wind_data.csv"
fileout_wind <- "st_croix_wind_data_daily.csv"

# Load, clean, and select relevant columns
df_wind <- readr::read_csv(
  here::here("data", file_wind),
  , show_col_types = FALSE
) |>
  # Convert timestamp string to a datetime
  dplyr::mutate(valid_time = as.POSIXct(valid_time)) |>
  # Keep only the timestamp, U/V components (for speed), and wind gust
  dplyr::select(valid_time, u10, v10, fg10)

# Aggregate hourly data into a daily table
df_wind_daily <- df_wind |>
  dplyr::mutate(
    # Calculate Wind Speed (m/s) using U and V components
    wind_speed_ms = sqrt(u10^2 + v10^2),
    # Normalize the datetime to a simple Date for grouping
    date = as.Date(valid_time)
  ) |>
  # Group data by calendar day
  dplyr::group_by(date) |>
  # Calculate daily metrics: Max Speed, Mean Speed, and Peak Gust
  dplyr::summarise(
    max_wind_ms  = max(wind_speed_ms, na.rm = TRUE),
    mean_wind_ms = mean(wind_speed_ms, na.rm = TRUE),
    max_gust_ms  = max(fg10, na.rm = TRUE)
  ) |>
  # Tidyverse best practice remove grouping
  dplyr::ungroup()

# Export the processed data to the 'processed' sub folder
readr::write_csv(
  df_wind_daily, 
  here::here("data", "processed", fileout_wind)
)

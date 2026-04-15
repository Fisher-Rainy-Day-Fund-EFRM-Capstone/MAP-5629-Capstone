# Define the file names for the wave dataset
filein_wave <- "st_croix_wave_data.csv"
fileout_wave <- "st_croix_wave_data_daily.csv"

# Load, clean, and select relevant columns
df_wave <- readr::read_csv(
  here::here("data", filein_wave),
  , show_col_types = FALSE
) |>
  # Convert timestamp string to a datetime
  dplyr::mutate(valid_time = as.POSIXct(valid_time)) |>
  dplyr::select(valid_time, swh)

# Aggregate hourly data into a daily table
df_wave_daily <- df_wave |>
  dplyr::mutate(date = as.Date(valid_time)) |>
  # Group data by calendar day
  dplyr::group_by(date) |>
  # Calculate daily metrics: Max Speed, Mean Speed, and Peak Gust
  dplyr::summarise(
    max_swh_m = max(swh, na.rm = TRUE),
    mean_swh_m = mean(swh, na.rm = TRUE)
  ) |>
  # Tidyverse best practice remove grouping
  dplyr::ungroup()

# Export the processed data to the 'processed' sub folder
readr::write_csv(
  df_wave_daily, 
  here::here("data", "processed", fileout_wave)
)


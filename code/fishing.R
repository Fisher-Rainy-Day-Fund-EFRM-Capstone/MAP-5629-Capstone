filein_fishing <- "stx_com_csl_daily_8623.csv"
fileout_fishing <- "st_croix_fishing_data_daily.csv"

START_DATE <- as.Date("1985-01-01")
END_DATE   <- as.Date("2024-12-28")

# Read, format dates, and fill missing calendar days
df_fishing_daily <- readr::read_csv(
  here::here("data", filein_fishing),
  show_col_types = FALSE
) |>
  dplyr::mutate(date = lubridate::mdy(date)) |>
  # Automatically inserts rows for any gaps in the daily sequence
  tidyr::complete(date = seq(START_DATE, END_DATE, by = "1 day")) |>
  # Ensure the numeric columns are 0 instead of NA for the newly added dates
  dplyr::mutate(
    dplyr::across(c(n_trips, n_csl_trips, all_all, csl_all), 
                  ~tidyr::replace_na(., 0)),
    # Add time-based features for the full range
    year      = lubridate::year(date),
    weekday   = weekdays(date),
    is_sunday = as.integer(weekday == "Sunday")
  )

# Export the processed data to the 'processed' sub folder
readr::write_csv(
  df_fishing_daily, 
  here::here("data", "processed", fileout_fishing)
)

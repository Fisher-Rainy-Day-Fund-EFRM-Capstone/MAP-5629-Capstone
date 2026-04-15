# Process wave, wind, fishing, and population data
source(here::here("code", "wave.R"))
source(here::here("code", "wind.R"))
source(here::here("code", "fishing.R"))
source(here::here("code", "population.R"))

# Join wave, wind, fishing, and population data

# Put all daily-indexed data into a list
daily_data_list <- list(df_fishing_daily, df_wind_daily, df_wave_daily)

# Join them all at once using 'date' as the key
df_main <- daily_data_list |> 
  purrr::reduce(dplyr::left_join, by = "date") |>
  # Then join the population data which uses 'year' as the key
  dplyr::left_join(df_population_annual, by = "year") |>
  # Normalize effort by population
  dplyr::mutate(
    csl_trips_per_1k = (n_csl_trips / population) * 1000,
    csl_lbs_per_1k = (csl_all / population) * 1000
  ) |>
  # Normalize effort dividing by total trips per year
  dplyr::group_by(year) |>
  dplyr::mutate(
    # Aggregated annual totals for denominators
    total_annual_trips = sum(n_trips, na.rm = TRUE),
    total_annual_csl   = sum(n_csl_trips, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    # Daily proportions of the year's total effort
    trip_share_daily     = n_trips / total_annual_trips,
    csl_trip_share_daily = n_csl_trips / total_annual_csl
  )

# Export the processed data to the 'processed' sub folder
readr::write_csv(
  df_main, 
  here::here("data", "processed", "st_croix_daily.csv")
)


prep_contract_calendar <- function(df_main, wind_limit, wave_limit,
                                   start_mmdd, end_mmdd) {
  
  # Prepare Contract Probabilities and remove Feb 29th
  df_fdi <- df_main |>
    dplyr::mutate(
      fdi = as.integer(max_wind_ms <= wind_limit & max_swh_m <= wave_limit),
      date = as.Date(date)
    ) |>
    dplyr::filter(
      !(lubridate::month(date) == 2 & lubridate::day(date) == 29)
    ) |>
    dplyr::mutate(month_day = format(date, "%m-%d"))
  
  # Build Dummy Reference Year (2001)
  calendar_df <- dplyr::tibble(contract_date_dummy = seq(as.Date("2001-01-01"), 
                              as.Date("2001-12-31"), by = "day")) |>
    dplyr::mutate(
      month_day = format(contract_date_dummy, "%m-%d"),
      day_of_year = dplyr::row_number()
    )
  
  # Filter to Contract Window
  start_dummy <- as.Date(paste0("2001-", start_mmdd))
  end_dummy   <- as.Date(paste0("2001-", end_mmdd))
  
  if (start_dummy <= end_dummy) {
    contract_calendar <- calendar_df |> 
      dplyr::filter(contract_date_dummy >= start_dummy & 
                      contract_date_dummy <= end_dummy)
  } else {
    contract_calendar <- calendar_df |> 
      dplyr::filter(contract_date_dummy >= start_dummy | contract_date_dummy <= end_dummy)
  }
  
  # Join with Historical Probabilities
  daily_prob_all <- df_fdi |>
    dplyr::group_by(month_day) |>
    dplyr::summarise(
      p_fishable = mean(fdi, na.rm = TRUE),
      n_years = dplyr::n(),
      .groups = "drop"
    )
  
  # Final Return: Attach the input parameters as columns
  contract_calendar |>
    dplyr::left_join(daily_prob_all, by = "month_day") |>
    dplyr::mutate(
      contract_day_index = dplyr::row_number(),
      wind_threshold = wind_limit,  # Added metadata
      wave_threshold = wave_limit   # Added metadata
    )
}

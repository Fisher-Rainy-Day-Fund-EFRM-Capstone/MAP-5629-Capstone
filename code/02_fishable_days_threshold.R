# Define the file name
filein <- "st_croix_daily.csv"

# Load the main data
df_main <- readr::read_csv(
  here::here("data", "processed", filein),
  show_col_types = FALSE
) 

# Set values for sensitivity
wind_min <- 8
wind_max <- 12
wave_min <- 1
wave_max <- 3

# Generate sequences based on the actual observed range
# We round to 1 decimal place to keep the table readable
wind_thresholds <- seq(from = wind_min, to = wind_max, by = 0.5)
wave_thresholds <- seq(from = wave_min, to = wave_max, by = 0.25)

sensitivity_grid <- tidyr::crossing(wnd = wind_thresholds, wav = wave_thresholds)

# Fishable Threshold
df_fishable <- sensitivity_grid |>
  dplyr::group_by(wnd, wav) |>
  dplyr::group_modify(~ {
    df_temp <- df_main |>
      dplyr::mutate(
        wind_ok = (max_wind_ms <= .y$wnd),
        wave_ok = (max_swh_m <= .y$wav),
        both_ok = (wind_ok & wave_ok)
      )
    
    annual_metrics <- df_temp |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        days_both_ok = sum(both_ok, na.rm = TRUE),
        .groups = "drop"
      )
    
    tibble::tibble(
      avg_days_combined = mean(annual_metrics$days_both_ok, na.rm = TRUE),
      pct_year_fishable = (avg_days_combined / 365.25) * 100
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::rename(wind_thresh_ms = wnd, wave_thresh_m = wav)

# Heatmap for Combined Fishable Days
ggplot2::ggplot(df_fishable, ggplot2::aes(x = wind_thresh_ms, y = wave_thresh_m, fill = avg_days_combined)) +
  ggplot2::geom_tile(color = "white") +
  # Add text labels to the tiles
  ggplot2::geom_text(ggplot2::aes(label = round(avg_days_combined, 0)), size = 3, color = "black") +
  # Use a color scale that goes from 'restricted' (red/yellow) to 'open' (blue/green)
  ggplot2::scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  ggplot2::labs(
    title = "Combined Threshold Sensitivity",
    subtitle = "Avg. Annual Fishable Days (Wind and Waves combined)",
    x = "Wind Threshold (m/s)",
    y = "Wave Threshold (m)",
    fill = "Days/Year"
  ) +
  ggplot2::theme_minimal()


# Fished Threshold 
df_fished <- sensitivity_grid |>
  dplyr::group_by(wnd, wav) |>
  dplyr::group_modify(~ {
    df_temp <- df_main |>
      dplyr::mutate(
        is_rough = (max_wind_ms > .y$wnd | max_swh_m > .y$wav),
        no_trips = (n_trips == 0)
      )
    
    df_rough <- df_temp |> dplyr::filter(is_rough == TRUE)
    
    tibble::tibble(
      zero_effort_rate_total = if(nrow(df_rough) > 0) mean(df_rough$no_trips) * 100 else NA
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::rename(wind_thresh_ms = wnd, wave_thresh_m = wav)


ggplot2::ggplot(df_fished, 
                ggplot2::aes(x = wind_thresh_ms, y = wave_thresh_m, fill = zero_effort_rate_total)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::scale_fill_viridis_c(option = "magma", direction = 1) +
  ggplot2::labs(
    title = "Validation: Zero-Effort Rate During 'Bad' Weather",
    subtitle = "% of days where trips = 0 when thresholds were exceeded",
    x = "Wind Threshold (m/s)",
    y = "Wave Threshold (m)",
    fill = "% Zero Effort"
  ) +
  ggplot2::theme_minimal()

# Effort as percentage threshold

df_final_sensitivity <- sensitivity_grid |>
  dplyr::group_by(wnd, wav) |>
  dplyr::group_modify(~ {
    df_temp <- df_main |>
      dplyr::mutate(is_fishable = (max_wind_ms <= .y$wnd & max_swh_m <= .y$wav))
    
    effort_capture <- df_temp |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        captured_share_csl = sum(csl_trip_share_daily[is_fishable], na.rm = TRUE),
        .groups = "drop"
      )
    
    tibble::tibble(
      avg_pct_csl_captured = mean(effort_capture$captured_share_csl, na.rm = TRUE) * 100
    )
  }) |>
  dplyr::ungroup() |>
  dplyr::rename(wind_thresh_ms = wnd, wave_thresh_m = wav)


# Heatmap: Visualizing the 'Sweet Spot' for CSL Capture
ggplot2::ggplot(df_final_sensitivity, 
                ggplot2::aes(x = wind_thresh_ms, y = wave_thresh_m, fill = avg_pct_csl_captured)) +
  ggplot2::geom_tile(color = "white") +
  # Adding a color scale that highlights the transition (Plasma is colorblind friendly)
  ggplot2::scale_fill_viridis_c(option = "plasma", name = "% CSL Captured") +
  # Overlay the actual percentages for precise reading
  ggplot2::geom_text(ggplot2::aes(label = round(avg_pct_csl_captured, 1)), 
                     size = 3.5, color = "white", fontface = "bold") +
  ggplot2::labs(
    title = "CSL Effort Capture Sensitivity",
    subtitle = "Avg. % of annual lobster trips occurring below specified weather limits",
    x = "Wind Threshold (m/s)",
    y = "Wave Threshold (m)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )



# Heatmap: Avg. % of Year Fishable (based on 365.25 days)
df_fishable |>
  dplyr::mutate(pct_fishable = (avg_days_combined / 365.25) * 100) |>
  ggplot2::ggplot(ggplot2::aes(x = wind_thresh_ms, y = wave_thresh_m, fill = pct_fishable)) +
  ggplot2::geom_tile(color = "white") +
  # Using 'mako' for environmental availability (darker = less available)
  ggplot2::scale_fill_viridis_c(option = "mako", name = "% of Year", direction = -1) +
  # Overlay the percentage labels
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(pct_fishable, 1), "%")), 
                     size = 3.5, color = "white", fontface = "bold") +
  ggplot2::labs(
    title = "Environmental Availability: % of Year Fishable",
    subtitle = "Percentage of 364 days where both wind and wave thresholds are met",
    x = "Wind Threshold (m/s)",
    y = "Wave Threshold (m)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

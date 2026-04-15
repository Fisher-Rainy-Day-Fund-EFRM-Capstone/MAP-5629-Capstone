# Read in fishery data format and add missing dates
csl_data <- read.csv(
  here::here("data","stx_com_csl_daily_8623.csv")
) |>
  dplyr::mutate(date = lubridate::mdy(date)) |>
  tidyr::complete(date=seq(min(date), max(date), by="1 day"))

# Plot the maximum number of trips in a day by year
max_day <- csl_data |>
  dplyr::group_by(trip_year) |>
  dplyr::summarize(max = max(n_csl_trips))

plot(max_day$trip_year, max_day$max, type = "l",
     ylab = "Maximum Trips in a Day", xlab = "Year")

# Notice what months the max day fished tends to occur
max_months <- csl_data |>
  dplyr::group_by(trip_year) |>
  dplyr::filter(n_csl_trips == max(n_csl_trips)) |>
  dplyr::ungroup()

table(max_months$trip_month) # July and August

# Get percentage of annual daily max. Scale max day to 1.
csl_scaled <- csl_data |>
  dplyr::group_by(trip_year) |>
  dplyr::mutate(max_csl_trips = max(n_csl_trips)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    p_csl_trips = dplyr::case_when(is.na(n_csl_trips) ~ 0,
                                   TRUE ~ n_csl_trips / max_csl_trips)
  )

plot(x = csl_scaled$date,y = csl_scaled$p_csl_trips, type = "l",
     ylab = "Pecent of best day", xlab = "Year")

# Read in df_main.csv
main_data <- read.csv(
  here::here("data","df_master.csv")
)

nosun_data <- main_data |>
  dplyr::filter(weekday != "Sunday")


wind_check <- prop.table(
  table(nosun_data$n_csl_trips > 0, nosun_data$wind_ok), 
  margin = 2
)

wave_check <- prop.table(
  table(nosun_data$n_csl_trips > 0, nosun_data$wave_ok), 
  margin = 2
)

index_check <- prop.table(
  table(nosun_data$n_csl_trips > 0, 
        nosun_data$wave_ok > 0 & nosun_data$wind_ok > 0), 
  margin = 2
)

index_check <- round(prop.table(
  table(nosun_data$wave_ok,
        nosun_data$wind_ok), 
  margin = 2
), 2)


# Define a sequence of wind thresholds to test
thresholds <- seq(2, 29, by = 0.25)

# Calculate the percentage of "Zero Trip" days for each threshold
wind_sensitivity <- sapply(thresholds, function(t) {
  # Subset data to only days above the current threshold
  days_above <- nosun_data[$max_wind_ms >= t, ]
  
  if(nrow(days_above) == 0) return(NA) # Avoid dividing by zero
  
  # Calculate proportion of these days where trips were 0
  mean(days_above$n_csl_trips == 0)
})

# Combine into a clean results table
results <- data.frame(
  threshold_ms = thresholds,
  prob_zero_trips = wind_sensitivity
)

print(results)

plot(results$threshold_ms, results$prob_zero_trips, 
     type = "b", 
     xlab = "Wind Speed Threshold (m/s)", 
     ylab = "Probability of Zero Trips",
     main = "Sensitivity Analysis of Fishing Cessation", 
     ylim = c(0,1))





# Define a sequence of wind thresholds to test
thresholds <- seq(2, 29, by = 0.25)

# Calculate the percentage of "Zero Trip" days for each threshold
wind_sensitivity <- sapply(thresholds, function(t) {
  # Subset data to only days above the current threshold
  days_above <- nosun_data[nosun_data$max_wind_ms >= t, ]
  
  if(nrow(days_above) == 0) return(NA) # Avoid dividing by zero
  
  # Calculate proportion of these days where trips were 0
  mean(days_above$n_csl_trips == 0)
})

# Combine into a clean results table
results <- data.frame(
  threshold_ms = thresholds,
  prob_zero_trips = wind_sensitivity
)

print(results)

plot(results$threshold_ms, results$prob_zero_trips, 
     type = "b", 
     xlab = "Wind Speed Threshold (m/s)", 
     ylab = "Probability of Zero Trips",
     main = "Sensitivity Analysis Wind", 
     ylim = c(0,1))






# Define a sequence of wind thresholds to test
thresholds <- seq(0.5, 7.5, by = 0.25)

# Calculate the percentage of "Zero Trip" days for each threshold
wave_sensitivity <- sapply(thresholds, function(t) {
  # Subset data to only days above the current threshold
  days_above <- nosun_data[nosun_data$max_swh_m >= t, ]
  
  if(nrow(days_above) == 0) return(NA) # Avoid dividing by zero
  
  # Calculate proportion of these days where trips were 0
  mean(days_above$n_csl_trips == 0)
})

# Combine into a clean results table
results <- data.frame(
  threshold_m = thresholds,
  prob_zero_trips = wave_sensitivity
)

print(results)

plot(results$threshold_m, results$prob_zero_trips, 
     type = "b", 
     xlab = "Wave Threshold (m)", 
     ylab = "Probability of Zero Trips",
     main = "Sensitivity Analysis Wave", 
     ylim = c(0,1))




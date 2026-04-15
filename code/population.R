# Define the file names for the population dataset
filein_population  <- "usvi_pop.csv"
fileout_population <- "st_croix_population_data_annual.csv"

# Load the raw population data
df_population_annual <- readr::read_csv(
  here::here("data", filein_population),
  show_col_types = FALSE
) |>
  # Rename columns for clarity: 
  dplyr::rename(
    year = observation_date, 
    population = POPTOTVIA647NWDB
  ) |>
  # Extract the four-digit year from the date object to use as a join key
  dplyr::mutate(year = lubridate::year(year))

# Export the processed population data to the 'processed' subfolder
readr::write_csv(
  df_population_annual, 
  here::here("data", "processed", fileout_population)
)

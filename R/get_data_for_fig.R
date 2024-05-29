get_data_for_fig <- function(extreme_date, data_radius = 4, delta = 0.2, stn_lat, stn_long, meta_data){

  lat_upp = stn_lat + delta #-27.4 - 1974 fig window
  lat_low = stn_lat - delta #-27.55
  long_low = stn_long - delta #152.9
  long_upp = stn_long + delta #153.1

  rainfall_vars = c("PRCP", "DAPR", "DWPR", "MDPR")
  fct_levels = c("No rain", "Rain", "Missing", "Accum")
  fct_shapes = c(1, 16, 4, 13)

  # -----------------------------------------------------------------------
  # Pull all station data for figure
  # -----------------------------------------------------------------------

  fig_stations_meta <- meta_data |>
    filter(latitude > lat_low & latitude < lat_upp) |>
    filter(longitude > long_low & longitude < long_upp)

  monitors <- fig_stations_meta$id

  fig_station_data_all <- meteo_pull_monitors(monitors = monitors,
                                              keep_flags =  TRUE,
                                              var = rainfall_vars)

  # -----------------------------------------------------------------------
  # Filter station data to necessary subset
  # -----------------------------------------------------------------------

  # Filter to figure dates, assign variables types, join meta data
  fig_dates = extreme_date + - date_radius:date_radius
  fig_data = fig_station_data_all |>
    filter(date %in% fig_dates) |>
    mutate(prcp = as.numeric(prcp)/10,
           dapr = as.numeric(dapr),
           mdpr = as.numeric(mdpr)/10) |>
    left_join(meta_data |> filter(element == "PRCP"))

  # -----------------------------------------------------------------------
  # Create a new variable for the shape
  # -----------------------------------------------------------------------

  # Combine daily prcp and tagged accumulations
  fig_data_for_plot = fig_data |>
    mutate(dapr_noNA = ifelse(is.na(dapr), 0, dapr)) |>
    mutate(prcp_combined = ifelse(dapr_noNA > 0, mdpr, prcp)) |>
    mutate(prcp_type = 'Rain') |>
    mutate(prcp_type = ifelse(prcp == 0, 'No rain', prcp_type)) |>
    mutate(prcp_type = ifelse(is.na(prcp_combined), 'Missing', prcp_type)) |>
    mutate(prcp_type = ifelse(dapr_noNA > 0, 'Accum', prcp_type)) |>
    mutate(prcp_type = factor(prcp_type, levels = fct_levels)) |>
    mutate(qflag_prcp = as.factor(qflag_prcp)) |>
    select(id, longitude, latitude, prcp_combined, prcp_type, date, qflag_prcp)

  return(fig_data_for_plot)

}

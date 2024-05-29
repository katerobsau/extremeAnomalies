meta_data = readRDS("data/aus_stations_meta.rds")
stn_id = "ASN00040368"

delta = 0.25
stn_lat = meta_data |> filter(id == stn_id) |> pull(latitude)
stn_long = meta_data |> filter(id == stn_id) |> pull(longitude)

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

### Which days in the domain are extreme / heavy?

extr_quantiles <- fig_station_data_all |>
  left_join(meta_data) |>
  mutate(prcp = as.numeric(prcp),
         num_years = last_year - first_year + 1) |>
  filter(num_years > 20) |>
  group_by(id) |>
  summarise(q975 = quantile(prcp, 0.975, na.rm = TRUE)) |>
  ungroup()

mean(extr_quantiles$q975)
median(extr_quantiles$q975)

extr_threshold = 1500

extr_rf_obs = fig_station_data_all |>
  mutate(prcp = as.numeric(prcp)) |>
  filter(prcp > extr_threshold)

extr_rf_dates = extr_rf_obs |> select(date) |> distinct()
dim(extr_rf_dates)

### Of these is there any missing data the occurred on that date
missing_on_extr_days = fig_station_data_all |>
  filter(date %in% extr_rf_dates$date) |>
  filter(is.na(prcp))

missing_dates = missing_on_extr_days |> select(date) |> distinct()

extreme_date = as_date("2011-01-10")
stn_id = "ASN00040059"
stn_meta = meta_data |>
  filter(id == stn_id)

fig_data_for_plot <- get_data_for_fig(
  extreme_date,
  stn_lat = stn_meta$latitude,
  stn_long = stn_meta$longitude,
  meta_data = meta_data)

obs_month_str = month(extreme_date, label = TRUE, abbr = TRUE) |>
  as.character()

eg2_outlier_plot <- create_temporal_plot(
  fig_data_for_plot,
  city_country_str = stn_meta$name,
  month_year_str = paste(obs_month_str, year(extreme_date)))

eg2_outlier_plot

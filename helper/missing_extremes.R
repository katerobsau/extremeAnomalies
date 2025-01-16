library(rnoaa)
library(tidyverse)
library(oz)
devtools::load_all()

## ---- data ----
aus_code <- "ASN"
min_years <- 50
# var = c("PRCP", "DAPR", "DWPR", "MDPR")
var <- c("PRCP")
delta <- 50
east_coast_longitude <- 150

if (FALSE) {
  all_stations <- ghcnd_stations()
  # qs::qsave(all_stations, "station_meta.qs")
  # Southeast Queensland Stations
  seq_stations <- all_stations |>
    filter(element == "PRCP") |>
    filter(str_detect(id, aus_code)) |>
    filter(latitude < -25 & latitude > -28.5) |>
    filter(longitude > 151 & longitude < 153.6)

  # maybe pick smaller contries that rely on these flags
  # first year and last year
  # length of record


  seq_station_plot <- ggplot() +
    geom_point(
      data = seq_stations,
      aes(x = longitude, y = latitude),
      shape = 21, size = 0.2
    ) +
    geom_point(
      data = seq_stations,
      aes(x = longitude, y = latitude),
      shape = 21, fill = "red"
    ) +
    coord_fixed()

  # Reduced Figure Stations
  fig_stations <- seq_stations |>
    filter(latitude < -27.4 & latitude > -27.55) |>
    filter(longitude > 152.9 & longitude < 153.1)

  fig_station_plot <- ggplot() +
    geom_point(
      data = seq_stations,
      aes(x = longitude, y = latitude),
      shape = 21, size = 0.2
    ) +
    geom_point(
      data = fig_stations,
      aes(x = longitude, y = latitude),
      shape = 21, fill = "red"
    ) +
    coord_fixed()

  # Data download
  # ~12 minutes for 435 stations
  monitors <- seq_stations$id
  time1 <- Sys.time()
  reduced_station_data <- meteo_pull_monitors(
    monitors = monitors,
    keep_flags = TRUE,
    var = c("PRCP", "dapr") # dapr is used to determine tagged accumulation
  )
  time2 <- Sys.time()
}
# reduced_station_data <- readRDS("data/all_stations_meta.rds")
# qs::qsave(reduced_station_data, "reduced_station_data.qs")
reduced_station_data <- qs::qread("reduced_station_data.qs")
station_meta <- qs::qread("station_meta.qs")

## ---- inspect ----

df_prcp <- reduced_station_data %>%
  select(id, date, prcp, qflag_prcp) %>%
  mutate(prcp = as.numeric(prcp))
station_loc <- station_meta %>%
  distinct(id, latitude, longitude, elevation, name) %>%
  filter(latitude < -25 & latitude > -28.5) %>%
  filter(longitude > 151 & longitude < 153.6)

# Check missing value
# 333178 missing values in total
naniar::miss_var_summary(df_prcp)


# All stations id
# 1296 stations in total
id_stations <- distinct(df_prcp, id)
# Stations having missing values
# 191 stations have missing values
id_missing <- df_prcp %>%
  group_by(id) %>%
  filter(!anyNA(prcp)) %>%
  ungroup() %>%
  distinct(id)


# Pick one station with missing values
# ASN00040452 has 5 missings in a raw
# between Jan 25 and Jan 29 in 1974
# See vignette

id_example <- "ASN00040452"
period_example <- c(ymd("1974-01-25"), ymd("1974-01-29"))
df_missing <- df_prcp %>%
  filter(
    id == id_example,
    between(date, period_example[[1]], period_example[[2]])
  )
# id_example <- "ASN00039070"
# period_example <- ymd("1998-09-11") %>%
#   {
#     c(. - days(7), . + days(7))
#   }


df_nbr <- nbr_subset(df_missing$id, station_loc, Inf)

# Check if the surrounding stations have extremes
id_nbr_bydist <- df_nbr %>%
  arrange(dist) %>%
  pull(neighbour) %>%
  unique()
id_plot <- c(id_example, id_nbr_bydist)
df_prcp %>%
  filter(
    id %in% id_plot,
    between(date, period_example[[1]] - days(3), period_example[[2]] + days(3))
  ) %>%
  mutate(
    prcp_type = case_when(
      is.na(prcp) ~ "Missing",
      prcp == 0 ~ "No Rain",
      # qflag_prcp == "" ~ "Accum",
      .default = "Rain"
    ),
    id = factor(id, levels = rev(id_plot))
  ) %>%
  ggplot() +
  geom_point(
    aes(
      x = date, y = id,
      col = prcp, shape = prcp_type
    ),
    size = 4, stroke = 1.15
  ) +
  scale_shape_manual(
    name = "Obs Type",
    values = c("Missing" = 4, "No Rain" = 1, "Rain" = 16)
  ) +
  geom_hline(yintercept = id_example, colour = "red", alpha = 0.2) +
  scale_color_gradient(name = "Prcp (mm)", low = "skyblue", high = "navy") +
  xlab("Date") +
  ylab("Station Id") +
  # ggtitle("Rainfall Observations", "Brisbane, Australia January 1974") +
  theme_bw()
# Missing should be casued by the extreme event on Jan 25
# Jan 25 - 27 should be missing extreme values
# Missings from Jan 28 - 29 might be just because the station is not repaired


## ---- extreme ----
# identifying if that day is an extreme day

# very slow in scale
df_nbr_prcp <- distinct(df_nbr, target, neighbour) %>%
  rename(id = neighbour) %>%
  left_join(df_prcp) %>%
  drop_na()


df_thr <- df_nbr_prcp %>%
  group_by(target) %>%
  filter(prcp > 0) %>%
  # summarise(thr = quantile(prcp, 0.9999), .groups = "drop")
  summarise(thr = quantile(prcp, 0.99), .groups = "drop")

df_extreme <- df_nbr_prcp %>%
  semi_join(distinct(df_missing, date)) %>%
  group_by(target, date) %>%
  summarise(max_prcp = max(prcp), .groups = "keep") %>%
  left_join(df_thr) %>%
  mutate(extreme = max_prcp >= thr)
# trace back to first day of missing and extreme
# number of days since day of missing and extreme
df_extreme

# pick a biggest extreme event
# prop of missing values

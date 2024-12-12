library(rnoaa)
library(tidyverse)
library(oz)

## ---- data ----
aus_code <- "ASN"
min_years <- 50
# var = c("PRCP", "DAPR", "DWPR", "MDPR")
var <- c("PRCP")
delta <- 50
east_coast_longitude <- 150

# all_stations <- ghcnd_stations()
# qs::qsave(all_stations, "station_meta.qs")
# if(FALSE) {
# Southeast Queensland Stations
seq_stations <- all_stations |>
  filter(element == "PRCP") |>
  filter(str_detect(id, aus_code)) |>
  filter(latitude < -25 & latitude > -28.5) |>
  filter(longitude > 151 & longitude < 153.6)


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
  var = "PRCP"
)
time2 <- Sys.time()
# }
# reduced_station_data <- readRDS("reduced_data.rds")
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

# Getting station flaged as outlier
df_prcp_outlier <- df_prcp %>%
  filter(qflag_prcp == "O")

# Identify neighbours of these target stations
df_nbr <- nbr_subset(df_prcp_outlier$id, station_loc, Inf)

# pick the neighbours with 3 days observations
# or
# pick the neighbours than filter out ones without 3 days observations?
# second one now


df_target_prcp <- df_nbr %>%
  left_join(df_prcp_outlier,
    by = c("target" = "id"),
    relationship = "many-to-many"
  ) %>%
  rename(target_prcp = prcp)

df_nbr_date <- distinct(df_target_prcp, neighbour, date)
df_nbr_3day <- bind_rows(
  df_nbr_date,
  mutate(df_nbr_date, date = date - 1),
  mutate(df_nbr_date, date = date + 1)
) %>%
  rename(nbr_date = date) %>%
  mutate(date = rep(df_nbr_date$date, 3)) %>%
  left_join(df_prcp,
    by = c(
      "neighbour" = "id",
      "nbr_date" = "date"
    ),
    relationship = "many-to-many"
  ) %>%
  arrange(neighbour, nbr_date) %>%
  select(!qflag_prcp)


df_nbr_prcp <- df_target_prcp %>%
  left_join(df_nbr_3day,
    by = c("neighbour", "date"),
    relationship = "many-to-many"
  )


df_nbr_close_prcp <- df_nbr_prcp %>%
  group_by(target, neighbour, date) %>%
  # two target + date are filtered ouot because no available nearby stations
  filter(all(!is.na(prcp))) %>%
  group_by(target, date) %>%
  slice_min(dist, n = 7 * 3) %>%
  ungroup()

## Step 1: calculate minimum absolute target-neighbour difference (matnd)
df_matnd <- df_nbr_close_prcp %>%
  group_by(target, date, target_prcp) %>%
  summarise(
    min_nbr_prcp = min(prcp),
    max_nbr_prcp = max(prcp),
    .groups = "drop"
  ) %>%
  mutate(matnd = case_when(
    between(target_prcp, min_nbr_prcp, max_nbr_prcp) ~ 0,
    target_prcp > max_nbr_prcp ~ abs(target_prcp - max_nbr_prcp),
    target_prcp < min_nbr_prcp ~ abs(target_prcp - min_nbr_prcp)
  ))

## Step 2: calculate minimum absolute target-neighbour difference (matnd)
## for percentile


df_station <- bind_rows(
  select(df_nbr_close_prcp, target, date, target_prcp) %>%
    rename(id = target, prcp = target_prcp),
  select(df_nbr_close_prcp, neighbour, nbr_date, prcp) %>%
    rename(id = neighbour, date = nbr_date)
) %>%
  distinct()

ls_prcp_ref <- df_prcp %>%
  mutate(ref_date = `year<-`(date, 2024)) %>%
  {
    `names<-`(split(., .$id), unique(.$id))
  }

df_percent_rank <- df_station %>%
  mutate(data = lapply(id, \(id) ls_prcp_ref[[id]])) %>%
  mutate(perc = pbapply::pbmapply(\(id, date, prcp, data){
    ref_date <- date
    year(ref_date) <- 2024
    min_date <- ref_date - days(14)
    max_date <- ref_date + days(14)
    sample_prcp <- data %>%
      filter(
        between(.data$ref_date, .env$min_date, .env$max_date),
        .data$prcp != 0
      ) %>%
      pull(prcp)
    if (length(sample_prcp) < 20) {
      perc <- NA
    } else {
      perc <- ecdf(sample_prcp)(prcp) * 100
    }
    perc
  }, id, date, prcp, data, SIMPLIFY = TRUE)) %>%
  select(!c(data, prcp))

df_matnd_perc <- df_nbr_close_prcp %>%
  select(!ends_with("prcp")) %>%
  left_join(df_percent_rank, by = c(
    "target" = "id",
    "date" = "date"
  )) %>%
  rename(target_perc = perc) %>%
  left_join(df_percent_rank, by = c(
    "neighbour" = "id",
    "nbr_date" = "date"
  )) %>%
  group_by(target, date, target_perc) %>%
  summarise(
    min_nbr_perc = min(perc),
    max_nbr_perc = max(perc),
    .groups = "drop"
  ) %>%
  mutate(matnd_perc = case_when(
    between(target_perc, min_nbr_perc, max_nbr_perc) ~ 0,
    target_perc > max_nbr_perc ~ abs(target_perc - max_nbr_perc),
    target_perc < min_nbr_perc ~ abs(target_perc - min_nbr_perc)
  ))


df_reflag <- df_matnd_perc %>%
  select(target, date, matnd_perc) %>%
  left_join(select(df_matnd, target, date, matnd)) %>%
  mutate(thr = case_when(
    is.na(matnd_perc) ~ 269.24,
    matnd_perc != 0 ~ -45.72 * log(matnd_perc) + 269.24,
    matnd_perc == 0 ~ 269.24
  )) %>%
  mutate(new_flag = case_when(
    matnd < thr ~ "Spatial consistent",
    TRUE ~ "O"
  )) %>%
  left_join(df_prcp_outlier, by = c("target" = "id", "date"))
saveRDS(df_reflag, "data/prcp_reflag.rds")


df_reflag %>%
  filter(new_flag != "O") %>%
  # select(target) %>%
  left_join(station_meta, by = c("target" = "id")) %>%
  select(target:name, element, first_year, last_year) %>%
  mutate(length_year = last_year - first_year) %>%
  arrange(desc(length_year))


df_reflag %>%
  filter(new_flag != "O") %>%
  # select(target) %>%
  left_join(station_meta, by = c("target" = "id")) %>%
  filter(first_year == 1750) %>%
  select(!c(date, prcp, qflag_prcp)) %>%
  left_join(df_prcp, by = c("target" = "id")) %>%
  arrange(target, first_year, date)
## ---- end ----

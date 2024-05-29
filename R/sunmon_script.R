library(rnoaa)
library(tidyverse)
library(oz)
library(lubridate)
library(svMisc)

aus_code  = "ASN"
var = c("PRCP", "DAPR", "DWPR", "MDPR")

if(!file.exists("data/all_stations_meta.rds")){
  all_stations_meta <- ghcnd_stations()
  saveRDS(all_stations_meta, "data/all_stations_meta.rds")
}

# Australian stations
if(!file.exists("data/aus_stations_meta.rds")){
  aus_stations_meta <- all_stations_meta %>%
    filter(element == "PRCP") %>%
    filter(str_detect(id, aus_code)) %>%
    filter(longitude > 0) # removes ocean
  saveRDS(aus_stations_meta, "data/aus_stations_meta.rds")
}

# Southeast Queensland Stations
seq_stations_meta <- all_stations_meta |>
  filter(element == "PRCP") |>
  filter(str_detect(id, aus_code)) |>
  filter(latitude < -25 & latitude > -28.5) |>
  filter(longitude > 151 & longitude < 153.6)

seq_station_plot <- ggplot()  +
  geom_point(data  = aus_stations_meta,
             aes(x= longitude,  y = latitude),
             shape = 21, size = 0.2) +
  geom_point(data =  seq_stations_meta,
             aes(x= longitude,  y = latitude),
             shape = 21, fill = "red") +
  coord_fixed()


seq_station_plot

# Data download
# ~12 minutes for 435 stations / Seems to be taking longer
if(!file.exists("data/seq_stations_data.rds")){
  
  monitors <- seq_stations_meta$id
  time1 <- Sys.time()
  reduced_station_data <- meteo_pull_monitors(monitors = monitors,
                                              keep_flags =  TRUE,
                                              var = c("PRCP", "DAPR", "DWPR", "MDPR"))
  time2 <- Sys.time()
}

saveRDS(reduced_station_data, "all_stations_meta.rds")


# Chi-square test for counts and then check zeroes and accumulation on days failing test

stations <- reduced_station_data %>%
  filter(qflag_prcp == "O")

saveRDS(stations, "outliers.rds")

stations <- reduced_station_data

annual_count_rain_days <- function(data, station_id) {
  temp <- data %>%
    filter(id == station_id) %>%
    select(id, date, prcp) %>%
    mutate(day = wday(date, label = TRUE),
           year = year(date),
           rain = ifelse(is.na(prcp), 0, 
                         ifelse(prcp > 0, 1, 0)),
           weekdays = ifelse(day != "Sat" & day != "Sun", 1, 0),
           sundays = ifelse(day == "Sun", 1, 0),
           mondays = ifelse(day == "Mon", 1, 0),
           tuesdays = ifelse(day == "Tue", 1, 0),
           wednesdays = ifelse(day == "Wed", 1, 0),
           thursdays = ifelse(day == "Thu", 1, 0),
           fridays = ifelse(day == "Fri", 1, 0),
           saturdays = ifelse(day == "Sat", 1, 0)) %>%
    filter(year > 1750)
  
  grouped <- temp %>%
    group_by(year, day) %>%
    summarise(raindays = sum(rain),
              weekdays = sum(weekdays),
              sundays = sum(sundays),
              mondays = sum(mondays),
              tuesdays = sum(tuesdays),
              wednesdays = sum(wednesdays),
              thursdays = sum(thursdays),
              fridays = sum(fridays),
              saturdays = sum(saturdays)) %>%
           mutate(weekday = ifelse(day != "Sat" & day != "Sun", 1, 0),
           sunday = ifelse(day == "Sun", 1, 0),
           monday = ifelse(day == "Mon", 1, 0),
           tuesday = ifelse(day == "Tue", 1, 0),
           wednesday = ifelse(day == "Wed", 1, 0),
           thursday = ifelse(day == "Thu", 1, 0),
           friday = ifelse(day == "Fri", 1, 0),
           saturday = ifelse(day == "Sat", 1, 0))
  
  actual_rain <- grouped %>%
    group_by(year) %>%
    summarise(sun = sum(sunday*raindays),
              mon = sum(monday*raindays),
              tue = sum(tuesday*raindays),
              wed = sum(wednesday*raindays),
              thu = sum(thursday*raindays),
              fri = sum(friday*raindays),
              sat = sum(saturday*raindays))
  
  actual_norain <- grouped %>%
    group_by(year) %>%
    summarise(sun = sum(sundays) - sum(sunday*raindays),
              mon = sum(mondays) - sum(monday*raindays),
              tue = sum(tuesdays) - sum(tuesday*raindays),
              wed = sum(wednesdays) - sum(wednesday*raindays),
              thu = sum(thursdays) - sum(thursday*raindays),
              fri = sum(fridays) - sum(friday*raindays),
              sat = sum(saturdays) - sum(saturday*raindays))
  
  actual_list <- list()
  for (j in 1:nrow(actual_rain)) {
    df <- data.frame(matrix(NA, nrow = 7, ncol = 3))
    for (i in 1:7) {
      df[i,2] <- actual_rain[j,i+1]
      df[i,3] <- actual_norain[j, i+1]
    }
    df[1] <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    names(df) <- c("day", "rain", "no_rain")
    
    actual_list[[j]] <- df
  }
  
  names(actual_list) <- actual_rain$year
  return(actual_list)
}

sun_mon_test <- function(data, alpha) {
  pval <- data.frame(matrix(NA, ncol = 3))
  for(i in 1:length(data)) {
    temp <- chisq.test(data[[i]] %>%
                         select(-day))
    
    pval[i,2] <- temp$p.value
  }
  pval[1] <- names(data)
  pval[,3] <- ifelse(pval[,2] < alpha, "QA", NA)
  names(pval) <- c("year","pval", "flag")
  return(pval)
}

names(actual_seq[[1]])


actual_seq <- list()
for (i in 1:length(id_df)) {
  actual_seq[[i]] <- annual_count_rain_days(stations, id_df[i])
  progress(i, length(id_df))
}
names(actual_seq) <- id_df

sunmon_seq <- list()
for (j in 1:1297) {
  sunmon_seq[[j]] <- sun_mon_test(actual_seq[[j]], 0.05)
  progress(j, 1297)
}
names(sunmon_seq) <- id_df

vec <- numeric()
for(i in 1:1297) {
  vec[i] <- nrow(sunmon_seq[[i]])
}

max(vec)

years <- sunmon_seq[[1]][1] %>%
  as.matrix() %>%
  as.numeric()


year_match <- match(df[[1]], years)
length(which(!is.na(year_match)))

df <- data.frame(matrix(nrow = max(vec), ncol = length(sunmon_seq)+1))
df[1] <- seq(from = 2023-max(vec)+1, to = 2023, by = 1)
for (i in 1:1297) {
  years <- sunmon_seq[[i]][1] %>%
    as.matrix() %>%
    as.numeric()
  year_match <- match(df[[1]], years)
  year_match <- which(!is.na(year_match))

for (j in 1:length(year_match)) {
  df[year_match, i+1] <- sunmon_seq[[i]][j,3]
}
}

names(df)[1] <- "year"
names(df)[2:1298] <- id_df

qa.df <- df %>%
  pivot_longer(!year, names_to = "id", values_to = "QA")




bin <- qa.df %>%
  mutate(qa_bin = ifelse(!is.na(QA), 1, 0)) %>%
  group_by(year) %>%
  summarise(count = sum(qa_bin))

bin %>%
  ggplot() +
  geom_line(aes(x = year, y = count))


# Untagged Days
untagged_fun <- function(data, station_id, qa, qa_tidy) {

temp1 <- data %>%
  filter(id == station_id) %>%
  select(id, date, prcp, qflag_prcp) %>%
  mutate(year = year(date),
         day = wday(date, label = TRUE),
         prcp = as.numeric(prcp) / 10,
         diff = prcp - lag(prcp)) %>%
  filter(prcp == diff) %>%
  filter(year > 1750 & diff >= 100)

temp2 <- qa %>%
  filter(id == "ASN00040014")
temp2 <- which(temp2$QA == "QA")
new <- c()
for (i in 1:length(temp2)) {
  new[i] <- qa_tidy[temp2[i], 1]
}

year_match <- match(temp1$year, new)
new <- c()
for (i in 1:length(year_match)) {
  new[i] <- qa_tidy[year_match[i], 1]
}
temp1 <- temp1 %>%
  mutate(sunflag = ifelse(new %in% year, "QA", ""))

return(temp1)
}

untagged <- list()
for (i in 1:length(id_df)) {
  untagged[[i]] <- untagged_fun(stations, id_df[i], qa.df, df)
  progress(i, 1297)
}
names(untagged) <- id_df

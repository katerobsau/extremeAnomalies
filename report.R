library(tidyverse)
library(targets)
library(Matrix)
library(geosphere)
tar_source()
tar_load(df_prcp_zero)

# 1. extreme zeros
df_extreme_zero <- filter(df_prcp_zero, flag_extreme_zero)
df_extreme_zero

# can't do much else
# two example, 1 and 20
# for 1, the following value is not extreme, so only visual differece
# for 20, the problem is a stattion in the neighbour, no way of checking

eg_id <- 20
df_extreme_zero %>%
  slice(eg_id) %>%
  {
    tar_qplot(.$id, .$date,
      data = df_prcp_zero
    )
  }

eg_extreme_zero <- slice(df_extreme_zero, eg_id)

eg_extreme_zero %>%
  select(id) %>%
  left_join(df_prcp_zero) %>%
  filter(between(
    date,
    eg_extreme_zero$date - days(7),
    eg_extreme_zero$date + days(7)
  )) %>%
  View()

# 2. accumulated extremes
df_accumulated_extreme <- filter(df_prcp_zero, flag_accumulated_extreme)
df_accumulated_extreme

date <- ymd("2009-05-20")
id <- "ASN00040969"
tar_qplot(id, date = date, data = df_prcp_zero)

# 3. Condition
# accumulated_monday = !is.na(dapr) & dapr > 0

# 4. Some accumulated tags are not on Monday?
df_monday <- df_prcp_zero %>%
  mutate(
    accumulated_monday = !is.na(dapr) & dapr > 0
  ) %>%
  filter(accumulated_monday) %>%
  select(id, date, accumulated_monday, dapr, prcp, mdpr)
df_monday %>%
  mutate(wday = wday(date, label = TRUE)) %>%
  tail()

# 5. number of missing extremes
id <- "ASN00040452"
period <- c(ymd("1974-01-25"), ymd("1974-01-29"))
filter(df_prcp_zero, id == .env$id, between(date, period[[1]], period[[2]]))
tar_qplot(id, period = period, data = df_prcp_zero)

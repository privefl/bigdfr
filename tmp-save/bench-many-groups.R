library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":dbname:")
copy_to(con, nycflights13::flights, "flights",
        temporary = FALSE,
        indexes = list(
          c("year", "month", "day"),
          "carrier",
          "tailnum",
          "dest"
        ))

test <- FDF(nycflights13::flights)

flights_db <- tbl(con, "flights")
flights_db
flights_db %>% select(year:day, dep_delay, arr_delay)
flights_db %>% filter(dep_delay > 240)

flights_db %>%
  group_by(dest) %>%
  summarise(delay = mean(dep_time, na.rm = TRUE))

system.time(
  tailnum_delay_db <- flights_db %>%
    group_by(tailnum) %>%
    summarise(
      delay = mean(arr_delay, na.rm = TRUE),
      n = n()
    ) %>%
    arrange(desc(delay)) %>%
    filter(n > 100) %>%
    collect()
)

system.time(
  tailnum_delay_fdf <- test %>%
    group_by(tailnum) %>%
    summarise(
      delay = mean(arr_delay, na.rm = TRUE),
      n = n()
    ) %>%
    arrange(desc(delay)) %>%
    filter(n > 100)
)

test2 <- tailnum_delay_fdf <- test %>%
  group_by(tailnum)

system.time(summarise(
  test2,
  delay = mean(arr_delay, na.rm = TRUE),
  n = n()
))

system.time(mutate(
  test2,
  delay = mean(arr_delay, na.rm = TRUE),
  n = n()
))

system.time(filter(
  test2,
  arr_delay > 10,
  n() > 15
))

system.time(arrange(
  test2, arr_delay
))

test3 <- as_tibble(test2)
system.time(summarise(
  test3,
  delay = mean(arr_delay, na.rm = TRUE),
  n = n()
))

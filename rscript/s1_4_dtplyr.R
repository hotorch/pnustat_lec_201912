install.packages(c('nycflights13','microbenchmark'))

# load library ---------------------------------------------------------------

library(nycflights13)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(microbenchmark)


# load data ---------------------------------------------------------------

df = flights
df %>% dim()
df %>% head()

df_dt = data.table(df)
df_tb = as_tibble(df)
df_lz = lazy_dt(df)

summary(df_lz)
class(df_lz)


# comparision velocity - filter data ----------------------------------------------------

results = microbenchmark(
    data.table = df_dt[origin == 'JFK' & carrier == 'AA', ],
    dplyr = df_tb %>% filter(origin == 'JFK' & carrier == 'AA'),
    dtplyr = df_lz %>% filter(origin == 'JFK' & carrier == 'AA') %>% as_tibble(),
    times = 100
)

results


# comparision velocity - summarize data -----------------------------------------------------------------------


results2 = microbenchmark(
  
  `data.table` = df_dt[, .(mean_delay = mean(dep_delay, na.rm = TRUE)),
                       by = c('year', 'month', 'day', 'carrier', 'origin')][mean_delay >= 10],
  
  `dplyr` = df_tb %>%
    group_by(year, month, day, carrier, origin) %>%
    summarize(mean_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(mean_delay >= 10),
  
  `dtplyr` = df_lz %>%
    group_by(year, month, day, carrier, origin) %>%
    summarize(mean_delay = mean(dep_delay, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(mean_delay >= 10) %>%
    as_tibble(),
  
  times = 100
)

results2


# end of document ---------------------------------------------------------


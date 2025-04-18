setwd("Code")
library("conflicted")
library("glue")
library("tidyverse")
conflict_prefer("lag", "dplyr")
acled_raw <- as_tibble(read.csv("ACLED.csv"))
acled <- acled_raw |> mutate(year = year(event_date))
y_events <- acled |>
  group_by(year) |>
  summarize(nb_events_y = n(), nb_fatalities_y = sum(fatalities))
stopifnot(sum(acled$fatalities) == sum(y_events$nb_fatalities_y))
y_events_ext <- y_events
for (i in 1:3) {
  y_events_ext <- y_events_ext |> mutate(nb_events_y_minus_1 = lag(nb_events_y, 1))
  d[[glue("nb_events_y_minus_", i)]] <- y_events |>
    pull(nb_events_y) |>
    lag(i)
  d[[glue("nb_fatalities_y_minus_", i)]] <- y_events |>
    pull(nb_fatalities_y) |>
    lag(i)
}
y_events_ext <- y_events |> mutate(!!!d)

wb <- as_tibble(read.csv("WB year.csv"))
wb_descr <- as_tibble(read.csv("WB descr.csv"))
gdp <- acled$gdp

model <- lm(gdp ~ a + b, data = data)

# f_by_e <- t |>
#   group_by(event_type) |>
#   summarize(sum(fatalities))
# nb_f <- sum(t$fatalities)
# stopifnot(sum(t$fatalities) == sum(f_by_e[2]))
# f_by_i <- t |>
#   group_by(interaction) |>
#   summarize(sum(fatalities))

delta_gdp <- na.omit(diff(log(gdp$value), lag = 1)) # GDP growth rate

library("conflicted")
library("tidyverse")
acled <- as_tibble(read.csv("ACLED.csv"))
wb <- as_tibble(t(read.csv("WB.csv")))
# f_by_e <- t |>
#   group_by(event_type) |>
#   summarize(sum(fatalities))
# nb_f <- sum(t$fatalities)
# stopifnot(nb_f == sum(f_by_e[2]))
# f_by_i <- t |>
#   group_by(interaction) |>
#   summarize(sum(fatalities))

delta_gdp <- na.omit(diff(log(gdp$value), lag = 1)) # GDP growth rate

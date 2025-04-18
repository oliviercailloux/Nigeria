setwd("Code")
library("conflicted")
library("glue")
library("tidyverse")
library("stargazer")
conflict_prefer("lag", "dplyr")
acled_raw <- as_tibble(read.csv("ACLED.csv"))
acled <- acled_raw |> mutate(year = year(event_date))
y_events <- acled |>
  group_by(year) |>
  summarize(nb_events_y = n(), nb_fatalities_y = sum(fatalities)) |>
  mutate(log_nb_events_y = log(nb_events_y), log_nb_fatalities_y = log(nb_fatalities_y))
stopifnot(sum(acled$fatalities) == sum(y_events$nb_fatalities_y))
by_year <- y_events
for (i in 1:5) {
  by_year <- by_year |>
    mutate(
      "log_nb_events_y_minus_{i}" := lag(log_nb_events_y, i),
      "log_nb_fatalities_y_minus_{i}" := lag(log_nb_fatalities_y, i)
    )
}

wb <- as_tibble(read.csv("WB year.csv")) |> rename(year = Year)
wb_descr <- as_tibble(read.csv("WB descr.csv"))
log_gdp <- log(wb$NY.GDP.PCAP.KD)
diffs <- c(NA, diff(log_gdp, lag = 1))
wb_ext <- wb |> mutate(delta_log_gdp = diffs) |> rename(gdp = NY.GDP.PCAP.KD, infl = NY.GDP.DEFL.KD.ZG.AD, fdi = BM.KLT.DINV.WD.GD.ZS, trade = TG.VAL.TOTL.GD.ZS, military_exp = MS.MIL.XPND.ZS, export = TX.VAL.MRCH.XD.WD)

by_year <- by_year |>
  inner_join(wb_ext, by = "year")

for (i in 1:5) {
  lm_formula = as.formula(glue("delta_log_gdp ~ log_nb_events_y_minus_{i} + log_nb_fatalities_y_minus_{i} + infl + fdi + trade + military_exp + export"))
  model <- lm(lm_formula, by_year)
  stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_both_{i}.tex"))
}
for (i in 1:5) {
  lm_formula = as.formula(glue("delta_log_gdp ~ log_nb_events_y_minus_{i} + infl + fdi + trade + military_exp + export"))
  model <- lm(lm_formula, by_year)
  stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_events_{i}.tex"))
}
for (i in 1:5) {
  lm_formula = as.formula(glue("delta_log_gdp ~ log_nb_fatalities_y_minus_{i} + infl + fdi + trade + military_exp + export"))
  model <- lm(lm_formula, by_year)
  stargazer(model, type = "latex", title = "GDP growth rate model fats", dep.var.labels = c("GDP growth rate"), out = glue("../LaTeX/Generated/gdp_model_fats_{i}.tex"))
}
# delta_log_gdp = log gdp year y2 - log gdp year y1 = log(gdp year y2 / gdp year y1)
# delta_log_gdp = a + b1 * nb_events_y + b2 * nb_fatalities_y means that
# gdp year y2 = gdp year y1 * e^(a + b1 * nb_events_y + b2 * nb_fatalities_y)
# gdp year y2 = gdp year y1 * e^a * e^(b1 * nb_events_y + b2 * nb_fatalities_y)
# gdp year y2 = gdp year y1 * e^a * e^(b1 * nb_events_y) * e^(b2 * nb_fatalities_y)
# x2 = x1 × e^dlog_gdp
# f_by_e <- t |>
#   group_by(event_type) |>
#   summarize(sum(fatalities))
# nb_f <- sum(t$fatalities)
# stopifnot(sum(t$fatalities) == sum(f_by_e[2]))
# f_by_i <- t |>
#   group_by(interaction) |>
#   summarize(sum(fatalities))


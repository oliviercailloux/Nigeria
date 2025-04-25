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
  mutate(log_nb_events_y = log(nb_events_y + 1), log_nb_fatalities_y = log(nb_fatalities_y + 1))
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
wb_ext <- wb |>
  mutate(delta_log_gdp = diffs) |>
  rename(gdp = NY.GDP.PCAP.KD, infl = NY.GDP.DEFL.KD.ZG.AD, fdi = BM.KLT.DINV.WD.GD.ZS, trade = TG.VAL.TOTL.GD.ZS, military_exp = MS.MIL.XPND.ZS, export = TX.VAL.MRCH.XD.WD)

by_year <- by_year |>
  inner_join(wb_ext, by = "year")

models <- list()
for (i in 1:5) {
  lm_formula <- as.formula(glue("delta_log_gdp ~ log_nb_events_y_minus_{i} + log_nb_fatalities_y_minus_{i} + infl + fdi + trade + military_exp + export"))
  model <- lm(lm_formula, by_year)
  models[[i]] <- model
  # print(summary(model))
  # , report=('vc*p')
  stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_both_{i}.tex"), table.placement = "t", label = glue("tab:both{i}"))
}
for (i in 1:5) {
  m <- models[[i]]
  cs <- summary(m)$coefficients
  t <- as_tibble(cs, rownames = NA) |> rownames_to_column("variable")
  pvalues <- t |> select(c("variable", `Pr(>|t|)`))
  l <- split(pvalues[["Pr(>|t|)"]], pvalues$variable)
  ps[[i]] <- l[[glue("log_nb_events_y_minus_{i}")]]
}
ps

m_complete <- models[[3]]
s <- summary(m_complete)
r2 <- s$r.squared
scr_nc <- sum(m_complete$residuals^2)
a <- anova(m_complete)
t <- as_tibble(a) |> mutate(variable = row.names(a), .before = "Df")
stopifnot(t[t[["variable"]] == "Residuals", ] |> pull("Sum Sq") == scr_nc)
sct <- sum(t[["Sum Sq"]])
# sum(m$residuals^2) + sum((m$fitted.values - y)^2)
# stopifnot(sum(t[["Sum Sq"]]) == sct)
stopifnot(abs(r2 - (sct - scr_nc) / sct) < 0.00001)

ps <- list()

stargazer(models[[3]], type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_both_3_again.tex"), table.placement = "t", label = glue("tab:both3again"))

for (i in 1:5) {
  lm_formula <- as.formula(glue("delta_log_gdp ~ log_nb_events_y_minus_{i} + infl + fdi + trade + military_exp + export"))
  model <- lm(lm_formula, by_year)
  stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_events_{i}.tex"), table.placement = "t", label = glue("tab:events{i}"))
}
for (i in 1:5) {
  lm_formula <- as.formula(glue("delta_log_gdp ~ log_nb_fatalities_y_minus_{i} + infl + fdi + trade + military_exp + export"))
  model <- lm(lm_formula, by_year)
  stargazer(model, type = "latex", title = "GDP growth rate model fats", dep.var.labels = c("GDP growth rate"), out = glue("../LaTeX/Generated/gdp_model_fats_{i}.tex"), table.placement = "t", label = glue("tab:fats{i}"))
}

model <- lm(delta_log_gdp ~ log_nb_events_y_minus_3 + log_nb_fatalities_y_minus_3 + infl + trade + military_exp + export, by_year)
stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_vfitme.tex"), table.placement = "t", label = "tab:vfitme")

model <- lm(delta_log_gdp ~ log_nb_events_y_minus_3 + log_nb_fatalities_y_minus_3 + infl + trade + military_exp, by_year)
stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_vfitm.tex"), table.placement = "t", label = "tab:vfitm")

model <- lm(delta_log_gdp ~ log_nb_events_y_minus_3 + log_nb_fatalities_y_minus_3 + infl + military_exp, by_year)
stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_vfim.tex"), table.placement = "t", label = "tab:vfim")

model <- lm(delta_log_gdp ~ log_nb_events_y_minus_3 + infl + military_exp, by_year)
stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_vim.tex"), table.placement = "t", label = "tab:vim")

model <- lm(delta_log_gdp ~ log_nb_events_y_minus_3 + military_exp, by_year)
stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_vm.tex"), table.placement = "t", label = "tab:vm")

model <- lm(delta_log_gdp ~ log_nb_events_y_minus_3, by_year)
stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_v.tex"), table.placement = "t", label = "tab:v")

# stargazer(model, type = "latex", title = "GDP growth rate model", out = glue("../LaTeX/Generated/gdp_model_both_{i}.tex"))
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

m_small <- model
s <- summary(m_small)
r2 <- s$r.squared
scr_c <- sum(m_small$residuals^2)
a <- anova(m_small)
t <- as_tibble(a) |> mutate(variable = row.names(a), .before = "Df")
stopifnot(t[t[["variable"]] == "Residuals", ] |> pull("Sum Sq") == scr_c)
sct_c <- sum(t[["Sum Sq"]])
stopifnot(abs(sct_c - sct) < 0.001)
stopifnot(abs(r2 - (sct_c - scr_c) / sct_c) < 0.00001)
ddf_tot <- nrow(by_year) - 1
k_complete <- length(m_complete$coefficients) - 1
k_small <- length(m_small$coefficients) - 1
ddf_res_complete <- ddf_tot - k_complete - 1
q <- k_complete - k_small
f_stat <- ((scr_c - scr_nc) / q) / (scr_nc / ddf_res_complete)
pf(f_stat, q, ddf_res_complete)

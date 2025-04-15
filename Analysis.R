library(conflicted)
library(tidyverse)
library(acled.api)
email_and_api_key <- readLines("ACLED API key.txt")
email <- email_and_api_key[1]
api_key <- email_and_api_key[2]
# url <- "https://api.acleddata.com/acled/read/?key=****************1234&email=you@youremail.com"
# event_date <- "{2024-01-01|2024-12-31}"
# event_date_where <- "BETWEEN"
# country_url <- "https://api.acleddata.com/country/read?key={api_key}&email={email address}"
data <- acled.api(
  email.address = email, access.key = api_key,
  country = "Nigeria",
  start.date = "1900-01-01", end.date = "2024-12-31"
)
t_complete <- tibble(data)
years <- year(t_complete |> pull(event_date))
given_years <- t_complete |> pull(year)
stopifnot(years == given_years)
stopifnot(t_complete |> pull(region) |> unique() |> length() == 1)
stopifnot(t_complete |> pull(country) |> unique() |> length() == 1)
stopifnot(t_complete |> pull(admin3) |> unique() |> length() == 1)
t <- t_complete |> select(-c(region, country, year, admin3))
write.csv(t, "ACLED.csv", row.names = FALSE)
f_by_e <- t |> group_by(event_type) |> summarize(sum(fatalities))
nb_f <- sum(t$fatalities)
stopifnot(nb_f == sum(f_by_e[2]))
f_by_i <- t |> group_by(interaction) |> summarize(sum(fatalities))

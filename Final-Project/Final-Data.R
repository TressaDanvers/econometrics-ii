library(tidyverse)

possible.countries <- read.csv("data-sync/world-bank-data.csv")$Country.Name

fwc <- read.csv("data-sync/first-world-countries-2024.csv") |>
	(\(d) d[order(d$Hdi2021, decreasing=TRUE),])() |>
  filter(country %in% possible.countries) |>
	head(25) |>
	(\(d) d$country)()

data <- read.csv("data-sync/world-bank-data.csv") |>
	filter(Country.Name %in% fwc)

library(tidyverse)
library(lubridate)

CPIAUCSL <- "./data/CPIAUCSL.csv" |>
  read.csv()|>
  mutate(
    date = ymd(DATE),
    year = year(date),
    month = month(date)
  ) 

FEDFUNDS <- "./data/FEDFUNDS.csv" |>
  read.csv() |>
  mutate(
    date = ymd(DATE),
    year = year(date),
    month = month(date),
    FEDFUNDS = FEDFUNDS/100
  )

MORTGAGE30US <- "./data/MORTGAGE30US.csv" |>
  read.csv() |>
  mutate(
    date = ymd(DATE),
    year = year(date),
    month = month(date),
    MORTGAGE30US = MORTGAGE30US/100
  ) |>
  group_by(year,month) |>
  filter(date==min(date)) |>
  ungroup()

data <- MORTGAGE30US |> 
  full_join(FEDFUNDS |> select(year,month,FEDFUNDS) ,by = c("year","month")) |>
  full_join(CPIAUCSL |> select(year,month,CPIAUCSL), by = c("year","month")) |>
  select(-DATE,-date) |>
  mutate(date = paste0(year,"-",month,"-01") |> ymd()) |>
  mutate(
    dlogCPIAUCSL = log(CPIAUCSL)-log(lag(CPIAUCSL,n=12))
  ) |>
  filter(year>=1970)

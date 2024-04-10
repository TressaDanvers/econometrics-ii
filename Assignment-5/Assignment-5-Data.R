library(tidyverse)
library(Hmisc)
library(rlist)

gdp <- read.csv("./data/gdp.csv")

gdp$YEAR <- gdp$DATE |>
  map(\(x) x |> str_split("-") |> unlist()) |>
  map(as.numeric) |>
  map(\(x) x[[1]] + (x[[2]]-1) / 12 + (x[[3]]-1) / 365) |>
  unlist()

gdp$QUARTER <- gdp$YEAR * 4

gdp <- gdp |>
  mutate(dGDP = GDP - Lag(GDP)) |>
  mutate(dGDP = dGDP - Lag(dGDP)) # second differencing

kpss <- gdp$GDP |> kpss.test()
kpss2 <- gdp$dGDP |> kpss.test()

models <- list()
for (i in 1:3)
  for (j in c(0,i))
    models[[i+(j/i)*3]] <- gdp$dGDP |>
      arima(order=c(i,1,j))

best.model <- models[[1]]
for (model in models) if (model$aic > best.model$aic) best.model <- model
best.aic <- models |> map(\(x) x$aic) |> unlist() |> max()

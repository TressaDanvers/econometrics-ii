library(tidyverse)
library(stargazer)

theme_set(theme_bw())

prek_path <- "data/PreK.csv"

prek <- prek_path |>
  read.csv() |>
  subset(select=c(age, wjtest01, white, black,
                  hispanic, freelunch, female)) |>
  na.omit()

prek_3months <- prek |>
  filter(age %in% seq(-90, 90))

prek_model <- prek |>
  lm(formula=wjtest01~age+I(age>=0)+age*I(age>=0)+white+black+hispanic+freelunch+female)

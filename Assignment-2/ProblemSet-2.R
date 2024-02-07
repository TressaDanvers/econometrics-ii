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

prek_model <- prek_3months |>
  lm(formula=wjtest01~age+I(age<0)+age*I(age<0)+white+black+hispanic+freelunch+female)

prek_value <- prek_model$coefficients[1]
no_prek_difference <- prek_model$coefficients[3]
no_prek_value <- no_prek_difference + prek_value

summary_table <- tibble()
for (window in 1:6) {
  data <- prek |>
    filter(age %in% seq(-window * 30, window * 30))

  regression <- data |>
    lm(formula=wjtest01~age+I(age<0)+age*I(age<0)+white+black+hispanic+freelunch+female)

  regression_summary_coef <-
    summary(regression)$coefficients[3, c("Estimate", "Std. Error")]

  summary_table <- summary_table |> rbind(
    tibble(
      estimate=regression_summary_coef[1],
      StdErr=regression_summary_coef[2],
      wjtest01=data$wjtest01,
      age=data$age,
      window=(window * 30)
    )
  )

  remove(data, regression, regression_summary_coef)
}

summary_estimates <- summary_table$estimate |> unique()

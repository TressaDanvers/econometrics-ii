ols_model <- educationCrime |>
  lm(formula = prison ~ educ + as.factor(state) + as.factor(year))

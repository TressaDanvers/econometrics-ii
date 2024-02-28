demonstration_model <- educationCrime |>
  lm(formula = educ ~ as.factor(req_sch) + as.factor(state) + as.factor(year))

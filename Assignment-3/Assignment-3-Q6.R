pl_model <- educationCrime |>
  plm(formula = prison ~ educ,
      index=c("state", "year"), effect="twoways")

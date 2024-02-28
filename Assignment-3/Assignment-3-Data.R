educationCrime <- "data/LochnerMoretti2004_EducationCrime.rds" |> readRDS() |>
  select(prison, educ, state, year, req_sch) |>
  na.omit()

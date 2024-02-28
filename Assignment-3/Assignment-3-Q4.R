inst_model <-
  ivreg(data = educationCrime,
        formula = prison ~ educ + as.factor(state) + as.factor(year) |
          as.factor(req_sch) + as.factor(state) + as.factor(year))

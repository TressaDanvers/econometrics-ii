library(tidyverse)
library(plm)

possible.countries <- read.csv("data-sync/world-bank-data.csv")$Country.Name

FWC <- read.csv("data-sync/first-world-countries-2024.csv") |>
	(\(d) d[order(d$Hdi2021, decreasing=TRUE),])() |>
  filter(country %in% possible.countries) |>
	head(25) |>
	(\(d) d$country)()

DATA <- read.csv("data-sync/world-bank-data.csv") |>
	select(-Series.Code) |>
	filter(Country.Name %in% FWC) |>
	pivot_longer(cols = X1960..YR1960. :X2023..YR2023., names_to="YEAR", values_to="V") |>
	mutate(YEAR=YEAR |> substr(10,13) |> as.numeric()) |>
	pivot_wider(names_from = Series.Name, names_glue = "{Series.Name}", values_from=V) |>
	filter(YEAR > 2013) |>

	mutate(PHCR.NPL=data$"Poverty headcount ratio at national poverty lines (% of population)" |> as.numeric()) |>
	mutate(PHCR.SPL=data$"Poverty headcount ratio at societal poverty line (% of population)" |> as.numeric()) |>
	mutate(PHCR.2.15=data$"Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)" |> as.numeric()) |>
	mutate(PHCR.3.65=data$"Poverty headcount ratio at $3.65 a day (2017 PPP) (% of population)" |> as.numeric()) |>
	mutate(PHCR.6.85=data$"Poverty headcount ratio at $6.85 a day (2017 PPP) (% of population)" |> as.numeric()) |>

	mutate(GDP=data$"GDP (current US$)" |> as.numeric()) |>
	mutate(INFLATION=data$"Inflation, consumer prices (annual %)" |> as.numeric()) |>

	mutate(MR.AD.M=data$"Mortality rate, adult, male (per 1,000 male adults)" |> as.numeric()) |>
	mutate(MR.AD.F=data$"Mortality rate, adult, female (per 1,000 female adults)" |> as.numeric()) |>
	mutate(MR.U5=data$"Mortality rate, under-5 (per 1,000 live births)" |> as.numeric()) |>
	mutate(MR.IN=data$"Mortality rate, infant (per 1,000 live births)" |> as.numeric()) |>
	mutate(MR.NN=data$"Mortality rate, neonatal (per 1,000 live births)" |> as.numeric()) |>
	mutate(MR.S=data$"Suicide mortality rate (per 100,000 population)" |> as.numeric()) |>

	mutate(COUNTRY=Country.Name) |>
	select(c(COUNTRY,YEAR,PHCR.NPL:PHCR.6.85,GDP,INFLATION,MR.AD.M:MR.S))

DATA.NPL <- DATA |> select(c(COUNTRY,YEAR,PHCR.NPL,GDP,INFLATION,MR.AD.M:MR.S)) |> na.omit()
DATA.SPL <- DATA |> select(c(COUNTRY,YEAR,PHCR.SPL,GDP,INFLATION,MR.AD.M:MR.S)) |> na.omit()
DATA.DLI <- DATA |> select(c(COUNTRY,YEAR,PHCR.2.15:PHCR.6.85,GDP,INFLATION,MR.AD.M:MR.S)) |> na.omit()

MODEL.NPL.AD.M <- plm(MR.AD.M ~ PHCR.NPL, data=DATA_NPL, index=c("COUNTRY"), model="pooling")
MODEL.NPL.AD.F <- plm(MR.AD.F ~ PHCR.NPL, data=DATA_NPL, index=c("COUNTRY"), model="pooling")
MODEL.SPL.AD.M <- plm(MR.AD.M ~ PHCR.SPL, data=DATA_SPL, index=c("COUNTRY"), model="pooling")
MODEL.SPL.AD.F <- plm(MR.AD.F ~ PHCR.SPL, data=DATA_SPL, index=c("COUNTRY"), model="pooling")

library(stargazer)
table <- stargazer(MODEL.NPL.AD.M, MODEL.NPL.AD.F, MODEL.SPL.AD.M, MODEL.SPL.AD.F, type="text")

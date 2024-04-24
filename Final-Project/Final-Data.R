library(tidyverse)
library(plm)
library(stargazer)

possible.countries <- read.csv("data/world-bank-data.csv")$Country.Name

FWC <- read.csv("data/first-world-countries-2024.csv") |>
	(\(d) d[order(d$Hdi2021, decreasing=TRUE),])() |>
	filter(country %in% possible.countries) |>
	filter(!(country %in% c("Denmark", "Netherlands", "Belgium", "United Kingdom", "United Arab Emirates"))) |>
	head(25) |>
	(\(d) d$country)()

remove(possible.countries)

DATA <- read.csv("data/world-bank-data.csv") |>
	select(-Series.Code) |>
	filter(Country.Name %in% FWC) |>
	pivot_longer(cols=X1960..YR1960.:X2023..YR2023., names_to="YEAR", values_to="V") |>
	mutate(YEAR=YEAR |> substr(10,13) |> as.numeric()) |>
	pivot_wider(names_from=Series.Name, names_glue="{Series.Name}", values_from=V) |>

	(\(x) x |>
		mutate(PHCR.2.15=x$"Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)" |> as.numeric()) |>
		mutate(PHCR.3.65=x$"Poverty headcount ratio at $3.65 a day (2017 PPP) (% of population)" |> as.numeric()) |>
		mutate(PHCR.6.85=x$"Poverty headcount ratio at $6.85 a day (2017 PPP) (% of population)" |> as.numeric()) |>

		mutate(GDP=x$"GDP (current US$)" |> as.numeric()) |>
		mutate(POP=x$"Population, total" |> as.numeric()) |>
		mutate(INF=x$"Inflation, consumer prices (annual %)" |> as.numeric()) |>
		mutate(HEA=x$"Current health expenditure (% of GDP)" |> as.numeric()) |>

		mutate(MR.AD.M=x$"Mortality rate, adult, male (per 1,000 male adults)" |> as.numeric()) |>
		mutate(MR.AD.F=x$"Mortality rate, adult, female (per 1,000 female adults)" |> as.numeric()) |>
		mutate(MR.U5=x$"Mortality rate, under-5 (per 1,000 live births)" |> as.numeric()) |>
		mutate(MR.IN=x$"Mortality rate, infant (per 1,000 live births)" |> as.numeric()) |>
		mutate(MR.NN=x$"Mortality rate, neonatal (per 1,000 live births)" |> as.numeric()) |>
		mutate(MR.S=x$"Suicide mortality rate (per 100,000 population)" |> as.numeric()) |>

		mutate(COUNTRY=Country.Name) |>
		select(c(COUNTRY, YEAR, PHCR.2.15:PHCR.6.85, POP, HEA, GDP, INF, MR.AD.M:MR.S))
	)() |>
	na.omit() |>
	filter(COUNTRY %in% (COUNTRY |> unique() |> head(25)))

M.DATA <- DATA |>
	(\(x) x[order(x$YEAR),])() |>
	group_by(COUNTRY) |>
	mutate(PHCR.6.85= PHCR.6.85 - PHCR.3.65) |>
	mutate(PHCR.3.65= PHCR.3.65 - PHCR.2.15) |>
	mutate(PHCR.6.85.LAG= c(NA, head(PHCR.6.85, -1))) |>
	mutate(PHCR.3.65.LAG= c(NA, head(PHCR.3.65, -1))) |>
	mutate(PHCR.2.15.LAG= c(NA, head(PHCR.2.15, -1))) |>
	ungroup()

MODEL.D1 <- plm(MR.AD.F ~ PHCR.6.85 + PHCR.3.65 + PHCR.2.15 + HEA,
                data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

MODEL.D2 <- plm(MR.AD.M ~ PHCR.6.85 + PHCR.3.65 + PHCR.2.15 + HEA,
                data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

MODEL.D3 <- plm(MR.U5 ~ PHCR.6.85 + PHCR.3.65 + PHCR.2.15 + HEA,
                data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

MODEL.D4 <- plm(MR.IN ~ PHCR.6.85 + PHCR.3.65 + PHCR.2.15 + HEA,
								data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

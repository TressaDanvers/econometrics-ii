library(tidyverse)
library(plm)
library(stargazer)

possible.countries <- read.csv("data-sync/world-bank-data.csv")$Country.Name

FWC <- read.csv("data-sync/first-world-countries-2024.csv") |>
	(\(d) d[order(d$Hdi2021, decreasing=TRUE),])() |>
	filter(country %in% possible.countries) |>
	filter(!(country %in% c("Denmark", "Netherlands", "Belgium", "United Kingdom", "United Arab Emirates"))) |>
	head(25) |>
	(\(d) d$country)()

remove(possible.countries)

DATA <- read.csv("data-sync/world-bank-data.csv") |>
	select(-Series.Code) |>
	filter(Country.Name %in% FWC) |>
	pivot_longer(cols=X1960..YR1960.:X2023..YR2023., names_to="YEAR", values_to="V") |>
	mutate(YEAR=YEAR |> substr(10,13) |> as.numeric()) |>
	pivot_wider(names_from=Series.Name, names_glue="{Series.Name}", values_from=V) |>

	(\(x) x |>
		mutate(PHCR.NPL=x$"Poverty headcount ratio at national poverty lines (% of population)" |> as.numeric()) |>
		mutate(PHCR.SPL=x$"Poverty headcount ratio at societal poverty line (% of population)" |> as.numeric()) |>
		mutate(PHCR.2.15=x$"Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)" |> as.numeric()) |>
		mutate(PHCR.3.65=x$"Poverty headcount ratio at $3.65 a day (2017 PPP) (% of population)" |> as.numeric()) |>
		mutate(PHCR.6.85=x$"Poverty headcount ratio at $6.85 a day (2017 PPP) (% of population)" |> as.numeric()) |>

		mutate(GDP=x$"GDP (current US$)" |> as.numeric()) |>
		mutate(INFLATION=x$"Inflation, consumer prices (annual %)" |> as.numeric()) |>

		mutate(MR.AD.M=x$"Mortality rate, adult, male (per 1,000 male adults)" |> as.numeric()) |>
		mutate(MR.AD.F=x$"Mortality rate, adult, female (per 1,000 female adults)" |> as.numeric()) |>
		mutate(MR.U5=x$"Mortality rate, under-5 (per 1,000 live births)" |> as.numeric()) |>
		mutate(MR.IN=x$"Mortality rate, infant (per 1,000 live births)" |> as.numeric()) |>
		mutate(MR.NN=x$"Mortality rate, neonatal (per 1,000 live births)" |> as.numeric()) |>
		mutate(MR.S=x$"Suicide mortality rate (per 100,000 population)" |> as.numeric()) |>

		mutate(COUNTRY=Country.Name) |>
		select(c(COUNTRY,YEAR,PHCR.2.15:PHCR.6.85,GDP,INFLATION,MR.AD.M:MR.S))
	)() |>
	na.omit() |>
	filter(COUNTRY %in% (COUNTRY |> unique() |> head(25)))

M.DATA <- DATA |>
	(\(x) x[order(x$YEAR),])() |>
	group_by(COUNTRY) |>
	mutate(PHCR.6.85.LAG= c(NA, head(PHCR.6.85, -1))) |>
	mutate(PHCR.3.65.LAG= c(NA, head(PHCR.3.65, -1))) |>
	mutate(PHCR.2.15.LAG= c(NA, head(PHCR.2.15, -1))) |>
	ungroup()


MODEL.A1 <- lm(MR.AD.F ~ PHCR.6.85, data=M.DATA)
MODEL.A2 <- lm(MR.AD.M ~ PHCR.6.85, data=M.DATA)
MODEL.A3 <- lm(MR.S ~ PHCR.6.85, data=M.DATA)

MODEL.B1 <- plm(MR.AD.F ~ PHCR.6.85, data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")
MODEL.B2 <- plm(MR.AD.M ~ PHCR.6.85, data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")
MODEL.B3 <- plm(MR.S ~ PHCR.6.85, data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

MODEL.C1 <- plm(MR.AD.F ~ PHCR.6.85 + PHCR.6.85.LAG, data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")
MODEL.C2 <- plm(MR.AD.M ~ PHCR.6.85 + PHCR.6.85.LAG, data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")
MODEL.C3 <- plm(MR.S ~ PHCR.6.85 + PHCR.6.85.LAG, data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

MODEL.D1 <- plm(MR.AD.F ~ PHCR.6.85 + PHCR.6.85.LAG
                        + PHCR.3.65 + PHCR.3.65.LAG
                        + PHCR.2.15 + PHCR.2.15.LAG,
                data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

MODEL.D2 <- plm(MR.AD.M ~ PHCR.6.85 + PHCR.6.85.LAG
                        + PHCR.3.65 + PHCR.3.65.LAG
                        + PHCR.2.15 + PHCR.2.15.LAG,
                data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

MODEL.D3 <- plm(MR.S ~ PHCR.6.85 + PHCR.6.85.LAG
                     + PHCR.3.65 + PHCR.3.65.LAG
                     + PHCR.2.15 + PHCR.2.15.LAG,
                data=M.DATA, index=c("COUNTRY", "YEAR"), model="pooling")

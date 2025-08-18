library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Univariate regression

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)


tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)


## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())


## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))


#my code
#number 3
tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(nsibs, sleep_wkdy, sleep_wknd, income),
	method = lm,
	#intercept = TRUE,
	label = list(
		nsibs ~ "Number of sibilings",
		sleep_wkdy ~ "Hours of sleep during week",
		sleep_wknd ~ "Hours of sleep on weekend",
		income ~ "Annual income"))


#number 4
poisson_model <- glm(nsibs ~ income + race_eth_cat + region_cat,
											data = nlsy, family = poisson())

tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		income ~ "Annual income",
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region"
	))


#number 5
binomial_glasses_model <- glm(glasses ~ eyesight_cat + sex_cat,
										 data = nlsy, family = binomial(link = "log"))

binomial <- tbl_regression(
	binomial_glasses_model,
	exponentiate = TRUE,
	label = list(
		eyesight_cat ~ "Eyesight",
		sex_cat ~ "Sex"
	))


#number 6
library(sandwich)
poisson_glasses_model <- glm(glasses ~ eyesight_cat + sex_cat,
															data = nlsy, family = poisson(link = "log"))

poisson <- tbl_regression(
	poisson_glasses_model,
	exponentiate = TRUE,
	label = list(
		eyesight_cat ~ "Eyesight",
		sex_cat ~ "Sex"
	),
	tidy_fun = partial(tidy_robust, vcov = "HC1"))



#number 7
tbl_merge(list(binomial, poisson),
					tab_spanner = c("**Log-Binomial**", "**Poisson**"))

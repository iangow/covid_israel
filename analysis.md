Covid vaccine effectivenss: Analysis of Israeli data
================

This analysis is inspired by and similar to that on [Jeffrey Morris’s
blog](https://www.covid-datascience.com/post/israeli-data-how-can-efficacy-vs-severe-disease-be-strong-when-60-of-hospitalized-are-vaccinated).

``` r
library(readxl)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
```

## Getting the data

This code uses data from Israel’s [Ministry of Health Covid
Dashboard](https://datadashboard.health.gov.il/COVID-19/general). You
can download the data by clicking the link indicated in the image below

![here](webpage.png)

The code below assumes that the downloaded file is in the same directory
as the code (in turn assumed to be the working directory in R) and that
it has been renamed `israel_covid.xlsx`.

## Importing the data

``` r
t <- "israel_covid.xlsx"

israel_covid_raw <- 
  read_excel(t, skip = 1) %>%
  select(1:10)

names(israel_covid_raw) <- c("age_group",   
                         "case_full", "case_partial", "case_unvax",
                         "case100k_full", "case100k_partial", "case100k_unvax", 
                         "severe_full", "severe_partial",   "severe_unvax")
```

``` r
israel_covid <- 
  israel_covid_raw %>%
  pivot_longer(cols = -"age_group") %>%
  separate(name, into = c("outcome", "status"), sep = "_") %>%
  pivot_wider(names_from = "outcome", values_from = "value",
              values_fill = 0)  %>%
  mutate(pop = round(case/case100k * 1e5))

case_eff <- 
  israel_covid %>%
  mutate(case_rate = case/pop) %>%
  select(age_group, status, case_rate) %>%
  pivot_wider(names_from = "status", values_from = "case_rate", 
              values_fill = 0, names_prefix = "case_rate_") %>%
  mutate(case_risk_reduction = 1 - case_rate_full/case_rate_unvax,
         case_risk_reduction_partial = 1 - case_rate_partial/case_rate_unvax)


pop_data <-
  israel_covid %>%
  group_by(age_group) %>%
  summarize(pop = sum(pop))

severe_eff <-
  israel_covid %>%
  mutate(severe_rate = severe/pop) %>%
  select(age_group, status, severe_rate) %>%
  pivot_wider(names_from = "status", values_from = "severe_rate", 
              values_fill = 0, names_prefix = "severe_rate_") %>%
  mutate(severe_risk_reduction = 1 - severe_rate_full/severe_rate_unvax,
         severe_risk_reduction_partial = 1 - severe_rate_partial/severe_rate_unvax)

merged_data <-
  pop_data %>%
  inner_join(case_eff, by = "age_group") %>%
  inner_join(severe_eff, by = "age_group")
```

# Effectiveness in reducing cases

Effectiveness of vaccination with and without booster (`_partial`)
against being unvaccinated.

``` r
case_eff %>% select(-starts_with("case_rate")) %>% knitr::kable()
```

| age_group | case_risk_reduction | case_risk_reduction_partial |
|:----------|--------------------:|----------------------------:|
| 12-15     |                 NaN |                   0.9057396 |
| 16-19     |           0.9486107 |                   0.5429095 |
| 20-29     |           0.9481473 |                   0.4554966 |
| 30-39     |           0.9260056 |                   0.5218381 |
| 40-49     |           0.9305667 |                   0.5373762 |
| 50-59     |           0.9498982 |                   0.6313209 |
| 60-69     |           0.9485848 |                   0.5329092 |
| 70-79     |           0.9374292 |                   0.4787051 |
| 80-89     |           0.9243448 |                   0.4330919 |
| 90+       |           0.8123883 |                   0.4375568 |

# Effectiveness in reducing severe cases

Effectiveness of vaccination with and without booster (`_partial`)
against being unvaccinated.

``` r
severe_eff %>% select(-starts_with("severe_rate")) %>% knitr::kable()
```

| age_group | severe_risk_reduction | severe_risk_reduction_partial |
|:----------|----------------------:|------------------------------:|
| 12-15     |                   NaN |                           NaN |
| 16-19     |             1.0000000 |                     1.0000000 |
| 20-29     |             1.0000000 |                     0.9446586 |
| 30-39     |             1.0000000 |                     0.9892054 |
| 40-49     |             0.9887955 |                     0.9314650 |
| 50-59     |             0.9949283 |                     0.9429101 |
| 60-69     |             0.9870560 |                     0.8589830 |
| 70-79     |             0.9820659 |                     0.7766725 |
| 80-89     |             0.9670552 |                     0.6773915 |
| 90+       |             0.9820940 |                     0.8000865 |

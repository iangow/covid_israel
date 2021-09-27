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
| 12-15     |                 NaN |                   0.9042968 |
| 16-19     |           0.9457740 |                   0.5214494 |
| 20-29     |           0.9420067 |                   0.4307427 |
| 30-39     |           0.9231017 |                   0.4788148 |
| 40-49     |           0.9311355 |                   0.5277969 |
| 50-59     |           0.9441260 |                   0.5799435 |
| 60-69     |           0.9480063 |                   0.5166154 |
| 70-79     |           0.9363000 |                   0.4528362 |
| 80-89     |           0.9205333 |                   0.4436236 |
| 90+       |           0.8159194 |                   0.5171278 |

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
| 20-29     |             1.0000000 |                     0.9170177 |
| 30-39     |             1.0000000 |                     1.0000000 |
| 40-49     |             0.9936612 |                     0.9552369 |
| 50-59     |             0.9927878 |                     0.9295448 |
| 60-69     |             0.9869073 |                     0.8506755 |
| 70-79     |             0.9776597 |                     0.7929483 |
| 80-89     |             0.9608817 |                     0.6986413 |
| 90+       |             0.9346162 |                     0.8196167 |

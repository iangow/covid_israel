Covid vaccine effectivenss: Analysis of Israeli data
================

This analysis is inspired by and similar to that on [Jeffrey Morris’s
blog](https://www.covid-datascience.com/post/israeli-data-how-can-efficacy-vs-severe-disease-be-strong-when-60-of-hospitalized-are-vaccinated).

The code uses a few libraries that do not come with
[R](https://cran.rstudio.com). These can be installed by typing
`install.packages(c("readxl", "dplyr", "tidyr"))` on the R console (or
using the “Install” button on the `Packages` tab in
[RStudio](https://www.rstudio.com/products/rstudio/download/#download)).

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

According toe Jeffrey Morris, these data are based on numbers “all
patients currently hospitalized for COVID-19”, which means that these
numbers need to be interpreted with care. These do not necessarily
reflect a “lifetime risk reduction”, but they may provide a good
indication of that if that risk does not change at different rates for
different groups over time. An upside of these data is that they are
very current, so in a sense fully capture the impact of the
effectiveness of vaccines fading over time and the rise of the Delta
variant. (As different age groups probably got the vaccines at different
times, the impact of the effectiveness of vaccines fading over time
likely differs between groups. Also I haven’t bothered to check that the
Delta variant is the dominant variant in Israel at this time, though [a
recent article in The
Guardian](https://www.theguardian.com/world/2021/aug/31/israel-registers-record-daily-coronavirus-cases)
suggests it was on the rise there recently.)

As in practice, the impact of the effectiveness of vaccines fading over
time and the rise of the Delta variant occurred at the same time
(loosely speaking) in Israel, it would require more detailed data that
what we have here to disentangle these two effects on the effectiveness
of the vaccine.

## Importing the data

The following code reads in the data from the Excel spreadsheet and
renames the variables using English variable names (the original data
use Hebrew).

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

The following code rearranges the imported data in a way that makes the
analysis easier. Note that I calculate the implied number of people in
each row by comparing the number of cases with the number of cases per
100,000 people.

``` r
israel_covid <- 
  israel_covid_raw %>%
  pivot_longer(cols = -"age_group") %>%
  separate(name, into = c("outcome", "status"), sep = "_") %>%
  pivot_wider(names_from = "outcome", values_from = "value",
              values_fill = 0)  %>%
  mutate(pop = round(case/case100k * 1e5))
```

## Calculating risk reduction

I calculate the risk reduction of, say, vaccination with booster by:

1.  Dividing the case rate when, say, vaccinated with booster by the
    case rate when, say, unvaccinated (this measures the relative risk
    of the two groups). We can denote the result of this calculation as
    *r*.
2.  Subtracting the result of the previous step (*r*) from 1 (i.e.,
    1 − *r*).

I perform this calculation for both all cases and for severe cases. I
also perform the calculation comparing fully vaccinated with boosters
with unvaccinated, comparing fully vaccinated without boosters with
unvaccinated, and comparing fully vaccinated with boosters with fully
vaccinated without boosters.

``` r
case_eff <- 
  israel_covid %>%
  mutate(case_rate = case/pop) %>%
  select(age_group, status, case_rate) %>%
  pivot_wider(names_from = "status", values_from = "case_rate", 
              values_fill = 0, names_prefix = "case_rate_") %>%
  mutate(case_risk_reduction = 1 - case_rate_full/case_rate_unvax,
         case_risk_reduction_partial = 1 - case_rate_partial/case_rate_unvax,
         case_risk_reduction_booster = 1 - case_rate_full/case_rate_partial)
         
severe_eff <-
  israel_covid %>%
  mutate(severe_rate = severe/pop) %>%
  select(age_group, status, severe_rate) %>%
  pivot_wider(names_from = "status", values_from = "severe_rate", 
              values_fill = 0, names_prefix = "severe_rate_") %>%
  mutate(severe_risk_reduction = 1 - severe_rate_full/severe_rate_unvax,
         severe_risk_reduction_partial = 1 - severe_rate_partial/severe_rate_unvax,
         severe_risk_reduction_booster = 1 - severe_rate_full/severe_rate_partial)
```

## Effectiveness in reducing cases

It seems that when Jeffrey Morris made his original blog post (see link
above), the data supplied by Israel’s \[Ministry of Health broke out the
case data by three vaccination status: fully vaccinated, partially
vaccinated, and unvaccinated.

Now the data have changed. I don’t read Hebrew and Google Translate does
not make things entirely clear, but triangulating with other data
sources suggests that the three statuses now reported are: vaccinated
with booster, vaccinated without booster, and unvaccinated.

Effectiveness of vaccination with and without booster (`_partial`)
against being unvaccinated, and with the booster against without
(`_booster`).

``` r
case_eff %>% select(-starts_with("case_rate")) %>% knitr::kable()
```

| age_group | case_risk_reduction | case_risk_reduction_partial | case_risk_reduction_booster |
|:----------|--------------------:|----------------------------:|----------------------------:|
| 12-15     |           0.9081336 |                         NaN |                         NaN |
| 16-19     |           0.8902143 |                   0.4605786 |                   0.7964751 |
| 20-29     |           0.8833793 |                   0.4407714 |                   0.7914615 |
| 30-39     |           0.9085623 |                   0.4934403 |                   0.8194928 |
| 40-49     |           0.9265233 |                   0.4674451 |                   0.8620299 |
| 50-59     |           0.9416678 |                   0.4679909 |                   0.8903549 |
| 60-69     |           0.9384669 |                   0.4172564 |                   0.8944080 |
| 70-79     |           0.9364062 |                   0.4205431 |                   0.8902527 |
| 80-89     |           0.9112397 |                   0.5835077 |                   0.7868862 |
| 90+       |           0.7923512 |                   0.7664400 |                   0.1109402 |

## Effectiveness in reducing severe cases

Effectiveness of vaccination with and without booster (`_partial`)
against being unvaccinated, and with the booster against without
(`_booster`).

``` r
severe_eff %>% select(-starts_with("severe_rate")) %>% knitr::kable()
```

| age_group | severe_risk_reduction | severe_risk_reduction_partial | severe_risk_reduction_booster |
|:----------|----------------------:|------------------------------:|------------------------------:|
| 12-15     |             1.0000000 |                           NaN |                           NaN |
| 16-19     |             1.0000000 |                     1.0000000 |                           NaN |
| 20-29     |             0.9768922 |                     0.7934156 |                     0.8881436 |
| 30-39     |             1.0000000 |                     0.9670676 |                     1.0000000 |
| 40-49     |             0.9931360 |                     0.9668333 |                     0.7930449 |
| 50-59     |             0.9850508 |                     0.8889285 |                     0.8654092 |
| 60-69     |             0.9711947 |                     0.8179601 |                     0.8417636 |
| 70-79     |             0.9682850 |                     0.7255832 |                     0.8844275 |
| 80-89     |             0.9546816 |                     0.8027344 |                     0.7702672 |
| 90+       |             0.9620299 |                     1.0000000 |                          -Inf |

## Preparing the data for further analysis

I calculate the implied number of people in each cell by comparing the
number of cases with the number of cases per 100,000 people.

``` r
pop_data <-
  israel_covid %>%
  group_by(age_group) %>%
  summarize(pop = sum(pop))
```

The data frame `merged_data` combines the data into one data frame if
you want to look at it beyond the analysis I provide below.

``` r
merged_data <-
  pop_data %>%
  inner_join(case_eff, by = "age_group") %>%
  inner_join(severe_eff, by = "age_group")
```

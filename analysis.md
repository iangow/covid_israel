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

The following code reads in the data and rearranges it in a way that
makes the analysis easier.

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
```

## Calculating risk reduction

I calculate the risk reduction of, say, vaccination with booster by:

1.  Dividing the case rate when, say, vaccinated with booster by the
    case rate when, say, unvaccinated (this measures the relative risk
    of the two groups). We can denote this as *r*.
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
| 12-15     |                 NaN |                   0.9057396 |                         NaN |
| 16-19     |           0.9486107 |                   0.5429095 |                   0.8875731 |
| 20-29     |           0.9481473 |                   0.4554966 |                   0.9047707 |
| 30-39     |           0.9260056 |                   0.5218381 |                   0.8452524 |
| 40-49     |           0.9305667 |                   0.5373762 |                   0.8499141 |
| 50-59     |           0.9498982 |                   0.6313209 |                   0.8641047 |
| 60-69     |           0.9485848 |                   0.5329092 |                   0.8899246 |
| 70-79     |           0.9374292 |                   0.4787051 |                   0.8799704 |
| 80-89     |           0.9243448 |                   0.4330919 |                   0.8665477 |
| 90+       |           0.8123883 |                   0.4375568 |                   0.6664345 |

## Effectiveness in reducing severe cases

Effectiveness of vaccination with and without booster (`_partial`)
against being unvaccinated, and with the booster against without
(`_booster`).

``` r
severe_eff %>% select(-starts_with("severe_rate")) %>% knitr::kable()
```

| age_group | severe_risk_reduction | severe_risk_reduction_partial | severe_risk_reduction_booster |
|:----------|----------------------:|------------------------------:|------------------------------:|
| 12-15     |                   NaN |                           NaN |                           NaN |
| 16-19     |             1.0000000 |                     1.0000000 |                           NaN |
| 20-29     |             1.0000000 |                     0.9446586 |                     1.0000000 |
| 30-39     |             1.0000000 |                     0.9892054 |                     1.0000000 |
| 40-49     |             0.9887955 |                     0.9314650 |                     0.8365136 |
| 50-59     |             0.9949283 |                     0.9429101 |                     0.9111633 |
| 60-69     |             0.9870560 |                     0.8589830 |                     0.9082094 |
| 70-79     |             0.9820659 |                     0.7766725 |                     0.9196960 |
| 80-89     |             0.9670552 |                     0.6773915 |                     0.8978799 |
| 90+       |             0.9820940 |                     0.8000865 |                     0.9104315 |

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

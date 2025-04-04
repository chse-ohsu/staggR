library(tidytable)

#' Examine the dataset from SUDW
sq <- readRDS(file.path("~/.sud_data/qt_pos/curated/qt_pos_state_qtr_20241213.RDS"))
pq <- readRDS(file.path("~/.sud_data/qt_pos/curated/qt_pos_person_qtr_20241213.RDS"))



foo <- tidytable(guid = unique(purrr::map_chr(1:1000,
                                       function(x) {
                                         paste0(sample(c(LETTERS, 0:9), 12, replace = TRUE), collapse = "")
                                       }))) %>%
  mutate(grp = sample(LETTERS[1:15], size = 1),
         .by = guid) %>%
  mutate(intervention_dt = case_when(grp == "A" ~ lubridate::ymd("2011-02-15"),
                                     grp == "B" ~ NA,
                                     grp == "C" ~ lubridate::ymd("2011-05-17"),
                                     grp == "D" ~ lubridate::ymd("2011-09-10"),
                                     grp == "E" ~ NA,
                                     grp == "F" ~ lubridate::ymd("2013-01-01"),
                                     grp == "G" ~ lubridate::ymd("2013-06-12"),
                                     grp == "H" ~ NA,
                                     grp == "I" ~ lubridate::ymd("2015-02-01"),
                                     grp == "J" ~ lubridate::ymd("2015-04-05"),
                                     grp == "K" ~ lubridate::ymd("2015-07-01"),
                                     grp == "L" ~ lubridate::ymd("2015-10-01"),
                                     grp == "M" ~ lubridate::ymd("2015-12-01"),
                                     grp == "N" ~ NA,
                                     grp == "O" ~ lubridate::ymd("2017-11-03"))) %>%
  mutate(cohort = case_when(is.na(intervention_dt) ~ "0",
                            TRUE ~ as.character(lubridate::year(intervention_dt) - 2010)),
         tx = !is.na(intervention_dt)) %>%
  mutate(age = pmax(1, rnorm(n = 1,
                             mean = 35,
                             sd = 5)),
         sex = sample(c("F", "M"), size = 1),
         comorb = as.logical(sample(c(0,1), size = 1)),
         .by = guid)

#' Define effects for exposure and covariates
beta_tx <- -0.05



foo %>%
  # Convert cohort into dummy variables
  arrange(cohort) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = cohort, values_from = value, values_fill = FALSE, names_prefix = "cohort_") %>%

  # Make repeated measures for each guid
  mutate(yr_2010 = 1,
         yr_2011 = 1,
         yr_2012 = 1,
         yr_2013 = 1,
         yr_2014 = 1,
         yr_2015 = 1,
         yr_2016 = 1,
         yr_2017 = 1,
         yr_2018 = 1,
         yr_2019 = 1,
         yr_2020 = 1) %>%
  pivot_longer(cols = starts_with("yr_"), names_to = "yr", values_to = "fu_yr",
               names_prefix = "yr_") %>%
  arrange(guid, yr) %>%
  mutate(fu_yr = cumsum(fu_yr),
         futime = runif(n = 1, min = 1, max = 11),
         .by = guid) %>%
  mutate(dt = case_when(fu_yr = is.na(intervention_dt) ~ ymd("2010-01-01") +
                          days(as.integer(round(runif(n = 1,
                                                      min = 0,
                                                      max = as.numeric(ymd("2020-01-01") -
                                                                         ymd("2010-01-01"))), 1))),
                        TRUE ~ intervention_date + days(round(rnorm(n, mean = 0, sd = 365))))

  # Make follow-up time variable within guids
  mutate(, by = guid) %>%
  filter(fy_yr <= futime)

  mutate(grp_intercept = rnorm(n = 1, mean = 0, sd = 0.1),
         grp_tx_effect = rnorm(n = 1, mean = 0, sd = 0.01),
         .by = grp)



tidytable(id = 1:5, category = c("A", "B", "A", "C", "B")) %>%
  mutate(value = TRUE) %>%
  pivot_wider(names_from = category, values_from = value, values_fill = FALSE)

df_wide

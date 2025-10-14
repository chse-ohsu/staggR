library(tidytable)
library(lubridate)
library(janitor)

set.seed(40089)
hosp <-
  # Generate 10k unique Globally Unique IDs (guids)
  tidytable(guid = unique(purrr::map_chr(1:10000,
                                         function(x) {
                                           paste0(sample(c(LETTERS, 0:9), 12, replace = TRUE), collapse = "")
                                         }))) %>%

  # Assign each guid to a group
  mutate(grp = sample(LETTERS[1:15], size = 1),
         .by = guid) %>%

  # Assign groups to intervention years
  mutate(intervention_yr = case_when(grp == "A" ~ 2015,
                                     grp == "B" ~ NA,
                                     grp == "C" ~ 2015,
                                     grp == "D" ~ 2015,
                                     grp == "E" ~ NA,
                                     grp == "F" ~ 2016,
                                     grp == "G" ~ 2016,
                                     grp == "H" ~ NA,
                                     grp == "I" ~ 2017,
                                     grp == "J" ~ 2017,
                                     grp == "K" ~ 2017,
                                     grp == "L" ~ 2017,
                                     grp == "M" ~ 2017,
                                     grp == "N" ~ NA,
                                     grp == "O" ~ 2018)) %>%

  # Randomly generate an intervention date for each group
  mutate(base_dt = as.Date(case_when(!is.na(intervention_yr) ~
                                       paste0(intervention_yr, "-01-01"),
                                     TRUE ~ NA_character_))) %>%
  mutate(intervention_dt = case_when(!is.na(base_dt) ~ base_dt + round(runif(n = 1, min = 0, max = 364)),
                                     TRUE ~ NA_Date_),
         .by = grp) %>%
  select(-base_dt) %>%

  # Assign groups to cohorts
  mutate(cohort = case_when(is.na(intervention_yr) ~ "0",
                            TRUE ~ as.character(intervention_yr - 2010)),
         tx = !is.na(intervention_yr)) %>%

  # Generate covariates
  mutate(age = pmax(1, rnorm(n = 1,
                             mean = 35,
                             sd = 5)),
         sex = sample(c("F", "M"), size = 1),
         comorb = as.logical(sample(c(0,1), size = 1)),
         .by = guid) %>%

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
         yr_2020 = 1,
         yr_2021 = 1,
         yr_2022 = 1) %>%
  pivot_longer(cols = starts_with("yr_"), names_to = "yr", values_to = "fu_yr",
               names_prefix = "yr_") %>%

  # Make number of observations for each individual arbitrary
  arrange(guid, yr) %>%
  mutate(start_yr = case_when(!is.na(intervention_yr) ~ pmin(2020, pmax(2010,
                                                                        intervention_yr +
                                                                          as.integer(round(rnorm(n = 1, mean = 0, sd = 5)))
  )),
  TRUE ~ pmin(2020, pmax(2010, as.integer(round(runif(n = 1,
                                                      min = 2010,
                                                      max = 2021), 1))
  ))),
  .by = "guid") %>%
  mutate(end_yr = start_yr + as.integer(round(runif(n = 1, min = 0, max = 2020 - start_yr))),
         .by = c("guid")) %>%
  select(guid,
         age, sex, comorb,
         grp, cohort, intervention_yr, intervention_dt, tx,
         yr, start_yr, end_yr) %>%
  filter(yr >= start_yr & yr <= end_yr) %>%

  # Increment age each year
  mutate(age = age + as.integer(yr) - as.integer(start_yr)) %>%

  # Make comorb time-varying by year
  arrange(guid, yr) %>%
  # This bit makes subsequent comorbidity more likely if cormorbidity has been
  # previously observed.
  mutate(prev_comorb = case_when(is.na(lag(comorb)) ~ comorb,
                                 TRUE ~ as.integer(lag(comorb))),
         .by = "guid") %>%
  mutate(comorb = rbinom(.N, 1, plogis(prev_comorb))) %>%
  select(-prev_comorb) %>%

  # Convert cohort into dummy variables
  arrange(cohort) %>%
  rename(cohort_wide = cohort) %>%
  mutate(cohort = cohort_wide,
         value = TRUE) %>%
  pivot_wider(names_from = cohort_wide,
              values_from = value,
              values_fill = FALSE,
              names_prefix = "cohort_") %>%

  # Convert study year into dummy variables
  rename(yr_wide = yr) %>%
  mutate(yr = yr_wide,
         value = TRUE) %>%
  pivot_wider(names_from = yr_wide,
              values_from = value,
              values_fill = FALSE,
              names_prefix = "yr_")


#' Define regression formula
#' logit(P(y = 1)) = ⍺0 + ⍺1·cohort_5 + ⍺2·cohort_6 + ⍺3·cohort_7 + ⍺4·cohort_8 +
#'                   β1·yr_2011 + β2·yr_2012 + β3·yr_2013 + β4·yr_2014 + β5·yr_2015 +
#'                   β6·yr_2016 + β7·yr_2017 + β8·yr_2018 + β9·yr_2019 + β10·yr_2020 +
#'                   γ1·cohort_5·yr_2011 + γ2·cohort_5·yr_2012 + γ3·cohort_5·yr_2013 +
#'                   γ4·cohort_5·yr_2014 + γ5·cohort_5·yr_2015 + γ6·cohort_5·yr_2016 +
#'                   γ7·cohort_5·yr_2017 + γ8·cohort_5·yr_2018 + γ9·cohort_5·yr_2019 +
#'                   γ10·cohort_5·yr_2020 +
#'                   γ11·cohort_6·yr_2011 + γ12·cohort_6·yr_2012 + γ13·cohort_6·yr_2013 +
#'                   γ14·cohort_6·yr_2014 + γ15·cohort_6·yr_2015 + γ16·cohort_6·yr_2016 +
#'                   γ17·cohort_6·yr_2017 + γ18·cohort_6·yr_2018 + γ19·cohort_6·yr_2019 +
#'                   γ20·cohort_6·yr_2020 +
#'                   γ21·cohort_7·yr_2011 + γ22·cohort_7·yr_2012 + γ23·cohort_7·yr_2013 +
#'                   γ24·cohort_7·yr_2014 + γ25·cohort_7·yr_2015 + γ26·cohort_7·yr_2016 +
#'                   γ27·cohort_7·yr_2017 + γ28·cohort_7·yr_2018 + γ29·cohort_7·yr_2019 +
#'                   γ30·cohort_7·yr_2020 +
#'                   γ31·cohort_8·yr_2011 + γ32·cohort_8·yr_2012 + γ33·cohort_8·yr_2013 +
#'                   γ34·cohort_8·yr_2014 + γ35·cohort_8·yr_2015 + γ36·cohort_8·yr_2016 +
#'                   γ37·cohort_8·yr_2017 + γ38·cohort_8·yr_2018 + γ39·cohort_8·yr_2019 +
#'                   γ40·cohort_8·yr_2020 +
#'                   ẟ1·age + ẟ2·sexM + ẟ3·comorb

#' Simulate outcomes
fx <- tidytable(term = c(paste0("alpha", 0:4),
                         paste0("beta", 1:10),
                         paste0("gamma", 1:40),
                         paste0("delta", 1:3))) %>%
  # Flag groups of coefficients
  mutate(coef_grp = case_when(stringr::str_detect(term, "alpha") ~ "alpha",
                              stringr::str_detect(term, "beta") ~ "beta",
                              stringr::str_detect(term, "gamma[1-9]$|gamma10$") ~ "gamma_c5",
                              stringr::str_detect(term, "gamma1[1-9]$|gamma20$") ~ "gamma_c6",
                              stringr::str_detect(term, "gamma2[1-9]$|gamma30$") ~ "gamma_c7",
                              stringr::str_detect(term, "gamma3[1-9]$|gamma40$") ~ "gamma_c8",
                              stringr::str_detect(term, "delta") ~ "delta",
                              TRUE ~ "other")) %>%
  # Generate some random values
  mutate(rnd = rnorm(n = nrow(.), mean = 0, sd = 0.05),
         rnd_increase = rnorm(n = nrow(.), mean = 0.1, sd = 0.1),
         rnd_decrease = rnorm(n = nrow(.), mean = -0.1, sd = 0.1),
         rnd_mono_increase = abs(rnorm(n = nrow(.), mean = 0, sd = 0.1)),
         rnd_tiny_increase = abs(rnorm(n = nrow(.), mean = 0, sd = 0.05))) %>%
  mutate(rnd_increase = cumsum(rnd_increase),
         rnd_decrease = cumsum(rnd_decrease),
         rnd_mono_increase = cumsum(rnd_mono_increase),
         rnd_tiny_increase = cumsum(rnd_tiny_increase),
         .by = "coef_grp") %>%

  # Define effects for alphas (cohort levels) and deltas (covariates)
  mutate(effect = case_when(coef_grp %in% c("alpha", "delta") ~ rnd,
                            TRUE ~ NA)) %>%

  # Reduce overall prevalence of outcome
  mutate(effect = case_when(term == "alpha0" ~ -2,
                            TRUE ~ effect)) %>%

  # Define effects for betas (time trend, monotonically increasing)
  mutate(effect = case_when(coef_grp == "beta" ~ rnd_mono_increase,
                            TRUE ~ effect)) %>%

  # Define effect for gammas (cohort·time, randomly increasing or decreasing)
  mutate(effect = case_when(coef_grp == "gamma_c5" ~ rnd_increase,
                            coef_grp == "gamma_c6" ~ rnd_decrease,
                            coef_grp == "gamma_c7" ~ rnd_increase,
                            coef_grp == "gamma_c8" ~ rnd_decrease,
                            TRUE ~ effect)) %>%

  # Induce inflection points at time of intervention
  mutate(yr = case_when(coef_grp == "gamma_c5" ~ 2010 + as.integer(stringr::str_extract(term, "\\d{1,2}$")),
                        coef_grp == "gamma_c6" ~ 2010 + as.integer(stringr::str_extract(term, "\\d{1,2}$")) - 10,
                        coef_grp == "gamma_c7" ~ 2010 + as.integer(stringr::str_extract(term, "\\d{1,2}$")) - 20,
                        coef_grp == "gamma_c8" ~ 2010 + as.integer(stringr::str_extract(term, "\\d{1,2}$")) - 30,
                        TRUE ~ NA),
         intervention_yr = case_when(coef_grp == "gamma_c5" ~ 2015L,
                                     coef_grp == "gamma_c6" ~ 2016L,
                                     coef_grp == "gamma_c7" ~ 2017L,
                                     coef_grp == "gamma_c8" ~ 2018L,
                                     TRUE ~ NA_integer_)) %>%
  mutate(effect = case_when(!is.na(intervention_yr) & yr >= intervention_yr ~
                              effect + rnd_tiny_increase,
                            TRUE ~ effect)) %>%
  select(term, effect, yr, intervention_yr)


# Convert coefficients to list object
fx %<>%
  select(term, effect) %>%
  pivot_wider(names_from = term,
              values_from = effect) %>%
  as.list()

# Generate outcomes
hosp %<>%
  mutate(sex_int = as.integer(sex == "M")) %>%
  mutate(logit_p =
           fx$alpha0 + fx$alpha1*cohort_5 + fx$alpha2*cohort_6 + fx$alpha3*cohort_7 + fx$alpha4*cohort_8 +
           fx$beta1*yr_2011 + fx$beta2*yr_2012 + fx$beta3*yr_2013 + fx$beta4*yr_2014 + fx$beta5*yr_2015 +
           fx$beta6*yr_2016 + fx$beta7*yr_2017 + fx$beta8*yr_2018 + fx$beta9*yr_2019 + fx$beta10*yr_2020 +
           fx$gamma1*cohort_5*yr_2011 + fx$gamma2*cohort_5*yr_2012 + fx$gamma3*cohort_5*yr_2013 +
           fx$gamma4*cohort_5*yr_2014 + fx$gamma5*cohort_5*yr_2015 + fx$gamma6*cohort_5*yr_2016 +
           fx$gamma7*cohort_5*yr_2017 + fx$gamma8*cohort_5*yr_2018 + fx$gamma9*cohort_5*yr_2019 +
           fx$gamma10*cohort_5*yr_2020 +
           fx$gamma11*cohort_6*yr_2011 + fx$gamma12*cohort_6*yr_2012 + fx$gamma13*cohort_6*yr_2013 +
           fx$gamma14*cohort_6*yr_2014 + fx$gamma15*cohort_6*yr_2015 + fx$gamma16*cohort_6*yr_2016 +
           fx$gamma17*cohort_6*yr_2017 + fx$gamma18*cohort_6*yr_2018 + fx$gamma19*cohort_6*yr_2019 +
           fx$gamma20*cohort_6*yr_2020 +
           fx$gamma21*cohort_7*yr_2011 + fx$gamma22*cohort_7*yr_2012 + fx$gamma23*cohort_7*yr_2013 +
           fx$gamma24*cohort_7*yr_2014 + fx$gamma25*cohort_7*yr_2015 + fx$gamma26*cohort_7*yr_2016 +
           fx$gamma27*cohort_7*yr_2017 + fx$gamma28*cohort_7*yr_2018 + fx$gamma29*cohort_7*yr_2019 +
           fx$gamma30*cohort_7*yr_2020 +
           fx$gamma31*cohort_8*yr_2011 + fx$gamma32*cohort_8*yr_2012 + fx$gamma33*cohort_8*yr_2013 +
           fx$gamma34*cohort_8*yr_2014 + fx$gamma35*cohort_8*yr_2015 + fx$gamma36*cohort_8*yr_2016 +
           fx$gamma37*cohort_8*yr_2017 + fx$gamma38*cohort_8*yr_2018 + fx$gamma39*cohort_8*yr_2019 +
           fx$gamma40*cohort_8*yr_2020 +
           fx$delta1*age + fx$delta2*sex_int + fx$delta3*comorb,
         prob = plogis(logit_p),  # Convert logit to probability
         y = as.logical(rbinom(.N, 1, prob))) # Simulate outcome


# Restrict to necessary columns, identify intervention year, and make some columns more intuitive
hosp %<>%
  left_join(tidytable(grp = LETTERS[1:15],
                      county = c("Pine Hollow County",
                                 "Mapleford County",
                                 "Stonefield County",
                                 "Ashbrook County",
                                 "Silver Run County",
                                 "Meadowridge County",
                                 "Briar Glen County",
                                 "Northhaven County",
                                 "Driftwood County",
                                 "Clearfork County",
                                 "Cinder Bluff County",
                                 "Otter Pop County",
                                 "Banana Peel County",
                                 "Moonwhistle County",
                                 "Pickle Springs County")),
            by = "grp") %>%
  # Make age an integer and yr columns characters
  mutate(age = as.integer(age),
         intervention_yr = as.character(intervention_yr),
         yr = as.character(yr)) %>%
  rename(hospitalized = y) %>%
  select(guid, age, sex, comorb, hospitalized,
         county, intervention_yr, cohort,
         yr) %>%
  arrange(guid, yr) %>%
  as.data.frame()

# Make an aggregated version of the dataset
hosp_agg <- hosp %>%
  summarise(pct_hospitalized = mean(hospitalized),
            n_enr = n_distinct(guid),
            mean_age = mean(age),
            pct_fem = mean(sex == "F"),
            pct_cmb = mean(comorb),
            .by = c("yr", "county", "cohort", "intervention_yr")) %>%
  arrange(county, yr)

usethis::use_data(hosp, overwrite = TRUE)
usethis::use_data(hosp_agg, overwrite = TRUE)

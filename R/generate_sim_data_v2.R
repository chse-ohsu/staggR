library(tidytable)
library(lubridate)

#' Examine the dataset from SUDW
# sq <- readRDS(file.path("~/.sud_data/qt_pos/curated/qt_pos_state_qtr_20241213.RDS"))
# pq <- readRDS(file.path("~/.sud_data/qt_pos/curated/qt_pos_person_qtr_20241213.RDS"))
# pqr <- pq %>% select(hash_id, yr, qtr, state_abbr, grp, waiver_dt, cohort, study_qtr)


foo <-
  # Generate 10k unique guids
  tidytable(guid = unique(purrr::map_chr(1:10000,
                                         function(x) {
                                           paste0(sample(c(LETTERS, 0:9), 12, replace = TRUE), collapse = "")
                                         }))) %>%

  # Assign each guid to a group
  mutate(grp = sample(LETTERS[1:15], size = 1),
         .by = guid) %>%

  # Assign groups to intervention years
  mutate(intervention_yr = case_when(grp == "A" ~ 2011,
                                     grp == "B" ~ NA,
                                     grp == "C" ~ 2011,
                                     grp == "D" ~ 2011,
                                     grp == "E" ~ NA,
                                     grp == "F" ~ 2013,
                                     grp == "G" ~ 2013,
                                     grp == "H" ~ NA,
                                     grp == "I" ~ 2015,
                                     grp == "J" ~ 2015,
                                     grp == "K" ~ 2015,
                                     grp == "L" ~ 2015,
                                     grp == "M" ~ 2015,
                                     grp == "N" ~ NA,
                                     grp == "O" ~ 2017)) %>%

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
         yr_2020 = 1) %>%
  pivot_longer(cols = starts_with("yr_"), names_to = "yr", values_to = "fu_yr",
               names_prefix = "yr_") %>%

  # Make number of observations for each individual arbitrary
  arrange(guid, yr) %>%
  mutate(start_yr = case_when(!is.na(intervention_yr) ~ intervention_yr +
                                as.integer(round(rnorm(n = 1, mean = 0, sd = 2))),
                              TRUE ~ as.integer(round(runif(n = 1,
                                                            min = 2010,
                                                            max = 2021), 1)))) %>%
  mutate(end_yr = start_yr + as.integer(round(runif(n = 1, min = 0, max = 2020 - start_yr))),
         .by = c("guid")) %>%
  select(guid,
         age, sex, comorb,
         grp, cohort, intervention_yr, intervention_dt, tx,
         yr, start_yr, end_yr) %>%
  filter(yr >= start_yr & yr <= end_yr) %>%

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
#' logit(P(y = 1)) = ⍺0 + ⍺1·cohort_1 + ⍺2·cohort_3 + ⍺3·cohort_5 + ⍺4·cohort_7 +
#'                   β1·yr_2011 + β2·yr_2012 + β3·yr_2013 + β4·yr_2014 + β5·yr_2015 +
#'                   β6·yr_2016 + β7·yr_2017 + β8·yr_2018 + β9·yr_2019 + β10·yr_2020 +
#'                   γ1·cohort_1·yr_2011 + γ2·cohort_1·yr_2012 + γ3·cohort_1·yr_2013 +
#'                   γ4·cohort_1·yr_2014 + γ5·cohort_1·yr_2015 + γ6·cohort_1·yr_2016 +
#'                   γ7·cohort_1·yr_2017 + γ8·cohort_1·yr_2018 + γ9·cohort_1·yr_2019 +
#'                   γ10·cohort_1·yr_2020 +
#'                   γ11·cohort_3·yr_2011 + γ12·cohort_3·yr_2012 + γ13·cohort_3·yr_2013 +
#'                   γ14·cohort_3·yr_2014 + γ15·cohort_3·yr_2015 + γ16·cohort_3·yr_2016 +
#'                   γ17·cohort_3·yr_2017 + γ18·cohort_3·yr_2018 + γ19·cohort_3·yr_2019 +
#'                   γ20·cohort_3·yr_2020 +
#'                   γ21·cohort_5·yr_2011 + γ22·cohort_5·yr_2012 + γ23·cohort_5·yr_2013 +
#'                   γ24·cohort_5·yr_2014 + γ25·cohort_5·yr_2015 + γ26·cohort_5·yr_2016 +
#'                   γ27·cohort_5·yr_2017 + γ28·cohort_5·yr_2018 + γ29·cohort_5·yr_2019 +
#'                   γ30·cohort_5·yr_2020 +
#'                   γ31·cohort_7·yr_2011 + γ32·cohort_7·yr_2012 + γ33·cohort_7·yr_2013 +
#'                   γ34·cohort_7·yr_2014 + γ35·cohort_7·yr_2015 + γ36·cohort_7·yr_2016 +
#'                   γ37·cohort_7·yr_2017 + γ38·cohort_7·yr_2018 + γ39·cohort_7·yr_2019 +
#'                   γ40·cohort_7·yr_2020 +
#'                   ẟ1·age + ẟ2·sexM + ẟ3·comorb +

#' Simulate outcomes
tidytable(term = c(paste0("alpha", 0:4),
                   paste0("beta", 1:10),
                   paste0("gamma", 1:30),
                   paste0("delta", 1:3))) %>%
  # Flag groups of coefficients
  mutate(coef_grp = case_when(stringr::str_detect(term, "alpha") ~ "alpha",
                              stringr::str_detect(term, "beta") ~ "beta",
                              stringr::str_detect(term, "gamma[1-9]|gamma10") ~ "gamma_c1",
                              stringr::str_detect(term, "gamma1[1-9]|gamma20") ~ "gamma_c3",
                              stringr::str_detect(term, "gamma2[1-9]|gamma30") ~ "gamma_c5",
                              stringr::str_detect(term, "gamma3[1-9]|gamma40") ~ "gamma_c7",
                              stringr::str_detect(term, "delta") ~ "delta",
                              TRUE ~ "other")) %>%
  # Start with random values for all terms


  # Define effects for alphas
  mutate(effect = case_when(term == "alpha0" ~ -2,
                            stringr::str_detect(term, "alpha") ~ rnorm(n = 1, mean = 0, sd = 0.8),
                            TRUE ~ NA),
         .by = "term") %>%

  # Define effects for betas
  mutate(effect = ifelse(term == "beta1", yes = 0.1, no = effect)) %>%

  mutate(effect = case_when(stringr::str_detect(term, "beta(?:10|[2-9])") ~ abs(rnorm(n = 1,
                                                                                      mean = 0.1,
                                                                                      sd = 0.2)),
                            TRUE ~ effect),
         .by = "term") %>%
  mutate(tmp_effect = cumsum(effect),
         .by = "coef_grp") %>%
  mutate(effect = case_when(coef_grp == "beta" ~ tmp_effect,
                            TRUE ~ effect)) %>%

  # Define effect for gammas
  mutate(effect = case_when(stringr::str_detect(term, "gamma[123]?1\\b") ~ rnorm(n = 1,
                                                                                 mean = 0,
                                                                                 sd = 0.5),
                            TRUE ~ effect),
         .by = "term") %>%
  mutate(tmp_effect = cumsum(effect),
         .by = "coef_grp") %>%
  print(n = Inf)

                            ),
         .by = "term")

beta_0 <- -2      # BL log-odds
beta_yr <- 0.1    # Trend effect (increasing over time)
beta_tx <- 0.1    # Level effect of treatment




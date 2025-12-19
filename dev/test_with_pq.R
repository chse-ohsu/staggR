#!/bin/env Rscript

#' ---
#' title: 'Test staggR'
#' subtitle: 'namd_brief'
#' author:
#' - 'Kyle Hart'
#' date: '`r format(Sys.time(), "%A, %d %B, %Y")`'
#' output:
#'   html_document:
#'     df_print: kable
#'     highlight: tango
#'     theme: readable
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: false
#'     toc: yes
#'     toc_depth: 2
#'     numbered_sections: true
#'   pdf_document:
#'     df_print: kable
#'     highlight: espresso
#'     toc: yes
#'     toc_depth: 2
#' keep_md: no
#' fontsize: 11pt
#' urlcolor: blue
#' ---

#' ****************************************************************************************************
#+ echo=FALSE
tictoc::tic("Time to run script")

#' **File created by: [Kyle Hart](mailto:hartky@ohsu.edu), 2025-12-09**
#'
#' **Last Edited by: [Kyle Hart](mailto:hartky@ohsu.edu), 2024-12-09**
#'
#' \newpage

#' ****************************************************************************************************
#' # Preliminary Work
#+ prelim

#' ********************
#' ## Load Required Packages
#+ packages

library(tictoc)
library(magrittr)
library(tidytable)
library(staggR)
library(janitor)

#' ********************
#' ## Set Constants

#' Set paths
paths <- chse::read_as_list(file.path("~/.kyle/projects/arg/ARG_112_SUD_Waivers/02-Code/longitudinal/datapaths.txt"))

#' ****************************************************************************************************
#' # Data Processing
tic("Load state-quarter-level analysis-ready data")
latest <- chse::find_latest(file.path(paths$data, "curated"),
                            shards = FALSE, ext = ".RDS", aslist = TRUE)
pq <- readRDS(file.path(paths$data, "curated",
                        paste0("person_qtr_",
                               latest$person_qtr,
                               ".RDS"))) %>%
  mutate(yr = as.character(yr),
         qtr = as.character(qtr))
toc()

pq_sample <- pq %>%
  dplyr::sample_frac(0.01,
                     .by = c("yr", "state_abbr"))

#' Define study quarters
study_qtrs <- pq_sample %>%
  distinct(yr, qtr) %>%
  arrange(yr, qtr) %>%
  mutate(study_qtr = row_number())

moud <-
  pq_sample %>%
  # Exclude Puerto Rico
  filter(state_abbr != "PR") %>%

  # Exclude states with waivers before the study period
  filter(!(state_abbr %in% c("CA", "DC", "MD", "MA", "VT"))) %>%

  # Exclude states with waivers after the study period
  filter(!(state_abbr %in% c("ID", "MN", "RI"))) %>%

  # Exclude states with data quality problems
  filter(!(state_abbr %in% c("AR", "CO", "IL", "FL", "MD", "MS", "NV", "NC", "UT"))) %>%

  # Exclude 2016 Q1 and all of 2021
  filter(!(yr == "2016" & qtr == "1")) %>%

  select(yr, qtr, state, state_abbr, hash_id,
         moud_any) %>%

  # Define study quarters
  left_join(study_qtrs,
            by = c("yr", "qtr")) %>%

  # Define intervention quarters
  left_join(fread(file.path(paths$code, "..", "lookups", "waiver_dates.csv"),
                  select = c(state_abbr = "character",
                             implementation_date = "character")) %>%
              mutate(implementation_date = lubridate::mdy(implementation_date)) %>%
              mutate(yr = as.character(lubridate::year(implementation_date)),
                     qtr = as.character(lubridate::quarter(implementation_date))) %>%
              left_join(study_qtrs %>% rename(imp_qtr = study_qtr)) %>%
              rename(imp_date = implementation_date) %>%
              select(state_abbr, imp_date, imp_qtr),
            by = "state_abbr") %>%

  # Assign cohorts
  mutate(cohort = factor(case_when(is.na(imp_qtr) ~ 0,
                                   TRUE ~ imp_qtr)))


#' Double-check groups
moud %>%
  distinct(state_abbr, cohort) %>%
  arrange(cohort, state_abbr) %>% print(n = Inf)

#' Fit model
sdid_moud <- sdid(moud_any ~ cohort + study_qtr,
                  df = moud,
                  intervention_var = "imp_qtr",
                  .vcov = sandwich::vcovCL, cluster = moud$state_abbr)



#' Summarize model
summary(sdid_moud)
ave_coeff(sdid_moud, coefs = select_period(sdid_moud, "post"))

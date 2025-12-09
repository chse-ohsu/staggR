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
  mutate(yr_qtr = paste0(yr, "_",
                         stringr::str_pad(qtr, width = 2, side = "left", pad = "0"))) %>%

  left_join(fread(file.path(paths$code, "..", "lookups", "waiver_dates.csv"),
                  select = c(state_abbr = "character",
                             implementation_date = "character")) %>%
              mutate(implementation_date = lubridate::mdy(implementation_date)),
            by = "state_abbr") %>%
  mutate(implementation_qtr = case_when(is.na(implementation_date) ~ NA_character_,
                                        TRUE ~ paste0(lubridate::year(implementation_date),
                                                      "_",
                                                      sprintf("%02.0f", lubridate::quarter(implementation_date))))) %>%
  # mutate(grp = factor())

  # Assign cohorts
  arrange(implementation_qtr) %>%
  mutate(cohort = .GRP,
         .by = implementation_qtr) %>%
  mutate(cohort = factor(case_when(is.na(implementation_qtr) ~ 0,
                                   TRUE ~ cohort),
                         levels = as.character(0:11)))

# # Restrict to early-waiver, intermediate- or late-waiver, and selected comparison states
# mutate(grp = factor(grp, levels = c("Early-waiver states",
#                                     "All other waiver states",
#                                     "Non-waiver states")),
#        study_qtr = factor(as.character(study_qtr),
#                           levels = as.character(2:20)),
#        cohort = factor(as.character(cohort),
#                        levels = as.character(c(0,6:9, 11:16))),
#        waiver_qtr = case_when(cohort == 0 ~ NA_character_,
#                               TRUE ~ as.character(cohort)))

#' Double-check groups
moud %>%
  distinct(state_abbr, cohort) %>%
  arrange(cohort, state_abbr) %>% print(n = Inf)

sdid_moud <- sdid(moud_any ~ cohort + yr_qtr,
                  df = moud,
                  intervention_var = "implementation_qtr",
                  .vcov = sandwich::vcovCL, cluster = moud$state_abbr)



saveRDS(sdid_moud, file = file.path("~/.kyle/sdid_moud.RDS"))


foo <- readRDS("~/.kyle/sdid_moud.RDS")

summary(foo)
names(foo)
foo$tsi


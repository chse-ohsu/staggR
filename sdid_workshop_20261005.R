#' ---
#' title: 'Staggered DiD workshop'
#' author:
#' - 'Kyle Hart and Stephan Lindner'
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

#' ***************************************************************************
#+ echo=FALSE

#' **File created by: [Kyle Hart](mailto:hartky@ohsu.edu), 2026-10-05**
#' 
#' **Last Edited by: [Kyle Hart](mailto:hartky@ohsu.edu), 2026-10-05**
#'
#' \newpage

#' ***************************************************************************
#' # Purpose
#' 
#' This script demonstrates fitting staggered difference-in-differences models
#' using Callaway & Sant'Anna's `did` package and Hart & Lindner's `staggR`
#' package.
#' 
#' \newpage

#' ***************************************************************************
#' # Preliminary Work
#+ prelim 

#' ********************
#' ## Load Required Packages
#+ packages
library(dplyr)      # For tidy syntax
library(tidyr)
library(did)        # Callaway & Sant'Anna package
library(staggR)     # Hart & Lindner package
library(ggplot2)    # For customizing plots

#' ***************************************************************************
#' # Examine data
#' 
#' We're going to use a simulated data set from `staggR` called `hosp`. Imagine 
#' a policy intervention designed to reduce inpatient hospitalizations in 15 
#' counties. This longitudinal data set contains one row per individual-year. 
#' 
#' * Each individual is identified by a globally unique identifier (guid).
#' * We have age, sex, an indicator for comorbidities, and an indicator showing 
#'   whether the individual was hospitalized during the current year. 
#' * Each individual resides in a county, which is grouped into cohorts based on
#'   intervention year.
#' * The `yr` column indicates the year of the current observation.
(hosp <- hosp %>% as_tibble())

#' Counties are organized into cohorts by intervention year. 
#' intervention_yr is NA for counties that never had the intervention. 
hosp %>% distinct(county, cohort, intervention_yr) %>%
  arrange(cohort, county)

#' The study period includes 11 years, from 2010 through 2020.
#' Hospitalizations occur in every county-year. 
hosp %>%
  summarise(n = sum(hospitalized),
            .by = c("county", "yr")) %>%
  arrange(yr) %>%
  pivot_wider(names_from = yr,
              values_from = n)

#' ***************************************************************************
#' # Visualize hospitalization trends

hosp %>%
  summarise(pct_hospitalized = mean(hospitalized), 
            .by = c("cohort", "yr")) %>% 
  mutate(cohort = factor(cohort)) %>%
  ggplot(aes(x = yr, y = pct_hospitalized, 
             group = cohort, 
             color = cohort)) +
  
  # Plot trajectories of hospitalizations
  geom_line(linewidth = 1.0) +
  
  # Identify intervention years
  geom_point(data = hosp %>%
               summarise(pct_hospitalized = mean(hospitalized), 
                         .by = c("cohort", "yr", "intervention_yr")) %>%
               filter(yr == intervention_yr),
             aes(x = intervention_yr, 
                 y = pct_hospitalized, 
                 color = cohort), shape = 3, size = 4, stroke = 1.5) +
  
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "% Hospitalized",
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  
  theme_minimal()


#' There are also built-in functions in `staggR` for doing this.
ts_plot(hospitalized ~ cohort + yr,
        df = hosp,
        intervention_var = "intervention_yr") +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "% Hospitalized",
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"))

#' Same plot
ts_plot(y = "hospitalized",
        group = "cohort",
        time_var = "yr",
        intervention_var = "intervention_yr",
        df = hosp) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "% Hospitalized",
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"))

#' Align time since intervention
time_since_intervention <- id_tsi(df = hosp,
                                  cohort_var = "county",
                                  time_var = "yr",
                                  intervention_var = "intervention_yr")

time_since_intervention

ts_plot(hospitalized ~ county + yr,
        df = hosp,
        intervention_var = "intervention_yr",
        tsi = time_since_intervention) + 
  scale_x_continuous(name = "Years since intervention",
                     breaks = seq(-8, 5, by = 2)) +
  scale_y_continuous(name = "% Hospitalized",
                     labels = function(x) sprintf("%.0f%%", x * 100)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"))

#' ***************************************************************************
#' # Fit a staggered DiD model: Callaway & Sant'Anna's `did` package

#' att_gt() expects outcome, guid, and time variables to be numeric.
new_hosp <- hosp %>%
  mutate(hospitalized = as.integer(hospitalized),
         guid = as.numeric(factor(guid)),
         yr = as.numeric(yr),
         intervention_yr = case_when(!is.na(intervention_yr) ~ as.numeric(intervention_yr),
                                     TRUE ~ 0))
new_hosp

#' Fit model using `att_gt()`: Group-time average treatment effects
cs_mdl <- att_gt(yname = "hospitalized",
                 tname = "yr",
                 idname = "guid",
                 gname = "intervention_yr",
                 data = new_hosp,
                 xformla = ~ age + sex + comorb,
                 est_method = "dr",                # Selects the 2x2 DiD estimator
                 control_group = "notyettreated",  # Including not-yet-treated observations in control group.
                 panel = TRUE,                     # We have panel data, i.e., repeated observations for the same individuals.
                 allow_unbalanced_panel = TRUE,    # We do not have a perfectly rectangular data set. 
                 base_period = "universal")        # The time period preceding intervention should be the referent for all cohorts.

summary(cs_mdl)

#' ******************************
#' Evaluate parallel trends assumption
ggdid(cs_mdl)

#' ******************************
#' Overall effect of the intervention
aggte(cs_mdl, type = "simple")

#' ******************************
#' Dynamic effects / event study
(agg_es <- aggte(cs_mdl, type = "dynamic"))

#' Event-study plot
(plot_cs_es <- 
    ggdid(agg_es) +
    scale_color_manual(values = c("grey50", "firebrick")) +
    labs(x = "Years from intervention") +
    theme_minimal())

#' ******************************
#' Group-specific effects
(plot_cs_gs <- ggdid(aggte(cs_mdl, type = "group")))

#' ******************************
#' Calendar time effects
(plot_cs_ct <- ggdid(aggte(cs_mdl, type = "calendar")))

#' ******************************
#' Can we cluster standard errors at the county level?
cs_mdl_cl <- att_gt(yname = "hospitalized",
                    tname = "yr",
                    idname = "guid",
                    gname = "intervention_yr",
                    data = new_hosp,
                    xformla = ~ age + sex + comorb,
                    est_method = "dr",
                    control_group = "notyettreated", 
                    panel = TRUE,
                    base_period = "universal",
                    allow_unbalanced_panel = TRUE,
                    clustervars = "county")

all.equal(cs_mdl$att, cs_mdl_cl$att)
all.equal(cs_mdl$se, cs_mdl_cl$se)

#' Calculate and save aggregated estimates:
#' Overall effect
(cs_rslts <- purrr::map_dfr(c("simple", "dynamic", "group", "calendar"),
                           function(x) {
                             tidy(aggte(cs_mdl_cl, x))
                           }))


#' ******************************
#' Can we use aggregated data weighted by individuals?
#' We need to convert the time variable to numeric and define a numeric county
#' identifier, since we no longer have individual guids. The time-based 
#' intervention group must also be numeric (set to 0 for the never-treated group)
(new_hosp_agg <- hosp_agg %>%
    mutate(yr = as.numeric(yr),
           cnty_id = as.numeric(factor(county)),
           intervention_yr = case_when(!is.na(intervention_yr) ~ as.numeric(intervention_yr),
                                       TRUE ~ 0)))

#' We have one row per county-year.
nrow(new_hosp_agg); nrow(unique(new_hosp_agg[,c("yr", "county")]))

#' * New parameters: weightsname to specificy weights, fix_weights to specify
#'                   how to handle time-varying weights. 
#' * We change `idname` to `cnty_id`. 
#' * We no longer need to `allow_unbalanced_panel`, because now the unit of 
#'   analysis is county, and the panel is balanced. 
#' * We no longer need to specify `clustervars = "county"`, because `att_gt()`
#'   automatically clusters SEs at the level of `idname`. 
#' * We change estimation method from "dr" to "reg" to prevent singularities
#' * We still have estimation problems. `did::att_gt()` is better suited to
#'   data with a larger number of observations per cohort.

cs_mdl_agg <- att_gt(yname = "pct_hospitalized",
                     tname = "yr",
                     idname = "cnty_id",
                     gname = "intervention_yr",
                     data = new_hosp_agg,
                     weightsname = "n_enr",
                     fix_weights = "varying",
                     xformla = ~ mean_age + pct_fem + pct_cmb,
                     est_method = "reg",
                     control_group = "notyettreated", 
                     bstrap = FALSE,
                     panel = TRUE)



#' ***************************************************************************
#' # Fit a staggered DiD model: Hart & Lindner's `staggR` package
#' Arguments:
#'   * Regression formula. Dependent variable is on the left side. For the
#'     right side, order matters!
#'      1 Cohorts
#'      2 Time period for each observation
#'      3 Covariates
#'   * df specifies the data set
#'   * intervention_var specifies the column that contains the time period 
#'     during which each cohort implemented the intervention.

sdid_hosp <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
                  df = hosp,
                  intervention_var = "intervention_yr")

summary(sdid_hosp)

#' Key difference: supplied formula vs fitted formula
#'  * Fitted formula contains terms that do not exist in our data. 
#'    See prep_data(). 

#' Reference levels:
#'  * First levels of cohort and time period columns are referents by default. 
#'  * For cohort-time interactions, the time period immediately before 
#'    intervention is the referent by default. 

sdid_hosp$cohort
sdid_hosp$time

#' We can specify these referents manually
sdid_hosp2 <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
                   df = hosp,
                   cohort_ref = "0",
                   time_ref = "2010",
                   cohort_time_refs = list(`5` = "2014",
                                           `6` = "2015",
                                           `7` = "2016",
                                           `8` = "2017"), 
                   intervention_var = "intervention_yr")

all.equal(sdid_hosp$mdl$coefficients,
          sdid_hosp2$mdl$coefficients)
all.equal(sdid_hosp$vcov,
          sdid_hosp2$vcov)

#' ******************************
#' Can we cluster standard errors at the county level?
sdid_hosp3 <- sdid(hospitalized ~ cohort + yr + age + sex + comorb,
                   df = hosp,
                   intervention_var = "intervention_yr",
                   .vcov = sandwich::vcovCL,
                   cluster = hosp$county)

all.equal(sdid_hosp$mdl$coefficients,
          sdid_hosp3$mdl$coefficients)
all.equal(sdid_hosp$vcov,
          sdid_hosp3$vcov)


#' ***************************************************************************
#' # Combine coefficients and SEs

#' ******************************
#' Post-intervention period
(post_coefs <- select_period(sdid = sdid_hosp,
                             period = "post"))

ave_coeff(sdid = sdid_hosp,
          coefs = post_coefs,
          name = "Post-intervention")

#' ******************************
#' Pre-intervention period
(pre_coefs <- select_period(sdid = sdid_hosp,
                            period = "pre"))

ave_coeff(sdid = sdid_hosp,
          coefs = pre_coefs,
          name = "Pre-intervention")

#' ******************************
#' Post-intervention period for cohorts 5 and 6 only
(post_coefs_56 <- select_period(sdid = sdid_hosp,
                                period = "post",
                                cohorts = c("5", "6")))

ave_coeff(sdid = sdid_hosp,
          coefs = post_coefs_56, 
          name = "Post-intervention, cohorts 5 and 6")

#' ******************************
#' Just the year 2018 for cohorts 5 and 6
(terms_2018_cohorts56 <- select_terms(sdid = sdid_hosp,
                                      selection = list(cohorts = c("5", "6"),
                                                       times = "2018")))

ave_coeff(sdid = sdid_hosp,
          coefs = terms_2018_cohorts56)

#' ***************************************************************************
#' # Aggregated data

#' One row per county-year
nrow(hosp_agg); nrow(unique(new_hosp_agg[,c("yr", "county")]))

sdid_hosp_agg <- sdid(pct_hospitalized ~ cohort + yr + 
                        mean_age + pct_fem + pct_cmb,
                      df = hosp_agg,
                      weights = "n_enr",
                      intervention_var = "intervention_yr",
                      # Cluster standard errors at the county level
                      .vcov = sandwich::vcovCL,
                      cluster = hosp_agg$county)

ave_coeff(sdid = sdid_hosp_agg,
          coefs = select_period(sdid = sdid_hosp_agg,
                                period = "post"))

#' ***************************************************************************
#' # Detrending

#' ******************************
#' ## Calculate de-trending adjustments
hosp_det <- detrend(sdid = sdid_hosp,
                    df = hosp) %>%
  as_tibble()

hosp_det %>% arrange(guid)

sdid_hosp_det <- sdid(hospitalized_detrended ~ cohort + yr + 
                        age + sex + comorb,
                      df = hosp_det,
                      intervention_var = "intervention_yr",
                      .vcov = sandwich::vcovCL, cluster = hosp_det$county)

bind_rows(ave_coeff(sdid = sdid_hosp,
                    coefs = select_period(sdid = sdid_hosp, period = "post")) %>%
            mutate(model = "DiD estimate") %>%
            select(model, everything()),
          
          ave_coeff(sdid = sdid_hosp_det,
                    coefs = select_period(sdid = sdid_hosp_det, period = "post")) %>%
            mutate(model = "Trend-adjusted DiD estimate") %>%
            select(model, everything()))

#' ***************************************************************************
#' # Compare results from `did` to results from `staggR`

#' Aggregate simple, event-study, group, and calendar effects from `staggR`
hl_rslts <- bind_rows(
  #' Simple
  ave_coeff(sdid = sdid_hosp,
            coefs = select_period(sdid = sdid_hosp,
                                  period = "post"),
            name = "Simple"),
  
  #' Event-study
  ave_coeff(sdid = sdid_hosp, type = "es"),
  
  #' Group
  purrr::map_dfr(as.character(5:8), 
                 function(x) {
                   ave_coeff(sdid = sdid_hosp, 
                             coefs = select_period(sdid = sdid_hosp, 
                                                   cohorts = x),
                             name = paste0("Cohort ", x))
                 }),
  
  #' Calendar
  ave_coeff(sdid = sdid_hosp,
            type = "calendar",
            times = as.character(2015:2020))
)

#' Curate estimates from both pacakges
compare <- 
  hl_rslts %>%
  rename(hl_est = est, 
         hl_lb = lb,
         hl_ub = ub) %>%
  select(term, hl_est, hl_lb, hl_ub) %>%
  left_join(cs_rslts %>%
              rename(cs_est = estimate, 
                     cs_lb = conf.low,
                     cs_ub = conf.high) %>%
              filter(!term %in% c("ATT(Average)", "ATT(-1)")) %>%
              mutate(term = hl_rslts$term) %>%
              select(term, cs_est, cs_lb, cs_ub),
            by = "term") %>%
  mutate(term = sub("X", "", term))

#' Display a table of estimates from both packages
compare %>%
  mutate(staggR = paste0(sprintf("%.2f", round(hl_est, 2)), " (",
                         sprintf("%.2f", round(hl_lb, 2)), ", ",
                         sprintf("%.2f", round(hl_ub, 2)), ")"),
         did = paste0(sprintf("%.2f", round(cs_est, 2)), " (",
                      sprintf("%.2f", round(cs_lb, 2)), ", ",
                      sprintf("%.2f", round(cs_ub, 2)), ")")) %>%
  select(term, staggR, did)
  
#' Plot estimates from both packages for easier visual comparison
compare %>%
  pivot_longer(cols = -term,
               names_to = c("Package", ".value"),
               names_sep = "_") %>%
  mutate(Package = factor(Package,
                          levels = c("hl", "cs"),
                          labels = c("staggR", "did")),
         term = forcats::fct_rev(term)) %>%

  # Plot
  ggplot(aes(y = term, group = Package, colour = Package)) +
  # facet_wrap(~ est_type) +
  geom_point(aes(x = est), shape=16, size=3,
             position = position_dodge(width = 0.6)) +
  geom_linerange(aes(xmin = lb,
                     xmax= ub),
                 linewidth = 0.7,
                 position = position_dodge(width = 0.6)) +
  labs(x="") +
  geom_vline(xintercept = 0, linetype="dashed") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y= element_blank(),
        axis.title.y= element_blank(),
        axis.text = element_text(size = 12),
        legend.position = "right")

#' ***************************************************************************
#' # Session Info 
#+ session, echo=FALSE, comment=NA, results='hold'

print(sessionInfo(),locale=F)

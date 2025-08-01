library(magrittr)
library(tidytable)
library(janitor)
library(ggplot2)

devtools::load_all()

data(hosp)

#' Outcome by group
hosp %>%
  mutate(intervention_yr = lubridate::year(intervention_dt)) %>%
  summarise(y = mean(y),
            .by = c("grp", "cohort", "intervention_yr", "yr", "tx")) %>%
  mutate(tx = factor(tx,
                     levels = c(FALSE, TRUE),
                     labels = c("Comparison", "Exposed")),
         grp = factor(paste0("Group ", grp , " (", tx, ")")),
         cohort = factor(cohort, levels = as.character(c(0, 5:8))),
         yr = as.integer(yr)) %>%
  ggplot(aes(x = yr, y = y, group = grp)) +
  facet_wrap(~grp) +
  geom_point() +
  geom_line(alpha = 0.4) +
  geom_vline(aes(xintercept = intervention_yr),
             linetype = "dashed", color = "red")


#' Outcome by exposed group, compared to combined comparison groups

# Start by combining all comparison groups into one group and duplicating for
# each exposed group
purrr::map_dfr(hosp %>% filter(tx == TRUE) %>% select(grp) %>% unique() %>% pull(),
               function(x) {
                 hosp %>%
                   filter(tx == FALSE) %>%
                   mutate(grp = !!x) %>%
                   summarise(y = mean(y),
                             .by = c("grp", "yr", "tx"))
               }) %>%

  # Bind with all of the exposed groups
  bind_rows(hosp %>%
              filter(tx == TRUE) %>%
              summarise(y = mean(y),
                        .by = c("grp", "intervention_yr", "yr", "tx"))) %>%

  # Create factors and whatnot
  mutate(tx_bin = factor(tx, levels = c(FALSE, TRUE),
                         labels = c("Comparison", "Exposed")),
         tx = case_when(tx == FALSE ~ "Comparison",
                        TRUE ~ grp),
         yr = as.integer(yr)) %>%

  # Plot!
  ggplot(aes(x = yr, y = y,
             group = tx, colour = tx_bin)) +
  facet_wrap(~grp, ncol = 3) +
  geom_point() +
  geom_line(alpha = 0.8, linewidth = 1.0) +
  scale_colour_manual(values = c("#80B1D3", "#FB8072")) +
  scale_x_continuous(name = "",
                     limits = c(2010, 2020),
                     breaks = seq(2010, 2020, by = 2)) +
  scale_y_continuous(name = "% hospitalized",
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25),
                     labels = scales::percent) +
  geom_vline(aes(xintercept = intervention_yr - 0.5),
             linetype = "dashed", linewidth = 1,
             color = "darkgrey") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "#BEBADA", colour = "white"),
        strip.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.spacing = unit(1.5, "lines"))




library(tidyverse);library(here);library(wesanderson)

# get the row number for no intervention data
id <- 
  here("data/processed/intervention_1.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(intervention == "none") %>% 
  select(id)

# get the parameter that need to be used
var_name <- c('intervention', 'p_rewire', 'p_com', 'bc_threshold_mean', 
              'alpha', 'conf_mean', 'r_mean', 'credibility_mean', 'step',
              "p_underestimate", "p_inconsistent", "p_wwoh")
file_path <- paste0("data/processed/", var_name, "_1.csv")

joint_dat <- id
for (i in 1:length(file_path)) {
  joint_dat <- 
    here(file_path[i]) %>% 
    read_delim(delim = ",") %>% 
    left_join(joint_dat, ., by = "id")
}

# directly read from the file
# joint_dat <-
#   here("data/processed/intervention_none_all_steps.csv") %>%
#   read_csv()

# filter out the target parameter combination
outcome_mean <- 
  joint_dat %>% 
  filter(p_rewire == 0.5, bc_threshold_mean == 0.5, alpha == 0.3, conf_mean == 0.3) %>% 
  group_by(p_com, r_mean, step) %>% 
  summarise(
    p_underestimate_mean = mean(p_underestimate),
    p_inconsistent_mean = mean(p_inconsistent),
    p_wwoh_mean = mean(p_wwoh),
    .groups = "drop"
  ) %>% 
  pivot_longer(p_underestimate_mean:p_wwoh_mean, names_to = "var") %>% 
  mutate(var = factor(var, levels = c("p_underestimate_mean",
                                      "p_inconsistent_mean", "p_wwoh_mean"))) 

outcome_se <- 
  joint_dat %>% 
  filter(p_rewire == 0.5, bc_threshold_mean == 0.5, alpha == 0.3, conf_mean == 0.3) %>% 
  group_by(p_com, r_mean, step) %>% 
  summarise(
    p_underestimate_mean = mean(p_underestimate),
    p_underestimate_se = sd(p_underestimate)/sqrt(n()),
    p_inconsistent_mean = mean(p_inconsistent),
    p_inconsistent_se = sd(p_inconsistent)/sqrt(n()),
    p_wwoh_mean = mean(p_wwoh),
    p_wwoh_se = sd(p_wwoh)/sqrt(n()),
    .groups = "drop"
  ) %>% 
  mutate(
    p_underestimate_meanll = p_underestimate_mean - 1.96*p_underestimate_se,
    p_underestimate_meanul = p_underestimate_mean + 1.96*p_underestimate_se,
    p_inconsistent_meanll = p_inconsistent_mean - 1.96*p_inconsistent_se,
    p_inconsistent_meanul = p_inconsistent_mean + 1.96*p_inconsistent_se,
    p_wwoh_meanll = p_wwoh_mean - 1.96*p_wwoh_se,
    p_wwoh_meanul = p_wwoh_mean + 1.96*p_wwoh_se,
    ) %>% 
  select(p_com:step, p_underestimate_meanll:p_wwoh_meanul) %>% 
  pivot_longer(p_underestimate_meanll:p_wwoh_meanul, 
               names_to = c("var",".value"), names_sep = -2) %>% 
  mutate(var = factor(var, levels = c("p_underestimate_mean",
                                      "p_inconsistent_mean", "p_wwoh_mean"))) 

legend_label <- c("*P<sub>U</sub>*", "*P<sub>I</sub>*", "*P<sub>A</sub>*")
facet_label <- 
  labeller(
    p_com = c(
      `0.1` = "*p<sub>c</sub>* = 0.1",
      `0.3` = "*p<sub>c</sub>* = 0.3",
      `0.5` = "*p<sub>c</sub>* = 0.5"
    ),
    r_mean = c(
      `0.1` = "*r<sub>mean</sub>* = 0.1",
      `0.3` = "*r<sub>mean</sub>* = 0.3",
      `0.5` = "*r<sub>mean</sub>* = 0.5"))

outcome_mean %>% 
  ggplot(aes(x = step, color = var, fill = var)) +
  geom_line(data = outcome_mean, aes(y = value)) + 
  geom_ribbon(data = outcome_se, aes(ymin = ll, ymax = ul),
              color = NA, alpha = 0.2, show.legend = FALSE) +
  facet_grid(r_mean ~ p_com, labeller = facet_label) +
  scale_x_continuous(breaks = c(0, 50, 100, 150)) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1)) +
  scale_color_discrete(name = "", 
                       type = wes_palette("GrandBudapest1", 3, type = "discrete"),
                       labels = legend_label) +
  scale_fill_discrete(name = "", 
                       type = wes_palette("GrandBudapest1", 3, type = "discrete"),
                       labels = legend_label) +
  labs(x = "Time steps", y = "Proportion of agents") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_markdown(),
    strip.text = element_markdown(size = 11),
    strip.text.y = element_markdown(size = 11)
  )

ggsave("submission/figures/factor_for_pi.pdf", height = 8, width = 10)

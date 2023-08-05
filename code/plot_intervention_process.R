library(tidyverse);library(here);library(wesanderson)

### file 1 ###

# get the row number for each selected parameter value
id_1_pr <- 
  here("data/processed/p_rewire_1.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(p_rewire == 0.5) %>% 
  select(id)

id_1_pc <- 
  here("data/processed/p_com_1.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(p_com == 0.3) %>% 
  select(id)

id_1_bc <- 
  here("data/processed/bc_threshold_mean_1.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(bc_threshold_mean == 0.5) %>% 
  select(id)

id_1_alpha <- 
  here("data/processed/alpha_1.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(alpha == 0.3) %>% 
  select(id)

id_1_conf <- 
  here("data/processed/conf_mean_1.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(conf_mean == 0.1) %>% 
  select(id)

id_1_r <- 
  here("data/processed/r_mean_1.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(r_mean == 0.1) %>% 
  select(id)

# get the row number for the selected parameter combination

id_1 <- inner_join(id_1_pr, id_1_pc) %>% 
  inner_join(., id_1_bc) %>% 
  inner_join(., id_1_alpha) %>% 
  inner_join(., id_1_conf) %>% 
  inner_join(., id_1_r)

# get the parameter that need to be used
var_name <- c('intervention', 'credibility_mean', 'step',
              "p_underestimate", "p_inconsistent", "p_wwoh")
file_path <- paste0("data/processed/", var_name, "_1.csv")

joint_dat_1 <- id_1
for (i in 1:length(file_path)) {
  joint_dat_1 <- 
    here(file_path[i]) %>% 
    read_delim(delim = ",") %>% 
    left_join(joint_dat_1, ., by = "id")
}
joint_dat_1 <- joint_dat_1 %>% select(-id)



### file 2 ###

# get the row number for each selected parameter value
id_2_pr <- 
  here("data/processed/p_rewire_2.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(p_rewire == 0.5) %>% 
  select(id)

id_2_pc <- 
  here("data/processed/p_com_2.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(p_com == 0.3) %>% 
  select(id)

id_2_bc <- 
  here("data/processed/bc_threshold_mean_2.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(bc_threshold_mean == 0.5) %>% 
  select(id)

id_2_alpha <- 
  here("data/processed/alpha_2.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(alpha == 0.3) %>% 
  select(id)

id_2_conf <- 
  here("data/processed/conf_mean_2.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(conf_mean == 0.1) %>% 
  select(id)

id_2_r <- 
  here("data/processed/r_mean_2.csv") %>% 
  read_delim(delim = ",") %>% 
  filter(r_mean == 0.1) %>% 
  select(id)

# get the row number for the selected parameter combination

id_2 <- inner_join(id_2_pr, id_2_pc) %>% 
  inner_join(., id_2_bc) %>% 
  inner_join(., id_2_alpha) %>% 
  inner_join(., id_2_conf) %>% 
  inner_join(., id_2_r)

# get the parameter that need to be used
var_name_2 <- c('intervention', 'credibility_mean', 'step',
              "p_underestimate", "p_inconsistent", "p_wwoh")
file_path_2 <- paste0("data/processed/", var_name, "_2.csv")

joint_dat_2 <- id_2
for (i in 1:length(file_path_2)) {
  joint_dat_2 <- 
    here(file_path_2[i]) %>% 
    read_delim(delim = ",") %>% 
    left_join(joint_dat_2, ., by = "id")
}
joint_dat_2 <- joint_dat_2 %>% select(-id)


# put together
joint_dat <- bind_rows(joint_dat_1, joint_dat_2)


# write_csv(joint_dat, file = "data/processed/selected_par_all_steps.csv")
# directly from the file
# joint_dat <- here("data/processed/selected_par_all_steps.csv") %>%
#   read_csv()


# calculate mean and se for each group
outcome_mean <- 
  joint_dat %>% 
  group_by(intervention, credibility_mean, step) %>% 
  summarise(
    p_underestimate_mean = mean(p_underestimate),
    p_inconsistent_mean = mean(p_inconsistent),
    p_wwoh_mean = mean(p_wwoh),
    .groups = "drop"
  ) %>% 
  pivot_longer(p_underestimate_mean:p_wwoh_mean, names_to = "var") %>% 
  mutate(var = factor(var, levels = c("p_underestimate_mean",
                                      "p_inconsistent_mean", "p_wwoh_mean"))) %>% 
  mutate(
    v1 = ifelse(intervention == "none", NA, 30),
    v2 = ifelse(intervention == "none" | intervention == "sum-info", NA, 40),
    v3 = ifelse(intervention == "sum-info-three", 50, NA)
  )

outcome_se <- 
  joint_dat %>% 
  group_by(intervention, credibility_mean, step) %>% 
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
  select(intervention:step, p_underestimate_meanll:p_wwoh_meanul) %>% 
  pivot_longer(p_underestimate_meanll:p_wwoh_meanul, 
               names_to = c("var",".value"), names_sep = -2) %>% 
  mutate(var = factor(var, levels = c("p_underestimate_mean",
                                      "p_inconsistent_mean", "p_wwoh_mean"))) 

# plotting

legend_label <- c("*P<sub>U</sub>*", "*P<sub>I</sub>*", "*P<sub>A</sub>*")
facet_label <- 
  labeller(
    intervention = c(
      `none` = "No intervention",
      `sum-info` = "Intervention once",
      `sum-info-two` = "Intervention twice",
      `sum-info-three` = "Intervention three times"
    ),
    credibility_mean = c(
      `0.3` = "*s<sub>mean</sub>* = 0.3",
      `0.5` = "*s<sub>mean</sub>* = 0.5",
      `0.7` = "*s<sub>mean</sub>* = 0.7"))

outcome_mean %>% 
  ggplot(aes(x = step, color = var, fill = var)) +
  geom_vline(data = outcome_mean, aes(xintercept = v1), 
             color = "steelblue", size = 0.35, linetype = "dashed") +
  geom_vline(data = outcome_mean, aes(xintercept = v2), 
             color = "steelblue", size = 0.35, linetype = "dashed") +
  geom_vline(data = outcome_mean, aes(xintercept = v3), 
             color = "steelblue", size = 0.35, linetype = "dashed") +
  geom_line(data = outcome_mean, aes(y = value)) + 
  geom_ribbon(data = outcome_se, aes(ymin = ll, ymax = ul),
              color = NA, alpha = 0.2, show.legend = FALSE) +
  facet_grid(factor(intervention, 
                    levels = c("none", "sum-info", "sum-info-two", "sum-info-three"),
                    labels = c("No intervention", "Intervention once", "Intervention twice", "Intervention three times")) ~ credibility_mean, 
             labeller = facet_label) +
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

ggsave("submission/figures/intervention_process.pdf", height = 11, width = 10)

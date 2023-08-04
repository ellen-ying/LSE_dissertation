library(tidyverse);library(here);library(broom);library(wesanderson)

# parameter combination
par <- 
  here("data/processed/p_wwoh_against_empirical.csv") %>% 
  read_csv() %>% 
  select(p_rewire, p_com, bc_threshold_mean, alpha, conf_mean, r_mean)

# get the data
joint_dat <- 
  here("data/processed/joint_dat_1_time_150.csv") %>% 
  read_delim(delim = ",")

# attach the data for intervention 1
joint_dat <- 
  here("data/processed/joint_dat_2_time_150.csv") %>% 
  read_delim(delim = ",") %>% 
  bind_rows(joint_dat, .)

# obtain data for analysis of the intervention effects
dat_intervention <- 
  left_join(par, joint_dat, 
            by = c("p_rewire", "p_com", "bc_threshold_mean", "alpha", "conf_mean", "r_mean"))

### Regression analysis ###

# conf_mean dropped fro being perfectly correlated with p_com
# r_mean dropped for the lack of variation
p_underestimate_coef <- 
  dat_intervention %>% 
  lm(p_underestimate ~ p_rewire + p_com + bc_threshold_mean + alpha + intervention + credibility_mean + 
       intervention*(p_rewire + p_com + bc_threshold_mean + alpha + credibility_mean), data = .) %>% 
  tidy()
names(p_underestimate_coef) <- c("term", "estimate_p_underestimate",
                                 "std.error_p_underestimate", "statistic_p_underestimate",
                                 "p.value_p_underestimate")

p_inconsistent_coef <- 
  dat_intervention %>%
  lm(p_inconsistent ~ p_rewire + p_com + bc_threshold_mean + alpha + intervention + credibility_mean +
       intervention*(p_rewire + p_com + bc_threshold_mean + alpha + credibility_mean), data = .) %>% 
  tidy() 
names(p_inconsistent_coef) <- c("term", "estimate_p_inconsistent",
                                "std.error_p_inconsistent", "statistic_p_inconsistent",
                                "p.value_p_inconsistent")


p_wwoh_coef <- 
  dat_intervention %>% 
  lm(p_wwoh ~ p_rewire + p_com + bc_threshold_mean + alpha + intervention + credibility_mean +
       intervention*(p_rewire + p_com + bc_threshold_mean + alpha + credibility_mean), data = .) %>% 
  tidy() 
names(p_wwoh_coef) <- c("term", "estimate_p_wwoh",
                        "std.error_p_wwoh", "statistic_p_wwoh",
                        "p.value_p_wwoh")

bind_cols(p_underestimate_coef, select(p_inconsistent_coef, -term), select(p_wwoh_coef, -term)) %>% 
  write_csv("data/processed/intervention_effect_coef.csv")

### Plot the effects ###

# also output the parameter combination
par <- dat_intervention %>% 
      group_by(p_rewire, p_com, bc_threshold_mean, alpha, credibility_mean) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      select(-n)

facet_label <- 
  labeller(
    bc_threshold_mean = c(
      `0.3` = "*\u03B5<sub>mean</sub>* = 0.1",
      `0.5` = "*\u03B5<sub>mean</sub>* = 0.3",
      `0.7` = "*\u03B5<sub>mean</sub>* = 0.5"
    ),
    alpha = c(
      `0.1` = "*\u03B1* = 0.1",
      `0.3` = "*\u03B1* = 0.3",
      `0.5` = "*\u03B1* = 0.5"))

# rate of inconsistency
tibble(
  # once
  I1_base = filter(intervention_coef, term == "interventionsum-info")$estimate_p_inconsistent,
  I1xp_rewire = filter(intervention_coef, term == "p_rewire:interventionsum-info")$estimate_p_inconsistent,
  I1xp_com = filter(intervention_coef, term == "p_com:interventionsum-info")$estimate_p_inconsistent,
  I1xbc = filter(intervention_coef, term == "bc_threshold_mean:interventionsum-info")$estimate_p_inconsistent,
  I1xalpha = filter(intervention_coef, term == "alpha:interventionsum-info")$estimate_p_inconsistent,
  I1xcredibility = filter(intervention_coef, term == "alpha:interventionsum-info")$estimate_p_inconsistent,
  # twice
  I2_base = filter(intervention_coef, term == "interventionsum-info-two")$estimate_p_inconsistent,
  I2xp_rewire = filter(intervention_coef, term == "p_rewire:interventionsum-info-two")$estimate_p_inconsistent,
  I2xp_com = filter(intervention_coef, term == "p_com:interventionsum-info-two")$estimate_p_inconsistent,
  I2xbc = filter(intervention_coef, term == "bc_threshold_mean:interventionsum-info-two")$estimate_p_inconsistent,
  I2xalpha = filter(intervention_coef, term == "alpha:interventionsum-info-two")$estimate_p_inconsistent,
  I2xcredibility = filter(intervention_coef, term == "alpha:interventionsum-info-two")$estimate_p_inconsistent,
  # three times
  I3_base = filter(intervention_coef, term == "interventionsum-info-three")$estimate_p_inconsistent,
  I3xp_rewire = filter(intervention_coef, term == "p_rewire:interventionsum-info-three")$estimate_p_inconsistent,
  I3xp_com = filter(intervention_coef, term == "p_com:interventionsum-info-three")$estimate_p_inconsistent,
  I3xbc = filter(intervention_coef, term == "bc_threshold_mean:interventionsum-info-three")$estimate_p_inconsistent,
  I3xalpha = filter(intervention_coef, term == "alpha:interventionsum-info-three")$estimate_p_inconsistent,
  I3xcredibility = filter(intervention_coef, term == "alpha:interventionsum-info-three")$estimate_p_inconsistent,
  count = nrow(par)
) %>% 
  # repeat the row
  uncount(count) %>% 
  bind_cols(par, .) %>% 
  # calculate the effect
  mutate(
    I1 = I1_base + p_rewire*I1xp_rewire + p_com*I1xp_com + bc_threshold_mean*I1xbc + alpha*I1xalpha + credibility_mean*I1xcredibility,
    I2 = I2_base + p_rewire*I2xp_rewire + p_com*I2xp_com + bc_threshold_mean*I2xbc + alpha*I2xalpha + credibility_mean*I2xcredibility,
    I3 = I3_base + p_rewire*I3xp_rewire + p_com*I3xp_com + bc_threshold_mean*I3xbc + alpha*I3xalpha + credibility_mean*I3xcredibility
  ) %>% 
  select(I1, I2, I3) %>% 
  bind_cols(par, .) %>% 
  filter(p_rewire == 0.5) %>% 
  mutate(p_com = factor(p_com, levels = c(0.1, 0.3, 0.5))) %>% 
  pivot_longer(cols = I1:I3, names_to = "intervention") %>% 
  ggplot(aes(x = credibility_mean, y = value, color = intervention)) +
  geom_hline(yintercept = 0, color = "steelblue", size = 0.5, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.1), size = 3,
             aes(group = p_com, shape = p_com)) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7), labels = c(0.3, 0.5, 0.7)) +
  scale_y_continuous(breaks = c(-0.2, -0.1, 0, 0.1), labels = c(-0.2, -0.1, 0, 0.1)) +
  scale_color_discrete(name = "", 
                       type = wes_palette("GrandBudapest1", 3, type = "discrete"),
                       labels = c("Intervention once", "Intervention twice", "Intervention three times")) +
  scale_shape_manual(name = "",
                     values = c(19, 3),
                     labels = c("*p<sub>c</sub>* = 0.1", "*p<sub>c</sub>* = 0.3")) +
  facet_grid(bc_threshold_mean ~ alpha, labeller = facet_label) +
  labs(x = "Mean perceived credibility", y = "Estimated effects on the rate of inconsistency") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_markdown(),
    strip.text = element_markdown(size = 11),
    strip.text.y = element_markdown(size = 11)
  )

ggsave("submission/figures/intervention_effect_p_inconsistency.png", 
       height = 8, width = 10)


# rate of wwoh
tibble(
  # once
  I1_base = filter(intervention_coef, term == "interventionsum-info")$estimate_p_wwoh,
  I1xp_rewire = filter(intervention_coef, term == "p_rewire:interventionsum-info")$estimate_p_wwoh,
  I1xp_com = filter(intervention_coef, term == "p_com:interventionsum-info")$estimate_p_wwoh,
  I1xbc = filter(intervention_coef, term == "bc_threshold_mean:interventionsum-info")$estimate_p_wwoh,
  I1xalpha = filter(intervention_coef, term == "alpha:interventionsum-info")$estimate_p_wwoh,
  I1xcredibility = filter(intervention_coef, term == "alpha:interventionsum-info")$estimate_p_wwoh,
  # twice
  I2_base = filter(intervention_coef, term == "interventionsum-info-two")$estimate_p_wwoh,
  I2xp_rewire = filter(intervention_coef, term == "p_rewire:interventionsum-info-two")$estimate_p_wwoh,
  I2xp_com = filter(intervention_coef, term == "p_com:interventionsum-info-two")$estimate_p_wwoh,
  I2xbc = filter(intervention_coef, term == "bc_threshold_mean:interventionsum-info-two")$estimate_p_wwoh,
  I2xalpha = filter(intervention_coef, term == "alpha:interventionsum-info-two")$estimate_p_wwoh,
  I2xcredibility = filter(intervention_coef, term == "alpha:interventionsum-info-two")$estimate_p_wwoh,
  # three times
  I3_base = filter(intervention_coef, term == "interventionsum-info-three")$estimate_p_wwoh,
  I3xp_rewire = filter(intervention_coef, term == "p_rewire:interventionsum-info-three")$estimate_p_wwoh,
  I3xp_com = filter(intervention_coef, term == "p_com:interventionsum-info-three")$estimate_p_wwoh,
  I3xbc = filter(intervention_coef, term == "bc_threshold_mean:interventionsum-info-three")$estimate_p_wwoh,
  I3xalpha = filter(intervention_coef, term == "alpha:interventionsum-info-three")$estimate_p_wwoh,
  I3xcredibility = filter(intervention_coef, term == "alpha:interventionsum-info-three")$estimate_p_wwoh,
  count = nrow(par)
) %>% 
  # repeat the row
  uncount(count) %>% 
  bind_cols(par, .) %>% 
  # calculate the effect
  mutate(
    I1 = I1_base + p_rewire*I1xp_rewire + p_com*I1xp_com + bc_threshold_mean*I1xbc + alpha*I1xalpha + credibility_mean*I1xcredibility,
    I2 = I2_base + p_rewire*I2xp_rewire + p_com*I2xp_com + bc_threshold_mean*I2xbc + alpha*I2xalpha + credibility_mean*I2xcredibility,
    I3 = I3_base + p_rewire*I3xp_rewire + p_com*I3xp_com + bc_threshold_mean*I3xbc + alpha*I3xalpha + credibility_mean*I3xcredibility
  ) %>% 
  select(I1, I2, I3) %>% 
  bind_cols(par, .) %>% 
  filter(p_rewire == 0.5) %>% 
  mutate(p_com = factor(p_com, levels = c(0.1, 0.3, 0.5))) %>% 
  pivot_longer(cols = I1:I3, names_to = "intervention") %>% 
  ggplot(aes(x = credibility_mean, y = value, color = intervention)) +
  geom_hline(yintercept = 0, color = "steelblue", size = 0.5, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.1), size = 3,
             aes(group = p_com, shape = p_com)) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7), labels = c(0.3, 0.5, 0.7)) +
  #scale_y_continuous(breaks = c(-0.2, -0.1, 0, 0.1), labels = c(-0.2, -0.1, 0, 0.1)) +
  scale_color_discrete(name = "", 
                       type = wes_palette("GrandBudapest1", 3, type = "discrete"),
                       labels = c("Intervention once", "Intervention twice", "Intervention three times")) +
  scale_shape_manual(name = "",
                     values = c(19, 3),
                     labels = c("*p<sub>c</sub>* = 0.1", "*p<sub>c</sub>* = 0.3")) +
  facet_grid(bc_threshold_mean ~ alpha, labeller = facet_label) +
  labs(x = "Mean perceived credibility", y = "Estimated effects on the rate of WWOH") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_markdown(),
    strip.text = element_markdown(size = 11),
    strip.text.y = element_markdown(size = 11)
  )

ggsave("submission/figures/intervention_effect_p_wwoh.png", 
       height = 8, width = 10)

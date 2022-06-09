# everyone ---------------------------------------------------------------------
tempo_all <- tempo_data %>% 
  group_by(year, minute) %>% 
  count(actcat , wt = wt20) %>% 
  mutate(per = n/sum(n))

tempo_all$actcat <- as.factor(tempo_all$actcat)
tempo_all$actcat <- ordered(tempo_all$actcat, 
                           levels = c( "Carework", "Housework", "Other", 
                                       "Personal Care", "Sedentary Leisure", "Social Leisure",
                                       "Travel", "Work & Educ."))

fig2A <- tempo_all %>%
  ggplot(aes(x=minute, y=per, fill=actcat)) + 
  geom_area(alpha    = 0.8, 
            size     = 1, 
            colour   = "black", 
            position = position_fill(reverse = TRUE)) +
  facet_grid(cols = vars(year)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.position     = "bottom",
        legend.title        = element_blank(),
        plot.subtitle       = element_text(size = 11, vjust = 1),
        plot.caption        = element_text(vjust = 1, size =8, colour = "grey"), 
        strip.text.y.left   = element_text(angle = 0, face = "bold"),
        strip.text.x        = element_text(face = "bold"),
        strip.placement     = "outside") +
  scale_fill_manual(values  = c("#E16A86", "#C7821C", 
                                "#909800", "#e5d8bd", 
                                "#00A846", "#00A2D3",
                                "#9183E6", "#D766C9")) +
  scale_x_continuous(limits = c(0, 1440),
                     breaks = c(0, 500, 1000, 1440)) +
  labs(x        = NULL, 
       y        = NULL)
# ggtitle("Figure 2. Tempograms: Proportion in Each Activity") 


fig2A

ggsave(file.path(outDir, "sequences_fig2A.png"), fig2A, width = 12, height = 6, units = "in", dpi = 300)


# MEN --------------------------------------------------------------------------

tempo_men <- tempo_data %>% 
  filter(sex == "Man") %>%
  group_by(year, minute) %>% 
  count(actcat , wt = wt20) %>% 
  mutate(per = n/sum(n))

tempo_men$actcat <- as.factor(tempo_men$actcat)
tempo_men$actcat <- ordered(tempo_men$actcat, 
                            levels = c( "Carework", "Housework", "Other", 
                                        "Personal Care", "Sedentary Leisure", "Social Leisure",
                                        "Travel", "Work & Educ."))

fig2B <- tempo_men %>%
  ggplot(aes(x=minute, y=per, fill=actcat)) + 
  geom_area(alpha    = 0.8, 
            size     = 1, 
            colour   = "black", 
            position = position_fill(reverse = TRUE)) +
  facet_grid(cols = vars(year)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.position     = "bottom",
        legend.title        = element_blank(),
        plot.subtitle       = element_text(size = 11, vjust = 1),
        plot.caption        = element_text(vjust = 1, size =8, colour = "grey"), 
        strip.text.y.left   = element_text(angle = 0, face = "bold"),
        strip.text.x        = element_text(face = "bold"),
        strip.placement     = "outside") +
  scale_fill_manual(values  = c("#E16A86", "#C7821C", 
                                "#909800", "#e5d8bd", 
                                "#00A846", "#00A2D3",
                                "#9183E6", "#D766C9")) +
  scale_x_continuous(limits = c(0, 1440),
                     breaks = c(0, 500, 1000, 1440)) +
  labs(x        = NULL, 
       y        = NULL)
# ggtitle("Figure 2. Tempograms: Proportion of Men in Each Activity by Minute") 


fig2B

ggsave(file.path(outDir, "sequences_fig2B.png"), fig2B, width = 12, height = 6, units = "in", dpi = 300)


# WOMEN --------------------------------------------------------------------------

tempo_fem <- tempo_data %>% 
  filter(sex == "Woman") %>%
  group_by(year, minute) %>% 
  count(actcat , wt = wt20) %>% 
  mutate(per = n/sum(n))

tempo_fem$actcat <- as.factor(tempo_fem$actcat)
tempo_fem$actcat <- ordered(tempo_fem$actcat, 
                            levels = c( "Carework", "Housework", "Other", 
                                        "Personal Care", "Sedentary Leisure", "Social Leisure",
                                        "Travel", "Work & Educ."))

fig2C <- tempo_fem %>%
  ggplot(aes(x=minute, y=per, fill=actcat)) + 
  geom_area(alpha    = 0.8, 
            size     = 1, 
            colour   = "black", 
            position = position_fill(reverse = TRUE)) +
  facet_grid(cols = vars(year)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        legend.position     = "bottom",
        legend.title        = element_blank(),
        plot.subtitle       = element_text(size = 11, vjust = 1),
        plot.caption        = element_text(vjust = 1, size =8, colour = "grey"), 
        strip.text.y.left   = element_text(angle = 0, face = "bold"),
        strip.text.x        = element_text(face = "bold"),
        strip.placement     = "outside") +
  scale_fill_manual(values  = c("#E16A86", "#C7821C", 
                                "#909800", "#e5d8bd", 
                                "#00A846", "#00A2D3",
                                "#9183E6", "#D766C9")) +
  scale_x_continuous(limits = c(0, 1440),
                     breaks = c(0, 500, 1000, 1440)) +
  labs(x        = NULL, 
       y        = NULL)
# ggtitle("Figure 2. Tempograms: Proportion of Women in Each Activity by Minute") 


fig2C

ggsave(file.path(outDir, "sequences_fig2C.png"), fig2C, width = 12, height = 6, units = "in", dpi = 300)

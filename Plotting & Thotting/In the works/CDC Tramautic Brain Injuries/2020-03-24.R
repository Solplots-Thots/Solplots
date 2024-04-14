library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2020-03-24')

tbi_age <- tuesdata$tbi_age

tbi_age %>% view()
tbi_age %>% str()

tbi_age$age_group <- as.factor(tbi_age$age_group)
tbi_age$injury_mechanism <- as.factor(tbi_age$injury_mechanism)

levels(tbi_age$age_group)
levels(tbi_age$injury_mechanism)

# counts
p1 <- tbi_age %>%
  filter(!age_group %in% c("Total", "0-17")) %>% 
  ggplot(aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(seq(0, 500000, by = 100000)),
                     labels = c("0","100K", "200K", "300K", "400K", "500K")) +
  scale_x_discrete(limits = rev(c("0-4", "5-14", "15-24",
                              "25-34", "35-44", "45-54",
                              "55-64", "65-74", "75+"))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(labels = c("Assault",
                                 "Intentional self-harm",
                                 "Auto Crashes",
                                 "Unspecified",
                                 "Other unintentional\ninjury",
                                 "Unintentional Fall",
                                 "Unintentinally struck\nby/against object"),
                    palette = "Spectral") +
  
  labs(title = "Estimated Number of Traumatic Brain\nInjuries by Age Group and Mechanism, 2014",
       y = "",
       x = "Age Group",
       caption = "Source: CDC\nTidy Tuesday Week 13, 2020") +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white",
                                     color = "black"),
    text = element_text(family = "Georgia"),
    axis.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(linetype = "dotted",
                                      color = "black")
  )

# pct
p2 <- tbi_age %>%
  filter(!age_group %in% c("Total", "0-17")) %>% 
  ggplot(aes(x = age_group, y = number_est, fill = injury_mechanism)) +
  geom_col(position = "fill") +
  coord_flip() +
  scale_x_discrete(limits = (rev(c("0-4", "5-14", "15-24",
                               "25-34", "35-44", "45-54",
                               "55-64", "65-74", "75+")))) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(labels = c("Assault",
                               "Intentional self-harm",
                               "Auto Crashes",
                               "Unspecified",
                               "Other unintentional\ninjury",
                               "Unintentional Fall",
                               "Unintentinally struck\nby/against object"),
                    palette = "Spectral") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Estimated Pct Number of Traumatic Brain\nInjuries by Age Group and Mechanism, 2014",
       y = "",
       x = "Age Group",
       caption = "Source: CDC\nTidy Tuesday Week 13, 2020") +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white",
                                     color = "black"),
    text = element_text(family = "Georgia"),
    axis.text = element_text(face = "bold")
  )

p1
p2

ggsave("tbi_1.png",
       plot = p1,
       width = 4.76,
       height = 4.06,
       units = "in")

ggsave("tbi_2.png",
       plot = p2,
       width = 4.76,
       height = 4.06,
       units = "in")

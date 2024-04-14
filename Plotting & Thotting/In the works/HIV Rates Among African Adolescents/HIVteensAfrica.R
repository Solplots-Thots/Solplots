df <- read.csv("https://query.data.world/s/ukvgbvp4zpb2ljqds4rlvodyfjvfwk", 
               header=TRUE, 
               stringsAsFactors=FALSE)

library(tidyverse)

df %>%
  view()


p <- df %>%
  ggplot(aes(x = Year, y = Estimated.number.of.annual.new.HIV.infections,
             group = interaction(Country, Sex), color = Sex, alpha = Sex)) +
  geom_line() +
  scale_color_manual(values = c("#FFA561", "#618BFF")) +
  scale_y_continuous(limits = c(-2000, 100000),
                     expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1989, 2021)) +
  scale_alpha_manual(values = c(1, .5)) +
  labs(title = "Gender Inequality in African Adolescent HIV Rates",
       subtitle = "Estimated Number of New HIV Infections by Country and Gender",
       y = "Estimated New HIV Infections\nAmong 10-19 year-olds",
       x = "",
       caption = "Makeover Monday Week 2 2021") +
  annotate("text", x = 2002, y = 94000,
           label = "South\nAfrica", family = "Georgia",
           color = "#FFA561", size = 3.5, lineheight = .7) +
  annotate("text", x = 2010, y = 25000,
           label = "Mozambique", family = "Georgia",
           color = "#FFA561", size = 3.5) +
  annotate("text", x = 2000, y = 24000,
           label = "South\nAfrica", family = "Georgia",
           color = "#618BFF", size = 3.5, lineheight = .7) +
  annotate("text", x = 1995, y = 33000,
           label = "Kenya", family = "Georgia",
           color = "#FFA561", size = 3.5) +
  annotate("text", x = 1999.1, y = 31500,
           label = "Zimbabwe", family = "Georgia",
           color = "#FFA561", size = 3.5) +
  annotate("segment", x = 1995, xend = 1997,
           y = 23000, yend = 30000, 
           color = "lightgrey") +
  theme_classic() + 
  theme(
    panel.grid.major = element_line(linetype = "dotted",
                                    color = "lightgrey"),
    text = element_text(family = "Georgia"),
    plot.title = element_text(face = "bold"),
    legend.position = c(.8, .8),
    legend.background = element_rect(color = "black",
                                     fill = "white"),
    legend.title = element_blank()
  )
  
p

df %>%
  filter(Year == 1995) %>%
  select(Country, Sex, Estimated.number.of.annual.new.HIV.infections) %>%
  arrange(desc(Estimated.number.of.annual.new.HIV.infections)) %>%
  view()

ggsave("HIV Rates Among African Adolescents/HIVteensAfrica.png",
       plot = p,
       width = 5.94,
       height = 5.71,
       units = "in")
  
  
  
  
  
  

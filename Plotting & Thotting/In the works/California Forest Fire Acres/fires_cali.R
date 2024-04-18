
# packages ----------------------------------------------------------------

# setwd("content/blog")

library(tidyverse)
library(pdftools)
library(janitor)


# processing --------------------------------------------------------------

pdf = "fires-acres-all-agencies-thru-2018.pdf"
tib = pdf_data(pdf) %>% 
  .[[1]] 

partial = tib %>% 
  .[23:278, c(4,6)] %>% 
  group_by(y) %>% 
  nest() %>% 
  pull(data) %>% 
  bind_cols() %>% 
  row_to_names(1) %>% 
  add_column(source = c("fires_cal",
                        "acres_cal",
                        "fires_fed",
                        "acres_fed",
                        "fires_local",
                        "acres_local",
                        "fires_total"),
             .before = "1987") %>% 
  pivot_longer(cols = -1) %>% 
  pivot_wider(names_from = "source",
              values_from = "value") %>% 
  dplyr::rename(year = name)

more = tib %>% 
  .[323:354, 6] %>% 
  select(acres_total = text)

df = partial %>% 
  bind_cols(more) %>% 
  mutate(across(2:9, ~ as.numeric(eeptools::decomma(.))),
         year = as.numeric(partial$year)) 

rest = tibble(
  year = c(2019, 2020, 2021, 2022),
  fires_cal = c(3086, 3501, 3054, NA),
  acres_cal = c(129914, 1458881, 279703, NA) ,
  fires_fed = c(997+156+34+80+15+2, 
                1421+217+79+97+13+5, 
                1267+183+115+83+20+1,
                NA),
  acres_fed = c(1112399+8539+111+334+2754+33, 
                2520946+142201+76796+22210+45+11476,
                2029239+30145+109420+98793+552+1000,
                NA),
  fires_local = c(2370+408, 2849 + 466, 2420+253, NA),
  acres_local = c(7220+1598, 17062 + 54762, 13828+6706, NA),
  fires_total = c(7148, 8648, 7396, 7490),
  acres_total = c(277285, 4304379, 2569386, 362455)
)


df = df %>% 
  rbind(rest)

df$fires_cal
# plots -------------------------------------------------------------------

font_add_google("Lora")
ash = "#806e6c"
orange = "#fd9a6f"

# CRAN version
char = palettetown::ichooseyou("charizard")

## proportion of fires and acres fought/managed by agency


p = df %>% 
    pivot_longer(-year, "type") %>% 
    filter(str_starts(type, "acres") & !str_ends(type, "total")) %>% 
    mutate(
      type = case_when(
        type == "acres_local" ~ "Local",
        type == "acres_cal" ~ "State",
        type == "acres_fed" ~ "Federal"
    ) %>% 
      as.factor(.) %>% 
      recode_factor(
        ., 
        "Local" = "Local",
        "State" = "State",
        "Federa" = "Federal")) %>% 
    ggplot(aes(x = year, y = value/1000, fill = type)) +
    geom_col(color = "white",
             width = 1) +
    labs(title = "California forest fire acres battled by agency",
         subtitle = "1987-2021, CalFire",
         x = "",
         y = "",
         fill = "",
         caption = "@solplots") +
    scale_fill_manual(values = c("Federal" = char[11],
                                 "State" = char[8],
                                 "Local" = char[2])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  annotate("text", 
           x = rep(1985, 5),
           y = seq(130, 4130, 1000),
           label = c(0:3, "4 million acres"),
           color = "white",
           family = "Lora",
           hjust = 0,
           size = 4) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid = element_line(color ="white"),
        panel.border = element_blank(),
        plot.title = element_text(size = rel(1.4),
                                  face = "bold"),
        plot.subtitle = element_text(size = rel(1.3)), 
        text = element_text(color = "white",
                            family = "Lora"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "white",
                                   size = rel(1.4)),
        panel.background = element_rect(fill = char[4]),
        plot.background = element_rect(fill = char[4]),
        legend.background = element_rect(fill = char[4],
                                         color = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1.1)),
        legend.position = "bottom",
        axis.ticks = element_blank(),
        plot.caption = element_text(size = rel(1.05),
                                    face = "bold"),
        plot.margin = unit(c(5,5,3,0), "mm"))

ggsave("fighting_acres.png",
       height = 5.1,
       width = 5.1,
       unit = "in")

p +
  transition_time(year)












df %>% 
  pivot_longer(-year, "type") %>% 
  filter(str_starts(type, "fires") & !str_ends(type, "total")) %>% 
  ggplot(aes(x = year, y = value, fill = type)) +
  geom_col()

## total acres burned

df %>% 
  ggplot(aes(x = year, y = acres_total/1000)) +
  geom_col(fill = orange) + 
  scale_y_continuous(breaks = seq(0, max(df$acres_total/1000), 1000)) +
  labs(y = "Thousands of Acres Burned",
       x = "", 
       title = "Total Acres Burned of Wildfires in California, 1987-2022*",
       subtitle = "*as of 2022/11/28",
       caption = "Source: CalFire") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_line(color = "white",
                                          size = 0.2),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(vjust = -12,
                                  size = rel(1.5)),
        plot.subtitle = element_text(vjust = -16,
                                     size = rel(1.05)),
        plot.background = element_rect(fill = ash),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white",
                                 size = 12))
# total number of fires 

df %>% 
  ggplot(aes(x = year, y = fires_total/1000)) +
  geom_line(color = orange) +
  scale_y_continuous(breaks = seq(0, max(df$fires_total/1000), 4),
                     limits = c(0, 14)) +
  labs(y = "Thousands of Fires",
       x = "",
       title = "Total Number of Fires in California, 1987-2022*",
       subtitle = "*as of 2022/11/28",
       caption = "Source: CalFire") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(color = "white",
                                          size = 0.2),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(vjust = -12,
                                  size = rel(1.5)),
        plot.subtitle = element_text(vjust = -16,
                                     size = rel(1.05)),
        plot.background = element_rect(fill = ash),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white",
                                 size = 12))


 
library(tidyverse)

# install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load("2021-01-05")

df <- tuesdata$transit_cost

# install.pacakges("janitor")

df <- df %>%
  janitor::clean_names() %>%
  filter(real_cost != "MAX")

df$country

# install.packages("countrycode")
library(countrycode)

df <- codelist %>%
  select(iso2c, country.name.en, continent) %>%
  inner_join(df, by = c("iso2c" = "country"))

p <- df %>%
  filter(rr == 1) %>%
  group_by(city, continent) %>%
  summarize(avg_cost_km = mean(cost_km_millions)) %>%
  arrange(desc(avg_cost_km)) %>%
  ggplot(aes(x = reorder(city, avg_cost_km),
             y = avg_cost_km,
             fill = continent)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(limits = c(0, 3050)) +
  coord_flip() +
  labs(title = "Railroad Projects by City and Continent",
       subtitle = "Average Cost per KM",
       y = "Average Cost per KM (in Millions of USD)",
       x = "",
       fill = "",
       caption = "Tidy Tuesday Week 2 2021") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Georgia"),
    panel.grid.major.x = element_line(linetype = "dotted",
                                      color = "darkgrey"),
    panel.background = element_rect(fill = "beige"),
    plot.background = element_rect(fill = "beige"),
    legend.background = element_rect(fill = "beige"),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

ggsave("rr_cost.png",
       plot = p,
       width = 5.81,
       height = 4.77,
       unit = "in")























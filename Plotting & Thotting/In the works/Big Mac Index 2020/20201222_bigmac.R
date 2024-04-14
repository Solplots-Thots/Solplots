library(tidyverse)

tusdata <- tidytuesdayR::tt_load("2020-12-22")

mac <- tusdata[[1]]

mac$date %>%
  str()

mac %>%
  filter(date == "2020-07-01") %>%
  select(date, name, dollar_price) %>%
  top_n(5)

mac %>%
  filter(date == "2020-07-01") %>%
  select(date, name, dollar_price) %>%
  top_n(-5)

'%!in%' <- function(x,y)!('%in%'(x,y))

p <- mac %>%
  select(date, name, dollar_price) %>%
  mutate(top_bottom = case_when(
    name %in% c("Switzerland", "Lebanon",
                "Norway", "Sweden", 
                "United States") ~ "top5",
    name %in% c("Mexico", "Russia", 
                "Turkey", "Ukraine", 
                "South Africa") ~ "bottom5",
    name %!in% c("Switzerland", "Lebanon",
                 "Norway", "Sweden", 
                 "United States",
                 "Mexico", "Russia", 
                 "Turkey", "Ukraine", 
                 "South Africa") ~ "other")
  ) %>%
  ggplot() +
  geom_line(aes(x = date, y = dollar_price,
                group = name, color = top_bottom,
                alpha = top_bottom)) +
  scale_color_manual(values = c("#FFC72C", "darkgrey", "black")) +
  scale_alpha_manual(values = c(1, .6, 1)) +
  scale_x_date(limits = as.Date(c("2000-01-01", "2023-01-01"))) + 
  labs(title = "Big Mac Index, 2000-2020",
       subtitle = "Top 5 & Bottom 5 Countries with Most Expensive Big Mac Prices, 2020",
       x = "Year",
       y = "Dollar Price (USD)") +
  annotate("text",
           x = as.Date(c("2020-09-01", "2020-09-01", "2020-09-01", "2020-09-01", "2020-09-01")),
           y = c(6.91, 6.25, 6, 5.75, 5.5),
           hjust = 0,
           label = c("Switzerland", "Lebanon", "Sweden", "United States", "Norway")) +
  annotate("text",
           x = as.Date(c("2020-09-01", "2020-09-01", "2020-09-01", "2020-09-01", "2020-09-01")),
           y = c(2.5, 2.27, 2.04, 1.85, 1.63),
           hjust = 0,
           label = c("Mexico", "Ukraine", "Turkey", "Russia", "South Africa"),
           color = "#FFC72C") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", face = "bold", color = "#FFC72C"),
    plot.background = element_rect(fill = "#DA291C"),
    panel.background = element_rect(fill = "#DA291C"),
    plot.title = element_text(size = rel(1.8)),
    plot.subtitle = element_text(size = rel(1.3)),
    axis.line = element_line(color = "#FFC72C"),
    panel.grid = element_line(color = "#FFC72C"),
    panel.grid.major = element_line(linetype = "dashed"),
    axis.text = element_text(color = "#FFC72C", size = rel(1.05)),
    axis.title = element_text(size = rel(1.2))
  )

p

ggsave("Big Mac Index 2020/big_mac_2020.png",
       plot = p,
       width = 7.49,
       height = 4.91,
       units = "in")


# New Big Mac Graph -------------------------------------------------------

library(tidyverse)

mac1 <- mac %>%
  group_by(name) %>%
  summarize(
    usd_inflation = 
      (last(dollar_price) - first(dollar_price)) / 
      first(dollar_price) * 100)

mac1 <- mac %>%
  group_by(name) %>%
  summarize(
    usd_inflation = 
      (last(dollar_price) - first(dollar_price) / 
      first(dollar_price)) * 100 ) 
   
mac2 <- mac %>%
  group_by(name) %>%
  summarize(
    local_inflation = 
      (last(local_price) - first(local_price) / 
      first(local_price)) * 100 )

view(mac1)
view(mac2)


red <- "#DA291C"
yellow <- "#FFC72C"


p2 <- mac1 %>%
  filter(usd_inflation > 100 | usd_inflation <= -10) %>%
  arrange(desc(usd_inflation)) %>%
  ggplot() +
  geom_col(aes(x = reorder(name, usd_inflation),
               y = usd_inflation),
           fill = "#FFC72C",
           color = "black") +
  coord_flip() +
  scale_y_continuous(limits = c(-60, 650),
                     breaks = c(0, 100, 200, 300, 400, 500)) +
  labs(title = "Change in Big Mac Prices, USD, 2000-2020",
       subtitle = "Price Change over Years Included in data",
       x = "",
       y = "Percent Change in USD") +
  annotate("text",
           y = rep(510, 6),
           x = c(19, 18, 17, 12, 10, 1),
           hjust = 0,
           label = c("*2004-2020", "*2005-2020", "*2004-2020", 
                     "*2004-2020", "*2001-2020", "*2002-2020"),
           fontface = "bold",
           family = "Helvetica",
           color = yellow,
           size = 4.5) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", face = "bold", color = yellow),
    plot.background = element_rect(fill = red),
    panel.background = element_rect(fill = red),
    plot.title = element_text(size = rel(1.4)),
    axis.line = element_line(color = yellow),
    panel.grid = element_line(color = yellow),
    panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(color = yellow, size = rel(1.05)),
    axis.title = element_text(size = rel(1.2)),
    axis.ticks.x = element_line(color = yellow),
    axis.ticks.y = element_blank(),
    plot.caption = element_text(lineheight = .35)
  )


ggsave("mac_usd.pdf",
       plot = p2,
       width = 6.2,
       height = 5.04,
       units = "in")


mac %>%
  group_by(name) %>%
  summarize(first(date)) %>%
  view()

mac %>%
  filter(name == "Turkey") %>%
  view()

red <- "#DA291C"
yellow <- "#FFC72C"

mac2 %>%
  view()

p3 <- mac2 %>%
  filter(local_inflation > 100 | local_inflation < 0) %>%
  arrange((local_inflation)) %>%
  ggplot() +
  geom_col(aes(x = reorder(name, local_inflation),
               y = local_inflation),
           fill = "#FFC72C",
           color = "black"
  ) +
  scale_y_continuous(limits = c(-500, 12000),
                     breaks = c(0, 2500, 5000, 7500, 10000)) +
  annotate("text",
           y = rep(10050, 10),
           x = c(25, 23, 22, 21,
                 20, 19, 13, 8,
                 3, 1),
           hjust = 0,
           label = c("*2004-2020", "*2004-2020", "*2004-2020", "*2003-2020",
                     "*2004-2020", "*2004-2020", "*2001-2020", "*2011-2020",
                     "*2004-2020", "*2002-2020"),
           fontface = "bold",
           family = "Helvetica",
           color = yellow) +
  coord_flip() +
  labs(title = "Change in Big Mac Prices, Local Currency, 2000-2020",
       subtitle = "Price Change over Years Included in data",
       x = "",
       y = "Percent Change in Local Currency"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", face = "bold", color = yellow),
    plot.background = element_rect(fill = red),
    panel.background = element_rect(fill = red),
    axis.line = element_line(color = yellow),
    panel.grid = element_line(color = yellow),
    panel.grid.major = element_line(linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(color = yellow, size = rel(1.05)),
    axis.title = element_text(size = rel(1.2)),
    axis.ticks.x = element_line(color = yellow),
    axis.ticks.y = element_blank()
  )

ggsave("mac_local.pdf",
       plot = p3,
       width = 6.2,
       height = 5.4,
       units = "in")


# US over time ------------------------------------------------------------

mac %>%
  filter(iso_a3 == "USA") %>% 
  select(iso_a3, date, dollar_price) %>%
  mutate(min_wage = case_when(
    date < as.Date("2008-01-01", format = "%Y-%m-%d") ~ 5.15,
    date < as.Date("2009-01-01", format = "%Y-%m-%d") &
      date > as.Date("2008-01-01", format = "%Y-%m-%d") ~ 5.85,
    date < as.Date("2010-01-01", format = "%Y-%m-%d") &
      date > as.Date("2009-01-01", format = "%Y-%m-%d") ~ 6.55,
    date > as.Date("2009-12-31", format = "%Y-%m-%d") ~ 7.25
  )) %>%
  ggplot(aes(x = date, y = min_wage/dollar_price)) +
  geom_line()


mac2 <- mac %>%
  filter(iso_a3 == "USA") %>% 
  select(iso_a3, date, dollar_price) %>%
  mutate(min_wage = case_when(
    date < as.Date("2008-01-01", format = "%Y-%m-%d") ~ 5.15,
    date < as.Date("2009-01-01", format = "%Y-%m-%d") &
      date > as.Date("2008-01-01", format = "%Y-%m-%d") ~ 5.85,
    date < as.Date("2010-01-01", format = "%Y-%m-%d") &
      date > as.Date("2009-01-01", format = "%Y-%m-%d") ~ 6.55,
    date > as.Date("2009-12-31", format = "%Y-%m-%d") ~ 7.25
  ))


mac3 <- mac2 %>% 
  add_row(iso_a3 = "USA", date = as.Date("2021-01-01", format = "%Y-%m-%d"),
          dollar_price = 5.66, min_wage = 15)

mac3 %>% 
  ggplot(aes(x = date, y = min_wage/dollar_price)) +
  geom_line()







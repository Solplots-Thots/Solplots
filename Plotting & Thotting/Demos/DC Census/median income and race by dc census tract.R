library(tidyverse)
library(tidycensus)

census_api_key("696165c2f1261f2f4ddbb94380558a8dd0e4a8bd")
options(tigris_use_cache = TRUE)

dc <- get_acs(geography = "tract", 
              variables = c("White" = "B19013A_001", # white
                            "Black" = "B19013B_001", # black
                            "Asian" = "B19013D_001", # asian
                            "Hispanic "= "B19013I_001"), # hispanic
              state = "DC", 
              year = 2022, 
              geometry = TRUE) |> 
  replace_na(list(estimate = 0))

remove <- 
  dc |> 
  filter(estimate == 0) |> 
  group_by(GEOID) |> 
  count() |> 
  filter(n == 4) |> 
  pull(GEOID) 

map <- 
  dc |> 
  filter(!GEOID %in% remove) |> 
  ggplot(aes(fill = estimate)) + 
  geom_sf() + 
  labs(title = "Race and Income in DC",
       subtitle = "2022 Inflation-adjusted USD\nMedian Household Income",
       caption = "Source: 2018-2022 5-year\nAmerican Community Servey") +
  scale_fill_viridis_c(labels = scales::dollar,
                       option = "inferno",
                       direction = -1) +
  facet_wrap(~ variable, nrow = 2) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        legend.spacing.y = unit(.3, "cm"),
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Garamond"))

map

ggsave("median_income_by_race_dc_census_tracts.png",
       plot = map,
       device = "png",
       width = 6.3,
       height = 4.3,
       unit = "in")


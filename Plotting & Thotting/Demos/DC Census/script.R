
# Plotting & Thotting ---------------------------------------------------------------

# Everything is a plot derived from a thot 

# Race and Income in DC ---------------------------------------------------

# Data Source: U.S. Census

# The U.S. Census is arguable the most famous data set that exists
# regarding the "state" of the United States. 

# The etymology of the word "statistics", according to Merriam Webster,
# is thus:

# German "Statistik" study of political facts and figures, 
# from New Latin "statisticus" of politics, 
# from Latin "status" state

# Let's agree to a consensus on the state of the state

# Let's plot & thot

# Packages ---------------------------------------------------------------

# Tidyverse (ggplot2, dplyr)

# As usual, we'll be working in the tidyverse (mainly ggplot2 and dplyr,
# grammars for plotting (data visualization) and data wrangling, respectively)

# Simple Features (sf)

# This is the very first Plotting & Thotting in which learn how to create
# a map using R

# However, we only need one function, sf::geom_sf(), which is a "geom"
# that is used within the ggplot framework

# This forum will not cover geo-computation or cartography, but rather
# this is a nice exercise in quickly putting something together
# with Census data

# Tidycensus (tidycensus)

# Kyle Walker is the author of tidycensus (from his website):

# tidycensus is an R package that allows users to interface 
# with a select number of the US Census Bureau’s data APIs and 
# return tidyverse-ready data frames, optionally with simple feature 
# geometry included

# tidycensus is designed to help R users get Census data that is 
# pre-prepared for exploration within the tidyverse, and optionally 
# spatially with sf

# For suggested resources that go into further detail, please refer
# to the slide deck presentation 

# Getting started ---------------------------------------------------------

# install.packages(c("tidyverse", "extrafont", "tidycensus", "sf"))

library(tidyverse)
library(tidycensus)

# The U.S. Census makes you submit your email account in order to receive
# an API key.

# You need an API key from the Census in order for this script

# census_api_key("my api key")
options(tigris_use_cache = TRUE)

# To download the data we need (median household income at the tract
# level of White, Black, Asian and Hispanic residents of DC) we will
# use `get_acs()`
dc <- 
  get_acs(
    geography = "tract",
    # this could be block, block group, tract, county, etc.
    variables = c(
      "White" = "B19013A_001",
      # white
      "Black" = "B19013B_001",
      # black
      "Asian" = "B19013D_001",
      # asian
      "Hispanic " = "B19013I_001"
      # hispanic
    ),
    # this could be whole plotting and thotting topic on its own...
    state = "DC",
    # DC counts as a state here
    year = 2022,
    # most recently available sample
    geometry = TRUE
    # we want to return an sf object (similar to tidy table/tibble that 
    # also has a geometric component)
  ) |>
  replace_na(list(estimate = 0))
  # if it's NA it equals 0 in our situation

# Ok so we know that nobody lives on the National Mall (which is its own
# census tract), so if we included that row in our map, it'll be filled 
# in just like the others

# I took the creative liberty to decide to remove all tracts where the 
# value of median household income is 0

remove <- 
  dc |> 
  filter(estimate == 0) |> 
  # all rows where estimate is 0
  group_by(GEOID) |> 
  # create groups based on the ID of the geometric feature
  count() |> 
  # for each GEOID, how many rows have a 0 in estimate?
  filter(n == 4) |> 
  # all rows where value is 4 (four variables, four zeroes)
  pull(GEOID) 
  # retrieve GEOIDs of tracts we want to remove

map <- 
  dc |> 
  filter(!GEOID %in% remove) |> 
  # filter for rows where not GEOID are in remove (GEOID not in remove)
  ggplot(aes(fill = estimate)) + 
  # start ggplot object
  geom_sf() + 
  # apply sf geom
  labs(title = "Race and Income in DC",
       subtitle = "2022 Inflation-adjusted USD\nMedian Household Income",
       caption = "Source: 2018-2022 5-year\nAmerican Community Servey") +
  # set title, subtitle and caption (using new line character "\n")
  scale_fill_viridis_c(labels = scales::dollar,
                       option = "inferno",
                       direction = -1) +
  # apply a colorblind-friendly color palette
  # we also want dollar signs on our legend
  # the "inferno" color palette
  # reverse the high/low direction from default
  facet_wrap(~ variable, nrow = 2) +
  # instead of one DC
  # we get four DC's (for each variable, what is being faceted)
  # ensure there are 2 rows of plots
  theme_bw() + 
  # black and white theme 
  theme(panel.grid = element_blank(),
        # remove all grid lines (graticules are not necessary here)
        axis.text = element_blank(),
        # no need for degrees longitude and latitude
        axis.ticks = element_blank(),
        # ticks also go
        legend.title = element_blank(),
        # a title for the legend is redundant so it goes as well
        strip.background = element_rect(fill = "white"),
        # change from grey
        legend.spacing.y = unit(.3, "cm"),
        # adjust spacing a tiny bit
        plot.title = element_text(face = "bold"),
        # put the title to bold
        text = element_text(family = "Garamond")
        # change font to Garamond
  )

map

ggsave("median_income_by_race_dc_census_tracts.png",
       plot = map,
       device = "png",
       width = 6.3,
       height = 4.3,
       unit = "in")



























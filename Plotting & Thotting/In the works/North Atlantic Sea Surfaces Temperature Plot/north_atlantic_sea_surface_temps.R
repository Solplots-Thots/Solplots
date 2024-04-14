# north atlantic sea surface temperature anomaly #


# libraries ----
library(tidyverse)
library(jsonlite)

# link to data ----
url <- "https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_natlan1_sst_day.json"

# reading data ----
raw <- 
  fromJSON(url) |> 
  as_tibble() |> 
  unnest(data) 

df <- 
  raw |> 
  rename(year = name, surface_temp = data) |> 
  filter(!year %in% c("1981", 
                      "1982-2011 mean", 
                      "plus 2σ",
                      "minus 2σ")) |> 
  mutate(year = as.numeric(year)) |> 
  group_by(year) |> 
  mutate(elapsed_days = row_number()) |> 
  ungroup() 



# check to see if mean calculation is correct ----
mu_82_11 <-
  df |> 
  filter(year %in% 1982:2011) |> 
  group_by(elapsed_days) |> 
  summarize(mean_temp = mean(surface_temp))

check <- 
  raw |> 
  filter(name == "1982-2011 mean") |> 
  mutate(elapsed_days = row_number()) |> 
  left_join(mu_82_11)

# it is

# prepping data for plot ----
mu <- 
  df |> 
  filter(year %in% 1991:2020) |> 
  group_by(elapsed_days) |> 
  summarize(mean_surface_temp = mean(surface_temp))

prep <- 
  df |> 
  left_join(mu) |> 
  mutate(diff = surface_temp - mean_surface_temp) |> 
  group_by(year) |> 
  mutate(max_diff = max(diff, na.rm = T)) |> 
  ungroup()

# plotting ----
prep |> 
  ggplot(aes(x = elapsed_days, y = diff, group = year, color = diff)) +
  geom_line(show.legend = FALSE) +
  scale_color_gradient2(low = "darkblue",
                        mid = "white",
                        high = "darkred") +
  labs(title = "North Atlantic Sea Surface Temperature Anomaly: 1982-2023",
       subtitle = "Difference from 1991-2020 Mean",
       caption = "Climate Reanalyzer\nClimate Change Institute | University of Maine",
       x = "Day of year",
       y = "Difference (\u00B0C)") +
  theme_minimal()

 # color lines by El Niño/La Niña
prep |> 
  ggplot(aes(x = elapsed_days, y = diff, group = year, color = year)) +
  geom_line() +
  labs(title = "North Atlantic Sea Surface Temperature Anomaly: 1982-2023",
       subtitle = "Difference from 1991-2020 Mean",
       caption = "Climate Reanalyzer\nClimate Change Institute | University of Maine",
       x = "Day of year",
       y = "Difference (\u00B0C)") +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "pink") +
  geom_label(data = prep |> filter(year == 2023), aes(label = year))



# not needed ----

oni_years <- 
  tibble(
    year = 1982:2023,
    season = 1981:2022,
  ) |> 
  mutate(enso_type = case_when(
    season %in% c(1981,
                  1985,
                  1989,
                  1990,
                  1992,
                  1993,
                  1996,
                  2001,
                  2003,
                  2012,
                  2013,
                  2019) ~ "None",
    season %in% c(1982,
                  1997,
                  2015) ~ "Very Strong El Niño",
    season %in% c(1987, 1991) ~ "Strong El Niño",
    season %in% c(1986, 1994, 2002, 2009) ~ "Moderate El Niño",
    season %in% c(2004, 2006, 2014, 2018) ~ "Weak El Niño",
    season %in% c(1988, 1998, 1999, 2007, 2010) ~ "Strong La Niña",
    season %in% c(1995, 2911, 2020, 2021) ~ "Moderate La Niña",
    season %in% c(1983, 1984, 2000, 2005, 2008, 2016, 2017, 2022) ~ "Weak La Niña"
  ),
  enso = case_when(
    enso_type == "None" ~ "None",
    enso_type %in% c("Strong La Niña", "Moderate La Niña", "Weak La Niña") ~ "La Niña",
    TRUE ~ "El Niño"
  ))
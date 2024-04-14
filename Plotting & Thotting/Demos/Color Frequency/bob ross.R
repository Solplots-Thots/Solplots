library(tidyverse)
library(DataExplorer)
library(extrafont)
library(ggtext)

tt = tidytuesdayR::tt_load("2023-02-21")
df = tt$bob_ross

font_import()
loadfonts()


# data analysis -----------------------------------------------------------

plot_missing(df) # nothing is missing!!

## I want 2 plots from this data:
## - One showing the total times each color was used
## - One showing mean colors used per season (colored by most commonly used)


# total times used --------------------------------------------------------

colornames = c("Titanium_White",
         "Bright_Red",
         "Alizarin_Crimson",
         "Van_Dyke_Brown",
         "Cadmium_Yellow",
         "Yellow_Ochre",
         "Phthalo_Blue",
         "Midnight_Black",
         "Sap_Green",
         "Indian_Yellow",
         "Dark_Sienna",
         "Prussian_Blue",
         "Phthalo_Green",
         "Black_Gesso",
         "Burnt_Umber",
         "Liquid_Clear",
         "Liquid_Black",
         "Indian_Red") |> sort()




## colors

get_code = function(df, var) {df |> filter({{ var }})}
get_code(df, Yellow_Ochre) |> view()

cols = tibble(
  color_name = colornames,
  codes = c('#4E1500',
                     '#000000',
                     '#DB0000',
                     '#8A3324',
                     '#FFEC00',
                     '#5F2E1F',
                     '#CD5C5C',
                     '#FFB800',
                     '#000000',
                     '#FFFFFF',
                     '#000000',
                     '#0C0040',
                     '#102E3C',
                     '#021E44',
                     '#0A3410',
                     '#FFFFFF',
                     '#221B15',
                     '#C79B00'
                     )
)

## counts
counts = 
  df |> 
  select(10:ncol(df)) |> 
  mutate(across(everything(), ~ sum(.))) |> 
  distinct() |> 
  pivot_longer(1:18,
               names_to = "color_name",
               values_to = "count") |> 
  inner_join(cols) |> 
  arrange(color_name) |> 
  mutate(color_name = str_replace(color_name, "_", " ") |> str_replace("_", " "),
         color_label = if_else(color_name %in% c("Titanium White", "Liquid Clear"), "black", codes)) 
 
counts |> 
  ggplot(aes(x = count, 
             y = reorder(color_name, count),
             fill = color_name,
             label = count)) + 
  geom_col(show.legend = FALSE) +
  geom_text(aes(y = color_name),
            show.legend = FALSE,
            hjust = -.1,
            family = "ITC Korinna",
            size = 5) +
  scale_fill_manual(values = counts$codes) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 425)) +
  theme_minimal() +
  labs(title = "<b>Color Frequency, Bob Ross' <i>The Joy of Painting</i></b>",
       subtitle = "Across 403 episodes (paintings), how often is each color used?",
       caption = "Tidy Tuesday 21 February 2023<br><b>@solplots</b>") +
  theme(text = element_text(family = "ITC Korinna",
                            size = 16,
                            color = "black",
                            face = "bold"),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = '#FFEDA3'),
        panel.background = element_rect(fill = "#FFEDA3",
                                        color = "#FFEDA3"),
        plot.caption = element_markdown()) 

ggsave("bobross.png",
       height = 6,
       width = 6,
       unit = "in",
       device = png)













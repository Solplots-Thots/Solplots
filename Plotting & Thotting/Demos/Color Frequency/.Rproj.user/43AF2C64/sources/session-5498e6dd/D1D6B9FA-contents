
# Plotting & Thotting ---------------------------------------------------------------

# Everything is a plot derived from a tho(ugh)t 

# Color Frequency, Bob Ross' The Joy of Painting --------------------------

# Data Source: Tidy Tuesday Project

# Every Tuesday, volunteers prepare a data set for people 
# to practice data tidying and plotting skills with R

# For the February 21, 2023 edition of Tidy Tuesday, I 
# plotted Color Frequency, a story about Bob Ross'
# The Joy of Painting.

# I find this plot to be a great bar plot example 
# for its use of color and labeling to tell a story. 

# Upon learning that the producers intentionally limited the 
# show's palette to 18 colors, I wanted to find the answer
# to this question: how often is each color used?

# It's also a great example because this information can't be 
# immediately gleaned from the data set. The granularity is 
# at the episode level, meaning 403 episodes of the show 
# are contained in 403 observations in the data. 

# The information pertaining to colors used and those colors' 
# hex code had to be messaged out of the source and 
# be organized into a new data frame from 
# which to build a plot.


# Tidyverse ---------------------------------------------------------------

# Prep work on the data is required in order to get to this plot.

# The "tidyverse", a family of package libraries (extra
# functionality), helps us with data prep and plotting

# Tidy data analysis is the preferred method for working
# with data (this can be a deeper discussion another time)

# Getting started ---------------------------------------------------------

# install.packages(c("tidyverse", "extrafont", "ggtext"))

library(tidyverse) 
# family of packages
library(extrafont) 
# additional plots load, not necessary
library(ggtext) 
# text position functions
library(BobRossColors) 
# extra help

# Here we can download our desired zip file*
tt = tidytuesdayR::tt_load("2023-02-21")
tt

# `tt` is an object that contains two items:
# - an HTML file 
# - a list of data frames (of which there is only one)
df = tt$bob_ross

# If the extrafont package works initially for you 
# AND you've downloaded and initiated the ITC Korinna font 
# in your operating system, run the following
# to load in the font to your session
font_import()
loadfonts()

# The font is not necessary, simply an aesthetic touch
# See me during q/a if you want more info about fonts

# Shape of data -----------------------------------------------------------

# This data contains 403 observations and 27 variables
dim(df)

# Each observation is a painting completed by Bob Ross for 
# each episode that aired on PBS. 

# 403 total episodes (observations, or rows)
# 27 unique features (variables, or columns)

# In the description of the Tidy Tuesday entry, 
# we can find a data dictionary that helps 
# explain the path we'll take to the plot.

# This data set contains a lot of cool information, 
# such as URL links to not just the paintings 
# themselves but also the videos of their respective
# episodes uploaded to YouTube.

# I encourage you all, as you develop your R skills, 
# to explore this data set further and see what other 
# insights you can gather.

# If you ever get lost, the data dictionary is a great
# place to return to

# Data prep ------------------------------------------------------

# For this plot, we're only interested in the final 
# 18 variables of the data, each named after the 18 colors. 

# Each of these variables is of the "logical" type,
# meaning it can take one of two values: TRUE or FALSE

# They indicate whether or not the color was used
# in the painting.

# Here we can trim our original data frame/tibble 
# object to the variables we want
ex <- 
  df |> 
  select(
    Black_Gesso:Alizarin_Crimson 
    # placing a colon between the names 
    # of two variables will select all variables 
    # in between and inclusive of the two variables
    # and discard all others
    )

# Next, we want to sum up every TRUE for each variable

# The `mutate()` function works on a 
# variable-by-variable basis

# We could mutate each variable and list 
# them out line by line...
ex |> 
  mutate(
    Black_Gesso = sum(Black_Gesso),
    Bright_Red = sum(Bright_Red),
    Burnt_Umber = sum(Burnt_Umber)
    #...
  )

# However, we're doing the same thing for each variable, 
# using `sum()`, so we should "apply" that function 
# "across" each of the columns simultaneously
# rather than individually

# Saves us time

# We use `across()` to apply a function across multiple
# variables
ex <- 
  ex |> 
  mutate( 
    across(
      everything(), 
      # helper function to select every variable
      sum 
      # function to be applied
      )
    )

dim(ex)
# Now we have 403 observations and 18 variables where 
# each variable contains 403 observations of the same 
# value, i.e. one row is repeated 402 times

# We can remove the repetitions using `distinct()`
ex <- ex |> distinct()

# We now have a data frame/tibble of 1 observation 
# and 18 variables, showing the total number of 
# times each color is used throughout the show.

# We are still not at the dimension we need for plotting
# because it is not tidy

# We must *pivot* the data so that it has 18 observations 
# and two variables:
# - one for the name of the color
# - the other for sum count

# Let's use `pivot_longer()` to pivot the data into 
# "long" format
ex <-
  ex |> 
  pivot_longer(
    Black_Gesso:Alizarin_Crimson, 
    # columns we want to pivot
    names_to = "color_name", 
    # the name of the new column to contain 
    # the values of the old column names (the colors)
    values_to = "count" 
    # the name of the new column to contain
    # the values of the counts
    )

ex

# We now need one addition variable of information, 
# and that is the hexidecimal code to display the 
# actual wavelengths that correspond to the used colors

# The person who prepared this data set also built 
# another R package on GitHub called
# BobRossColors that contains the codes we need

# install.packages("devtools")
# install.github("BobRossColors")
BobRossColors::unique_bob_ross_colors

# Depending on your computer, the attempt to download
# might come with a few hiccups, so here is the data
# frame itself

unique_bob_ross_colors <-
  structure(
  list(
    color_name = c(
      "Alizarin_Crimson",
      "Black_Gesso",
      "Bright_Red",
      "Burnt_Umber",
      "Cadmium_Yellow",
      "Dark_Sienna",
      "Indian_Red",
      "Indian_Yellow",
      "Liquid_Black",
      "Liquid_Clear",
      "Midnight_Black",
      "Phthalo_Blue",
      "Phthalo_Green",
      "Prussian_Blue",
      "Sap_Green",
      "Titanium_White",
      "Van_Dyke_Brown",
      "Yellow_Ochre"
    ),
    codes = c(
      "#4E1500",
      "#000000",
      "#DB0000",
      "#8A3324",
      "#FFEC00",
      "#5F2E1F",
      "#CD5C5C",
      "#FFB800",
      "#000000",
      "#FFFFFF",
      "#000000",
      "#0C0040",
      "#102E3C",
      "#021E44",
      "#0A3410",
      "#FFFFFF",
      "#221B15",
      "#C79B00"
    )
  ),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA,-18L)
)

# In order to map the color to its corresponding bar
# in the plot, we need to join this color codes 
# data frame to our main data frame

# We will use a "left join" 
ex <- 
  ex |> 
  # data frame that keeps all rows
  left_join(
      unique_bob_ross_colors 
      # data frame to keep rows if their value exists 
      # in left because the join columns in both 
      # data frames have the same name, we don't
      # need to specify; if this was not the case, 
      # we'd have to add a `by = ` argument to the function
  )

# Our data is now prepared for plotting
fin <- ex

# Below is the whole process written out in one 
# single pipe chain:
fin <- 
  df |>
  select(Black_Gesso:Alizarin_Crimson) |>
  mutate(across(everything(), ~ sum(.))) |>
  distinct() |>
  pivot_longer(Black_Gesso:Alizarin_Crimson,
               names_to = "color_name",
               values_to = "count") |>
  mutate(
    color =  str_replace(color_name, "_", " ") |> str_replace("_", " ")
  ) |>
  left_join(unique_bob_ross_colors) 


# Plotting ----------------------------------------------------------------

# The key to ggplot is that you build a plot in layers


# Aesthetics --------------------------------------------------------------

# We start every plot by initiating a ggplot object 
# using `ggplot()` and defining the mapping aesthetics
p <- 
  fin |> 
  ggplot( 
    # start a ggplot object
    mapping = 
      # where is the data going to be used?
        aes( 
          x = count, 
          # what's going on the x-axis?
          y = reorder(color, count), 
          # what's going on the y-axis?
          fill = codes, 
          # what's going to determine the fill color?
          label = count 
          # what value will show up as the label?
        )
  )

p 
# We don't see anything plotted because there are no
# defined layers

# Geoms -------------------------------------------------------------------

# Add the first geom layer to draw the desired shape
p <- 
  p +
  geom_col(
    # all aesthetics are inherited from the 
    # `ggplot()` call
  ) 

p

  
# Add the second geom layer to draw the text labels
p <- 
  p +
  geom_text( 
    # we don't need to map any aesthetics as 
    # they're already here
    hjust = -.1, 
    # horizontal justification of text
    family = "ITC Korinna", 
    # font family to be used, ignore if inaccessible
    size = 5 
    # size of text
  )

p

# Scales ------------------------------------------------------------------

# The aesthetics are mapped and geoms are drawn, now we
# need to configure the scales

# The following changes the default fill color of 
# the bars to the values in `fin$codes`
p <- 
  p + 
  scale_fill_identity(
    # the strings that R uses to visualize color 
    # are embedded in the "fill" mapping
  ) 

p

# The other aesthetic that needs to be scaled is the x-axis
# - remove the space between the tick label and 
#   the base of the bar
# - include enough space so that none of our 
#   labels are cut off
p <- 
  p +
  scale_x_continuous(
    expand = c(0, 0), 
    # expand plotting space
    limits = c(0, 425) 
    # the continuous scale will show everything 
    # with x-axis units between 0 and 425, inclusive
  )

p

# Labels ------------------------------------------------------------------
  
# Setting overall plot labels
# **Notice** the HTML tags present in the text strings
p <- 
  p +
  labs(
    title = "<b>Color Frequency, Bob Ross' <i>The Joy of Painting</i></b>",
    subtitle = "Across 403 episodes (paintings), how often is each color used?",
    caption = "Tidy Tuesday 21 February 2023<br><b>@solplots</b>"
    )

p

# The `labs()` function is where I'd set x- and y-axis 
# titles if I were to include them; 
# in this plot I don't find them necessary

# Themes ------------------------------------------------------------------

# It looks weird with the tags still appearing in string
# we'll fix that within `theme()`

# Rather than pass an empty string, which would still 
# allocate space for an empty string, 
# I'll remove it altogether in `theme()`

# The final polishing step in the process is to 
# define the theme of the plot. 

# That is, what are all of the non-data aspects 
# of the plot going to look like?

p <- 
  p + 
  theme(
    text = 
      # all text elements will have the following...
      element_text(
        family = "ITC Korinna", 
        size = 16, 
        color = "black", 
        face = "bold"
      ),
    plot.title = element_markdown(
      # applies HTML tags present in title string
    ), 
    plot.subtitle = element_markdown(
      # applies HTML tags present in subtitle string
    ), 
    plot.caption = element_markdown(
      # applies HTML tags present in caption string
    ), 
    axis.title = element_blank(
      # removes default title space allocated for both axes
    ), 
    axis.text.x = element_blank(
      # removes all text along the x-axis
    ), 
    axis.ticks = element_blank(
      # removes all tick marks on both axes
    ), 
    panel.grid = element_blank(
      # removes all grid lines
    ), 
    plot.background =
      element_rect(
        fill = '#FFEDA3' 
        # sets the fill of the plot space to this color
        ),
    panel.background =
      element_rect(
        fill = "#FFEDA3", 
        # set the fill of the panel space to this color
        color = "#FFEDA3"
        # this space also has a border; 
        # set the border color as well
        ), 
    ) 

p
  
# Here is the code altogether:
fin |> 
  ggplot(aes(x = count, 
             y = reorder(color, count),
             fill = codes,
             label = count)) + 
  geom_col() +
  geom_text(hjust = -.1,
            family = "ITC Korinna",
            size = 5) +
  scale_fill_identity() +
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
  

# Saving plot ---------------------------------------------

# The plot lives in your environment but it has not 
# been granted access to your hard drive. 

# So you need to write it by naming a file and choosing 
# its device, size and units measured

ggsave(
  "bobross_updated.png", 
  # name of file
  plot = p, 
  # plot object to be saved 
  # (optional, will default to last printed plot)
  height = 8.5, 
  width = 8.5, 
  unit = "in", 
  # units to define height and width, in this case inches
  device = png 
  # force to save as .png file
  )
  
# Summary -----------------------------------------------------------------

# The goal of plotting & thotting is not just telling 
# a story but also inscribing the method through which 
# it was realized

# The following skills were applied...
# - reading in data from the internet
# - counting sums from a list of logical columns
# - pivoting wide to long
# - filling a bar plot with color hex values inside the data
# - labeling counts directly on the plot

# There are many other ways to understand this data, 
# this was simply my interpretation. 

# I however left out lost of information from the original
# data set, meaning much more expansive plots are possible

# Go forth and plot and thot









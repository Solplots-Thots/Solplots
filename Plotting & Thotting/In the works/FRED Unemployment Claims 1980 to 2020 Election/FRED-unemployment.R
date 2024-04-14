library(tidyverse)

df <- read.csv("ICSA.csv")

df$DATE <- as.Date(df$DATE, format = "%Y-%m-%d")
df$DATE

df %>%
  filter(DATE < "2020-03-21") %>%
  arrange(desc(ICSA)) %>%
  slice(1:20)

# Before March of 2020, the top 20 weeks in which the most
# unemployment claims were filed occured in 1982, 2009, and
# May of 1980.

# The greatest number of unemployment claims filed in a 
# single week before the pandemic was 695,000.

df %>%
  filter(DATE >= "2020-03-21") %>%
  top_n(-1)

# The minimum number of unemployment claims filed since
# the beginning of the pandemic was 711,000, during the
# week of November 7, 2020, which happened to be the 
# week of the 2020 election. 

p <- df %>%
  filter(DATE > "1980-01-01") %>%
  ggplot(aes(x = DATE, y = ICSA)) +
  geom_area(fill = "blue", alpha = 0.2, 
            color = "black") +
  geom_hline(yintercept = 695000,
             color = "red",
             linetype = "dashed") +
  scale_y_continuous(labels = c("0", "1", "2",
                                "3", "4", "5", "6"),
                     breaks = c(0, 1000000,
                                2000000, 3000000,
                                4000000, 5000000,
                                6000000)) +
  labs(title = "Is the Economy Transforming Before Our Eyes?",
       subtitle = "Weekly Unemployment Claims, Millions, 1980-present",
       x = "",
       y = "",
       caption = "Source: FRED") +
  annotate("text", y = 4000000, x = as.Date("1982-10-02",
                                            format = "%Y-%m-%d"), 
           label = "Largest pre-COVID unemployment\nclaims filing: 695,000, 10/2/1982",
           hjust = "inward", 
           lineheight = 1, family = "Georgia",
           size = 3.5) +
  annotate("segment", xend = as.Date("1982-10-02",
                                  format = "%Y-%m-%d"),
           x = as.Date("1990-01-01",
                          format = "%Y-%m-%d"),
           yend = 730000, y = 3300000, 
           arrow = arrow(angle = 30, length = unit(.2, "cm"))) +
  annotate("text", x = as.Date("2000-10-02",
                               format = "%Y-%m-%d"),
           y = 1800000, label = "The post-COVID minimum\nis 711,000, 11/7/2020",
           lineheight = 1, family = "Georgia", size = 3.5, hjust = 0) +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Georgia"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold")) 

ggsave("FRED-claims.png",
       plot = p,
       width = 5.02,
       height = 3.25,
       unit = "in")








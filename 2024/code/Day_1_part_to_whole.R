# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggparliament)
library(patchwork)
library(ggtext)
library(glue)
library(emojifont)
library(showtext)

# Load Eurovision votes dataset ------------------------------------------------
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')

# load fonts and titles

font_add_google("Henny Penny", "hennypenny")
font_add_google("Roboto", "roboto")
showtext_auto()

logo_txt <- "white"

twitter <- glue("<span style='font-family:\"fontawesome-webfont\";color:{logo_txt}'>{emojifont::fontawesome('fa-twitter')}</span>")

github <- glue("<span style='font-family:\"fontawesome-webfont\";color:{logo_txt}'>{emojifont::fontawesome('fa-github')}</span>")

caption <- str_wrap(glue(
  "Data source: Eurovision •  @SaenzJohanS • Code: SebasSaenz/TidyTuesday"))

base_color <- c("#fd8d3c", "#49006a")

# Wrangle data
df <- eurovision %>% 
  filter(semi_final =="f",
         jury_or_televoting == "T") %>%
  group_by(from_country, to_country) %>%
  summarise(sum_points = sum(points), .groups = "drop") %>% 
  mutate(voted = if_else(sum_points > 0, "YES", "NO"))


germnany <- df %>% 
  filter(to_country == "Germany",
         from_country != "Germany") %>% 
  count(voted)

germany_semicircle <- parliament_data(election_data = germnany,
                                 type = "semicircle", # Parliament type
                                 parl_rows = 4,      # Number of rows of the parliament
                                 party_seats = germnany$n)


germany_plot <- ggplot(germany_semicircle, aes(x = x, y = y, colour = voted)) +
  geom_parliament_seats(size = 4) +
  geom_text(x =0, y = 0.2, label = "Germany", color = "white", size=5, family = "Henny Penny") +
  theme_ggparliament(legend = "none") +
  scale_color_manual(values = base_color) 


france <- df %>% 
  filter(to_country == "France",
         from_country != "France") %>% 
  count(voted)

france_semicircle <- parliament_data(election_data = france,
                                      type = "semicircle", # Parliament type
                                      parl_rows = 4,      # Number of rows of the parliament
                                      party_seats = france$n)


france_plot <- ggplot(france_semicircle, aes(x = x, y = y, colour = voted)) +
  geom_parliament_seats(size = 4) +
  geom_text(x =0, y = 0.2, label = "France", color = "white", size=5, family = "Henny Penny") +
  theme_ggparliament(legend = "none") +
  scale_color_manual(values = base_color)


spain <- df %>% 
  filter(to_country == "Spain",
         from_country != "Spain") %>% 
  count(voted)

spain_semicircle <- parliament_data(election_data = spain,
                                     type = "semicircle", # Parliament type
                                     parl_rows = 4,      # Number of rows of the parliament
                                     party_seats = spain$n)


spain_plot <- ggplot(spain_semicircle, aes(x = x, y = y, colour = voted)) +
  geom_parliament_seats(size = 4) +
  geom_text(x =0, y = 0.2, label = "Spain", color = "white", size=5, family = "Henny Penny") +
  theme_ggparliament(legend = "none") +
  scale_color_manual(values = base_color)


uk <- df %>% 
  filter(to_country == "United Kingdom",
         from_country != "United Kingdom") %>% 
  count(voted)

uk_semicircle <- parliament_data(election_data = uk,
                                    type = "semicircle", # Parliament type
                                    parl_rows = 4,      # Number of rows of the parliament
                                    party_seats = uk$n)


uk_plot <- ggplot(uk_semicircle, aes(x = x, y = y, colour = voted)) +
  geom_parliament_seats(size = 4) +
  geom_text(x =0, y = 0.2, label = "UK", color = "white", size=5, family = "Henny Penny") +
  theme_ggparliament(legend = "none") +
  scale_color_manual(values = base_color)


# Main plot
(germany_plot + uk_plot) / (spain_plot + france_plot) +
  plot_annotation(title = 'Eurovision',
                  subtitle ="Number of countries that have <b style='color:#fd8d3c'>never voted</b> or <b style='color:#49006a'>voted at least once</b> for the Big four<br /> in the Eurovision contest (1975-2022). Only votes from the public were used.",
                  caption = caption,
                  theme= theme(plot.title  = element_markdown(family = "Henny Penny",
                                                          colour = "white",
                                                   size = 25),
                               plot.subtitle = element_markdown(family ="Luminari",
                                                                size =11,
                                                                hjust = 0,
                                                                margin = margin(5,0,15,0),
                                                                lineheight = 0.1),
                               plot.caption = element_markdown(size = 5))) &
  theme(text = element_text(color = "white"),
        plot.background = element_rect(color = "black",
                                        fill = "black"),
panel.background = element_rect(color = "black",
                              fill = "black"))

ggsave(filename = "2024/plots/eurovision.png", width = 6, height = 5)

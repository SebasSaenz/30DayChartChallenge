library(tidyverse)
library(here)
library(tidytuesdayR)
library(rvest)
library(stringr)


caption <- "Data: package/tdf - @SaenzJohanS - GitHub: SebasSaenz"

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv') %>% 
  select(Type, Winner, Winner_country=Winner_Country)

# After 2019 the variable is call Type
process_tour_de_france <- function(url) {
  tf <- url %>% 
    read_html() %>% 
    html_table(header=TRUE, fill = TRUE)
  
  df <- tf[[3]][-5] %>% 
    select(`Stage type`, Winner) %>% 
    separate(Winner,
             into = c("Winner", "Winner_country"),
             sep = "\\(") %>% 
    mutate(Winner_country = str_remove(Winner_country, "\\)")) %>% 
    drop_na() %>% 
    filter(row_number() <= n()-1)
  
  var_name <- deparse(substitute(url))
  
  assign(var_name, df, envir = .GlobalEnv)
}


url2018 <- "https://en.wikipedia.org/wiki/2018_Tour_de_France"
url2019 <- "https://en.wikipedia.org/wiki/2019_Tour_de_France"
url2020 <- "https://en.wikipedia.org/wiki/2020_Tour_de_France"
url2021 <- "https://en.wikipedia.org/wiki/2021_Tour_de_France"
url2022 <- "https://en.wikipedia.org/wiki/2022_Tour_de_France"

process_tour_de_france(url2018)

url2018 <- url2018 %>% 
  rename(Type=`Stage type`)

list_df <- list(tdf_winners, url2018, url2019, url2020, url2021, url2022)

all_df <- do.call("rbind", list_df)


#------------------------
base_color <- c('#edf8b1','#7fcdbb','#2c7fb8')

all_df %>% 
  drop_na() %>% 
  filter(Winner_country == "COL" | Winner_country == "GER") %>% 
  count(Type, Winner_country) %>% 
  mutate(Type = case_when(Type=="Transition stage" ~ "Flat stage",
                          Type=="Stage with mountain(s)" ~ "Mountain stage",
                          Type=="Plain stage" ~ "Flat stage",
                          Type=="Medium mountain stage" ~ "Mountain stage",
                          Type=="Hilly stage" ~ "Mountain stage",
                          Type=="High mountain stage" ~ "Mountain stage",
                          .default = as.character(Type))) %>% 
  group_by(Type, Winner_country) %>% 
  summarise(sum_n = sum(n), .groups = "drop") %>%
  complete(Type, Winner_country, fill = list(sum_n = 0)) %>% 
  ggplot(aes(y = sum_n,
             x = Winner_country ,
             fill = Type)) +
  geom_col(position=position_dodge(),
           width = 0.5) +
  geom_hline(yintercept=0, 
             color = "black", linewidth=0.5) +
  labs(y = NULL,
       x = NULL,
       title = "Los escarabajos vs. German Cyclists",
       subtitle = "In the Tour of France, Colombian cyclists have mostly won mountain stages,\nwhile Germans have mostly won flat stages (1903-2022).",
       caption = caption) +
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0, 55, 10)) +
  scale_x_discrete(breaks = c("COL", "GER"),
                   labels = c("Colombians", "Germans")) +
  scale_fill_manual(values = base_color) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica",
                            size = 14),
        axis.text.x = element_text(size = 14,
                                   vjust = 4),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.05),
        legend.title = element_blank(),
        legend.position = c(0.28, 0.96),
        legend.text = element_text(size = 10),
        axis.text.y = element_text(vjust=-0.5
                                   ,margin = margin(l = 20, r = -10)),
        plot.title = element_text(face = "bold",
                                  size =22),
        plot.subtitle = element_text(margin = margin(10, 0, 15, 0)),
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    family = "Helvetica",hjust = 0.5),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.background = element_rect(
          color = "white",
          fill = "white")) +
  guides(fill = guide_legend(direction = "horizontal"))

ggsave(filename = "2024/plots/Day_12_Reuters.png",
       width = 7,
       height = 6,
       dpi = 400)
  


         
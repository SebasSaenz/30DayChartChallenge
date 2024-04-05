# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggtext)
library(glue)

# Set colors and titles --------------------------------------------------------
base_color <- c('#f1eef6','#bdc9e1','#74a9cf','#0570b0')

title <- "**Comparado al promedio del equipo,<br /> los defensas de <span style='color:blue'>**Millonarios**</span> son los<br /> que mas minutos han jugado.**"

caption <- "**Data: FBREF (05.04.2024) - @SaenzJohanS - GitHub: SebasSaenz**"

# Load data --------------------------------------------------------------------
df <- read.delim("2024/data/Millonarios.csv", 
           sep = ",",
           header = TRUE,
           check.names = FALSE,
           encoding="UTF-8") # Read spanish characters properly

# Wrangle data and make plot
df %>% 
  mutate(mean_min = round(mean(Min)),
         difference = Min - mean_min,
         Posc = factor(Posc,
                       levels = c("PO", "DF", "CC", "DL"))) %>% 
  ggplot(aes(x = difference,
             y = fct_reorder(Jugador, difference))) +
  geom_col(aes(fill =Posc)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_text(aes(label = Jugador,
            hjust = ifelse(difference < 0, 1.1, -0.1)),
            size = 3.5,
            family = "Optima") +
  scale_x_continuous(limits = c(-1000, 1000),
                     breaks = seq(-1000, 1000, 200)) +
  scale_fill_manual(values = base_color,
                    breaks = c("PO", "DF", "CC", "DL"),
                    labels = c("Portero", "Defensa", "Centro", "Delantero")) +
  labs(x = "Diferencia de minutos jugados",
       y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Optima"),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.background = element_rect(
          color = "white",
          fill = "white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linetype = 2),
        legend.position = c(0.9, 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.key.size = unit(1.8,"line"),
        plot.title = element_markdown(margin = margin(t=10,b=-30),
                                      size = 18))+
  geom_richtext(label = title,
                x = -1100, y = 30,
                stat = "unique",
                size = 6,
                fill = NA,
                label.color = NA,
                hjust = 0,
                family = "Optima",
                color = "black") +
  geom_richtext(label = caption,
                stat = "unique",
                fill = NA,
                label.color = NA,
                x = 600,
                y = 1,
                size = 3,
                family = "Optima")

# Save plot -------------------------------------------------------------------- 
ggsave(filename = "2024/plots/Day_5_Millonarios.png", width = 8, height = 8)

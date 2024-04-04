
# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(waffle)
library(ggtext)
library(glue)


# Load colors and titles --------------------------------------------------------
base_color <- c("#a6611a", "#dfc27d", "#80cdc1", "#018571")

caption <- str_wrap(glue(
  "Data source: DANE-ECV 2022 •  @SaenzJohanS • Code: SebasSaenz/TidyTuesday"
))

# Load dataset  ----------------------------------------------------------------
df <- read.table(
  file = "2024/data/Tecnologias de informaciขn y comunicaciขn.CSV",
  header = TRUE,
  sep = ";"
)

# Wrangle data -----------------------------------------------------------------

internet_users <- df %>% # Calculate the internet users
  select(P1084) %>%
  count(P1084) %>%
  filter(P1084 != 5) %>%
  summarise(sum_n = sum(n)) %>%
  pull(sum_n)

x <- df %>% # Calculate the percentage by device
  select(DIRECTORIO, P765S1, P765S2, P765S3, P765S4) %>%
  pivot_longer(-DIRECTORIO, names_to = "device") %>%
  mutate(
    value = if_else(is.na(value), FALSE, TRUE),
    device = case_when(
      device == "P765S1" ~ "Desktop",
      device == "P765S2" ~ "Laptop",
      device == "P765S3" ~ "Tablet",
      device == "P765S4" ~ "Mobile"
    ),
    device = factor(device,
      levels = c("Mobile", "Desktop", "Laptop", "Tablet")
    )
  ) %>%
  group_by(device) %>%
  summarise(sum_value = sum(value), .groups = "drop") %>%
  mutate(percentage = 100 * (sum_value / internet_users))


# Plot -------------------------------------------------------------------------
ggplot(x, aes(fill = device, values = percentage)) +
  labs(fill = NULL, colour = NULL) +
  # theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  geom_waffle(n_rows = 5, size = 0.3, colour = "white", flip = TRUE) +
  facet_wrap(~device, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(
    labels = function(x) x * 5, # make this multiplyer
  ) +
  scale_fill_manual(values = base_color) +
  labs(
    x = NULL, y = "Number of users (%)",
    title = "Forget about computers",
    subtitle = "Less than 20% of internet users in Colombia use computers<br /> to access the internet.",
    caption = caption
    
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(color = "white", fill = "white"),
    plot.background = element_rect(
      color = "white",
      fill = "white"),
    text = element_text(
      family = "Optima",
      colour = "black",
      size = 18
    ),
    plot.title = element_text(
      family = "Optima", colour = "black",
      size = 30
    ),
    plot.subtitle = element_markdown(
      family = "Optima",
      colour = "black",
      size = 18
    ),
    plot.caption = element_markdown(size = 8,
                                    face = "bold"),
    panel.grid = element_blank(),
    axis.ticks.y = element_line(),
    legend.position = "none"
  )

# Save the plot ----------------------------------------------------------------
ggsave(filename = "2024/plots/Day_4_Colombia_internet.png",
       width = 8,
       height = 6)

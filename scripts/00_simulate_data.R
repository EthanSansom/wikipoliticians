# Set Up -----------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)         # Plot Making
library(ggrepel)         # Adding Point Labels
library(scales)          # For formatting plot axes

set.seed(1)

# Simulate Data ----------------------------------------------------------------

# Expected Variable Factors
parties <- c("NDP", "Liberal", "Green", "CPC", "BQ")
positions <- c("Other", "Minister", "Prime Minister", "Party Leader", "Minister")

# Set Simulation Parameters
sample_size <- 100
mean_birthyear <- 1900
sd_birthyear <- 40

# Simulate Data
df <-
  tibble(
    id = 1:sample_size,
    party = sample(x = parties, size = sample_size, replace = TRUE),
    position = sample(x = positions, size = sample_size, replace = TRUE),
    birthyear = rnorm(n = sample_size, mean = mean_birthyear, sd = sd_birthyear),
    page_views = sample(x = 0:1000000, size = sample_size, replace = TRUE)
  )

# Add Anticipated Prime Minister Outliers
pm_index <- sample(x = 1:sample_size, size = 5)
df$page_views[pm_index] <- df$page_views[pm_index] + 1000000
df$position[pm_index] <- "Prime Minister"

#  Produce Plots ---------------------------------------------------------------
# Map Political Parties to Appropriate Colors
party_colour = 
  c(
    "BQ" = "#003F91",
    "Green" = "#6DA34D",
    "CPC" = "#1B9AAA",
    "NDP" = "#F26419",
    "Liberal" = "#C1292E"
  )

# Assign Names to Prime Ministers (for labelling)
df_plot <- df
df_plot$id <- ""
pm_names <- c("PM Steve", "PM Sally", "PM Sue", "PM Stan", "PM Sam")
df_plot$id[pm_index] <- pm_names


# Plot of Page Views by Political Party (with PM Labels)
pos <- position_jitter(width = 0.1, seed = 1)

ggplot(data = df_plot, aes(x = party, y = page_views, label = id), position = pos) +
  
  # Plot Points
  geom_point(
    aes(
      colour = party, 
      stroke = if_else(id != "", 0.5, 0) # Provide PM's points with an Outline
      ), 
    alpha = 0.7, 
    position = pos
    ) +
  
  # Label Points Belonging to Prime Ministers
  geom_text_repel(
    position = pos,
    box.padding = 1,
    segment.curvature = 0.5,
    segment.ncp = 5,
    segment.angle = 90
  ) +
  
  # Plot Labels
  xlab("Party") +
  ylab("Page Views") +
  labs(
    title = "Page Views by Political Party",
    subtitle = "Simulated Data",
    colour = "Party"
    ) +
  
  # Set Theme and Color Scheme
  theme_minimal() +
  scale_color_manual(values = party_colour)

# Plot of Proportion of Page Views, Pages, and Actual Seats by Party -----------
# Summarize party page views and page counts
party_proportions <-
  df |>
  group_by(party) |>
  summarize(
    party_pages = n(),
    party_pageviews = sum(page_views)
  ) |>
  mutate(seats = sample(x = 1:100, size = 5))

# Convert party page views and page counts to proportions, add actual election data
party_percents <-
  party_proportions |>
  mutate(
    pages = party_pages/sum(party_pages),
    pageviews = party_pageviews/sum(party_pageviews),
    seats = seats/sum(seats)
  ) |>
  select(party, pages, pageviews, seats)

party_percents <-
  party_percents |>
  pivot_longer(!party, names_to = "type", values_to = "percent")

# Create plot
party_percents |>
  ggplot(aes(x = percent, y = type, fill = party)) +
  geom_bar(stat = "identity", position = "stack") +
  
  # Text Labels with help from:
  # https://stackoverflow.com/questions/34903368/how-to-center-stacked-percent-barchart-labels
  geom_text(
    aes(
      label = paste0(round(100*percent),"%"), 
      ), 
    size = 3,
    position = position_stack(vjust=0.5), 
    colour = "white"
    ) +
  
  # Set labels
  xlab("Percent Share") +
  ylab("") +
  labs(
    title = "Share of Wikipedia Pages, Pageviews, and House Seats by Party",
    subtitle = "Simulated Data",
    fill = "Party"
  ) +
  
  # Set scales
  scale_x_continuous(labels = percent_format()) +
  
  # Set Theme and Color Scheme
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = party_colour)

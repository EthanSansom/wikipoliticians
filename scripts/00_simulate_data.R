# Set Up -----------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)         # Plot Making
library(ggrepel)         # Adding Point Labels
library(scales)          # For formatting plot axes

library(ggbeeswarm)      # For NYT Style Bee-swarm Plots
library(packcircles)     # For NYT Style Bee-swarm Plots

set.seed(1)

# Simulate Data ----------------------------------------------------------------

# Expected Variable Factors
parties <- c("NDP", "Liberal", "Green", "CPC", "BQ")
positions <- c("Other", "Minister", "Premier", "House", "Senate")

# Set Simulation Parameters
sample_size <- 100
mean_age <- 60
sd_age <- 10

# Simulate Data
df <-
  tibble(
    id = 1:sample_size,
    party = sample(x = parties, size = sample_size, replace = TRUE),
    position = sample(x = positions, size = sample_size, replace = TRUE),
    sex = sample(x = c("Male", "Female"), size = sample_size, replace = TRUE),
    age = rnorm(n = sample_size, mean = mean_age, sd = sd_age),
    page_views = sample(x = 0:1000000, size = sample_size, replace = TRUE),
    revert_percent = sample(x = 1:50, size = sample_size, replace = TRUE)
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
    #nudge_x = 0.25,
    box.padding = 1,
    #nudge_y = 5000,
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

# Plot of Proportion of Page Views, Pages, and Actual Seats by Party
party_proportions <-
  df |>
  group_by(party) |>
  summarize(
    party_pages = n(),
    party_pageviews = sum(page_views)
  ) |>
  mutate(seats = sample(x = 1:100, size = 5))

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


party_percents |>
  
  # Create Plot
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
  
  scale_x_continuous(labels = percent_format()) +
  
  # Set Theme and Color Scheme
  # theme_classic() +
  theme_minimal() +
  
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +

  scale_fill_manual(values = party_colour)

# Sort of Works for Beeswarm Plot - Can do better?
set.seed(10)
rand_points <- data.frame(x_group = "group",
                          x=rnorm(200, mean=50, sd=10),
                          size=rexp(200, 1))

beeswarm_without_size <- rand_points %>%
  ggplot(aes(x=x, y=x_group)) +
  geom_beeswarm(groupOnX=FALSE)

oldbee_chart_data <- ggplot_build(beeswarm_without_size)

newbee_frame <- data.frame(x=oldbee_chart_data$data[[1]]$x, 
                           y=oldbee_chart_data$data[[1]]$y, 
                           r=rand_points$size)

newbee_repel <- circleRepelLayout(newbee_frame, wrap=FALSE)

newbee_repel_out <- circleLayoutVertices(newbee_repel$layout, xysizecols = 1:3)

newbee_repel_out %>% ggplot(aes(x, y, group=id)) +
  # Altering the SIZE Parameter can adjust for slight overlap
  geom_polygon(aes(color=id, fill=id), size = 0.15) +
  coord_equal() +
  labs(x="X value", y="",
       title="This is a Bee Swarm Chart in R") +
  theme_classic() #+ theme(legend.position = "none")


# Bubble Plot
df <- df |> rename("ident" = id)
beeswarm_without_size <- 
  df |>
  # If x-value is too large -> can't adjust the circle sizes
  # Have to scale x-value, then RE-SCALE AXIS Labels
  ggplot(aes(x=page_views/10000, y="group", colour = party)) +
  geom_beeswarm(groupOnX=FALSE)

oldbee_chart_data <- ggplot_build(beeswarm_without_size)

newbee_frame <- data.frame(x=oldbee_chart_data$data[[1]]$x, 
                           y=oldbee_chart_data$data[[1]]$y,
                           r=df$revert_percent
                           )

newbee_repel <- circleRepelLayout(newbee_frame, wrap=FALSE, maxiter = 10000)

newbee_repel_out <- circleLayoutVertices(newbee_repel$layout, xysizecols = 1:3)
newbee_repel_out <- cbind(newbee_repel_out, df[newbee_repel_out$id, ])
  
# circleLayoutVertices: IDs will be the row numbers of the input circle data

newbee_repel_out %>% ggplot(aes(x, y, group=id)) +
  # Altering the SIZE Parameter can adjust for slight overlap
  geom_polygon(aes(fill=party, colour = party), alpha = 0.7) +
  coord_equal() +
  labs(
    x = "X value", 
    y = "",
    title = "Bee Swarm Plot",
    subtitle = "Circle Size represents reverted edit percentage.",
    fill = "Party",
    colour = "Party"
    ) +
  theme_classic() +
  # Re-scale Labels
  scale_x_continuous(labels = ~ format(. * 10000, scientific = FALSE)) +
  scale_fill_manual(values = party_colour) +
  scale_colour_manual(values = party_colour)




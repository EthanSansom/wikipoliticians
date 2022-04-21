library(here)
library(tidyverse)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(reactable)       # For interactive tables
library(reactablefmtr)   # For easier reactable formatting

# Set Seed for Reproducibility
set.seed(1)

# Load and Clean Data ----------------------------------------------------------
wiki_data <- read_csv(here("inputs/data/wiki_data_pages.csv"))

# Get the total page views for each politician
wiki_data <-
  wiki_data |>
  rowwise() |>
  mutate(page_views = sum(across(starts_with("views"), na.rm = TRUE)))

# Create Party Factor and Position Factor Variable
wiki_data <-
  wiki_data |>
  mutate(
    party = case_when(
      cp == 1 ~ "CPC",
      liberal == 1 ~ "Liberal",
      green == 1 ~ "Green",
      ndp == 1 ~ "NDP",
      bq == 1 ~ "BQ"
    )
  )

# Create Interactive Pack Circle Plot ------------------------------------------
# Add a column with the text you want to display for each bubble:
wiki_data$text <- paste("Name:", wiki_data$name, "\n", "Views:", wiki_data$page_views)

# Filter Observations with NO Page views
wiki_data <- wiki_data |> filter(!is.na(page_views))

# Generate the layout
packing_data <- circleProgressiveLayout(wiki_data$page_views, sizetype='area')
circles_data <- circleLayoutVertices(packing_data, npoints=50)

# IMPORTANT: This attaches wiki_data to the circles data
# -   circleLayoutVertices assigns an ID to each distinct vertex that associates the vertex with
#     A. a circle and B. the row number of wiki_data used to created that circle
# -   wiki_data[circles_data$id, ] assigns the row of wiki_data (by row number) to each instance of ID
# -   i.e. if circles_data$id = c(1, 1, 2, 3, 3, 3, 3), row 1 of wiki_data will be repeated twice, row 2
#     once, and row 3 four times
# -   look at "test_data <- wiki_data[circles_data$id, ]" to see this
circles_data <- cbind(wiki_data[circles_data$id, ], circles_data)

# Define Party Colors for Plot
party_colour <-
  c(
    "BQ" = "#003F91",
    "Green" = "#6DA34D",
    "CPC" = "#1B9AAA",
    "NDP" = "#F26419",
    "Liberal" = "#C1292E"
  )

party_labels <-
  c(
    "BQ" = "Bloc Québécois",
    "Green" = "Green Party",
    "CPC" = "Conservative Party",
    "NDP" = "New Democratic Party",
    "Liberal" = "Liberal Party"
  )

# Make the plot with a few differences compared to the static version:
circle_plot <-
  ggplot(data = circles_data, aes(fill = party, colour = party, group = id)) + 
  geom_polygon_interactive(
    aes(
      data_id = id,
      x = x, y = y,    # Coordinate of vertex (to draw circle)
      tooltip = text,  # Defines text to display on hover
      ),
    alpha = 0.6
    ) +
  # geom_text(aes(x = x, y = y, label = gsub("Group_", "", group)), size=2, color="black") +
  theme_void() + 
  theme(legend.position="top", plot.margin=unit(c(0,0,0,0),"cm")) +
  # Bold the label (LATER)
  labs(fill = "Party", colour = "Party") +
  scale_fill_manual(
    values = party_colour,
    labels = party_labels
    ) +
  scale_colour_manual(
    values = party_colour,
    labels = party_labels
    ) +
  coord_equal() 

circle_plot

# Make the plot interactive
circle_interactive_plot <- 
  ggiraph(
    ggobj = circle_plot,
    width_svg = 7,
    height_svg = 7
    ) |>
  girafe_options(
    opts_tooltip(
      opacity = 0.9, 
      # use_fill = TRUE,
      use_stroke = TRUE, 
      css = "padding:5pt;color:black;background-color:white;border-radius:5pt;border-style:solid;
      border-width:1pt"
    ),
    # opts_hover_inv(css = "opacity:0.1;"),
    opts_hover(css = "stroke-width:2;")
    )

circle_interactive_plot

# Create Interactive Table -----------------------------------------------------
party_data <-
  wiki_data |>
  # filter(gender == "male", prime_minister == 0) |>
  group_by(party) |>
  summarise(
    sum_page_views = sum(page_views, na.rm = TRUE),
    avg_page_views = mean(page_views, na.rm = TRUE),
    num_pages = n()
  ) |>
  
  # Add Name of most viewed politician per group
  left_join(
    y = wiki_data |> 
      group_by(party) |> 
      # filter(gender == "male", prime_minister == 0) |> # must apply same filters here
      slice(which.max(page_views)) |> 
      select(party, name),
    by = "party"
  ) |>
  rename("Most Viewed Page" = name) |>
  
  # Add color identifiers for each party
  left_join(
    y = enframe(party_colour, name = "party", value = "colour"),
    by = "party"
  )

# 
reactable(
  data = party_data |> select(-colour),
  columns = list(
    party = colDef(
      name = "Party", 
      cell = icon_sets(data = party_data, icon_color_ref = "colour", icon_position = "left")
      )
    )
  )
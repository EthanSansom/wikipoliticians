#  Better Labels Experiment ----------------------------------------------------
library(ggtext)         # For including rich text labels
library(ggplot2)        # For plots
library(tidyverse)      # For data wrangling
library(glue)
library(ggchicklet)     # For rounded bar charts
library(geomtextpath)   # For labelled line graphs
library(forcats)


# Create some fake data
set.seed(1)
size = 30

tib <-
  tibble(
    id = 1:size,
    group = sample(c("A", "B", "C", "D"), size = size, replace = TRUE),
    value = sample(10:100, size = size, replace = TRUE)
  )

# Create a summary of group means (mean of value), adding group colours and rich-text labels
tib_summary <-
  tib |>
  group_by(group) |>
  summarize(
    avg_value = mean(value)
  ) |>
  mutate(
    # Add bar colours for each group
    color = case_when(
      group == "A" ~ "#009E73",
      group == "B" ~ "#D55E00",
      group == "C" ~ "#0072B2",
      group == "D" ~ "#000000"
    ),
    # Add rich-text for each group name, add colored circle prior to group name
    # NOTE: \u25cf is unicode for circle -> you CAN use simple unicode chars (those with letters)
    name = glue("<i style='font-size:12pt; color:{color}'>\u25cf</i> Group {group}")
  )

# Plot using rich text labels
tib_summary |>
  ggplot(aes(y = name, x = avg_value, fill = group)) +
  geom_col() +
  theme(
    axis.text.y = element_markdown(),
    plot.caption = element_markdown(lineheight = 1.2)
  ) +
  labs(caption = "A Caption with some **bold text**<br>(and note below)") +
  scale_fill_manual(
    values = c(
      "A" = "#009E73",
      "B" = "#D55E00",
      "C" = "#0072B2",
      "D" = "#000000"
      )
    )

# Replicate Election Plot ------------------------------------------------------

# Expected Variable Factors
parties <- c("New Democrats", "Liberals", "Greens", "Conservatives", "Bloc Quebecois")

df <- 
  tibble(
    party = as.factor(parties),
    seats = c(25, 159, 5, 119, 33),
    seats_display = c(25, 159, 2, 119, 33)
  )

#  Produce Plots ---------------------------------------------------------------

# Map Political Parties to Appropriate Colors
party_colour = 
  c(
    "Bloc Quebecois" = "#003F91",
    "Greens" = "#6DA34D",
    "Conservatives" = "#1B9AAA",
    "New Democrats" = "#F26419",
    "Liberals" = "#C1292E"
  )

df <- 
  df |> 
  left_join(y = enframe(party_colour, name = "party", value = "color"), by = "party") |>
  mutate(name = glue("<i style='font-size:12pt; color:{color}'>\u25cf</i> {party}"))

# The circles cannot handle VERY Small values
# This requires a ton of customization
df |>
  ggplot(aes(x = party, y = seats, fill = party)) +
  
  # Create bars
  geom_chicklet(
    colour = NA, 
    show.legend = FALSE,
    width = 0.3,
    radius = grid::unit(5, "pt")
    ) +
  
  # Create party labels
  geom_richtext(
    aes(x = party, y = 0, label = name),
    fill = NA, 
    label.color = NA,
    nudge_x = 0.4,
    nudge_y = 0,
    hjust = 0
    ) +
  
  # Create Seat labels
  geom_richtext(
    aes(x = party, y = 200, label = seats_display),
    fill = NA, 
    label.color = NA,
    nudge_x = 0.2
  ) +
  
  # Add line needed for majority
  geom_texthline(
    yintercept = 170, 
    label = "170 Seats for Majority", 
    linetype = 2, 
    color = "gray",
    gap = TRUE
    ) +
  
  labs(
    title = "**Election Districts Tally**",
    subtitle = "Seats in which each party lead the vote count (*2021 Federal Election*)",
    caption = "With 338 of 338 ridings reporting results.<br>This is a graph replication - not my original design.",
    x = "",
    y = ""
    ) +
  scale_fill_manual(values = party_colour) +
  scale_y_continuous(limits = c(0, 200)) +
  
  # Flip plot 90 degrees
  coord_flip() +
  
  # Set themes
  theme_minimal() +
  theme(
    # Set rich text label elements
    plot.caption = ggtext::element_markdown(hjust = 0, lineheight = 1.2),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(),
    
    # Remove x-lines on grid
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text = element_blank(),
    
    # Fixed aspect ratio
    aspect.ratio = 0.40
  )


# I bet that you could do this a lot better by creating a graphing using geom_shape
# objects to create the individual rounded bars. Get the length of the bar (unrounded)
# from the observation (i.e. Liberals have 159 seats, NDP has 25 -> length_lib = 159,
# length_NDP = 25). Take the width of the bar (you can define this) and subtract half
# of it from either end of bar. This is your radius. Put a circle of this radius at either
# side of the bar, you now have your caps.

# You can probably include custom line segments that start at 0, as well as having length = 0
# just be a circle at this point. Additionally, you can define geom_text / geom_richtext labels
# at the end of the line (say a potential total of 3 such labels). Extend the horizontal lines to 
# accommodate this. Add functionality for adding an vertical line (i.e. 170 seats for majority) 
# as well. The party labels above the line with custom text can also be added with geom_richtext.

# Finally, using geom_richtext, add options for the "Party", and "Seats" labels, above the bars
# and end of line value labels that you didn't include here. Not, you will have to extend the
# graph boundaries above the top bar (y-axis) to accommodate these labels.

# Make sure that everything is relative - so distances between the bars are relative to 
# the width of the bars. And length of the overall plot is relative to max length of the bars.
# Font sizes will have to be updated dynamically

#### NOTES #####
# - I'm thinking one function to output all the needed data (maybe 2)
#     - fxn 1. inputs the bar chart data, outputs spatial poly with each bar as a poly group, include bar colors too
#     - fxn 2. inputs the bar chart data, output the line data (maybe label position data?), 
#       corresponding to the data output of fxn 1
# Next, a function for plotting
# Inputs are data from fxn 1 and fxn 2, creates the bar chart
# Maybe with labels by default and other options to edit the resulting plot

# Polygon Testing --------------------------------------------------------------
# see here https://rpubs.com/Earlien/Creating_and_Manipulating_Polygon_Objects
library(rgeos)       # For polygon unions
library(sp)          # For editing polygons
library(plotrix)     # For drawing shapes
library(maptools)    # For combining spatial polygons
library(dplyr)

# Create a square with width 1, bottom left corner at 0, 0
# start bottom left -> right -> up -> left
pnts <- tibble(
  x = c(0, 1, 1, 0),
  y = c(0, 0, 1, 1)
)

# Create a box
box <- Polygon(pts)
box <- SpatialPolygons(list(Polygons(list(box), ID = 1)))

# Create a circle cap to box (right side)
circle <- plotrix::draw.circle(x = 1, y = 0.5, radius = 0.5)
circle <- Polygon(circle)
circle <- SpatialPolygons(list(Polygons(list(circle), ID = 1)))

# This actually works
union <- rgeos::gUnion(box, circle, byid = FALSE)
plot(union)

# Return a SpatialPolygon which defines a rectangle
make_rect <- function(x, y, id){
  # Define rectangle bounds
  points = 
    tibble(
      x = c(0, x, x, 0),
      y = c(0, 0, y, y)
    )
  
  # Convert to sp object
  rect <- Polygon(points)
  rect <- SpatialPolygons(list(Polygons(list(rect), ID = id)))
  
  return(rect)
}

# Return a SpatialPolygon which defines a circle
make_circle <- function(x, y, radius, id){
  # Define circle bounds
  points = plotrix::draw.circle(x = x, y = y, radius = radius)
  
  # Convert to sp object
  circle <- Polygon(points)
  circle <- SpatialPolygons(list(Polygons(list(circle), ID = id)))
  
  return(circle)
}

# Make a circle-capped bar object for bar chart
make_bar <- function(x, y, id){
  
  # Define rectangle bounds, leaving room cap at either end
  rect <- make_rect(x - y, y, id)
  
  # Define circle caps
  left_cap <- make_circle(x = 0, y = 0.5*y, radius = 0.5*y, id = id)
  right_cap <- make_circle(x = x - y, y = 0.5*y, radius = 0.5*y, id = id)
  
  union <- rgeos::gUnion(rect, left_cap, byid = FALSE)
  union <- rgeos::gUnion(union, right_cap, byid = FALSE)
  
  return(list(union))
}

two_sum <- function(x, y) {return(x + y)}

tib <-
  tibble(
    x = 1:5,
    y = 1:5
  )


# Return a translated SpatialPolygons object
trans_sp <- function(spoly, x_shift, y_shift) {
  # Define new polygon
  new_poly <- spoly
  
  # Horizontal shift
  new_poly@polygons[[1]]@Polygons[[1]]@coords[, 1] <- 
    spoly@polygons[[1]]@Polygons[[1]]@coords[, 1] + c(x_shift)
  
  # Vertical shift
  new_poly@polygons[[1]]@Polygons[[1]]@coords[, 2] <- 
    spoly@polygons[[1]]@Polygons[[1]]@coords[, 2] + c(y_shift)
  
  return(new_poly)
}

bars <- list(make_bar(x = 10, y = 1, id = 2))


#  DIVIDER ---------------------------------------------------------------------

bars <- function(data, value, group, width) {
  # Generate the bars as polygons
  bars <- 
    data |>
     mutate(
      # Create sp object for each bar
      poly = make_bar({{value}}, width, id = {{group}}),
      
      # Shift each polygon to avoid overlaps
      poly = trans_poly(poly[1], y_shift = -2*width*(row_number() - 1))
    )
  
  return(bars$poly)
}

df <- 
  tibble(
    party = c("New Democrats", "Liberals", "Greens", "Conservatives", "Bloc Quebecois"),
    seats = c(25, 159, 5, 119, 33)
  )

t1 <- bars(data = df, value = seats, group = party, width = 1)



# DIVIDER ----------------------------------------------------------------------




# Plot the circle capped bar and new bar
bar <- make_bar(5, 1)
plot(bar, xlim = c(0, 10), ylim = c(-10, 10))

b2 <- trans_sp(bar, 0, 2)
b3 <- trans_sp(bar, 0, -2)

plot(b2, add = TRUE)
plot(b3, add = TRUE)

# Horizontal shift, required to align on top of one another
box@polygons[[1]]@Polygons[[1]]@coords[, 1] <- box@polygons[[1]]@Polygons[[1]]@coords[, 1] + c(-0.5)

# Vertical shift
# box@polygons[[1]]@Polygons[[1]]@coords[, 2] <- box@polygons[[1]]@Polygons[[1]]@coords[, 2] + c(1)

# When we plot the un-shifted output of make_bar, the center of the left circle is at 0 (x-axis).
# This is desirable. So, we may also want the center of the right circle to end at the x-value,
# as right now the edge (not center) of the right cap ends here.

# Plotting using ggplot
ggplot(data = bar, aes(x = long, y = lat)) + 
  
  # Add Horizontal Line Components (will need to provide data for this for full plot fxn)
  geom_segment(aes(x = 0, y = 0.5, xend = 8, yend = 0.5)) +
  
  # Add bars
  geom_polygon(fill = "red") +
  
  # Add bar labels (above), probably use the data provided to geom_segment for this
  # Also need to define the colors for the bars somewhere, maybe require a color column in fxn output
  geom_richtext(
    data = tibble(x = 0, y = 1.5),
    aes(x = x, y = y, label = "<i style='font-size:12pt; color:red'>\u25cf</i> Label"),
    fill = NA, 
    label.color = NA,
  ) +
  
  # Add blank space to the plot
  expand_limits(x = c(0, 10), y = c(-5, 5)) +
  
  # Remove space between plot and x-axis edges, mult = c(left_space, right_space)
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(breaks = -5:5) +
  
  # Set fixed coordinate width (1:1 scaling of axes)
  coord_fixed() +

  # Edit theme
  theme_void() +
  theme()

# Spatial Polygon Maniputation -------------------------------------------------
# Combining spatial polygons
b2 <- sp::spChFIDs(b2, "2") # Change FIDs of spatial polygon object 
combined <- maptools::spRbind(b2, b3) # Combine two sp's with unique ID's
plot(combined)

# Combining Polygons into one spatial poly from the start
points_c1 = plotrix::draw.circle(x = 0, y = 0, radius = 1)
points_c2 = plotrix::draw.circle(x = 3, y = 3, radius = 2)

c1 <- Polygon(points_c1)
c2 <- Polygon(points_c2)

circles <- 
  SpatialPolygons(
    # A list of polygons to combine
    list(
      Polygons(list(c1), ID = "A"),
      Polygons(list(c2), ID = "B")
      )
    )

plot(circles)

# Plotting circles using ggplot
ggplot(data = circles, aes(x = long, y = lat)) + 
  # Plot each polygon in circles, filling by group
  geom_polygon(aes(fill = id)) +
  
  # Remove space between plot and x-axis edges, mult = c(left_space, right_space)
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  
  # Set fixed coordinate width (1:1 scaling of axes)
  coord_fixed()


# NOTE: You only really have to create the proper circle once, then perform the union
# operation with every bar -> slight efficiency boost

# how to create multiple polys in one spatial poly object
# https://mhallwor.github.io/_pages/basics_SpatialPolygons

# Create full function set -----------------------------------------------------

# Expected Variable Factors
parties <- c("New Democrats", "Liberals", "Greens", "Conservatives", "Bloc Quebecois")

df <- 
  tibble(
    party = as.factor(parties),
    seats = c(25, 159, 5, 119, 33)
  )

party_colour = 
  c(
    "Bloc Quebecois" = "#003F91",
    "Greens" = "#6DA34D",
    "Conservatives" = "#1B9AAA",
    "New Democrats" = "#F26419",
    "Liberals" = "#C1292E"
  )

groups <- df$party
values <- df$seats

# Get the rectangles

bars <- function(data, value, group, width) {
  
  # Generate the bars as polygons
  rows <- dim(data)[1]
  bars <- list()
  shift_x <- 0
  
  
  
  for (row in 1:rows) {
    bar <- make_bar(x = data[[xcol]][row], y = width)
    ID <- as.character(data[[ycol]][row])
    bar_poly <- Polygons(list(bar), ID = ID)
    
    # Translate each bar (to avoid overlap)
    bar_poly <- trans_poly(bar_poly, y_shift = -2*width)
    
    # Add bar to bars
    bars <- append(bars, bar_poly)
  }
  
  # Combine the bars as a Spatial Polygon
  bars <- SpatialPolygons(bars)
}

poly_bars <- bars(data = df, xcol = "seats", ycol = "party", width = 1)

# Return a Polygon which defines a rectangle
make_rect <- function(x_dim, y_dim){
  # Define rectangle bounds
  points = 
    tibble(
      x = c(0, x_dim, x_dim, 0),
      y = c(0, 0, y_dim, y_dim)
    )
  
  # Convert to sp object
  rect <- Polygon(points)
  
  return(rect)
}

# Return a Polygon which defines a circle
make_circle <- function(x, y, radius){
  # Define circle bounds
  points = plotrix::draw.circle(x = x, y = y, radius = radius)
  
  # Convert to sp object
  circle <- Polygon(points)
  
  return(circle)
}

# Make a circle-capped bar object for bar chart
make_bar <- function(x_dim, y_dim){
  
  if (y_dim > x_dim) {
    stop("Rectangle must be wider than it is tall (y_dim < x_dim).")
  }
  
  # Define rectangle bounds, leaving room cap at either end
  rect <- make_rect(x_dim - y_dim, y_dim)
  
  # Define circle caps
  left_cap <- make_circle(x = 0, y = 0.5*y_dim, radius = 0.5*y_dim)
  right_cap <- make_circle(x = x_dim - y_dim, y = 0.5*y_dim, radius = 0.5*y_dim)
  
  union <- rgeos::gUnion(rect, left_cap, byid = FALSE)
  union <- rgeos::gUnion(union, right_cap, byid = FALSE)
  
  return(union)
}

SpatialPolygons(
  # A list of polygons to combine
  list(
    Polygons(list(c1), ID = "A"),
    Polygons(list(c2), ID = "B")
  )
)

c1 <- make_circle(0, 0, 1)
c1_sp <- SpatialPolygons(list(Polygons(list(c1), ID = 1)))

c2 <- make_rect(1, 1)
c2 <- trans_poly(c2, 0, 0.5)
c2_sp <- SpatialPolygons(list(Polygons(list(c2), ID = 2)))

plot(c1_sp)
plot(c2_sp, add = TRUE)

# Return a translated polygon object
trans_poly <- function(poly, x_shift, y_shift) {
  # Define new polygon
  new_poly <- poly
  
  # Horizontal and Vertical shift
  new_poly@coords[ ,1] <- new_poly@coords[ ,1] + c(x_shift)
  new_poly@coords[ ,2] <- new_poly@coords[ ,2] + c(y_shift)
  
  return(new_poly)
}



df <-
  tibble(
    x = 1:5,
    y = 6:10
  )



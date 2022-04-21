library(treemap)        # For creating tree maps
library(ggplot2)        # For graphing
library(ggforce)        # For geom_shape -> plotting better shapes
library(tidyverse)      # For data cleaning

# Take a look as the business data to understand how "index" heirarchy works
data <- data(business) # Get some data

# NOTE:
# My function(s) use the colours defined in the treemap() function. If you wish
# to change the colours, then you must change them there.

# Plot with no Subgroups -------------------------------------------------------
# Create a treemap
map <-
  treemap(
    business,
    index=c("NACE1"),  # Index defines the subgroups used to form squares (only one in this case)
    vSize="turnover", 
    type="index"       # Altering type will change how colours are generated, can be done manually
  )

# Get the treemap data
shapes <- map$tm

# Get the other coordinates from (x0, y0), height, and width
# We need four coordinates per observation:
# (x0, y0), (x1, y0), (x0, y1), (x1, y1)

# Provide ID to associate set of four points with an observation
shapes <- 
  shapes |> 
  mutate(
    id = row_number(),
    center_x = x0 + w/2,
    center_y = y0 + h/2
    )

# Get the length (row number) of data
shapes_length <- dim(shapes)[1]

# Repeat every row of the data four times
shapes <- shapes[sapply(1:shapes_length, function (x) rep(x, 4)), ]

# Increment over each row of shapes to create (x0, y0), (x1, y0), (x0, y1), (x1, y1)
for (i in 1:(shapes_length*4)) {
  
  if (i %% 4 == 0) {
    # Change (x0, y0) to (x1, y0)
    shapes[i, ]$x0 <- shapes[i, ]$x0 + shapes[i, ]$w
    
  } else if (i %% 4 == 1) {
    # Change (x0, y0) to (x0, y1)
    shapes[i, ]$x0 <- shapes[i, ]$x0 + shapes[i, ]$w
    shapes[i, ]$y0 <- shapes[i, ]$y0 + shapes[i, ]$h

  } else if (i %% 4 == 2) {
    # Change (x0, y0) to (x1, y1)
    shapes[i, ]$y0 <- shapes[i, ]$y0 + shapes[i, ]$h
    
  } else { 
    # Do Nothing
  }
}

# Plot the squares
ggplot(shapes, aes(x = x0, y = y0, group = id, fill = as.factor(id))) +
  
  # Plot Each Square, Shrink Size by -0.5mm, Create Rounded Borders
  geom_shape(expand = unit(-0.5, 'mm'), radius = unit(1, 'mm')) +
  
  # Add Labels to Squares
  geom_text(aes(x = center_x, y = center_y, label = id)) +
  
  # Add colours, deframe converts tibble columns to named list c("id1" = "color1", ...)
  scale_fill_manual(values = deframe(shapes |> select(id, color))) +
  
  # Void theme and Remove Legend
  theme_void() +
  theme(legend.position="none")

#  Plot with Subgroups ---------------------------------------------------------
# Make data about factories producing output from several areas
size = 30
set.seed(1)

factory_data <-
  tibble(
    factory = 1:size,
    output = sample(10:10000, size = size, replace = TRUE),
    area = sample(c("A", "B", "C", "D"), replace = TRUE, size = size)
  )

# Create a treemap, factories nested in areas
map <-
  treemap(
    factory_data,
    index = c("area", "factory"),
    vSize = "output", 
    type = "index"
  )

map

# Get the treemap data
shapes <- map$tm

# Provide ID to associate set of four points with an observation
shapes <- 
  shapes |> 
  mutate(
    id = row_number(),
    # Get center x and y coords of each square, for adding labels
    center_x = x0 + w/2,
    center_y = y0 + h/2
  )

# Get the length (row number) of data
shapes_length <- dim(shapes)[1]

# Repeat every row of the data four times
shapes <- shapes[sapply(1:shapes_length, function (x) rep(x, 4)), ]

# Increment over each row of shapes to create (x0, y0), (x1, y0), (x0, y1), (x1, y1)
for (i in 1:(shapes_length*4)) {
  
  if (i %% 4 == 0) {
    # Change (x0, y0) to (x1, y0)
    shapes[i, ]$x0 <- shapes[i, ]$x0 + shapes[i, ]$w
    
  } else if (i %% 4 == 1) {
    # Change (x0, y0) to (x0, y1)
    shapes[i, ]$x0 <- shapes[i, ]$x0 + shapes[i, ]$w
    shapes[i, ]$y0 <- shapes[i, ]$y0 + shapes[i, ]$h
    
  } else if (i %% 4 == 2) {
    # Change (x0, y0) to (x1, y1)
    shapes[i, ]$y0 <- shapes[i, ]$y0 + shapes[i, ]$h
    
  } else { 
    # Do Nothing
  }
}

# Plot the squares
ggplot(
  data = shapes, 
  aes(
    x = x0, y = y0, 
    group = id, 
    fill = as.factor(id)    # Change fill to area for mono coloured blocks
    )
  ) +
  
  # Plot Each Sub-Level Square, Shrink Size by -0.5mm, Create Rounded Borders
  geom_shape(
    data = shapes |> filter(level == 2),
    expand = unit(-0.2, 'mm'), 
    radius = unit(1, 'mm')
    ) +
  
  # Add Area Labels to Groups, with white background
  geom_label(
    data = shapes |> filter(level == 1),
    aes(
      x = center_x, 
      y = center_y, 
      label = area
    ),
    fill = "white"
  ) +
  
  # Add Factory Labels to Squares
  geom_text(aes(x = center_x, y = center_y, label = factory)) +
  
  # Add colors: deframe() converts tibble columns to named list c("id1" = "color1", ...)
  # Change select to area, color for mono colored blocks
  scale_fill_manual(values = deframe(shapes |> select(id, color))) +
  
  # Void theme and Remove Legend
  theme_void() +
  theme(legend.position="none")

# Next Steps -------------------------------------------------------------------
# Find an easy way to place several of these in a single plot, around one another
# arbitrarily, as in the NYT's graph. Honestly, you could get away with shifting
# all of the coordinates before hand by same (x, y) shift for each block, then
# plot them together that way. As it is, the close packing format works nicely for
# multiple groups anyways.

print("\U0001f600")

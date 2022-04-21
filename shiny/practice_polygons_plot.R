# Load Packages ----------------------------------------------------------------
library(ggtext)         # For including rich text labels
library(ggplot2)        # For plots
library(tidyverse)      # For data wrangling
library(glue)           # For string manipulation
library(ggchicklet)     # For rounded bar charts
library(geomtextpath)   # For labelled line graphs
library(rgeos)          # For polygon unions
library(sp)             # For editing polygons
library(plotrix)        # For drawing shapes
library(maptools)       # For combining spatial polygons
library(dplyr)
library(forcats)

# NOTE: This one's a work in progress

# Define Functions -------------------------------------------------------------

### NON-VECTOR FUNCTIONS
# Define four points of a rectangle given (x, y) = (width, height)
get_points <- function(x, y) {
  if (x < y) { stop("Bar must be wider than it is long (x > y)") }
  points = tibble(x = c(0, x, x, 0), y = c(0, 0, y, y))
  return(points)
}

# Convert a polygon and id to a SpatialPolygons object
make_spatial <- function(poly, id) {
  SpatialPolygons(list(Polygons(list(poly), ID = id)))
}

# Combine two circles and a rectangle
make_cap_bar <- function(bar, left_cap, right_cap, id) {
  cap_bar <- rgeos::gUnion(bar, left_cap, byid = FALSE)
  cap_bar <- rgeos::gUnion(cap_bar, right_cap, byid = FALSE)
  cap_bar <- sp::spChFIDs(cap_bar, id)
  return(cap_bar)
}

# Translate a spatial polygon
trans_sp <- function(spoly, x_shift, y_shift) {
  # Define new polygon
  new_poly <- spoly
  
  # Horizontal shift
  new_poly@Polygons[[1]]@coords[ ,1] <- new_poly@Polygons[[1]]@coords[ ,1] + c(x_shift)
  
  # Vertical shift
  new_poly@Polygons[[1]]@coords[ ,2] <- new_poly@Polygons[[1]]@coords[ ,2] + c(y_shift)
  
  return(new_poly)
}

### VECTOR FUNCTIONS
# Convert vectors of widths, heights, and ids to vector of spatial poly rectangles
make_rect <- function(x, y, id){
  # Define rectangle bounds
  points <- mapply(get_points, x, y, SIMPLIFY = FALSE)
  
  # Convert to sp object
  poly <- lapply(points, Polygon)
  rect <- mapply(make_spatial, poly, id)
  
  return(rect)
}

# Convert vectors of widths, heights, radius, and ids to vector of spatial poly circles
make_circle <- function(x, y, radius, id){
  # Define circle bounds
  points = mapply(plotrix::draw.circle, x, y, radius, SIMPLIFY = FALSE)
  
  # Convert to sp object
  poly <- lapply(points, Polygon)
  circle <- mapply(make_spatial, poly, id)
  
  return(circle)
}

# Combine circles and bars into rounded bar
make_bar <- function(x, width, id){
  
  # Define the bars and circles needed
  bar <- make_rect(x - width, width, id)
  left_cap <- make_circle(x = 0, y = 0.5*width, radius = 0.5*width, id = id)
  right_cap <- make_circle(x = x - width, y = 0.5*width, radius = 0.5*width, id = id)
  
  # Combine
  bars <- mapply(make_cap_bar, bar, left_cap, right_cap, id, SIMPLIFY = FALSE)
  
  return(bars)
}

# Combine bars into a single polygon, with space between them
merge_bars <- function(bars, width) {
  # How to join spatial polygons from:
  # https://gis.stackexchange.com/questions/180682/merge-a-list-of-spatial-polygon-objects-in-r
  len <- length(bars)
  spaces <- 1:len
  spaces <- spaces * (2 * width)
  
  joined <- SpatialPolygons(lapply(bars, function(x){x@polygons[[1]]}))
  joined@polygons <- mapply(trans_sp, joined@polygons, 0, spaces)
  
  return(joined)
}

# Get the bars -> join and space them out
bars <- make_bar(x = 2:6, width = 1, id = c("A", "B", "C", "D", "E"))
spaced <- merge_bars(bars, 1)

# Plot
ggplot(data = spaced, aes(x = long, y = lat)) + 
  # Plot each polygon in circles, filling by group
  geom_polygon(aes(fill = id)) +
  
  # Remove space between plot and x-axis edges, mult = c(left_space, right_space)
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(
    breaks = 1:5 * 2 + 0.5,
    labels = c("A", "B", "C", "D", "E") # Adding labels and breaks manually for now
    ) +
  
  # Set fixed coordinate width (1:1 scaling of axes)
  coord_fixed()



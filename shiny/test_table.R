# Load Packages ----------------------------------------------------------------
library(reactable)       # For interactive tables
library(reactablefmtr)   # For easier reactable formatting
library(tidyverse)

data <- MASS::Cars93[20:49, c("Make", "MPG.city", "MPG.highway")]

## By default, icon_sets() assigns blue circles to the lowest-third values,
## grey circles to the middle-third values,
## and orange to the top-third values
reactable(data, defaultColDef = colDef(cell = icon_sets(data)))

## Apply pre-set icon sets with icon_set:
reactable(data, defaultColDef = colDef(cell = icon_sets(data, icon_set = 'ski rating')))

## Assign custom colors
reactable(data, defaultColDef = colDef(cell = icon_sets(data, colors = c("tomato", "grey", "dodgerblue"))))

## Assign icons from Font Awesome's icon library
reactable(data, defaultColDef = colDef(cell = icon_sets(data, icons = c("arrow-down","minus","arrow-up"))))

## Use number_fmt to format numbers using the scales package
car_prices <- MASS::Cars93[20:49, c("Make", "Price")]

reactable(car_prices, defaultColDef = colDef(cell = icon_sets(car_prices, number_fmt = scales::dollar)))

## Position icons relative to the numbers. Options are to the left, right, above, below, or over.
reactable(car_prices, defaultColDef = colDef(cell = icon_sets(car_prices, icon_position = "above")))

test <-
  tibble(
    "Year" = c(2019, 2020, 2021, 2022),
    "Rating" = 1:4,
    "Again?" = c("Yes", "No", "No", "Yes"),
    "color" = c("red", "blue", "green", "yellow")
  )

# Assigns Default Colours Based on Rating
reactable(
  data = test |> select(-color), 
  columns = list(
    party = colDef(name = "Party", cell = icon_sets(test, icon_color_ref = "color", icon_position = "left"))
    )
  )

party <- 


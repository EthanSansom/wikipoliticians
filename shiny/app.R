# Preamble ---------------------------------------------------------------------
# Purpose: Launch a Shiny app supplement to the paper, which allow users to see
# the full sample of politicians, their page views, and party affiliation.
# Author: Ethan Sansom
# Contact: ethan.sansom@mail.utotoronto.ca
# Date: April 27, 2021

#  Load Packages ---------------------------------------------------------------
library(shiny)
library(dplyr)
library(tidyverse)
library(packcircles)    # For arranging circle plots
library(here)           # For file path management
library(ggplot2)
library(viridis)
library(ggiraph)        # For interactive graphs
library(ggtext)         # For rich text support
library(readr)

#  Shiny -----------------------------------------------------------------------
ui <- shinyUI(fluidPage(
  
  titlePanel("Canadian Politicians on Wikipedia"),
  
  sidebarLayout(
    sidebarPanel(
      h5("Explore the Wikipedia Page Views Data"),
      p("Circle size indicates the number of page views a politician recieved in 2021."),
      p("Circle colour indicates the party affiliation of the politician."),
      p("Hover over circles to see politician's names and page views"),
      h5("Paper Supplement"),
      p("This Shiny application serves as a supplement to a paper investigating the relationship 
        between the Wikipedia page views of Canadian politicians' biographies, and politician 
        characteristics - including party affiliation, poltical position, and birth year. The
        full paper is available at the GitHub repo below."),
      h5("Information"),
      p("Author: Ethan Sansom"),
      p("Created: April 27, 2022"),
      a("https://github.com/EthanSansom/wikipoliticians")
      ),
    mainPanel(
      ggiraphOutput("plotCircles", width = "100%", height = "750px")
      )
    )
  )
)


server <- shinyServer(function(input, output) {
  
  output$plotCircles <- renderggiraph({
    
    file <- "https://raw.githubusercontent.com/EthanSansom/wikipoliticians/master/inputs/data/wiki_data_shiny.csv"
    wiki_data <- read_csv(file)
    
    # Add a column with the text you want to display in tooltip:
    wiki_data$text <- paste("Name:", wiki_data$name, "\n", "Views:", wiki_data$page_views)
    
    # Filter Observations with no Page views
    wiki_data <- wiki_data |> filter(!is.na(page_views))
    
    # Generate the layout
    packing_data <- circleProgressiveLayout(wiki_data$page_views, sizetype='area')
    circles_data <- circleLayoutVertices(packing_data, npoints=50)
    
    # Attaches wiki_data to the circles geometry data
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
    
    # Define party labels
    party_labels <-
      c(
        "BQ" = "Bloc Québécois",
        "Green" = "Green Party",
        "CPC" = "Conservative Party",
        "NDP" = "New Democratic Party",
        "Liberal" = "Liberal Party"
      )
    
    # Define Plot
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
      
      # Set theme
      theme_void() + 
      theme(legend.position="top", plot.margin=unit(c(0,0,0,0),"cm")) +
      
      # Set labels
      labs(fill = "", colour = "") +
      
      # Set Scales
      scale_fill_manual(
        values = party_colour,
        labels = party_labels
      ) +
      
      scale_colour_manual(
        values = party_colour,
        labels = party_labels
      ) +
      
      # Set coordinates to equal for circle rendering
      coord_equal() 
    
    ggiraph(
      ggobj = circle_plot,
      width_svg = 7,
      height_svg = 7,
    ) |>
      girafe_options(
        opts_tooltip(
          opacity = 0.9,
          use_stroke = TRUE, 
          css = "padding:5pt;color:black;background-color:white;border-radius:5pt;border-style:solid;
      border-width:1pt"
        ),
        opts_hover(css = "stroke-width:2;"),
        opts_selection(type = "none")
      )
  })
  
})

shinyApp(ui = ui, server = server)


#importing libraries
library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coral Bleaching on the Great Barrier reef from 2010 to 2017"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkGroup", label = h5("Choose the Coral Types"), 
                         choices = list("Soft Corals" = "soft corals", "Sea Fans" = "sea fans", "Hard Corals" = "hard corals","Sea Pens" = "sea pens","Blue Corals" = "blue corals"),
                         selected = "soft corals"),
       selectInput("smoothType",
                   label = h5("Select the type of Smoothening"),choices = c("lm","glm","loess","gam"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       leafletOutput("myMap")
    )
  )
))

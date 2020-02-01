#importing necessary libraries
library(shiny)
library(ggplot2)
library(grid)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    
    # reading the contents of the file
    coral_data <- read.csv("vic_coral_data.csv")
    coral_data$value <- as.numeric(sub("%", "", as.character(coral_data$value)))
    plotFacet <- function(coral_type,smoothening){
      new_coral <- coral_data[order(coral_data$latitude),]
      subset <- subset(new_coral,(coralType %in% coral_type))
      p <- ggplot(data = subset,mapping = aes(x = year,y=value))+geom_point(color = 'blue')
      q <- p + facet_grid(coralType~factor(location,levels = unique(location))) + ggtitle(paste("Coral Bleaching for",toString(input$checkGroup))) + ylab("Bleach level (in %)")
      sm_plot <- q + geom_smooth(method = smoothening,aes(color = location))
      return(sm_plot)
    }
    plotFacet(input$checkGroup,input$smoothType)
  })
  
  #rendering the map output
  output$myMap <- renderLeaflet({
    coral_data <- read.csv("vic_coral_data.csv")
    coral_data$value <- as.numeric(sub("%", "", as.character(coral_data$value)))
    getColor <- function(coral_data) {
      agg_result <- data.frame(aggregate(coral_data$value,by = list(coral_data$location),FUN = mean,na.rm = TRUE))
      sapply(agg_result$x, function(res) {
          if((res > 0) & (res<=30)) {
            "yellow"
          }else if((res > 30) & (res<=40)) {
            "orange"
          }else{
            "red"
          }
        })
    }
    # awesome Icons to create customer location markers
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'glyphicon',
      markerColor = getColor(coral_data)
    )
    subset <- subset(coral_data,(coralType %in% input$checkGroup))
    p <- leaflet(subset) %>% addTiles() %>% addAwesomeMarkers(lng = subset$longitude,lat = subset$latitude,label = subset$location,labelOptions = labelOptions(noHide = T),icon = icons) %>% addLegend(colors = c("yellow","orange","red"),labels = c("0-30%","30-40%",">40%"),position = "bottomright",title = "Bleach level(in %)")
    return(p)
  })
  
})





#R script to produce static visualizations for facetting the data and plotting the leaflet map showing locations of
# all the locations and indicating the bleach level for each of the locations based and subsequently applying color codes
# to the markers indicating the severity in overall bleaching

#including libraries
library(ggplot2)
library(leaflet)

#formatting the data in proper format
coral_data <- read.csv("assignment-02-data-formated.csv")
coral_data$value <- as.numeric(sub("%", "", as.character(coral_data$value)))
coral_data <- coral_data[order(coral_data$latitude),]

#function to plot static facets for the entire datafrae
plotFacet <- function(coral_data,smoothening){
  p <- ggplot(data = coral_data,mapping = aes(x = year,y=value))+geom_point(color = 'blue')
  q <- p + facet_grid(coralType~factor(location,levels = unique(location))) + ggtitle(paste("Coral Bleaching for all the Coral Types from 2010 to 2017")) + ylab("Bleach level (in %)")
  sm_plot <- q + geom_smooth(method = smoothening,aes(color = location,group = 1))
  return(sm_plot)
}

#calling the function to plot with base smoothening as linear models "lm"
plotFacet(coral_data,"lm")


#function to apply color property to a site location based on the bleaching level to those coral types.
#indicating the severity of bleaching
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

# making use of awesomeIcons function to apply colors to different location markers
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'glyphicon',
  markerColor = getColor(coral_data)
)
#function to render the leaflet map for all the sites indicating 
plotLeaflet <- function(coral_data){
  p <- leaflet(coral_data) %>% addTiles() %>% addAwesomeMarkers(lng = coral_data$longitude,lat = coral_data$latitude,label = coral_data$location,labelOptions = labelOptions(noHide = T),icon = icons) %>% addLegend(colors = c("yellow","orange","red"),labels = c("0-30%","30-40%",">40%"),position = "bottomright",title = "Bleach level(in %)")
  return(p)
}

plotLeaflet(coral_data)


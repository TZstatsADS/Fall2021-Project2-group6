#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("leaflet")) {
    install.packages("leaflet")
    library(leaflet)
}
if (!require("leaflet.extras")) {
    install.packages("leaflet.extras")
    library(leaflet.extras)
}
if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
}
if (!require("magrittr")) {
    install.packages("magrittr")
    library(magrittr)
}
# if (!require("mapview")) {
#     install.packages("mapview")
#     library(mapview)
# }
if (!require("leafsync")) {
    install.packages("leafsync")
    library(leafsync)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

colors_pal <- c(
  "General" = "#4C00FF",
  "Traffic" = "#004CFF",
  "Environment" = "#00E5FF",
  "Noise" = "#00FF4D",
  "Neighborhood Condition" = "#C6FF00",
  "Infrastructure Condition" = "#FFFF00",
  "Virus (Covid-19)" = "#FFE0B3"
)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    ## Initialize map locates at NYC
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -73.93, lat = 40.73, zoom = 11)
    })
    
    ## Draw circle with color based on complaint type and radius based on count
    observe({
      covid_period <- input$covid
      checkGroup <- input$checkGroup
      data_select = na.omit(data_preCovid_count)
      if(covid_period == "pre-covid"){
        data_select = na.omit(data_preCovid_count)
      }else if(covid_period == "during-covid"){
        data_select = na.omit(data_duringCovid_count)
      }else{
        data_select = na.omit(data_postCovid_count)
      }
      data_select = data_select[data_select[["Complaint Type"]] %in% checkGroup,]
      type = sort(unique(data_select[["Complaint Type"]]))
      colors = vector(mode='character',length = length(type))
      for(i in 1:length(type)){
        colors[i] = as.character(colors_pal[type[i]])
      }
      colorData = data_select[["Complaint Type"]]
      pal <- colorFactor(colors, colorData)
      radius <- data_select[["n"]]*0.25
      
      leafletProxy("map", data = data_select) %>%
        clearShapes() %>%
        addCircles(~Longitude, ~Latitude, radius=radius,
                   stroke=FALSE, fillOpacity=0.6, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData,
                  layerId="colorLegend")
    })
    
    
    
    ## Shelter plot section
    shelter_data <- read.csv('data/DHS_Daily_Report.csv')
    shelter_data$Date.of.Census <- as.Date(shelter_data$Date.of.Census, '%m/%d/%Y')
    
    overview_plot <- ggplot(shelter_data) + 
      geom_line(aes(x=Date.of.Census, y=Total.Individuals.in.Shelter, color='total')) +
      geom_line(aes(x=Date.of.Census, y=Total.Adults.in.Shelter, color='adults')) +
      geom_line(aes(x=Date.of.Census, y=Total.Children.in.Shelter, color='children')) +
      scale_color_manual(values=c(
        'total'='red',
        'adults'='blue',
        'children'='green'
      )) +
      labs(title='Overview: COVID-19 has caused a fall in shelter occupancy')
    
    family_plot <- ggplot(shelter_data %>% mutate(
      single_adults_pct=Total.Single.Adults.in.Shelter / Total.Individuals.in.Shelter,
      adults_with_adults_pct=Individuals.in.Adult.Families.in.Shelter / Total.Individuals.in.Shelter,
      adults_with_children_pct=Adults.in.Families.with.Children.in.Shelter / Total.Individuals.in.Shelter,
      children_pct=Total.Children.in.Shelter / Total.Individuals.in.Shelter
    )) + 
      geom_line(aes(x=Date.of.Census, y=single_adults_pct, color='Single Adults')) +
      geom_line(aes(x=Date.of.Census, y=adults_with_adults_pct, color='Adults in adult families')) +
      geom_line(aes(x=Date.of.Census, y=adults_with_children_pct, color='Adults in families with children')) +
      geom_line(aes(x=Date.of.Census, y=children_pct, color='Children')) +
      scale_color_manual(values=c(
        'Single Adults'='red',
        'Adults in families with children'='blue',
        'Adults in adult families'='green',
        'Children'='purple')) +
      labs(
        title='COVID-19 changed the variety of inviduals who entered homeless shelters',
        subtitle='While the overall occupancy of homeless shelters fell during the pandemic, the proportion of single adults has risen to over a third.\nNYC seemed to provide better support for struggling families than for single adults during the pandemic.',
        color='Family situation') +
      xlab('Date of Census') + ylab('Percentage of total shelter occupancy')
    
    adult_plot <- ggplot(shelter_data) +
      geom_line(aes(x=Date.of.Census, y=Total.Single.Adults.in.Shelter, color='Total Single Adults')) +
      geom_line(aes(x=Date.of.Census, y=Single.Adult.Men.in.Shelter, color='Single Men')) +
      geom_line(aes(x=Date.of.Census, y=Single.Adult.Women.in.Shelter, color='Single Women')) +
      scale_color_manual(values=c(
        'Total Single Adults'='red',
        'Single Men'='blue',
        'Single Women'='green'
      )) +
      labs(
        title="The pandemic's increase of single adults in shelters is completely driven by the rise in the occupancy of single men.",
        subtitle='This trend did not continue into 2021, when COVID-19 began to subside.\nThis explains why 2021 has shown a steeper decline in overall shelter occupancy than 2020.')
    
    output$shelter_plot <- renderPlot(
      switch(input$shelter_plot_choice,
             overview=overview_plot,
             family=family_plot,
             adult=adult_plot)
    )

})



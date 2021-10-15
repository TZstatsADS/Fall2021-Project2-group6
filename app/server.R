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
if (!require("mapview")) {
    install.packages("mapview")
    library(mapview)
}
if (!require("leafsync")) {
    install.packages("leafsync")
    library(leafsync)
}

#Data Processing
total_citi_bike_df = read.csv('../data/citibike_data.csv')
##compute the daily in and out difference for the station
total_citi_bike_df$day_diff = total_citi_bike_df$endcount - total_citi_bike_df$startcount
#assign each column to weekend or weekday
total_citi_bike_df$weekend_or_weekday = ifelse(total_citi_bike_df$weekday %in% c('Saturday','Sunday'), "Weekend", "Weekday")

#station info
citi_bike_station_info <- total_citi_bike_df[,c('station_id','station_name','station_longitude','station_latitude')]
#remove the duplicates based on station id 
citi_bike_station_info <- citi_bike_station_info[!duplicated(citi_bike_station_info[ , c("station_id")]),]

#split the bike data to pre-covid and covid time period
citi_bike_pre_covid_df = total_citi_bike_df[difftime(total_citi_bike_df$date,"2019-05-31")<=0,] #2019-05-01 ~ 2019-05-31
citi_bike_covid_df = total_citi_bike_df[difftime(total_citi_bike_df$date,"2020-04-30")>=0,] #2020-05-01 ~ 2021-05-31


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    ## Map Tab section
    
    output$left_map <- renderLeaflet({
    
    #adjust for weekday/weekend effect
    if (input$adjust_time =='Overall') {
        leaflet_plt_df <- citi_bike_pre_covid_df %>% 
                            group_by(station_id) %>%
                            summarise(total_start_count = sum(startcount),
                                      total_end_count = sum(endcount),
                                      total_day_diff = sum(day_diff),
                                      total_diff_percentage = sum(day_diff)/sum(startcount),
                            ) %>% left_join(citi_bike_station_info,by='station_id')
    } else {
        leaflet_plt_df <- citi_bike_pre_covid_df %>% 
                            filter(weekend_or_weekday == input$adjust_time) %>%
                            group_by(station_id) %>%
                                summarise(total_start_count = sum(startcount),
                                          total_end_count = sum(endcount),
                                          total_day_diff = sum(day_diff),
                                          total_diff_percentage = sum(day_diff)/sum(startcount),
                                ) %>% left_join(citi_bike_station_info,by='station_id')
                            } 

        
    map_2019 <- leaflet_plt_df %>%
         leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
         addTiles() %>%
         addProviderTiles("CartoDB.Positron",
                          options = providerTileOptions(noWrap = TRUE)) %>%
         setView(-73.9834,40.7504,zoom = 12)
     
     if (input$adjust_score == 'start_cnt') {
         map_2019 %>%
             addHeatmap(
                        lng=~station_longitude,
                        lat=~station_latitude,
                        intensity=~total_start_count,
                        max=4000,
                        radius=8,
                        blur=10)
     }else if (input$adjust_score == 'end_cnt') {
         map_2019 %>%
             addHeatmap(
                        lng=~station_longitude,
                        lat=~station_latitude,
                        intensity=~total_end_count,
                        max=4000,
                        radius=8,
                        blur=10)
     } else if (input$adjust_score == 'day_diff_absolute'){
         map_2019 %>%
             addHeatmap(
                        lng=~station_longitude,
                        lat=~station_latitude,
                        intensity=~total_day_diff,
                        max=50,
                        radius=8,
                        blur=10)
         
     }else if (input$adjust_score == 'day_diff_percentage'){
         map_2019 %>%
             addHeatmap(
                        lng=~station_longitude,
                        lat=~station_latitude,
                        intensity=~total_diff_percentage,#change to total day diff percentage
                        max=0.1,
                        radius=8,
                        blur=10)
         
     }
     }) #left map plot
    
    output$right_map <- renderLeaflet({
        #adjust for weekday/weekend effect
        if (input$adjust_time =='Overall') {
            leaflet_plt_df <- citi_bike_covid_df %>% 
                group_by(station_id) %>%
                summarise(total_start_count = sum(startcount),
                          total_end_count = sum(endcount),
                          total_day_diff = sum(day_diff),
                          total_diff_percentage = sum(day_diff)/sum(startcount),
                ) %>% left_join(citi_bike_station_info,by='station_id')
        } else {
            leaflet_plt_df <- citi_bike_covid_df %>% 
                filter(weekend_or_weekday == input$adjust_time) %>%
                group_by(station_id) %>%
                summarise(total_start_count = sum(startcount),
                          total_end_count = sum(endcount),
                          total_day_diff = sum(day_diff),
                          total_diff_percentage = sum(day_diff)/sum(startcount),
                ) %>% left_join(citi_bike_station_info,by='station_id')
        } 
        #initial the map to plot on
        map_2020 <- leaflet_plt_df %>%
            leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
            addTiles() %>%
            addProviderTiles("CartoDB.Positron",
                             options = providerTileOptions(noWrap = TRUE)) %>%
            setView(-73.9834,40.7504,zoom = 12) 
        
        if (input$adjust_score == 'start_cnt') {
            map_2020 %>%
                addHeatmap(
                           lng=~station_longitude,
                           lat=~station_latitude,
                            intensity=~total_start_count, #change to total start count
                            max=4000,
                            radius=8,
                           blur=10)
        }else if (input$adjust_score == 'end_cnt') {
            map_2020 %>%
                addHeatmap(
                           lng=~station_longitude,
                           lat=~station_latitude,
                           intensity=~total_end_count,#change to total end count
                           max=4000,
                           radius=8,
                           blur=10)
        } else if (input$adjust_score == 'day_diff_absolute'){
            map_2020 %>%
                addHeatmap(
                           lng=~station_longitude,
                           lat=~station_latitude,
                           intensity=~total_day_diff,#change to total day diff
                           max=50,
                           radius=8,
                           blur=10)
            
        }else if (input$adjust_score == 'day_diff_percentage'){
            map_2020 %>%
                addHeatmap(
                           lng=~station_longitude,
                           lat=~station_latitude,
                           intensity=~total_diff_percentage,#change to total day diff percentage
                           max=0.1,
                           radius=8,
                           blur=10)
            
        }
        
    }) #right map plot
    
    
    ## Shelter plot section
    shelter_data <- read.csv('../data/DHS_Daily_Report.csv')
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
      labs(title='Overview: COVID-19 has caused a fall in shelter entry')
    
    adult_plot <- ggplot(shelter_data %>% mutate(
      single_adults_pct=Total.Single.Adults.in.Shelter / Total.Adults.in.Shelter,
      adults_with_adults_pct=Individuals.in.Adult.Families.in.Shelter / Total.Adults.in.Shelter,
      adults_with_children_pct=Adults.in.Families.with.Children.in.Shelter / Total.Adults.in.Shelter
    )) + 
      geom_line(aes(x=Date.of.Census, y=single_adults_pct, color='Single Adults')) +
      geom_line(aes(x=Date.of.Census, y=adults_with_adults_pct, color='Adults with Adults')) +
      geom_line(aes(x=Date.of.Census, y=adults_with_children_pct, color='Adults with Children')) +
      scale_color_manual(values=c(
        'Single Adults'='red',
        'Adults with Children'='blue',
        'Adults with Adults'='green')) +
      labs(
        title='COVID-19 changed the variety of adults who entered homeless shelters',
        subtitle='New York City seemed to provide better support for homeless families than for single homeless individuals.',
        color='Family situation') +
      xlab('Date of Census') + ylab('Percentage of adults in different family situations')
    
    output$shelter_plot <- renderPlot(
      switch(input$shelter_plot_choice,
             overview=overview_plot,
             adult=adult_plot,
             children=ggplot())
    )

})



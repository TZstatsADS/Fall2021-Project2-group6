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
if (!require("highcharter")) {
  install.packages("highcharter")
  library(highcharter)
}
if (!require("igraph")) {
  install.packages("igraph")
  library(igraph)
}
if (!require("tidyr")) {
  install.packages("tidyr")
  library(igraph)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
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
      labs(title="Overview: COVID-19 has caused a fall in shelter occupancy",
           subtitle='This is the result of different measures the city has taken during the pandemic in dealing with homelessness.',
           color='Group')
    
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
        title='The pandemic has caused less families and more single adults to enter homeless shelters',
        subtitle='The decrease in family occupancy in shelters during the pandemic explains the fall in overall occupancy.\nDuring the pandemic, NYC implemented several different relief programs to provide housing for struggling families.\nAt the same time, however, the proportion of single adults has risen significantly to over a third.',
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
        title="The pandemic's increase of single adults in shelters is completely driven by single men.",
        subtitle="The city's relief programs may not have reached homeless single men as much as they reached homeless families.\nThis trend did not continue into 2021, when NYC moved adults in shelters to hotel rooms as a temporary measure\nto prevent the spread of COVID-19. It is still to be seen whether single men will continue to enter homeless shelters at\na higher rate than other groups after this measure expires.",
        color='Adult Group')
    
    output$shelter_plot <- renderPlot(
      switch(input$shelter_plot_choice,
             overview=overview_plot,
             family=family_plot,
             adult=adult_plot)
    )
    
    ### ARRESTS SECTION ###
    # Load grouped data
    arrests_yr <- read.csv("data/arrests_yr.csv")
    felony_yr <- read.csv("data/felony_yr.csv")
    burglary_yr <- read.csv("data/burglary_yr.csv")
    child_yr <- read.csv("data/child_yr.csv")
    adult_yr <- read.csv("data/adult_yr.csv")
    
    # Create plots
    total_arrests_plot <- ggplot(data=arrests_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Arrests") +
      xlab("Year")
    felony_plot <- ggplot(data=felony_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Felonies") +
      xlab("Year")
    burglary_plot <- ggplot(data=burglary_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Burglaries") +
      xlab("Year")
    child_plot <- ggplot(data=child_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Crimes Committed by Children") +
      xlab("Year")
    adult_plot <- ggplot(data=adult_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Crimes Committed by Adults") +
      xlab("Year")
    
    output$arrest_plot <- renderPlot(
      switch(input$arrest_plot_choice,
             total=total_arrests_plot,
             burglaries=burglary_plot,
             felonies=felony_plot,
             child=child_plot,
             adult=adult_plot
             )
      )
    
    ###______hospital section___________
    covid_data <- read.csv("data/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
    covid_data$DATE_OF_INTEREST <- as.Date(covid_data$DATE_OF_INTEREST, '%m/%d/%Y')

    #Select subset of data by borough
    subsetborough <- function(subset){
      if (subset == "Bronx"){
        cov_data <- covid_data[,c(1,12:21)]
      }
      else if(subset == "Manhattan"){
        cov_data <- covid_data[,c(1,32:41)]
      }
      else if(subset == "Queens"){
        cov_data <- covid_data[,c(1,42:51)]
      }
      else if(subset == "Brooklyn"){
        cov_data <- covid_data[,c(1,22:31)]
      }
      else if(subset == "Staten Island"){
        cov_data <- covid_data[,c(1,52:61)]
      }
      return(cov_data)
    }

    # Interactive time series plot with highcharter


    observe({
      # Render highchart outcome
      output$his_covid <- renderHighchart({
        # subset data and make it tidy
        data_by_day_sub <- subsetborough(input$covid_borough) %>%
          tidyr::pivot_longer(
            cols = -DATE_OF_INTEREST,
            names_to = "line_var",
            values_to = "value") %>%
          dplyr::mutate(line_var = as.factor(line_var))

        # ---------------filter variables--------------------------------------------------
        # If no selection, generate a dataframe of zeros to avoid errors
        if (!input$cases_summary & !input$death_summary & !input$Hospitalized_summary){
          data_by_day_filter <- data_by_day_sub
          data_by_day_filter$value <- 0
        } else {
          if (input$cases_summary) {
            df1 <- data_by_day_sub %>%
              dplyr::filter(stringr::str_detect(line_var,"CASE_COUNT"))
          } else {df1 <- data.frame()}
          if (input$death_summary) {
            df2 <- data_by_day_sub %>%
              dplyr::filter(stringr::str_detect(line_var,"DEATH_COUNT"))
          } else {df2 <- data.frame()}
          if (input$Hospitalized_summary) {
            df3 <- data_by_day_sub %>%
              dplyr::filter(stringr::str_detect(line_var,"HOSPITALIZED_COUNT"))
          } else {df3 <- data.frame()}
          # aggregated dataframe for plot
          data_by_day_filter = rbind(df1, df2, df3)
        }

        hchart(data_by_day_filter, "line",
               hcaes(x = DATE_OF_INTEREST, y = value, group = line_var)) %>%
          hc_chart(zoomType = "x") %>%

          hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
          hc_xAxis(title = list(text = "Date"),
                   labels = list(format = '{value:%b %d %y}')) %>%
          hc_yAxis(title = list(text = "Count"),
                   tickInterval = 400,
                   max = max(data_by_day_filter$value)) %>%
          hc_title(text = paste0("<b>Covid-19 Summary for ",input$covid_borough, ", NY by Date</b>")) %>%

          hc_plotOptions(area = list(lineWidth = 0.5)) %>%
          hc_exporting(enabled = TRUE)
      })
    })


    #____________Doses_by_date__________________
    dose_data <- read.csv("data/doses-by-day.csv")
    dose_data$DATE <- as.Date(dose_data$DATE, '%m/%d/%Y')
    #Select subset of data by Dose 1 or Dose 2 or all_doses
    subsetdose <- function(subgroup){
      if(subgroup == "dose1"){
        data <- dose_data[,c(1,2)]
      }
      else if(subgroup == "dose2"){
        data <- dose_data[,c(1,4)]
      }
      else if(subgroup == "single"){
        data <- dose_data[,c(1,6)]
      }
      else if(subgroup == "alldose"){
        data <- dose_data[,c(1,8)]
      }
      return(data)
    }
    
    observe({
      output$his_dose <- renderHighchart({
        # subset data and make it tidy
        dose_by_day_sub <- subsetdose(input$covid_dose) %>%
        #dose_by_day_sub <- subsetdose("alldose") %>%
          tidyr::pivot_longer(
            cols = -DATE,
            names_to = "line_var_1",
            values_to = "value_1") %>%
          dplyr::mutate(line_var_1 = as.factor(line_var_1))
        
        hchart(dose_by_day_sub, "line",
               hcaes(x = DATE, y = value_1, group = line_var_1)) %>%
          hc_chart(zoomType = "x") %>%
          hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
          hc_xAxis(title = list(text = "Date"),
                   labels = list(format = '{value:%b %d %y}')) %>%
          hc_yAxis(title = list(text = "Count"),
                   tickInterval = 400,
                   max = max(dose_by_day_sub$value_1)) %>%
          hc_title(text = paste0("<b>Covid_19 Number of Vaccines Delivered Summary for ",input$covid_dose, ", NY by Date</b>")) %>%
          
          hc_plotOptions(area = list(lineWidth = 0.5)) %>%
          hc_exporting(enabled = TRUE)
        
      })
      
    })

})


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
 if (!require("highcharter")) {
     install.packages("highcharter")
     library(highcharter)
 }
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
      labs(title="Overview: COVID-19 has caused a fall in shelter occupancy",
           subtitle='This is the result of different measures the city has taken during the pandemic in dealing with homelessness.',
           color='Group')
    
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
        title='The pandemic has caused less families and more single adults to enter homeless shelters',
        subtitle='The decrease in family occupancy in shelters during the pandemic explains the fall in overall occupancy.\nDuring the pandemic, NYC implemented several different relief programs to provide housing for struggling families.\nAt the same time, however, the proportion of single adults has risen significantly to over a third.',
        color='Family situation') +
      xlab('Date of Census') + ylab('Percentage of total shelter occupancy')
    
    single_adult_plot <- ggplot(shelter_data) +
      geom_line(aes(x=Date.of.Census, y=Total.Single.Adults.in.Shelter, color='Total Single Adults')) +
      geom_line(aes(x=Date.of.Census, y=Single.Adult.Men.in.Shelter, color='Single Men')) +
      geom_line(aes(x=Date.of.Census, y=Single.Adult.Women.in.Shelter, color='Single Women')) +
      scale_color_manual(values=c(
        'Total Single Adults'='red',
        'Single Men'='blue',
        'Single Women'='green'
      )) +
      labs(
        title="The pandemic's increase of single adults in shelters is completely driven by single men.",
        subtitle="The city's relief programs may not have reached homeless single men as much as they reached homeless families.\nThis trend did not continue into 2021, when NYC moved adults in shelters to hotel rooms as a temporary measure\nto prevent the spread of COVID-19. It is still to be seen whether single men will continue to enter homeless shelters at\na higher rate than other groups after this measure expires.",
        color='Adult Group')    
    
    output$shelter_plot <- renderPlot(
      switch(input$shelter_plot_choice,
             overview=overview_plot,
             family=family_plot,
             adult=single_adult_plot)
    )
    
    ### ARRESTS SECTION ###
    # Load grouped data
    arrests_yr <- read.csv("data/arrests_yr.csv")
    felony_yr <- read.csv("data/felony_yr.csv")
    burglary_yr <- read.csv("data/burglary_yr.csv")
    child_yr <- read.csv("data/child_yr.csv")
    adult_yr <- read.csv("data/adult_yr.csv")
    
    # Create plots
    total_arrests_plot <- ggplot(data=arrests_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Arrests") +
      xlab("Year")
    felony_plot <- ggplot(data=felony_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Felonies") +
      xlab("Year")
    burglary_plot <- ggplot(data=burglary_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Burglaries") +
      xlab("Year")
    child_plot <- ggplot(data=child_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Crimes Committed by Children") +
      xlab("Year")
    adult_plot <- ggplot(data=adult_yr, aes(x=YEAR, y=n)) +
      geom_bar(stat="identity") +
      ylab("No. of Crimes Committed by Adults") +
      xlab("Year")
    
    output$arrest_plot <- renderPlot(
      switch(input$arrest_plot_choice,
             total=total_arrests_plot,
             burglaries=burglary_plot,
             felonies=felony_plot,
             child=child_plot,
             adult=adult_plot
      )
    )
    
    #____________Doses_by_date__________________
    dose_data <- read.csv("data/doses-by-day.csv")
    dose_data$DATE <- as.Date(dose_data$DATE, "%Y-%m-%d")
    #Select subset of data by Dose 1 or Dose 2 or all_doses
    subsetdose <- function(subgroup){
      if(subgroup == "dose1"){
        data <- dose_data[,c(1,2)]
      }
      else if(subgroup == "dose2"){
        data <- dose_data[,c(1,4)]
      }
      else if(subgroup == "single"){
        data <- dose_data[,c(1,6)]
      }
      else if(subgroup == "alldose"){
        data <- dose_data[,c(1,8)]
      }
      return(data)
    }
    
    observe({
      output$his_dose <- renderHighchart({
        # subset data and make it tidy
        dose_by_day_sub <- subsetdose(input$covid_dose) %>%
        #dose_by_day_sub <- subsetdose("dose1") %>%
          tidyr::pivot_longer(
            cols = -DATE,
            names_to = "line_var_1",
            values_to = "value_1") %>%
          dplyr::mutate(line_var_1 = as.factor(line_var_1))
        
        hchart(dose_by_day_sub, "line",
               hcaes(x = DATE, y = value_1, group = line_var_1)) %>%
          hc_chart(zoomType = "x") %>%
          hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
          hc_xAxis(title = list(text = "Date"),
                   labels = list(format = '{value:%b %d %y}')) %>%
          hc_yAxis(title = list(text = "Count"),
                   tickInterval = 400,
                  max = max(dose_by_day_sub$value_1)) %>%
          hc_title(text = paste0("<b>Covid_19 Number of Vaccines Delivered Summary for ",input$covid_dose, ", NY by Date</b>")) %>%
          
          hc_plotOptions(area = list(lineWidth = 0.5)) %>%
          hc_exporting(enabled = TRUE)
        
      })
      
    })
    
     
    
    ###______hospital section___________
    covid_data <- read.csv("data/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
    covid_data$DATE_OF_INTEREST <- as.Date(covid_data$DATE_OF_INTEREST, '%m/%d/%Y')

    #Select subset of data by borough
    subsetborough <- function(subset){
      if (subset == "Bronx"){
        cov_data <- covid_data[,c(1,12:21)]
      }
      else if(subset == "Manhattan"){
        cov_data <- covid_data[,c(1,32:41)]
      }
      else if(subset == "Queens"){
        cov_data <- covid_data[,c(1,42:51)]
      }
      else if(subset == "Brooklyn"){
        cov_data <- covid_data[,c(1,22:31)]
      }
      else if(subset == "Staten Island"){
        cov_data <- covid_data[,c(1,52:61)]
      }
      return(cov_data)
    }

    # Interactive time series plot with highcharter


    observe({
      # Render highchart outcome
      output$his_covid <- renderHighchart({
        # subset data and make it tidy
        data_by_day_sub <- subsetborough(input$covid_borough) %>%
          tidyr::pivot_longer(
            cols = -DATE_OF_INTEREST,
            names_to = "line_var",
            values_to = "value") %>%
          dplyr::mutate(line_var = as.factor(line_var))

        # ---------------filter variables--------------------------------------------------
        # If no selection, generate a dataframe of zeros to avoid errors
        if (!input$cases_summary & !input$death_summary & !input$Hospitalized_summary){
          data_by_day_filter <- data_by_day_sub
          data_by_day_filter$value <- 0
        } else {
          if (input$cases_summary) {
            df1 <- data_by_day_sub %>%
              dplyr::filter(stringr::str_detect(line_var,"CASE_COUNT"))
          } else {df1 <- data.frame()}
          if (input$death_summary) {
            df2 <- data_by_day_sub %>%
              dplyr::filter(stringr::str_detect(line_var,"DEATH_COUNT"))
          } else {df2 <- data.frame()}
          if (input$Hospitalized_summary) {
            df3 <- data_by_day_sub %>%
              dplyr::filter(stringr::str_detect(line_var,"HOSPITALIZED_COUNT"))
          } else {df3 <- data.frame()}
          # aggregated dataframe for plot
          data_by_day_filter = rbind(df1, df2, df3)
        }

        hchart(data_by_day_filter, "line",
               hcaes(x = DATE_OF_INTEREST, y = value, group = line_var)) %>%
          hc_chart(zoomType = "x") %>%

          hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
          hc_xAxis(title = list(text = "Date"),
                   labels = list(format = '{value:%b %d %y}')) %>%
          hc_yAxis(title = list(text = "Count"),
                   tickInterval = 400,
                   max = max(data_by_day_filter$value)) %>%
          hc_title(text = paste0("<b>Covid-19 Summary for ",input$covid_borough, ", NY by Date</b>")) %>%

          hc_plotOptions(area = list(lineWidth = 0.5)) %>%
          hc_exporting(enabled = TRUE)
      })
    })
    

    
    ## Salary and Working Hours
    By_borough_GrossSalary <- read.csv("data/By_borough_GrossSalary.csv")
    By_borough_WorkingHours <- read.csv("data/By_borough_WorkingHours.csv")
    By_agency_GrossSalary <- read.csv("data/By_agency_GrossSalary.csv")
    By_agency_WorkingHours <- read.csv("data/By_agency_WorkingHours.csv")
    
    
    By_borough_GrossSalary_plot <- By_borough_GrossSalary%>%
      ggplot() +
      geom_line(aes(x= Fiscal.Year, y=BRONX ,  color='Bronx'))+ 
      geom_line(aes(x= Fiscal.Year, y=BROOKLYN ,  color='Brooklyn'))+ 
      geom_line(aes(x= Fiscal.Year, y=MANHATTAN ,  color='Manhattan'))+ 
      scale_color_manual(values=c(
        'Bronx'='red',
        'Brooklyn'='blue',
        'Manhattan'='green'
      )) +
      labs(title='Average Gross Salary of Three Major Boroughs Increased During The Pandemic')
    
    
    By_borough_WorkingHours_plot <- By_borough_WorkingHours%>%
      ggplot() +
      geom_line(aes(x= Fiscal.Year, y=BRONX ,  color='Bronx'))+ 
      geom_line(aes(x= Fiscal.Year, y=BROOKLYN ,  color='Brooklyn'))+ 
      geom_line(aes(x= Fiscal.Year, y=MANHATTAN ,  color='Manhattan'))+ 
      scale_color_manual(values=c(
        'Bronx'='red',
        'Brooklyn'='blue',
        'Manhattan'='green'
      )) +
      labs(title='Average Working Hours of Three Major Boroughs Increased During The Pandemic')
    
    
    By_agency_GrossSalary_plot <- By_agency_GrossSalary%>%
      ggplot()+
      geom_point(mapping=aes(x=Agency.Name, y=gap, size = abs(gap), color =  GrowthOrNot ) )+
      labs(title='Salary Increased During The Pandemic in Most of Agency')
    
    
    
    By_agency_WorkingHours_plot <- By_agency_WorkingHours%>%
      ggplot()+
      geom_point(mapping=aes(x=Agency.Name, y=gap, size = abs(gap),color =  GrowthOrNot ) ) +
      labs(title='Working Hours Increased During The Pandemic in Most of Agency')
    
    output$SalaryHoursPlot <- renderPlot(
      switch(input$SalaryPlotin,
             Gross_Salary_By_Year=By_borough_GrossSalary_plot,
             Gross_Salary_By_Agency_2019_2020=By_agency_GrossSalary_plot,
             Working_Hours_By_Year=By_borough_WorkingHours_plot,
             Working_Hours_By_Agency_2019_2020=By_agency_WorkingHours_plot
      )
    )
})


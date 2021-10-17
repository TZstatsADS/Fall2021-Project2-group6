
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("leaflet.extras")) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}

vars <- c(
  "Pre-Covid" = "pre-covid",
  "During Covid" = "during-covid",
  "Post-Covid" = "post-covid"
)

complaint_types <- c(
  "General" = "General",
  "Traffic" = "Traffic",
  "Environment" = "Environment",
  "Noise" = "Noise",
  "Neighborhood Condition" = "Neighborhood Condition",
  "Infrastructure Condition" = "Infrastructure Condition",
  "Virus (Covid-19)" = "Virus (Covid-19)"
)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(strong("Covid 19 Influence Explorer",style="color: white;"),
               theme=shinytheme("cerulean"), # select your themes https://rstudio.github.io/shinythemes/
#------------------------------- tab panel - Maps ---------------------------------
        tabPanel("311 Complaint Map",icon = icon("map-marker-alt"),
             div(class="outer",
                 tags$head(
                   includeCSS('../lib/styles.css')
                 ),
                 leafletOutput("map", width="100%", height="100%"),
                 ## Control panel to select the covid period and complaint types
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                               width = 330, height = "auto",
                               
                               h2("Complaint Explorer"),
                               
                               selectInput("covid", "Covid Period", vars, selected = "during-covid"),
                               checkboxGroupInput("checkGroup", "Complaint Type",
                                                  choices = complaint_types,selected = complaint_types),
                 ),
             )
        ),



        tabPanel("Homeless Shelters",
                 sidebarPanel(
                   radioButtons('shelter_plot_choice',
                                'Shelter occupant categorization',
                                c('Overview of occupants'='overview',
                                  'Family situations of occupants'='family',
                                  'Single adult breakdown'='adult'))
                 ),
                 mainPanel(plotOutput('shelter_plot'))),

        conditionalPanel("false", icon("crosshair"))


    ) #navbarPage closing  
) #Shiny UI closing    

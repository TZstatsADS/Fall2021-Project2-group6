
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
if (!require("highcharter")) {
  install.packages("highcharter")
  library(highcharter)
}

vars <- c(
  "Pre-Covid" = "pre-covid",
  "During Covid" = "during-covid",
  "Post-Covid" = "post-covid"
)

borough_var <- c("BRONX" = "Bronx",
                  "BROOKLYN" = "Brooklyn",
                  "MANHATTAN" = "Manhattan",
                  "QUEENS" = "Queens", 
                  "STATEN ISLAND" = "Staten Island")

dose_var <- c("DOSE 1" = "dose1",
              "DOSE 2" = "dose2",
              "SINGLE" = "single",
              "ALL DOSES" = "alldose")

arrest_type <- c("All" = "ALL",
                 "Felony" = "FELONY ASSAULT",
                 "Burglary" = "BURGLARY")


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
                   includeCSS('lib/styles.css')
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


        ### TAB-HOMELESS SHELTERS ###
        tabPanel("Homeless Shelters",
                 sidebarPanel(
                   radioButtons('shelter_plot_choice',
                                'Shelter occupant categorization',
                                c('Overview of occupants'='overview',
                                  'Family situations of occupants'='family',
                                  'Single adult breakdown'='adult'))
                 ),
                 mainPanel(plotOutput('shelter_plot'))),
        
        ### TAB-ARRESTS ###
        tabPanel("Arrests",
                 titlePanel("Updates on Arrest Trends in New York City"),
                 sidebarPanel(
                   radioButtons("arrest_plot_choice",
                                "Arrest Chart Type",
                                c("Total Arrests"="total",
                                  "Burglaries"="burglaries",
                                  "Felonies" = "felonies",
                                  "Under Age" = "child",
                                  "Adult" = "adult"
                                  )
                                )
                   ),
                 mainPanel(plotOutput('arrest_plot'))),

      #_______tab for hospital section__________
      tabPanel("Covid_19 cases and Vaccine",
                titlePanel("Updates on Covid_19 cases and Vaccine Distribution in New York City"),
                span(tags$h2("Covid_19 Distribution")),
                sidebarLayout(position = "left",
                          sidebarPanel(
                                h3("NYC Boroughs", style="color:#045a8d"),
                                selectInput("covid_borough","Choose a borough",
                                            choices = borough_var,
                                            selected = borough_var[1]),
                                h3("Selected a topic:", align = "left", style = "color:#045a8d"),
                                checkboxInput("cases_summary", label = "Cases", value = TRUE),
                                checkboxInput("Hospitalized_summary", label = "Hospitalized", value = FALSE),
                                checkboxInput("death_summary", label = "Deaths", value = FALSE),
                                
                                h3("Information of COVID-19 vaccine doses administered",style="color:#045a8d"),
                                selectInput("covid_dose","Select a type of dose",
                                            choices = dose_var,
                                            selected = dose_var[1]),
                                h4("Definition of the type of dose:", align = "left", style="color:#3498db"),
                                h5("Doses administered is counted separately for each dose of a two-dose vaccine series or single-dose vaccine."),
                                h5("As of 5/13/2021, delivery information for Moderna/Pfizer vaccine by dose number is no longer tracked separately and only the total number of doses delivered is reported")

                              ),
                              mainPanel(
                                # div(fluidRow(highchartOutput("his_covid", width = "100%",
                                #                              height = "300px")),
                                #     fluidRow(highchartOutput("his_dose",width = "100%",
                                #                              height = "300px")))
                                
                                highchartOutput("his_covid"),
                                highchartOutput("his_dose")
                                
                              )

                              )

      )
          ,
          tabPanel("Salary and Working Hours",
                    sidebarPanel(
                           radioButtons('SalaryPlotin',
                                        'Salary and Working Hours',
                                         c("Gross Salary By Year" =  'Gross_Salary_By_Year',
                                             "Gross Salary By NYC Agency 2019-2020" = 'Gross_Salary_By_Agency_2019_2020',
                                             "Working Hours By Year" = 'Working_Hours_By_Year',
                                             "Working Hours By NYC Agency 2019-2020" = 'Working_Hours_By_Agency_2019_2020')
                                        )
                                ),
                      mainPanel(plotOutput('SalaryHoursPlot'))
                    )
            
    ) #navbarPage closing  
) #Shiny UI closing    

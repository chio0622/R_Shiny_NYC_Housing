library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(dygraphs)



shinyUI(dashboardPage(
  dashboardHeader(title = "NYC Housing Authority Utility Consumption"),
  dashboardSidebar(
   
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Electric", tabName = "electric", icon = icon("chart-line")),
      menuItem("Water", tabName = "water", icon = icon("chart-line")),
      menuItem("Gas", tabName = "gas", icon = icon("chart-line"))
      )
  

  ),
  dashboardBody(
    tabItems(
      
      # Home tab ####
      tabItem(tabName = "home",
              fluidRow(box(h3(strong("New York City Housing Authority (NYCHA) Electric, Water, and Heating Gas Consumption")),
                           br(), h4(strong('About New York Housing Authority')),
                           br(), "The New York City Housing Authority's mission is to increase opportunities for low- and 
                           moderate-income New Yorkers by providing safe, affordable housing and facilitating access to social 
                           and community services. More than 400,000 New Yorkers reside in NYCHA's 326 public housing developments
                           across the City’s five boroughs according to offical website.", 
                           
                           br(),br(), "The data I chose to analyze was electric, water, and heating gas consumption and 
                          cost across NYC housing developments by boroughs and time. I acquired the data on NYC Open Data: 'Heating Gas Consumption And Cost (2010 - March 2019)',
                          'Water Consumption And Cost (2013 - March 2019)', and 'Electric Consumption And Cost (2010 - March 2019).' I chose this topic because I wanted to anaylze
                           how utility usage changes across time and compare usage to similar months. I was curious to know if building issues such as water leaks or electric shortages or 
                          any other building issue affected utility consumption. The time frame I used was between Jan 2013 and Dec 2017.", br(), br(),
                           
              fluidRow(box(strong("About the App"), br(),
                           "App Sections:", 
                           br(),br(),
                           "Map: The folowing map depicts the 300 plus unique housing developments across all
                           boroughs. I am still working on adding inputs and displaying average cost and consumption across boroughs.",
                           br(),br(),
                           "Electric: Displays electric cost and consumption across time and borough. Also displays the relationship between kilowatt hour and cost. Kilowatt Hour 
                           is a measure of electrical energy equivalent to a power consumption of 1,000 watts for 1 hour.", 
                           br(), br(),
                           "Water: Displays the water cost and consumption across time and borough. Also displays the relationship between water consumption in hundred cubic 
                           feet and total cost.",
                           br(), br(),
                           "Heating Gas: Displays the heating gas cost and consumption across time and borough. Also displays the relationship between heating consumption
                           in therms and total cost. Heating Gas is measured in therm, which is the unit of measurement for natural gas use over time.",width = 14))
              ))
              ),
      
      
      
      
      tabItem(tabName = "map",
              leafletOutput("map", height = "900", width="75%")
      ),
          #"to be replaced with map"), 
      tabItem(tabName = "electric",
              fluidRow(
                column(width = 5, selectInput(inputId="borough_1", label = "CHOOSE BOROUGH", choices = unique(Emaster[,"Borough"]), selected = unique(Emaster[,"Borough"])[1])),
                column(width = 5, pickerInput(inputId="year_1", label = "CHOOSE YEAR(S)", choices = unique(Emaster[,"Year"]), selected = unique(Emaster[,"Year"])[5],options = list(`actions-box` = TRUE),
                                              multiple = TRUE))
              ),
              fluidRow(
                column(width = 6,
                       box(height = 500,
                           title = strong("Total Cost of Electric per Borough"), width = NULL, solidHeader = TRUE, 
                           status = "primary", dygraphOutput("electric"))
                ),
                column(width = 6,
                       box(height = 500,
                           title = strong("Total Kilowatt Hour per Borough"), width = NULL, solidHeader = TRUE, 
                           status = "primary", dygraphOutput("electric_2"))
                ),
                column(width = 12,
                       box(
                           title= strong("Scatter Plot Between Kilowatt Hour Consumption and Total Cost of Electric per Borough"), width=1000, solidHeader = TRUE,
                           status = "primary", ggiraphOutput("scatters"))
                       
                       )
              )
                ),
      
      
      tabItem(tabName = "water",
              fluidRow(
                column(width = 5, selectInput(inputId="borough_2", label = "CHOOSE BOROUGH", choices = unique(Wmaster[,"Borough"]), selected = unique(Wmaster[,"Borough"])[1])),
                column(width = 5, pickerInput(inputId="year_2", label = "CHOOSE YEAR(S)", choices = unique(Wmaster[,"Year"]), selected = unique(Wmaster[,"Year"])[5],options = list(`actions-box` = TRUE),
                                              multiple = TRUE))
              ),
              fluidRow(
                column(width = 6,
                       box(height = 500,
                           title = strong("Total Cost of Water per Borough"), width = NULL, solidHeader = TRUE, 
                           status = "primary", dygraphOutput("water"))
                ),
                column(width = 6,
                       box(height = 500,
                           title = strong("Total Water in Hundred Cubic Feet per Borough"), width = NULL, solidHeader = TRUE, 
                           status = "primary", dygraphOutput("water_2"))
                ),
                column(width = 12,
                       box(
                           title= strong("Scatter Plot Between Water Consumption in Hundred Cubic Feet and Total Cost of Water per Borough"), width=1000, solidHeader = TRUE,
                           status = "primary", ggiraphOutput("scatters_2"))
                       
                )
              )
      ),
      
      tabItem(tabName = "gas",
              fluidRow(
                column(width = 5, selectInput(inputId="borough_3", label = "CHOOSE BOROUGH", choices = unique(Hmaster[,"Borough"]), selected = unique(Hmaster[,"Borough"])[1])),
                column(width = 5, pickerInput(inputId="year_3", label = "CHOOSE YEAR(S)", choices = unique(Hmaster[,"Year"]), selected = unique(Hmaster[,"Year"])[5],options = list(`actions-box` = TRUE),
                                              multiple = TRUE))
              ),
              fluidRow(
                column(width = 6,
                       box(height = 500,
                           title = strong("Total Cost of Heating Gas per Borough"), width = NULL, solidHeader = TRUE, 
                           status = "primary", dygraphOutput("heat"))
                ),
                column(width = 6,
                       box(height = 500,
                           title = strong("Total Therm Consumption per Borough"), width = NULL, solidHeader = TRUE, 
                           status = "primary", dygraphOutput("heat_2"))
                ),
                column(width = 12,
                       box(
                           title= strong("Scatter Plot Between Total Therm Consumption and Total Cost of Heating Gas per Borough"), width=1000, solidHeader = TRUE,
                           status = "primary", ggiraphOutput("scatters_3"))
                       
                )
              )
      )
      
              
)
)))


    
  


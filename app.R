
############################## LOAD PACKAGES #################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny)
library(readr)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

################################# INSERT DATA #################################

load('sewanee_weather copy.rds')
load('utilities copy.rds')

#-------  MONTH DATAFRAME -----
month<-c('January','February','March','April','May','June','July','August','September','October','November','December')
numbers<-c(1,2,3,4,5,6,7,8,9,10,11,12)

numberm<-data.frame(month,numbers)

# ---- TEMPERATURE ----
temp <- sewanee_temp %>%
  filter(stat == 'avg',
         year%in%c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))%>%
  mutate(
    temp = as.numeric(temp)
  )

temp_wmonths <-merge(temp,numberm,by='month')
temp_wmonths<-temp_wmonths%>%arrange(year)

# ---- WATER ----

resiu<- utilities %>%
  filter(type == "Residential - Residence Hall") %>%
  mutate(
    gallons = as.numeric(gallons),
    water_cost = as.numeric(water_cost),
    capacity = as.numeric(capacity),
    month = as.numeric(month)
  )

#  ------ RAIN ----
sewanee_rain<-sewanee_rain%>%
  filter(year%in%c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))
 rain<-merge(sewanee_rain,numberm,by='month')
 rain<-rain%>%arrange(year)%>%
   mutate(numbers==as.numeric(numbers),
          inches==as.numeric(inches))
 weather<-merge(temp_wmonths,rain,by=c('month','year','numbers'))
 
######################################################################
ui <- fluidPage(
  titlePanel("Sewanee Climate & Water Systems"),
  p("Exploring how seasonal weather patterns influence residential water use and cost across Sewanee dormitories."),
  tabsetPanel(
    tabPanel("Water Use",
             fluidRow(
               column(2,
                      selectInput('year',
                                  'Select Year:',
                                  choices = unique(resiu$year),
                                  selected = max(resiu$year))
               ),
               
               column(3,
                      radioButtons("sort",
                                   "Sort Residential Halls:",
                                   choices = c("Alphabetical", "Capacity"),
                                   selected = "Alphabetical")
               ),
               
               column(3,
                      uiOutput("building.ui")
               ),
               
               column(3,
                      radioButtons('yvar',
                                   'Variable:',
                                   choices = c("Water Use (gallons)"="gallons",
                                               "Water Cost"="water_cost"),
                                   selected = "gallons")
               )
             ),
             
             fluidRow(
               column(12, plotlyOutput('resiuplot'))
             )
    ),
    
    ################ TEMPERATURE ################
    tabPanel("Weather",
             
             fluidRow(
               column(2,
                      selectInput('temp_year',
                                  'Select Year:',
                                  choices = unique(weather$year),
                                  selected = max(weather$year))
               ),
               column(3,
                      radioButtons('yvar2',
                                   'Variable:',
                                   choices = c("Rainfall (inches)"="inches",
                                               "Average Temperature"="temp"),
                                   selected = "inches")
             ),
             
             fluidRow(
               column(12, plotlyOutput('tempplot'))
             )
    )),
    
    ################ DATA VIEWER ################
    tabPanel("Data Viewer: Water Utilities",
             DTOutput('dt1')),
    
    tabPanel('Data Viewer: Weather',
             DTOutput('dt2')),
  
))
######################### SERVER #############################
server <- function(input, output) {
  
  ################ BUILDING DROPDOWN ################
  output$building.ui <- renderUI({
    
    building_data <- resiu %>%
      select(building, capacity) %>%
      distinct()
    
    if (input$sort == "Capacity") {
      building_data <- building_data %>%
        arrange(desc(capacity))
    } else {
      building_data <- building_data %>%
        arrange(building)
    }
    
    selectInput("building",
                "Select Residential Hall:",
                choices = building_data$building,
                multiple = TRUE,
                selected = building_data$building[1])
  })
  
  ################ WATER FILTER ################
  filtered_data <- reactive({
    
    req(input$building)
    
    resiu %>%
      filter(
        year == input$year,
        building %in% input$building
      )
  })
  
  ################ TEMPERATURE FILTER ################
  filtered_temp <- reactive({
    
    weather %>%
      filter(year == as.numeric(input$temp_year))
  })
  
  ################ WATER PLOT ################
  output$resiuplot <- renderPlotly({
    
    p <- ggplot(filtered_data(),
                aes(x = month,
                    y = .data[[input$yvar]],
                    color = building,
                    group = building)) +
      geom_line(size = 1) +
      geom_point() +
      scale_x_continuous(
        breaks = 1:12,
        labels = month.abb
      ) +
      labs(
        title = "Residential Water Use Across Months",
        x = "Month",
        y = input$yvar,
        color = "Dorm"
      ) +
      theme_classic()
    
    ggplotly(p)
  })
  
  ################ TEMPERATURE PLOT ################
  output$tempplot <- renderPlotly({
    
    df <- filtered_temp()
    
    p2 <- ggplot(df, aes(x =numbers, y = .data[[input$yvar2]])) +
      geom_line(group = 1, color = "red") +
      geom_point(color = "red") +
      scale_x_continuous(
        breaks = 1:12,
        labels = month.abb
      ) +
      labs(
        title = "Seasonal Weather Patterns",
        x = "Month",
        y = input$yvar2
      ) +
      theme_classic()
    
    ggplotly(p2)
  })
  
  ################ DATA TABLE ################
  output$dt1 <- renderDT({
    datatable(resiu)
  })
  
  output$dt2<-renderDT({
    datatable(weather)
  })
}
########################### RUN APP #########################

shinyApp(ui = ui, server = server)

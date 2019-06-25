library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("simplex"),
  
  h2("Road Collisions in Toronto"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("action", "Create Map"),
      br(), br(), 
      
      dateRangeInput("acc_date", label = "Date of Accident",
                     start = min(as.Date(accidents$Date)), end = max(as.Date(accidents$Date)),
                     min = min(as.Date(accidents$Date)), max = max(as.Date(accidents$Date))),
      
      checkboxGroupInput("fatal", label = "Fatal Accident",
                         choices = c("Yes", "No"),
                         selected = c("Yes", "No")),
      
      selectInput("auto_type", label = "Vehicles Involved",
                  choices = c("Automobile", "Pedestrian", "Bicycle", "Motorcycle", 
                              "Truck", "Transit Vehicle", "Emergency Vehicle"),
                  selected = c("Automobile"), multiple = TRUE),
      
      selectInput("precip", label = "Weather Condition",
                  choices = c("Rain", "Snow"), 
                  selected = NULL, multiple = TRUE),
      
      selectInput("road_class", label = "Road Class",
                  choices = c("Arterial", "Collector", "Expressway", "Local", "Any"),
                  selected = c("Any")),
      
      selectInput("traffic_ctrl", label = "Traffic Control",
                  choices = c("No Traffic Control", "Human Control", "Traffic Sign",
                              "Pedestrian Crossing", "Any"),
                  selected = c("Any")),
      
      sliderInput("acc_time", label = "Hour of accident",
                  min = 0, max = 24, step = 1,
                  value = c(0, 24)))
    ,
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Interactive Map", 
                           leafletOutput("acc_map"), 
                           dataTableOutput("acc_data"))
                  ,
                  tabPanel("Summary",
                           plotlyOutput("acc_plot_full"),
                           br(), br(), 
                           plotlyOutput("acc_plot_full_prop"),
                           br(), br(),
                           plotlyOutput("acc_plot_month"),
                           br(), br(), 
                           plotlyOutput("acc_plot_month_prop"))
                  ,
                  tabPanel("Frequency Tables",
                           plotlyOutput("impact_type_plot"),
                           br(), br(),
                           plotlyOutput("age_plot"),
                           br(), br(),
                           plotlyOutput("driver_action_plot"))
      )
    ))
))
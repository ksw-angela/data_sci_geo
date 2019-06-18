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
      
      selectInput("auto_type", label = "Involved Vehicles",
                  choices = c("Automobile", "Pedestrian", "Bicycle", "Motorcycle", 
                              "Truck", "Emergency Vehicle"),
                  selected = c("Automobile"), multiple = TRUE),
      
      selectInput("precip", label = "Weather Condition",
                  choices = c("Rain", "Snow"), 
                  selected = NULL, multiple = TRUE),
      
      selectInput("visib", label = "Visibility",
                  choices = c("Clear", "Not Clear"),
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
                  value = c(0, 24)),
      
      sliderInput("population", label = "2016 Population of Neighborhood",
                  min = 6000, max = 70000, step = 1000,
                  value = c(6000, 70000)),
      
      checkboxInput("pop_label", label = "Overlay 2016 Population",
                    value = F))
    ,
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Interactive Map", 
                           leafletOutput("acc_map"), 
                           dataTableOutput("acc_data"))
                  ,
                  tabPanel("Frequency Table",
                           plotlyOutput("acc_plot_full"),
                           plotlyOutput("acc_plot_full_prop"),
                           br(),
                           plotlyOutput("acc_plot_month"),
                           plotlyOutput("acc_plot_month_prop"),
                           br(), 
                           verbatimTextOutput("plotlyclick"),
                           dataTableOutput("acc_data2")))
    )
  ))
)
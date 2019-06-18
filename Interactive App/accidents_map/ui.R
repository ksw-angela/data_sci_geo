library(shiny)

shinyUI(fluidPage(
  
  h2("Road Accidents in Toronto"),
  
  sidebarLayout(
               sidebarPanel(
                 actionButton("action", "Create Map"),
                 br(), br(), 
                 
                 dateRangeInput("acc_date", label = "Date of accident",
                             start = as.Date("2007-01-01"), end = max(as.Date(accidents$date)),
                             min = min(as.Date(accidents$date)), max = max(as.Date(accidents$date))),
                 
                 checkboxGroupInput("fatal", label = "Accident Fatality",
                                    choices = c("Fatal", "Non-Fatal Injury"),
                                    selected = c("Fatal", "Non-Fatal Injury")),
                 
                 selectInput("auto_type", label = "Other Vehicles Involved",
                             choices = c("Automobile", "Pedestrian", "Bicycle", "Motorcycle", 
                                         "Truck", "Emergency Vehicle", "All"),
                             selected = c("All")),
                 
                 selectInput("precip", label = "Weather Condition",
                             choices = c("Clear", "Precipitated", "All"), 
                             selected = c("All")),
                 
                 selectInput("visib", label = "Visibility",
                             choices = c("Clear", "Not Clear", "All"),
                             selected = c("All")),
                 
                 selectInput("road_class", label = "Road Class",
                             choices = c("Arterial", "Collector", "Expressway", "Local", ""),
                             selected = c("")),
                 
                 selectInput("traffic_ctrl", label = "Traffic Control",
                             choices = c("No Traffic Control", "Human Control", "Traffic Sign",
                                         "Pedestrian Crossing", ""),
                             selected = c("")),
                 
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
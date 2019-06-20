library(shiny)

shinyServer(function(input, output) {
   
  # Filter accident data based on inputs in side panel
  # This step only takes place after the 'Create Map' button has been pressed
  
  filtered_accidents <- eventReactive(input$action, {
    
    accidents %>%
      # Filter by date of accident
      filter(Date >= as.Date(input$acc_date[1]) & Date <= as.Date(input$acc_date[2])) %>%
      
      # Filter by whether there was a fatality
      filter(fatal %in% input$fatal) %>%
      
      # Filter by vehicles involved
      filter_if("Automobile" %in% input$auto_type, AUTOMOBILE = "Yes") %>%
      filter_if("Pedestrian" %in% input$auto_type, PEDESTRIAN == "Yes") %>%
      filter_if("Bicycle" %in% input$auto_type, CYCLIST == "Yes") %>%
      filter_if("Motorcycle" %in% input$auto_type, MOTORCYCLE == "Yes") %>%
      filter_if("Truck" %in% input$auto_type, TRUCK == "YES") %>%
      filter_if("Emergency Vehicle" %in% input$auto_type, EMERG_VEH == "Yes") %>%
      filter_if("Transit Vehicle" %in% input$auto_type, TRSN_CITY_VEH == "Yes") %>%
      
      # Filter by whether it precipitated on day of accident
      filter_if("Rain" %in% input$precip, `Tot Rain` > 0) %>%
      filter_if("Snow" %in% input$precip, `Tot Snow` > 0) %>%
      
      # Filter by road class
      filter(input$road_class == "" | input$road_class == ROAD_CLASS) %>%
      
      # Filter by traffic control
      filter(input$traffic_ctrl == "" | input$traffic_ctrl == TRAFFCTL) %>%
      
      # Filter by hour of accident
      filter(hour(Date_Time) >= input$acc_time[1] & hour(Date_Time) <= input$acc_time[2])
  })
  
  # Data used for map
  filtered_plot_accidents <- reactive({
    filtered_accidents() %>%
      group_by(ACCNUM) %>%
      summarize_at(.vars = vars(LONGITUDE, LATITUDE, ACCLASS, Date, 
                                STREET1, STREET2),
                   .funs = funs(unique(.)))
  })
  
  # Pop-up label for accident
  labs_acc <- reactive({
    paste0("<b>", filtered_plot_accidents()$ACCLASS, "</b><br/>", 
           filtered_plot_accidents()$Date, "<br/>", 
           filtered_plot_accidents()$STREET1, ", ",
           filtered_plot_accidents()$STREET2)
  })
  
  # Leaflet map - Leaflet adds interactivity to map
  output$acc_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addCircles(
        data = filtered_plot_accidents(),
        lng = ~ LONGITUDE, lat = ~ LATITUDE,
        color = ~ pal2(ACCLASS),
        label = lapply(labs_acc(), HTML)
      )%>%
      addLegend(
        pal = pal2,
        values = filtered_plot_accidents()$ACCLASS,
        opacity = 1,
        title = 'Accident Class'
      )%>%
      setView(lng = -79.381989, lat = 43.729214, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
})

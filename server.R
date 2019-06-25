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
      filter(if ("Automobile" %in% input$auto_type) AUTOMOBILE == "Yes"
             else is.character(AUTOMOBILE)) %>%
      filter(if ("Pedestrian" %in% input$auto_type) PEDESTRIAN == "Yes"
             else is.character(PEDESTRIAN)) %>%
      filter(if ("Bicycle" %in% input$auto_type) CYCLIST == "Yes"
             else is.character(CYCLIST)) %>%
      filter(if ("Motorcycle" %in% input$auto_type) MOTORCYCLE == "Yes"
             else is.character(MOTORCYCLE)) %>%
      filter(if ("Truck" %in% input$auto_type) TRUCK == "YES"
             else is.character(TRUCK)) %>%
      filter(if ("Emergency Vehicle" %in% input$auto_type) EMERG_VEH == "Yes"
             else is.character(EMERG_VEH)) %>%
      
      # Filter by whether it precipitated on day of accident
      filter(if ("Rain" %in% input$precip) `Tot Rain` > 0 else is.numeric(`Tot Rain`)) %>%
      filter(if ("Snow" %in% input$precip) `Tot Snow` > 0 else is.numeric(`Tot Snow`)) %>%
      
      # Filter by road class
      filter(input$road_class == "Any" | input$road_class == ROAD_CLASS) %>%
      
      # Filter by traffic control
      filter(input$traffic_ctrl == "Any" | input$traffic_ctrl == TRAFFCTL) %>%
      
      # Filter by hour of accident
      filter(hour(Date_Time) >= input$acc_time[1] & hour(Date_Time) <= input$acc_time[2])
  })
  
  
  ### INTERACTIVE MAP TAB
  
  # Data used for map
  filtered_plot_accidents <- reactive({
    filtered_accidents() %>%
      group_by(ACCNUM) %>%
      summarize_at(.vars = vars(LONGITUDE, LATITUDE, ACCLASS, Date, 
                                STREET1, STREET2, Involved, Fatalities),
                   .funs = funs(unique(.)))
  })
  
  # Pop-up label for accident
  labs_acc <- reactive({
    paste0("<b>", filtered_plot_accidents()$ACCLASS, "</b><br/>", 
           filtered_plot_accidents()$Date, "<br/>", 
           filtered_plot_accidents()$STREET1, ", ",
           filtered_plot_accidents()$STREET2, "<br/>",
           "Parties involved: ", filtered_plot_accidents()$Involved, "<br/>",
           "Fatalities: ", filtered_plot_accidents()$Fatalities)
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
  
  # Displays a table of accidents that match inputs below the map
  # Also displays the table for accidents clicked on map
  filtered_table_accidents <- reactive({
    if(is.null(input$acc_map_click))
      return(filtered_accidents())
    else if(length(input$acc_map_click) == 3){
      return(filtered_accidents() %>%
               filter(LATITUDE == input$acc_map_click[1] & 
                        LONGITUDE == input$acc_map_click[2]))
    }
    else{
      return(filtered_accidents())
    }
  })
  
  # Data displayed as a data table
  output$acc_data <- renderDataTable({
    filtered_table_accidents() %>%
      select(Accident = ACCNUM, `Accident Class` = ACCLASS, Date = Date_Time,
             `Street 1` = STREET1, `Street 2` = STREET2,
             Person = INVTYPE, Age = INVAGE, Injury = INJURY, Vehicle = VEHTYPE,
             Manoeuver = MANOEUVER, `Driver Action` = DRIVACT,
             `Driver Condition` = DRIVCOND) %>%
      arrange(Date, Accident)})
  
  
  ### FREQUENCY TABLES TAB
  
  # Data displayed as plotly graphs in `Frequency Tables` tab
  plotlydata <- reactive({
    filtered_accidents() %>%
      group_by(ACCLASS) %>%
      ungroup() %>%
      mutate(year = year(Date),
             month = month(Date),
             num_days = as.numeric(days_in_month(as.Date(Date))))
  })
  
  # Interactive plot of number of accidents by month
  output$acc_plot_full <- renderPlotly({
    data <- plotlydata() %>%
      group_by(year, month, ACCLASS) %>%
      summarize(num = n()) %>%
      ungroup() %>%
      mutate(Date = as.Date(paste(year, month, "01", sep = "-")))
    
    p <- ggplot(data, aes(x = Date, y = num, col = ACCLASS)) +
      geom_point(aes(text = paste(month.abb[month], year, "<br><b>", 
                                  "Number of Accidents:</b>", num)), 
                 alpha = 0.8) + ylab("Number of Accidents") + xlab("Date") + 
      labs(color = "Accident Class") + ggtitle("Number of Accidents by Month") + 
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Interactive plot of proportion of deadly accidents by month
  output$acc_plot_full_prop <- renderPlotly({
    data <- plotlydata() %>%
      group_by(month, year) %>%
      summarize(perc_fatal = sum(ACCLASS == "Fatal")/n()) %>%
      ungroup() %>%
      mutate(Date = as.Date(paste(year, month, "01", sep = "-")))

    p <- ggplot(data, aes(x = Date, y = perc_fatal)) +
      geom_point(aes(text = paste(month.abb[month], year, "<br><b>", 
                                  "Fatal:</b>",
                                  round(perc_fatal * 100, 2), "%")), alpha = 0.8) +
      ylab("% Fatal") + xlab("Date") + labs(color = "Accident Class") + 
      ggtitle("Proportion of Fatal Accidents by Month") + theme_minimal()

    ggplotly(p, tooltip = "text")
  })
  
  # Interactive plot of filtered accidents by month normalized so each month has 30 days
  output$acc_plot_month <- renderPlotly({
    data <- plotlydata() %>%
      group_by(month, year, num_days, ACCLASS) %>%
      summarize(num_accidents = n()) %>%
      ungroup() %>%
      mutate(normalized_acc = num_accidents * (30/num_days)) %>%
      group_by(month, ACCLASS) %>%
      summarize(num = round(mean(normalized_acc)))

    p <- ggplot(data, aes(x = month, y = num, col = ACCLASS)) +
      geom_point(aes(text = paste(month.abb[month], "<br><b>",
                                  "Number of Accidents (Normalized):</b>", num)), alpha = 0.8) +
      geom_line() + ylab("Number of Accidents (Normalized)") + xlab("Month") +
      labs(color = "Accident Class") + scale_x_continuous(breaks = round(seq(1, 12, by = 1))) +
      ggtitle("Number of Accidents Assuming 30 Day Months") + theme_minimal()

    ggplotly(p, tooltip = "text")
  })
  
  # Interactive plot of proportion of deadly accidents by month
  output$acc_plot_month_prop <- renderPlotly({
    data <- plotlydata() %>%
      group_by(year, month, num_days) %>%
      summarize(num_fatalities = sum(ACCLASS == "Fatal")/n()) %>%
      ungroup() %>%
      mutate(normalized_fatal = num_fatalities * (30/num_days)) %>%
      group_by(month) %>%
      summarize(num = mean(normalized_fatal))

    p <- ggplot(data, aes(x = month, y = num)) +
      geom_point(aes(text = paste(month.abb[month], "<br><b>",
                                  "Proportion of Fatalities (Normalized):</b>", 
                                  round(num * 100, 2), "%")), alpha = 0.8) +
      geom_line() + ylab("% Fatal (Normalized)") + xlab("Month") +
      labs(color = "Accident Class") + scale_x_continuous(breaks = round(seq(1, 12, by = 1))) +
      ggtitle("Proportion of Fatal Accidents Assuming 30 Day Months") + theme_minimal()

    ggplotly(p, tooltip = "text")
  })
  
})

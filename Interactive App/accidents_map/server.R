library(shiny)

shinyServer(function(input, output) {
  
  # Filter accident data
  filtered_accidents <- eventReactive(input$action, {
    accidents %>%
      # Filter by date of accident
      filter(date >= as.Date(input$acc_date[1]) & date <= as.Date(input$acc_date[2])) %>%
      # Filter by whether there was a fatality
      filter(acc_class %in% input$fatal) %>%
      # Filter by other vehicles involved
      mutate(i_bike = if_else(inv_cyc == 1, "Bicycle", NA_character_),
             i_emerg = if_else(inv_emergveh == 1, "Emergency Vehicle", NA_character_),
             i_moto = if_else(inv_moto == 1, "Motorcycle", NA_character_),
             i_ped = if_else(inv_ped == 1, "Pedestrian", NA_character_),
             i_truck = if_else(inv_truck == 1, "Truck", NA_character_)) %>%
      mutate(vehicles_involved = gsub("NA", "", gsub(", NA", "",
                                      gsub("NA, ", "", paste(i_bike, i_emerg, i_moto, i_ped, i_truck,
                                                             sep = ", ")))),
             vehicles_involved_input = as.character(paste(sort(input$auto_type), collapse = ", "))) %>%
      filter(input$auto_type == "All" | (input$auto_type == "Automobile" & vehicles_involved == "") |
               vehicles_involved == vehicles_involved_input) %>%
      select(-i_bike, -i_emerg, -i_moto, -i_ped, -i_truck, -vehicles_involved,
             -vehicles_involved_input) %>%
      # Filter by whether it precipitated that day
      filter(input$precip == "All" | ("Precipitated" %in% input$precip & tot_precip_mm > 0) |
               ("Clear" %in% input$precip & tot_precip_mm == 0) | is.na(input$precip)) %>%
      # Filter by whether or not it was clear
      filter(input$visib == "All" | (input$visib == "Clear" & visibility == "Clear") | 
               (input$visib == "Not Clear" & visibility != "Clear")) %>%
      # Filter by road class
      mutate(road_class2 = if_else(grepl("Arterial", road_class), "Arterial",
                                   if_else(grepl("Collector", road_class), "Collector",
                                           if_else(grepl("Express", road_class), "Expressway", 
                                                   "Local")))) %>%
      filter(input$road_class == "" | input$road_class == road_class2) %>%
      select(-road_class2) %>%
      # # Filter by traffic control
      mutate(traffic_ctrl2 = if_else(traffic_ctrl %in% c("Police Control", "School Guard",
                                                         "Traffic Controller"), "Human Control",
                                     if_else(traffic_ctrl %in% c("Stop Sign", "Traffic Signal",
                                                                 "Yield Sign", "Traffic Gate"),
                                             "Traffic Sign",
                                             if_else(traffic_ctrl %in% c("Pedestrian Crossover",
                                                                         "Streetcar (Stop for)"),
                                                                         "Pedestrian Crossing",
                                                     "No Traffic Control")))) %>%
      filter(input$traffic_ctrl == "" | input$traffic_ctrl == traffic_ctrl2) %>%
      select(-traffic_ctrl2) %>%
      # Filter by hour of accident
      filter(hour >= input$acc_time[1] & hour <= input$acc_time[2]) %>%
      # Filter by population of neighborhood at 2016
      filter(population_2016 >= input$population[1] & population_2016 <= input$population[2])
  })
  
  # Plotting data
  filtered_plot_accidents <- reactive({
    filtered_accidents() %>%
      group_by(accident_key) %>%
      summarize_at(.vars = vars(long, lat, acc_class, date, street1, street2, parties_involved,
                                acc_class),
                   .funs = funs(unique(.)))
  })
  
  # Pop-up label for accident
  labs_acc <- reactive({
    paste0("<b>", filtered_plot_accidents()$acc_class, "</b><br/>", 
           filtered_plot_accidents()$date, "<br/>", 
           filtered_plot_accidents()$street1, ", ",
           filtered_plot_accidents()$street2, "<br/>",
           "Parties involved:", filtered_plot_accidents()$parties_involved)
  })
  
  # Leaflet map
  output$acc_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addCircles(
        data = filtered_plot_accidents(),
        lng = ~ long, lat = ~ lat,
        color = ~ pal2(acc_class),
        label = lapply(labs_acc(), HTML)
      )%>%
      addLegend(
        pal = pal_pop2,
        values = neighborhoods$X2016pop,
        opacity = 1,
        title = "2016 Population",
        labFormat = labelFormat(digits=7)
      )%>%
      addLegend(
        pal = pal2,
        values = filtered_plot_accidents()$acc_class,
        opacity = 1,
        title = 'Accident Class'
      )%>%
      setView(lng = -79.381989, lat = 43.729214, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  # Overlays the population data on map
  observe({
    if(input$pop_label){
      leafletProxy("acc_map") %>%
        clearShapes() %>%
        addPolygons(layerId = gsub(" *\\(.*?\\) *", "", neighborhoods$AREA_NAME),
          data = neighborhoods[3],
          color = ~pal_pop2(X2016pop),
          fillOpacity = 0.5,
          weight = 1,
          highlight = highlightOptions(
            weight = 2,
            color = "#666"
          ),
          label = lapply(labs_hood, HTML)
        )%>%
        addCircles(
          data = filtered_plot_accidents(),
          lng = ~ long, lat = ~ lat,
          color = ~ pal2(acc_class),
          label = lapply(labs_acc(), HTML)
        )
    }
    else if(!input$pop_label){
      leafletProxy("acc_map") %>%
        clearShapes() %>%
        addCircles(
          data = filtered_plot_accidents(),
          lng = ~ long, lat = ~ lat,
          color = ~ pal2(acc_class),
          label = lapply(labs_acc(), HTML)
        )
    }
  })
  
  # Allows the data to be filtered by accident clicked
  filtered_table_accidents <- reactive({
    if(is.null(input$acc_map_click))
      return(filtered_accidents())
    else if(length(input$acc_map_click) == 3){
      return(filtered_accidents() %>%
               filter(lat == input$acc_map_click[1] & 
                        long == input$acc_map_click[2]))
    }
    # Or by neighborhood
    else if(length(input$acc_map_shape_click) == 4){
      return(filtered_accidents() %>%
             filter(hood_name == input$acc_map_shape_click[1]))
    }
    else{
      return(filtered_accidents())
    }
  })
  
  # Data displayed as a data table
  output$acc_data <- renderDataTable({
    filtered_table_accidents() %>%
        select(Accident = accident_key, `Accident Class` = acc_class,
               Date = date, Time = time,
               `Street 1` = street1, `Street 2` = street2,
               Person = person_type, Age = person_age, Injury = injury,
               Vehicle = veh_type,
               Manoeuver = manoeuver, `Driver Action` = driver_act,
               `Driver Condition` = driver_cond,
               `Road Class` = road_class, Located = located,
               Visibility = visibility, Light = light) %>%
        arrange(Date, Accident)})
  
  # Plotly data
  plotlydata <- reactive({
    filtered_accidents() %>%
      group_by(accident_key) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      mutate(month_year = as.yearmon(date),
             month = month(date),
             num_days = as.numeric(days_in_month(as.Date(date))))
  })

  # Interactive plot of filtered accidents my month and year
  output$acc_plot_full <- renderPlotly({
    data <- plotlydata() %>%
      group_by(month_year, acc_class) %>%
      summarize(num = n())
    
    p <- ggplot(data, aes(x = month_year, y = num, 
                    col = acc_class)) +
      geom_point(aes(text = paste(month_year, "<br><b>", "Number of Accidents:</b>", num)), 
                 alpha = 0.8) + 
      stat_smooth(se = F) + ylab("Number of Accidents") + xlab("Date") + 
      labs(color = "Accident Class") + theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Interactive plot of proportion of deadly accidents by month and year
  output$acc_plot_full_prop <- renderPlotly({
    data <- plotlydata() %>%
      group_by(month_year) %>%
      summarize(perc_fat = sum(acc_class == "Fatal")/n())
    
    p <- ggplot(data, aes(x = month_year, y = perc_fat)) +
      geom_point(aes(text = paste(month_year, "<br><b>", "Proportion of Fatalities:</b>", 
                                  round(perc_fat * 100, 2), "%")), alpha = 0.8) + 
      stat_smooth(se = F) + ylab("Number of Accidents") + xlab("Date") + 
      labs(color = "Accident Class") + theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Interactive plot of filtered accidents by month normalized so each month has 30 days
  output$acc_plot_month <- renderPlotly({
    data <- plotlydata() %>%
      group_by(month_year, month, num_days, acc_class) %>%
      summarize(num_accidents = n()) %>%
      ungroup() %>%
      mutate(normalized_acc = num_accidents * (30/num_days)) %>%
      group_by(month, acc_class) %>%
      summarize(num = round(mean(normalized_acc)))
    
    p <- ggplot(data, aes(x = month, y = num, col = acc_class)) +
      geom_point(aes(text = paste(month.abb[month], "<br><b>", 
                                  "Number of Accidents (Normalized):</b>", num)), alpha = 0.8) + 
      geom_line() + ylab("Prop of Fatal Accidents (Normalized)") + xlab("Month") + 
      labs(color = "Accident Class") + scale_x_continuous(breaks = round(seq(1, 12, by = 1))) + 
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Interactive plot of proportion of deadly accidents by month
  output$acc_plot_month_prop <- renderPlotly({
    data <- plotlydata() %>%
      group_by(month_year, month, num_days) %>%
      summarize(num_fatalities = sum(acc_class == "Fatal")/n()) %>%
      ungroup() %>%
      mutate(normalized_fat = num_fatalities * (30/num_days)) %>%
      group_by(month) %>%
      summarize(num = round(mean(normalized_fat), 2))
    
    p <- ggplot(data, aes(x = month, y = num)) +
      geom_point(aes(text = paste(month.abb[month], "<br><b>", 
                                  "Proportion of Fatalities (Normalized):</b>", num)), alpha = 0.8) + 
      geom_line() + ylab("Proportion of Fatalities (Normalized)") + xlab("Month") + 
      labs(color = "Accident Class") + scale_x_continuous(breaks = round(seq(1, 12, by = 1))) + 
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
})

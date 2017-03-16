server <- shinyServer(function(input, output, session) {
    filteredData <- reactive({
      data = df_history[df_history$buildingCreateWestYear >= input$range[1] && df_history$buildingCreateWestYear <= input$range[2],]
    })
    
    output$map <- renderLeaflet({
      leaflet(df_history) %>% 
      addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })
    
    observe({
      proxy <- leafletProxy("map", data = df_history)
      proxy %>% clearControls()
      
      if (input$legend) {
        catego <- unique(df_history$owner)
        proxy %>% addLegend(position = "bottomright", colors = rgb,
                            labels = catego
        )
        
        leafletProxy("map", data = filteredData()) %>%
          clearShapes() %>% clearMarkerClusters() %>%
          addCircles(weight = ~hitRate/1000,
                     color = ~rgb,
                     opacity = 1,
                     fillOpacity = 1)
        
      }
      else{
        leafletProxy("map", data = filteredData()) %>%
          clearShapes() %>% clearMarkerClusters() %>%
          addMarkers(popup = ~paste("<h5>",df_history$name,"</h5>",
                                    df_history$buildingCreateWestYear,"年<br>",
                                    df_history$address,"<br>"),
                     clusterOptions = markerClusterOptions()) # %>%
        
      }
    })
  }
)
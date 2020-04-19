######################### Define SERVER logic ----##################

server <- function(input, output) {
    ## Num of commuters in Jan 2020 between any 2 busstops
    # Remove the matrix diagonal
    diag(myflows) <- 0
    
    # Selection of flows > 500
    flowSel1 <- firstflowsg(mat = myflows, method = "xfirst", k = 5000)
    # Selection of flows > 1000
    flowSel2 <- firstflowsg(mat = myflows, method = "xfirst", k = 10000)
    # table output where flowSel1 ( k <=5000))
    flowtable1 <- compmat(mat1 = myflows, mat2 = myflows * flowSel1, digits = 1)
    # table output where flowSel1 ( k <=10000))
    flowstable2 <- compmat(mat1 = myflows, mat2 = myflows * flowSel2, digits = 1)
    
    output$flow_jy <- renderPrint({ 
        {if(input$flow_input== 5000){flowtable1
        }else if(input$flow_input == 10000){flowstable2
        }
        }
    }) # end of renderPrint Statistics
    
    # Plot Flows diagram
    ## Remove the matrix diagonal
    diag(myflows) <- 0
    
    ## Select flows that represent at least 20% of the sum of outgoing flows for 
    ## each urban area. ( can select other methods )
    flowSel1 <- firstflows(mat = myflows/rowSums(myflows)*100, method = "xfirst", 
                           k = 20)
    
    ## Select the dominant flows (incoming flows criterion)
    flowSel2 <- domflows(mat = myflows, w = colSums(myflows), k = 1)
    
    ## Combine selections
    flowSel <- myflows * flowSel1 * flowSel2
    
    ## Node weights
    inflows <- data.frame(id = colnames(myflows), w = colSums(myflows))
    
    ## Plot dominant flows map
    output$polygon_jy <- renderImage({plot(mpsz, col = "#cceae7", border = NA)})
    #output$flow
    opar <- par(mar = c(0,0,2,0))
    
    ## Mengyong Plot Proportionate Symbol Map
    
    location_my_map <- reactive({
        location_my %>% dplyr::filter(PlanningArea == input$planning_region_my)%>% 
            dplyr::filter(Time == input$time_my)%>%
            dplyr::filter(Day == input$week_my)
    })
    
    output$map_my <- renderLeaflet({
        map_data <- location_my_map()
        map_data %>% leaflet(width = 800, height = 600)%>%
            addProviderTiles("CartoDB") %>% 
            addCircleMarkers(lng = ~lon, 
                             lat = ~lat,
                             radius = ~tap_in_out_radius,
                             fillOpacity = 0.75,
                             label = ~Description, 
                             popup = ~paste0("<b>", Description, "</b>", "<br/>", 'Tap in Volume: ', TapIns, "<br/>", 'Tap out volume: ', TapOuts),
                             color =  ~pal(PlanningArea)
                             ) %>%
            setMaxBounds(lng1 = 103.801959 + .25, 
                         lat1 = 1.32270 + .25, 
                         lng2 = 103.801959 - .25, 
                         lat2 = 1.32270 - .25)
    })
    
    output$tbl_my <- DT::renderDT({
        DT::datatable(
            #location_my_map(),
            location_my_map()[c('PlanningArea', 'Day', 'Time', 'RoadName', 'TapIns', 'TapOuts')],
            extensions = "Scroller",
            style = "bootstrap",
            class = "compact",
            width = "100%",
            options = list(
                deferRender = TRUE,
                scrollY = 300,
                scroller = TRUE,
                dom = 'tp')
        )
    })
    
    ## Mengyong Plot Centrality Map
    my_map_centrality_node <- reactive({
      if (input$planning_region_my_2 == 'Singapore') {
        map_table %>% 
          dplyr::filter(between.f >= input$betweenness_my[1] & between.f <= input$betweenness_my[2])%>%
          dplyr::filter(closeness.f >= input$closeness_my[1] & closeness.f <= input$closeness_my[2])%>%
          dplyr::filter(eigen.f >= input$eigen_my[1] & eigen.f <= input$eigen_my[2])%>%
          dplyr::filter(degree.f >= input$degree_my[1] & degree.f <= input$degree_my[2])
      }
      else {
        map_table %>% dplyr::filter(planning_area %in% input$planning_region_my_2)%>%
          dplyr::filter(between.f >= input$betweenness_my[1] & between.f <= input$betweenness_my[2])%>%
          dplyr::filter(closeness.f >= input$closeness_my[1] & closeness.f <= input$closeness_my[2])%>%
          dplyr::filter(eigen.f >= input$eigen_my[1] & eigen.f <= input$eigen_my[2])%>%
          dplyr::filter(degree.f >= input$degree_my[1] & degree.f <= input$degree_my[2])      
      }
    })
    
    my_map_centrality_edge <- reactive({
      if (input$planning_region_my_2 == 'Singapore') {
        edge_table %>% 
          dplyr::filter(between.f >= input$betweenness_my[1] & between.f <= input$betweenness_my[2])%>%
          dplyr::filter(closeness.f >= input$closeness_my[1] & closeness.f <= input$closeness_my[2])%>%
          dplyr::filter(eigen.f >= input$eigen_my[1] & eigen.f <= input$eigen_my[2])%>%
          dplyr::filter(degree.f >= input$degree_my[1] & degree.f <= input$degree_my[2])
      }
      else {
        edge_table %>% dplyr::filter(planning_area %in% input$planning_region_my_2)%>%
          dplyr::filter(between.f >= input$betweenness_my[1] & between.f <= input$betweenness_my[2])%>%
          dplyr::filter(closeness.f >= input$closeness_my[1] & closeness.f <= input$closeness_my[2])%>%
          dplyr::filter(eigen.f >= input$eigen_my[1] & eigen.f <= input$eigen_my[2])%>%
          dplyr::filter(degree.f >= input$degree_my[1] & degree.f <= input$degree_my[2])
      }
        })    
    
    output$map_my_centrality <- renderLeaflet({
      map_nodes <- my_map_centrality_node()
      map_edges <- my_map_centrality_edge()
      
     map3 <- map_nodes %>% leaflet(width = 800, height = 600)%>%
        addProviderTiles("CartoDB") %>% 
        setMaxBounds(lng1 = 103.801959 + .25, 
                     lat1 = 1.32270 + .25, 
                     lng2 = 103.801959 - .25, 
                     lat2 = 1.32270 - .25)
    
    for(i in 1:nrow(map_edges)){
      map3 <- addPolylines(map3, lat = as.numeric(map_edges[i, c('lat.f', 'lat.t')]), 
                           lng = as.numeric(map_edges[i, c('long.f', 'long.t')]),
                           #weight = 0.5,
                           color = 'blue'
      )
    }

    for(i in 1:nrow(map_nodes)){
      map3<-addCircleMarkers(map3, lat = as.numeric(map_nodes[i, 'lat.f']), 
                             lng = as.numeric(map_nodes[i,'long.f']),
                             radius =(as.numeric(map_nodes[i, 'combined.f'])*2),
                             color='red',
                             fillOpacity = 0.75,
                             label = map_nodes[i,'id.Description'],
                             popup = ~paste0("<b>", map_nodes[i,'Description'], "</b>", "<br/>", 'Betweenness Centrality: ', round(map_nodes[i,'between.f'],3), "<br/>", 
                                             'Closeness Centrality: ', round(map_nodes[i,'closeness.f'],3), "<br/>", 'Eigenvalue Centrality: ', round(map_nodes[i,'eigen.f'],3), 
                                             "<br/>", 'Degree Centrality: ', round(map_nodes[i,'degree.f'],3))
      )}
     map3
    })
    
    output$tbl_my_2 <- DT::renderDT({
      DT::datatable(
        #my_map_centrality_node()
        my_map_centrality_node()[c('planning_area', 'BusStopCode', 'RoadName', 'Description', 'between.f', 'closeness.f', 'eigen.f', 'degree.f')]%>%
          rename(c(PlanningArea = planning_area, BetweennessCentrality = between.f, ClosenessCentrality = closeness.f, EigenvalueCentrality= eigen.f, DegreeCentrality = degree.f)),
        extensions = "Scroller",
        style = "bootstrap",
        class = "compact",
        width = "100%",
        options = list(
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          dom = 'tp')
      )
    })

}
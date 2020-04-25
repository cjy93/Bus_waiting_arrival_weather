######################### Define SERVER logic ----##################

server <- function(input, output, session) {
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
  
  #https://mastering-shiny.org/action-dynamic.html
  sz_filter_my <- reactive({
    filter(busstops, planning_area == input$pa_my_1)
  })
  
  observeEvent(sz_filter_my(), {
    choices <- sort(unique(sz_filter_my()$subzone_name))
    updateSelectInput(session, "sz_filter_my1", choices = choices) 
  })
  
  
  location_my_map <- reactive({
    
    if (input$radio_my1 == 'SG' & input$radio_my_taps == 'tap_ins') {
      location_my %>%
        dplyr::filter(Time == input$time_my)%>%
        dplyr::filter(Day == input$week_my)%>%
        mutate(taps = TapIns)
    }
    
    else if (input$radio_my1 == 'SG' & input$radio_my_taps == 'tap_outs') {
      location_my %>%
        dplyr::filter(Time == input$time_my)%>%
        dplyr::filter(Day == input$week_my)%>%
        mutate(taps = TapOuts)
    }
    
    else if (input$radio_my1 == 'PA'& input$radio_my_taps == 'tap_ins') {
      location_my %>% dplyr::filter(PlanningArea == input$pa_my_1)%>% 
        dplyr::filter(Time == input$time_my)%>%
        dplyr::filter(Day == input$week_my)%>%
        mutate(taps = TapIns)
    }

    else if (input$radio_my1 == 'PA'& input$radio_my_taps == 'tap_outs') {
      location_my %>% dplyr::filter(PlanningArea == input$pa_my_1)%>% 
        dplyr::filter(Time == input$time_my)%>%
        dplyr::filter(Day == input$week_my)%>%
        mutate(taps = TapOuts)
    }
        
    else if (input$radio_my1 == 'SZ'& input$radio_my_taps == 'tap_ins'){
      location_my %>% dplyr::filter(subzone_name_my == input$sz_filter_my1)%>% 
        dplyr::filter(Time == input$time_my)%>%
        dplyr::filter(Day == input$week_my)%>%
        mutate(taps = TapIns)
    }
    
    else if (input$radio_my1 == 'SZ'& input$radio_my_taps == 'tap_outs'){
      location_my %>% dplyr::filter(subzone_name_my == input$sz_filter_my1)%>% 
        dplyr::filter(Time == input$time_my)%>%
        dplyr::filter(Day == input$week_my)%>%
        mutate(taps = TapOuts)
    }   
    
  })
  
  output$map_my <- renderLeaflet({
    
    map_data <- location_my_map()
    maxLat1 <- max(map_data$lat)
    maxLong1 <- max(map_data$lon)
    minLat1 <- min(map_data$lat)
    minLong1 <- min(map_data$lon)

    pal <- colorNumeric(palette = "RdPu", domain = map_data$taps**(1/2)/6)
    
    map_data %>% leaflet()%>%
      addProviderTiles("CartoDB") %>% 


      addCircleMarkers(lng = ~lon, 
                       lat = ~lat,
                       radius = ~taps**(1/2)/6,
                       fillOpacity = 0.5,
                       label = ~Description, 
                       popup = ~paste0("<b>", Description, "</b>", "<br/>", 'Planning Area: ', PlanningArea, "<br/>", 'Subzone: ', subzone_name_my, "<br/>", 'Tap in Volume: ', TapIns, "<br/>", 'Tap out volume: ', TapOuts),
                       color = 'black',
                       weight = 0.5,
                       fillColor = ~pal(taps**(1/2)/6)
                       #fillColor = '#FABFD2'
      ) %>%
      setMaxBounds(lng1 = 103.801959 + .25, 
                   lat1 = 1.32270 + .25, 
                   lng2 = 103.801959 - .25, 
                   lat2 = 1.32270 - .25) %>%
      fitBounds(minLong1,minLat1,maxLong1,maxLat1)
  })
  
  output$tbl_my <- DT::renderDT({
    DT::datatable(
      location_my_map()[c('PlanningArea', 'subzone_name_my', 'Day', 'Time', 'Description', 'RoadName', 'TapIns', 'TapOuts')]%>%
        rename(c(SubzoneName = subzone_name_my))%>%
        arrange(desc(TapIns)),
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
  
  location_my_plotly <- reactive({
    
    if (input$radio_my1 == 'SG') {
      location_plotly <- location_my %>% 
        dplyr::filter(Day == input$week_my)%>%
        dplyr::group_by(Time)%>%
        dplyr::summarise(TapIns = sum(TapIns), TapOuts = sum(TapOuts))%>%
        dplyr::ungroup()
    }
    else if (input$radio_my1 == 'PA') {
      
      location_plotly <- location_my %>% 
        dplyr::filter(PlanningArea == input$pa_my_1)%>%
        dplyr::filter(Day == input$week_my)%>%
        dplyr::group_by(Time)%>%
        dplyr::summarise(TapIns = sum(TapIns), TapOuts = sum(TapOuts))%>%
        dplyr::ungroup()
    }
    
    else {
      location_plotly <- location_my %>% 
        dplyr::filter(subzone_name_my == input$sz_filter_my1)%>%
        dplyr::filter(Day == input$week_my)%>%
        dplyr::group_by(Time)%>%
        dplyr::summarise(TapIns = sum(TapIns), TapOuts = sum(TapOuts))%>%
        dplyr::ungroup()
    }
  })
  
  title_my <- reactive({
    
    if (input$radio_my1 == 'SG') {
      'Singapore'}
    else if (input$radio_my1 == 'PA') {
      input$pa_my_1}
    else {
      input$sz_filter_my1}
  })

  output$trendPlot_my <- renderPlotly({
    location_plotly = location_my_plotly()
    title = title_my()
    
    location_plotly %>%
      plot_ly(x = ~Time, y = ~TapIns, type = "scatter", mode = "lines", color = I('dark green'), name = "Tap Ins") %>%
      add_trace(x = ~Time, y = ~TapOuts, type = "scatter", mode = "lines", color = I('red'), name = "Tap Outs") %>%
      layout(title = paste0("Passenger Volume at ", title, ' on ', input$week_my),
             xaxis = list(title = "Time of the Day"),
             yaxis = list(title = "Number of Tap Ins / Tap Outs"))
  })
    
  
  ## Mengyong Plot Centrality Map
  sz_filter_my_2 <- reactive({
    filter(busstops, planning_area == input$pa_my_2)
  })

  
  observeEvent(sz_filter_my_2(), {
    choices <- sort(unique(sz_filter_my_2()$subzone_name_my))
    updateSelectInput(session, "sz_filter_my_2", choices = choices) 
  })
  
  my_map_centrality_node <- reactive({
    if (input$radio_my2 == 'SG' & input$radio_my3 == 'between_my_filter') {
      map_table%>%
        dplyr::filter(between.f >= input$centrality_filter[1] & between.f <= input$centrality_filter[2])%>%
        mutate(combined.f = between.f)
    }
    
    else if (input$radio_my2 == 'SG' & input$radio_my3 == 'closeness_my_filter') {
      map_table%>%
        dplyr::filter(closeness.f >= input$centrality_filter[1] & closeness.f <= input$centrality_filter[2])%>%
        mutate(combined.f = closeness.f)
    }
    
    else if (input$radio_my2 == 'SG' & input$radio_my3 == 'degree_my_filter') {
      map_table%>%
      dplyr::filter(degree.f >= input$centrality_filter[1] & degree.f <= input$centrality_filter[2])%>%
        mutate(combined.f = degree.f)
    }    
    
    else if (input$radio_my2 == 'SG' & input$radio_my3 == 'eigen_my_filter') {
      map_table%>%
      dplyr::filter(eigen.f >= input$centrality_filter[1] & eigen.f <= input$centrality_filter[2])%>%
        mutate(combined.f = eigen.f)
    }   
    
    else if (input$radio_my2 == 'PA' & input$radio_my3 == 'between_my_filter') {
      map_table %>% dplyr::filter(planning_area == input$pa_my_2)%>%
        dplyr::filter(between.f >= input$centrality_filter[1] & between.f <= input$centrality_filter[2])%>%
        mutate(combined.f = between.f)
    }
    
    else if (input$radio_my2 == 'PA' & input$radio_my3 == 'closeness_my_filter') {
      map_table %>% dplyr::filter(planning_area == input$pa_my_2)%>%
        dplyr::filter(closeness.f >= input$centrality_filter[1] & closeness.f <= input$centrality_filter[2])%>%
        mutate(combined.f = closeness.f)     
    }
    
    else if (input$radio_my2 == 'PA' & input$radio_my3 == 'degree_my_filter') {
      map_table %>% dplyr::filter(planning_area == input$pa_my_2)%>%
        dplyr::filter(degree.f >= input$centrality_filter[1] & degree.f <= input$centrality_filter[2])%>%
        mutate(combined.f = degree.f)
    }
    
    else if (input$radio_my2 == 'PA' & input$radio_my3 == 'eigen_my_filter') {
      map_table %>% dplyr::filter(planning_area == input$pa_my_2)%>%
        dplyr::filter(eigen.f >= input$centrality_filter[1] & eigen.f <= input$centrality_filter[2])%>%
        mutate(combined.f = eigen.f)
    }
    
    else if (input$radio_my2 == 'SZ' & input$radio_my3 == 'between_my_filter') {
      map_table %>% dplyr::filter(subzone_name_my == input$sz_filter_my_2)%>%
        dplyr::filter(between.f >= input$centrality_filter[1] & between.f <= input$centrality_filter[2])%>%
        mutate(combined.f = between.f)
    }
    
    else if (input$radio_my2 == 'SZ' & input$radio_my3 == 'closeness_my_filter') {
      map_table %>% dplyr::filter(subzone_name_my == input$sz_filter_my_2)%>%
        dplyr::filter(closeness.f >= input$centrality_filter[1] & closeness.f <= input$centrality_filter[2])%>%
        mutate(combined.f = closeness.f)      
    }
    
    else if (input$radio_my2 == 'SZ' & input$radio_my3 == 'degree_my_filter') {
      map_table %>% dplyr::filter(subzone_name_my == input$sz_filter_my_2)%>%
        dplyr::filter(degree.f >= input$centrality_filter[1] & degree.f <= input$centrality_filter[2])%>%
        mutate(combined.f = degree.f)
    }
    
    else if (input$radio_my2 == 'SZ' & input$radio_my3 == 'eigen_my_filter') {
      map_table %>% dplyr::filter(subzone_name_my == input$sz_filter_my_2)%>%
        dplyr::filter(eigen.f >= input$centrality_filter[1] & eigen.f <= input$centrality_filter[2])%>%
        mutate(combined.f = eigen.f)
    }
  })
  
  my_map_centrality_edge <- reactive({
    if (input$radio_my2 == 'SG' & input$radio_my3 == 'between_my_filter') {
      edge_table %>%
      dplyr::filter(between.f >= input$centrality_filter[1] & between.f <= input$centrality_filter[2])%>%
        mutate(combined.f = between.f)
    }
    
    else if (input$radio_my2 == 'SG' & input$radio_my3 == 'closeness_my_filter') {
      edge_table %>%
      dplyr::filter(closeness.f >= input$centrality_filter[1] & closeness.f <= input$centrality_filter[2])%>%
        mutate(combined.f = closeness.f)
    }
    
    else if (input$radio_my2 == 'SG' & input$radio_my3 == 'degree_my_filter') {
      edge_table %>%
      dplyr::filter(degree.f >= input$centrality_filter[1] & degree.f <= input$centrality_filter[2])%>%
        mutate(combined.f = degree.f)
    }    
    
    else if (input$radio_my2 == 'SG' & input$radio_my3 == 'eigen_my_filter') {
      edge_table %>%
      dplyr::filter(eigen.f >= input$centrality_filter[1] & eigen.f <= input$centrality_filter[2])%>%
        mutate(combined.f = eigen.f)
    }   
    
    else if (input$radio_my2 == 'PA' & input$radio_my3 == 'between_my_filter') {
      edge_table %>% dplyr::filter(planning_area == input$pa_my_2)%>%
        dplyr::filter(between.f >= input$centrality_filter[1] & between.f <= input$centrality_filter[2])
    }
    
    else if (input$radio_my2 == 'PA' & input$radio_my3 == 'closeness_my_filter') {
      edge_table %>% dplyr::filter(planning_area == input$pa_my_2)%>%
        dplyr::filter(closeness.f >= input$centrality_filter[1] & closeness.f <= input$centrality_filter[2])        
    }
    
    else if (input$radio_my2 == 'PA' & input$radio_my3 == 'degree_my_filter') {
      edge_table %>% dplyr::filter(planning_area == input$pa_my_2)%>%
        dplyr::filter(degree.f >= input$centrality_filter[1] & degree.f <= input$centrality_filter[2])
    }
    
    else if (input$radio_my2 == 'PA' & input$radio_my3 == 'eigen_my_filter') {
      edge_table %>% dplyr::filter(planning_area == input$pa_my_2)%>%
        dplyr::filter(eigen.f >= input$centrality_filter[1] & eigen.f <= input$centrality_filter[2])
    }
    
    else if (input$radio_my2 == 'SZ' & input$radio_my3 == 'between_my_filter') {
      edge_table %>% dplyr::filter(subzone_name_my == input$sz_filter_my_2)%>%
        dplyr::filter(between.f >= input$centrality_filter[1] & between.f <= input$centrality_filter[2])
    }
    
    else if (input$radio_my2 == 'SZ' & input$radio_my3 == 'closeness_my_filter') {
      edge_table %>% dplyr::filter(subzone_name_my == input$sz_filter_my_2)%>%
        dplyr::filter(closeness.f >= input$centrality_filter[1] & closeness.f <= input$centrality_filter[2])        
    }
    
    else if (input$radio_my2 == 'SZ' & input$radio_my3 == 'degree_my_filter') {
      edge_table %>% dplyr::filter(subzone_name_my == input$sz_filter_my_2)%>%
        dplyr::filter(degree.f >= input$centrality_filter[1] & degree.f <= input$centrality_filter[2])
    }
    
    else if (input$radio_my2 == 'SZ' & input$radio_my3 == 'eigen_my_filter') {
      edge_table %>% dplyr::filter(subzone_name_my == input$sz_filter_my_2)%>%
        dplyr::filter(eigen.f >= input$centrality_filter[1] & eigen.f <= input$centrality_filter[2])
    }
  })
  
  output$map_my_centrality <- renderLeaflet({
    map_nodes <- my_map_centrality_node()
    map_edges <- my_map_centrality_edge()
    
    maxLat <- max(map_nodes$lat.f)
    maxLong <- max(map_nodes$long.f)
    minLat <- min(map_nodes$lat.f)
    minLong <- min(map_nodes$long.f)
    
    pal2 <- colorNumeric(palette = "RdPu", domain =c(0,1))
    
    map3 <- map_nodes %>% leaflet()%>%
      addProviderTiles("CartoDB") %>% 
      setMaxBounds(lng1 = 103.801959 + .25, 
                   lat1 = 1.32270 + .25, 
                   lng2 = 103.801959 - .25, 
                   lat2 = 1.32270 - .25)%>%
      fitBounds(minLong,minLat,maxLong,maxLat)

    if (nrow(map_edges) < 500) {
    for(i in 1:nrow(map_edges)){
      map3 <- addPolylines(map3, lat = as.numeric(map_edges[i, c('lat.f', 'lat.t')]), 
                           lng = as.numeric(map_edges[i, c('long.f', 'long.t')]),
                           weight = 2,
                           color = 'black'
      )
    }
    }
    
    for(i in 1:nrow(map_nodes)){
      map3<-addCircleMarkers(map3, lat = as.numeric(map_nodes[i, 'lat.f']), 
                             lng = as.numeric(map_nodes[i,'long.f']),
                             radius =(as.numeric(map_nodes[i, 'combined.f'])*8)**1.6,
                             color='black',
                             #fillColor = '#FABFD2',
                             fillColor = ~pal2(map_nodes[i, 'combined.f']**3),
                             weight = 1,
                             fillOpacity = 0.5,
                             label = map_nodes[i,'id.Description'],
                             popup = ~paste0("<b>", map_nodes[i,'RoadName'], "</b>", "<br/>", "<b>", map_nodes[i,'Description'], "</b>", "<br/>", 'Betweenness Centrality: ', round(map_nodes[i,'between.f'],3), "<br/>", 
                                             'Closeness Centrality: ', round(map_nodes[i,'closeness.f'],3), "<br/>", 'Eigenvalue Centrality: ', round(map_nodes[i,'eigen.f'],3), 
                                             "<br/>", 'Degree Centrality: ', round(map_nodes[i,'degree.f'],3))
                             
      )}
    map3
  })
  

  output$tbl_my_2 <- DT::renderDT({
    DT::datatable(
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
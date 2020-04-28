######################### Define SERVER logic ----##################

server <- function(input, output, session) {

  
  node <- reactive({if (input$radio == 'PA'){
    busstops <- busstops %>%
      mutate(planning_area = toupper(planning_area)) #capitalise the column in subzone
    busstops <- busstops %>% dplyr::select(c('planning_area','BusStopCode','district'))
    
    # Now inner join the 2 tables so i can find the weights
    node_pa <- dplyr::inner_join(busstops, PA, by= 'planning_area') %>%
      dplyr::select(c('OBJECTID','district','planning_area','X_ADDR','Y_ADDR'))%>%
      group_by(OBJECTID,planning_area,X_ADDR,Y_ADDR) %>%  
      summarise(weight = n()) %>%
      filter(weight > 1) %>%
      ungroup()  %>%
      rename(id = OBJECTID) %>%
      rename(name = planning_area)%>%
      rename(x = X_ADDR) %>%
      rename(y = Y_ADDR) 
    
    node_pa2 <- merge(node_pa, PA,by.x = "name", by.y="planning_area") %>%
      dplyr::select(c("id","name", "REGION_N", "x", "y","weight")) %>%
      rename(district=REGION_N)%>% mutate(id = as.character(id))
    
    node_pa2
    
  }
    else{
      busstops <- busstops %>%
        mutate(subzone_name = toupper(subzone_name)) #capitalise the column in subzone
      busstops <- busstops %>% dplyr::select(c('subzone_name','BusStopCode','district'))
      
      # Now inner join the 2 tables so i can find the weights
      node_SZ <- dplyr::inner_join(busstops, SZ, by =c('subzone_name')) %>%
        dplyr::select(c('OBJECTID','district','subzone_name','X_ADDR','Y_ADDR'))%>%
        group_by(OBJECTID,subzone_name,district,X_ADDR,Y_ADDR) %>%  
        summarise(weight = n()) %>%
        filter(weight > 1) %>%
        ungroup()  %>%
        rename(id = OBJECTID) %>%
        rename(name = subzone_name)%>%
        rename(x = X_ADDR) %>%
        rename(y = Y_ADDR) %>%
        mutate(id = as.character(id))
      node_SZ
    }
  })  # end of creating node
  
  
  edges_data <- read_csv("data/origin_dest_Full_Aggregated_BusTrips.csv")
  ## convert to upper case for Sankey
  edges_data_edit <- edges_data %>%
    mutate(subzone_origin = toupper(subzone_origin)) %>%
    mutate(subzone_destination = toupper(subzone_destination))
  
  
  edges_full<- reactive({ if (input$radio=='SZ'){
    edges <- edges_data
    #%>%
    #   select(-c('YEAR_MONTH','PT_TYPE','ORIGIN_PT_CODE','DESTINATION_PT_CODE','RoadName_Origin','Description_Origin','RoadName_Destination','Description_Destination'))
    # we need to append "subzone_origin" and "subzone_dest" so we can calculate weights based on busstops in these subzones
    edges_join <- merge(edges, busstops, by.x='BusStopCode_x', by.y = 'BusStopCode') %>% 
      rename(subzone_ori = subzone_name)  %>% rename(planning_area_origin = planning_area) #%>% select(-c("subzone_origin","subzone_destination"))
    edges_join2 <- merge(edges_join, busstops, by.x='BusStopCode_y', by.y = 'BusStopCode') %>% 
      rename(subzone_dest = subzone_name) %>% rename(planning_area_dest = planning_area)
    # remove the intermediate dataframe from cache
    
    edges <- edges_join2 %>%
      #select('subzone_ori','subzone_dest') %>%
      rename(from =subzone_ori) %>%
      rename(to = subzone_dest) 
    edges$from = toupper(edges$from)
    edges$to = toupper(edges$to)
    edges <- edges %>% dplyr::select(-c("district.x","district.y","subzone_origin","subzone_destination"))
    # add a column for category so it fit into the online model
    edges_full <- cbind(edges,category=1)
    edges_full}
    else{
      edges <- edges_data
      edges_join <- merge(edges, busstops, by.x='BusStopCode_x', by.y = 'BusStopCode') %>% 
        rename(planning_area_origin = planning_area) #%>% select(-c("subzone_origin","subzone_destination"))
      edges_join2 <- merge(edges_join, busstops, by.x='BusStopCode_y', by.y = 'BusStopCode') %>% 
        rename(planning_area_dest = planning_area)
      # remove the intermediate dataframe from cache
      
      edges <- edges_join2 %>%
        #select('subzone_ori','subzone_dest') %>%
        rename(from =planning_area_origin) %>%
        rename(to = planning_area_dest) 
      edges$from = toupper(edges$from)
      edges$to = toupper(edges$to)
      edges <- edges %>% dplyr::select(-c("district.x","district.y","subzone_origin","subzone_destination"))
      # add a column for category so it fit into the online model
      edges_full <- cbind(edges,category=1)
      edges_full
    }}) # end of reactive
  
  get_index = reactive({node() %>% dplyr::select(c('id','name'))})
  
  edge_id2 <- reactive({
    edge_id <- merge(edges_full(), get_index(), by.x = "from", by.y = "name") %>%
      dplyr::select(-c("from")) %>%
      rename(from=id) 
    edge_id2_local <- merge(edge_id, get_index(), by.x = "to", by.y = "name") %>%
      dplyr::select(-c("to")) %>%
      rename(to=id)
    
    edge_id2_local$to <- as.character(edge_id2_local$to)
    edge_id2_local$from <- as.character(edge_id2_local$from)
    edge_id2_local <- edge_id2_local %>%
      dplyr::select(c('from','to','DAY_TYPE','weight','category'))
    edge_id2_local
  }) # end of reactive edge_id2
  
  # plot the FROM Table
  # render tables
  output$mytableFrom = DT::renderDataTable({
    if (input$radio=="PA"){
      busstops%>% filter(busstops$planning_area %in% input$pa_from) %>% dplyr::select(c("planning_area", "subzone_name","BusStopCode","district"))
    }
    else{
      busstops%>% filter(toupper(busstops$subzone_name) %in% input$sz_from)%>% dplyr::select(c("planning_area", "subzone_name","BusStopCode","district"))
    }
  }) # end of renderDataTable FROM
  # https://community.rstudio.com/t/evaluation-error-operation-not-allowed-without-an-active-reactive-context/18468/3
  
  
  # plot the TO Table
  output$mytableTo = DT::renderDataTable({
    if (input$radio=="PA"){
      ## KIV
      busstops%>% filter(busstops$planning_area %in% input$pa_to)%>% dplyr::select(c("planning_area", "subzone_name","BusStopCode","district"))
    }
    else{
      busstops%>% filter(toupper(busstops$subzone_name) %in% input$sz_to)%>% dplyr::select(c("planning_area", "subzone_name","BusStopCode","district"))
    }
  }) # end of renderDataTable TO
  
  ### Very Important: To update the "To" list based on edges from the "From"
  ## for pa_to
  pa_selection_from <- reactive({
    nodeidFrom <- node()[which(node()$name %in% input$pa_from) ,] 
    edgeid <- edge_id2()[which(edge_id2()$from %in% nodeidFrom$id),] 
    nodeidTo <- node()[which(node()$id %in% edgeid$to) ,]
    nodeNameTo <- unique(nodeidTo$name)
    nodeNameTo #grab the last variable
  })
  
  # https://shiny.rstudio.com/articles/selectize.html
  # https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
  observe({
    updateSelectizeInput(session, 'pa_to', choices=sort(pa_selection_from()), server=TRUE)
  })
  ## for sz_to
  sz_selection_from <- reactive({
    nodeidFrom <- node()[which(node()$name %in% input$sz_from) ,] 
    edgeid <- edge_id2()[which(edge_id2()$from %in% nodeidFrom$id),] 
    nodeidTo <- node()[which(node()$id %in% edgeid$to) ,]
    nodeNameTo <- unique(nodeidTo$name)
    nodeNameTo #grab the last variable
  })
  
  # https://shiny.rstudio.com/articles/selectize.html
  # https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
  observe({
    updateSelectizeInput(session, 'sz_to', choices= sort(sz_selection_from()), server=TRUE)
  })
  ### end of update "To" list based on edges available
  
  ## to affect the district option in flow map
  ### Very Important: To update the "from" list in district based on the from in Edges
  sz_selection_district_from <- reactive({
    edgesId2 <- edge_id2()
    if (input$radio == 'PA'){
      nodeidFrom <- node()[which(node()$name %in% input$pa_from) ,] 
      edgeid <- edgesId2[which(edgesId2$from %in% nodeidFrom$id),] 
      nodeidTo <- node()[which(node()$id %in% edgeid$from) ,]
      nodeDistrictFrom <- unique(nodeidTo$district)
      nodeDistrictFrom #grab the last variable
    } else {
      nodeidFrom <- node()[which(node()$name %in% input$sz_from) ,] 
      edgeid <- edgesId2[which(edgesId2$from %in% nodeidFrom$id),] 
      nodeidTo <- node()[which(node()$id %in% edgeid$from) ,]
      nodeDistrictFrom <- unique(nodeidTo$district)
      nodeDistrictFrom #grab the last variable
    }
  })
  observe({
    updateSelectizeInput(session, 'district_from', choices=sz_selection_district_from(), selected = sz_selection_district_from(), server=TRUE) # not work did not grab only the available district
  })
  
  # to affect the district option in flow map. This will be done before the map can come out
  ### Very Important: To update the "from" list in district based on the To in Edges
  sz_selection_district_to <- reactive({
    edgesId2 <- edge_id2()
    if (input$radio == 'PA'){
      nodeidFrom <- node()[which(node()$name %in% input$pa_to) ,] 
      edgeid <- edgesId2[which(edgesId2$to %in% nodeidFrom$id),] 
      nodeidTo <- node()[which(node()$id %in% edgeid$to) ,]
      nodeDistrictFrom <- unique(nodeidTo$district)
      nodeDistrictFrom #grab the last variable
    } else {
      nodeidFrom <- node()[which(node()$name %in% input$sz_to) ,] 
      edgeid <- edgesId2[which(edgesId2$to %in% nodeidFrom$id),] 
      nodeidTo <- node()[which(node()$id %in% edgeid$to) ,]
      nodeDistrictFrom <- unique(nodeidTo$district)
      nodeDistrictFrom #grab the last variable
    }
  })
  observe({
    updateSelectizeInput(session, 'district_to', choices=sz_selection_district_to(), selected = sz_selection_district_to(),server=TRUE) # not work did not grab only the available district
  })
  ### end of update district
  
  # Download table "from"
  output$downloadFrom <- downloadHandler(
    filename = function(){"source.csv"}, 
    content = function(fname){
      write.csv(
        if (input$radio=="PA"){
          busstops%>% filter(busstops$planning_area %in% input$pa_from)
        }
        else{
          busstops%>% filter(busstops$subzone_name %in% input$sz_from)
        }
        , fname)
    }
  ) 
  
  # Download table "to"
  output$downloadTo <- downloadHandler(
    filename = function(){"destination.csv"}, 
    content = function(fname){
      write.csv(
        if (input$radio=="PA"){
          busstops%>% filter(busstops$planning_area %in% input$pa_to)
        }
        else{
          busstops%>% filter(busstops$subzone_name %in% input$sz_to)
        }
        , fname)
    }
  )
  
  ########################################################### Map FLow Jia Yi ######################################################################
  
  ##### Map Flow App depends on the selection output from AggregateFilter
  dataInput_From <- reactive({ 
    if (input$radio=="SZ"){
      input$sz_from
    }
    else{
      input$pa_from
    }
  })
  
  dataInput_To <- reactive({
    if (input$radio=="SZ"){  
      input$sz_to
    }
    else{
      input$pa_to
    }
  })
  
  ############################ for Map flow and aggregate filters in map flow ##############################
  
  edge_id2_filtered_reactive <- reactive({
    
    # filter the Edges with temporary variable "node2" and "node3"
    node2 <- node() %>% filter(name %in% dataInput_From() ) %>% filter(district %in% input$district_from)
    edge_id2_filtered <- edge_id2()[which(edge_id2()$from %in% node2$id) ,] 
    
    node3 <- node() %>% filter(name %in% dataInput_To())  %>% filter(district %in% input$district_to)
    edge_id2_filtered <- edge_id2_filtered[which(edge_id2_filtered$to %in% node3$id) ,] %>% filter(DAY_TYPE %in% input$checkGroup)
    edge_id2_filtered
    #}
  })
  
  node_filtered_reactive <- reactive({
    if (is.null(dataInput_From())== FALSE && is.null(dataInput_To())== FALSE){
      # filter nodes 
      node_filtered <- node() %>% filter(name %in% dataInput_From() | name %in% dataInput_To())# %>%
      node_filtered
    }
    
  })
  
  node_filtered_reactive_heatmap <- reactive({
    node_filtered <- node() %>% filter(name %in% dataInput_From() | name %in% dataInput_To()) %>%
      mutate(id = as.character(id))
    node_filtered
    
  })
  
  edges_for_plot4_reactive <- reactive({
    edges_for_plot <- edge_id2_filtered_reactive() %>% 
      inner_join(node_filtered_reactive() %>% dplyr::select(id, x, y), by = c('from' = 'id')) %>%
      inner_join(node_filtered_reactive() %>% dplyr::select(id, x, y), by = c('to' = 'id'))
    
    
    edges_for_plot4 <- edges_for_plot %>%
      dplyr::select(c('from','to','DAY_TYPE','weight','category','x.x','y.x','x.y','y.y')) %>%
      rename(x = x.x) %>%
      rename(y = y.x) %>%
      rename(xend = x.y) %>%
      rename(yend = y.y)
    
    # change columns from numerical to numeric then back to character (due to error msgs)
    edges_for_plot4$x    <- as.numeric(as.character(edges_for_plot4$x))
    edges_for_plot4$y    <- as.numeric(as.character(edges_for_plot4$y))
    edges_for_plot4$xend <- as.numeric(as.character(edges_for_plot4$xend))
    edges_for_plot4$yend <- as.numeric(as.character(edges_for_plot4$yend))
    
    edges_for_plot4
  })
  
  ################ Ploting MapJy##########
  output$map_jy <- #renderPlotly({
    renderPlot({
      validate(
        need(input$district_from != '', 'Please aggregate filters from the previous tab'),
        need(input$district_to != '', 'Please aggregate filters from the previous tab')
      )
      
      node_filtered = node_filtered_reactive()
      edge_id2_filtered = edges_for_plot4_reactive()
      
      # filter nodes based on slider
      
      node_filtered <- node_filtered %>% filter(weight <= input$flowsize[2]) %>% filter(weight >=input$flowsize[1])
      edge_id2_filtered = edge_id2_filtered[which(edge_id2_filtered$from %in% node_filtered$id) ,]
      edge_id2_filtered = edge_id2_filtered[which(edge_id2_filtered$to %in% node_filtered$id) ,]
      
      # filter edge based on slider
      edge_id2_filtered <- edge_id2_filtered %>% filter(weight >= input$flowEdge[1]) %>% filter(weight <= input$flowEdge[2])
      
      g <- graph_from_data_frame(edge_id2_filtered, directed = TRUE, vertices = node_filtered)
      
      maptheme <- theme(panel.grid = element_blank()) +
        theme(axis.text = element_blank()) +
        theme(axis.ticks = element_blank()) +
        theme(axis.title = element_blank()) +
        theme(legend.position = "bottom") +
        theme(panel.grid = element_blank()) +
        theme(panel.background = element_rect(fill = "#596673")) +
        theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
      
      
      node_pos <- node_filtered %>%
        dplyr::select(x, y)  # node positions must be called x, y
      
      
      validate(
        need(count(node_pos) > 0 , 'There are no node available with your current filters, please change your settings')
      )
      
      lay  <- create_layout(g, layout='drl')
      
      # filter the edges weight based on edges filter
      # solve the missing edge.id error
      edge_id2_filtered <- edge_id2_filtered %>% mutate(edge.id = row_number())
      
      # filter the nodes based on edges filter
      
      # Change error msg
      validate(
        need(count(edge_id2_filtered) > 0 & count(node_filtered) > 0, 'There are no edge flow available, please change your settings')
      )
      
      # ShapeFile for SZ
      # First read in the shapefile, using the path to the shapefile and the shapefile name minus the
      # extension as arguments
      if(input$radio == 'PA'){
        shapefile <- readOGR("data/geospatial", "MP14_PLNG_AREA_WEB_PL")
      }else{
        shapefile <- readOGR("data/geospatial", "MP14_SUBZONE_WEB_PL")}
      
      # Next the shapefile has to be converted to a dataframe for use in ggplot2
      shapefile_df <- fortify(shapefile)
      
      # Now the shapefile can be plotted as either a geom_path or a geom_polygon.
      
      ## This is to draw the shape file of Singapore outline
      map_gg2 <- geom_polygon(data = shapefile_df, 
                              aes(x = long, y = lat, group = group),
                              color = 'gray', fill = 'gray', size = .2) 
      map_gg3 <- geom_path(data = shapefile_df, 
                           aes(x = long, y = lat, group = group),
                           color = 'purple', fill = 'purple', size = .2)
      map_gg4 <- ggplot() + map_gg2 + map_gg3 +geom_point() +
        annotate("point", x = 31596, y = 29220, colour = "blue")
      
      # end of shapefile
      map_jy <- ggplot(lay) + map_gg2+ map_gg3 +# ggraph(lay)  
        geom_edge_arc(aes(edge_width = weight,   # draw edges as arcs
                          circular = FALSE,show.legend = TRUE),
                      data = edge_id2_filtered, curvature = 0.33,
                      alpha = 0.5) +
        scale_edge_width_continuous(range = c(0.5,5),             # scale for edge widths
                                    guide = FALSE) +
        geom_node_point(aes(size = weight, color= as.factor(district),alpha=0.5,stroke=0.5), # draw node # size = weight # color outside geom is color of 
                        data=node_filtered # somehow need to repoint data. if not will have error on higher lib ver
                        #outer boundary of marker # fill is internal shading of point marker # outside is constant color, aes is mapping variables, eg for color
        ) +
        scale_size_continuous(range = c(1, 10), guide = FALSE) +    # scale for node sizes
        geom_node_text(aes(label = name),
                       data=node_filtered, # somehow need to repoint data. if not will have error on higher lib ver
                       repel = TRUE, size = 3,
                       color = "white", fontface = "bold") +
        maptheme
      
      plot(map_jy)
    },height = 700, width = 1450) # end of output$map_jy 
  
  # create slider widget based on size of the node flows
  output$flowsize_slider_ui <- renderUI({
    sliderInput("flowsize", 
                label = "Range of interest for node weights:",
                min = min(node_filtered_reactive()$weight), max = max(node_filtered_reactive()$weight), value = c(min(node_filtered_reactive()$weight), max(node_filtered_reactive()$weight)))
  })
  
  # create slider widget based on size of the edges flows
  output$flowEdge_slider_ui_passenger <- renderUI({
    sliderInput("flowEdge",
                label = "Range of interest for edge weights:",
                min = min(edges_for_plot4_reactive()$weight), max = max(edges_for_plot4_reactive()$weight), value = c(min(edges_for_plot4_reactive()$weight), max(edges_for_plot4_reactive()$weight)))
  })
  ########################################### Sankey Diagram Jia Yi #####################################################
  node_from <- reactive({ if(input$radio == 'PA'){
    node() %>% filter(name %in% input$pa_from)
  } else {
    node() %>% filter(name %in% input$sz_from)
  }   }) # end of edge_from
  
  node_to <- reactive({ if(input$radio == 'PA'){
    input$pa_to
  } else {
    input$sz_to
  }})
  
  
  sankey_fn_node <- reactive({
    
    if (input$radio == 'PA' ){
      node_local_from = node() %>% filter(name %in% input$pa_from ) %>% dplyr:: select('name')
      node_local_to = node() %>% filter(name %in% input$pa_to ) %>% dplyr:: select('name')
      name_vec <- c(node_local_from$name, node_local_to$name)
      name_vec <- unique(name_vec)
      
    } else if (input$radio == 'SZ' ){
      node_local_from = node() %>% filter(name %in% input$sz_from ) %>% dplyr:: select('name')
      node_local_to = node() %>% filter(name %in% input$sz_to ) %>% dplyr:: select('name')
      name_vec <- c(node_local_from$name, node_local_to$name)
      name_vec <- unique(name_vec)
    }
    
    nodes_sank <- data.frame(name = name_vec, id = 0:(length(name_vec)-1)) %>%
      mutate(id = as.numeric(id)) %>% mutate(name = as.character(name))
    
    # from sankey
    nodes_sank
  })
  
  
  sankey_fn_edge <- reactive({
    nodes_sank <- sankey_fn_node()
    if (input$radio == 'PA'){
      # aggregate data
      edges_data2 <- edges_data%>%
        group_by(planning_area.x,planning_area.y) %>%
        summarise(weight =sum(weight))
      edges_data2
      
      sank_edge <- edges_data2 %>%
        left_join(nodes_sank,edges_data2, by=c('planning_area.x'='name')) %>%
        rename(from_id = id) %>%
        left_join(nodes_sank,edges_data2, by=c('planning_area.y'='name')) %>%
        rename(to_id = id)
      
      sank_left <- sank_edge %>% filter(planning_area.x %in% input$pa_from)  %>% 
        filter(planning_area.y %in% input$pa_to) 
      #sank_left   # left edge
    }
    else if (input$radio == 'SZ'){
      # aggregate data
      edges_data2 <- edges_data_edit%>%
        group_by(subzone_origin,subzone_destination) %>%
        summarise(weight =sum(weight))
      edges_data2
      
      #edges_data2
      sank_edge <- edges_data2 %>%
        left_join(nodes_sank,edges_data2, by=c('subzone_origin'='name')) %>%
        rename(from_id = id) %>%
        left_join(nodes_sank,edges_data2, by=c('subzone_destination'='name')) %>%
        rename(to_id = id)
      
      sank_left <- sank_edge %>% filter(subzone_origin %in% input$sz_from)  %>% 
        filter(subzone_destination %in% input$sz_to) 
      #sank_left   # left edge
    }
    sank_left
    
  })
  
  output$sankey_from <- renderSankeyNetwork({
    sankeyNetwork(Links = sankey_fn_edge(), Nodes = sankey_fn_node(), Source = 'from_id', Target = 'to_id',
                  Value = 'weight', NodeID = 'name', fontSize = 16)
    
  })
  
  #https://christophergandrud.github.io/networkD3/
  # to sankey
  ######################################### HeatMap Jiayi ####################################
  ## Preparing the Origin Destination matrix to be in the form "from","to","frequency"
  flow_ori_dest <- reactive({
    if(is.null(edge_id2_filtered_reactive())==FALSE ){ # & (input$radio=="SZ")
      a <- edge_id2_filtered_reactive() %>%
        dplyr::select('from','to','weight') %>%
        unite(from_to, from,to, sep = "_", remove=FALSE) %>%
        group_by(from_to) %>%
        summarise(Frequency = sum(weight))%>%
        separate(from_to, c("from", "to"))
      a
    }
  })
  
  
  # Combine selections for first heatmap based on absolute numbers
  flowSel <- reactive({
    if(is.null(edge_id2_filtered_reactive())==FALSE ){ # && (input$radio=="SZ")
      ## Select flows that represent at least 20% of the sum of outgoing flows for 
      # each urban area. ( can select other methods )
      if (nrow(flow_ori_dest()) >0 ) { 
        myflows_local <- prepflows(mat = flow_ori_dest(), i = "from", j = "to", fij = "Frequency")
        ## Remove the matrix diagonal
        diag(myflows_local) <- 0
        # https://www.rdocumentation.org/packages/flows/versions/1.1.1/topics/firstflows
        
        flowSel1 <- firstflows(mat = myflows_local, method = input$firstflows_ui, 
                               k = as.numeric(input$kvalue))
        # Select the dominant flows (incoming flows criterion)
        flowSel2 <- domflows(mat = myflows_local, w = colSums(myflows_local), k = 1)
        
        flowSel_local <- myflows_local * flowSel1
        node_local = node_filtered_reactive_heatmap()
        col_name_flow <- colnames(flowSel_local)
        df_col_name_flow <- as.data.frame(col_name_flow)
        df_col_name_flow <- df_col_name_flow %>% rename(id =col_name_flow)
        df_col_name_flow$id =  as.character(df_col_name_flow$id)
        
        
        heatmap_name <- inner_join(df_col_name_flow, node_local, by = 'id') 
        
        colnames(flowSel_local) <- heatmap_name$name
        rownames(flowSel_local) <- heatmap_name$name
        
        flowSel_local
      }
    }
  })
  
  observe({
    if(is.null(edge_id2_filtered_reactive())==FALSE){ #  && (input$radio=="SZ")
      flowSel()
    }
  })
  
  # Combine selections for first heatmap based on absolute numbers
  flowSel_dom <- reactive({
    if(is.null(edge_id2_filtered_reactive())==FALSE ){ # && (input$radio=="SZ")
      ## Select flows that represent at least 20% of the sum of outgoing flows for 
      # each urban area. ( can select other methods )
      
      if (nrow(flow_ori_dest()) >0 ) { 
        myflows_local <- prepflows(mat = flow_ori_dest(), i = "from", j = "to", fij = "Frequency")
        ## Remove the matrix diagonal
        diag(myflows_local) <- 0
        # https://www.rdocumentation.org/packages/flows/versions/1.1.1/topics/firstflows
        
        flowSel1 <- firstflows(mat = myflows_local, method = input$firstflows_ui, 
                               k = as.numeric(input$kvalue))
        # Select the dominant flows (incoming flows criterion)
        flowSel2 <- domflows(mat = myflows_local, w = colSums(myflows_local), k = as.numeric(input$kvalue_dom))
        # Select the dominant flows (incoming flows criterion)
        ##flowSel2 <- domflows(mat = myflows_local, w = colSums(myflows_local), k = 1)
        flowSel_local <- myflows_local * flowSel1 * flowSel2
        node_local = node_filtered_reactive_heatmap()
        col_name_flow <- colnames(flowSel_local)
        df_col_name_flow <- as.data.frame(col_name_flow)
        df_col_name_flow <- df_col_name_flow %>% rename(id =col_name_flow)
        df_col_name_flow$id =  as.character(df_col_name_flow$id) 
        
        heatmap_name <- inner_join(df_col_name_flow, node_local, by = 'id') 
        
        ## rename
        colnames(flowSel_local) <- heatmap_name$name
        rownames(flowSel_local) <- heatmap_name$name
        
        flowSel_local
      }
    }
  })
  
  # Combine selections for 2nd  heatmap based on absolute numbers
  flowSel2 <- reactive({
    if(is.null(edge_id2_filtered_reactive())==FALSE){ #  && (input$radio=="SZ")
      ## Select flows that represent at least 20% of the sum of outgoing flows for 
      # each urban area. ( can select other methods )
      if (nrow(flow_ori_dest()) >0 ) { 
        myflows_local <- prepflows(mat = flow_ori_dest(), i = "from", j = "to", fij = "Frequency")
        ## Remove the matrix diagonal
        diag(myflows_local) <- 0
        # https://www.rdocumentation.org/packages/flows/versions/1.1.1/topics/firstflows
        
        flowSel1 <- firstflows(mat = myflows_local/rowSums(myflows_local) * 100, method = input$firstflows2_ui, 
                               k = as.numeric(input$kvalue2))
        # Select the dominant flows (incoming flows criterion)
        ##flowSel2 <- domflows(mat = myflows_local, w = colSums(myflows_local), k = 1)
        flowSel_local <- myflows_local * flowSel1
        node_local = node_filtered_reactive_heatmap()
        col_name_flow <- colnames(flowSel_local)
        df_col_name_flow <- as.data.frame(col_name_flow)
        df_col_name_flow <- df_col_name_flow %>% rename(id =col_name_flow)
        
        df_col_name_flow$id =  as.character(df_col_name_flow$id)
        
        heatmap_name <- inner_join(df_col_name_flow, node_local, by = 'id') 
        
        
        ## rename
        colnames(flowSel_local) <- heatmap_name$name
        rownames(flowSel_local) <- heatmap_name$name
        flowSel_local
      }
    }
  })
  
  observe({
    if(is.null(edge_id2_filtered_reactive())==FALSE){ #  && (input$radio=="SZ")
      flowSel()
    }
  })
  
  # Combine selections for first heatmap based on absolute numbers
  flowSel2_dom <- reactive({
    if(is.null(edge_id2_filtered_reactive())==FALSE ){ # && (input$radio=="SZ")
      ## Select flows that represent at least 20% of the sum of outgoing flows for 
      # each urban area. ( can select other methods )
      
      if (nrow(flow_ori_dest()) >0 ) { 
        myflows_local <- prepflows(mat = flow_ori_dest(), i = "from", j = "to", fij = "Frequency")
        ## Remove the matrix diagonal
        diag(myflows_local) <- 0
        # https://www.rdocumentation.org/packages/flows/versions/1.1.1/topics/firstflows
        
        flowSel1 <- firstflows(mat = myflows_local/rowSums(myflows_local) * 100, method = input$firstflows2_ui, 
                               k = as.numeric(input$kvalue2))
        # Select the dominant flows (incoming flows criterion)
        flowSel2 <- domflows(mat = myflows_local, w = colSums(myflows_local), k = as.numeric(input$kvalue2_dom))
        flowSel_local <- myflows_local * flowSel1 * flowSel2
        node_local = node_filtered_reactive_heatmap()
        col_name_flow <- colnames(flowSel_local)
        df_col_name_flow <- as.data.frame(col_name_flow)
        df_col_name_flow <- df_col_name_flow %>% rename(id =col_name_flow)
        df_col_name_flow$id =  as.character(df_col_name_flow$id) 
        heatmap_name <- inner_join(df_col_name_flow, node_local, by = 'id') 
        
        
        ## rename
        colnames(flowSel_local) <- heatmap_name$name
        rownames(flowSel_local) <- heatmap_name$name
        
        
        flowSel_local
      }
    }
  })
  
  # creating sliders for k value in flows ## no have
  # plotting flows origin-dest map
  # first flows
  output$ori_dest <- renderPlotly({
    if( is.null(edge_id2_filtered_reactive())==FALSE && 
        #(input$radio=="SZ") && 
        (nrow(flow_ori_dest()) >0) ) {
      heatmaply(
        flowSel()
      )
    }
  })
  
  output$ori_dest2 <- renderPlotly({
    if( is.null(edge_id2_filtered_reactive())==FALSE && 
        #(input$radio=="SZ") && 
        (nrow(flow_ori_dest()) >0) ) {
      heatmaply(percentize(flowSel2())
                
      )
    }
  })
  
  # first flows with dom flows
  output$ori_dest_dom <- renderPlotly({
    if( is.null(edge_id2_filtered_reactive())==FALSE && 
        #(input$radio=="SZ") && 
        (nrow(flow_ori_dest()) >0) ) {
      heatmaply(
        flowSel_dom()
      )
    }
  })
  
  output$ori_dest2_dom <- renderPlotly({
    if( is.null(edge_id2_filtered_reactive())==FALSE && 
        #(input$radio=="SZ") && 
        (nrow(flow_ori_dest()) >0) ) {
      heatmaply(percentize(flowSel2_dom())
                
      )
    }
  })
  
  ########################################### EDA Jia Yi #############################################
  ## Density Plot
  # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  # Density Plot for y variables
  output$Density_jy <- renderPlot({
    if (input$densityPlotJY == "Y" && input$radioDensityJY == "Standard"){
      fIn <- ggplot(data = pass_central, aes(x=(frequencyIn))) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Tap In Volume")+ylim(0,0.80)
      
      fOut <-ggplot(data = pass_central, aes(x=(frequencyOut))) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Tap Out Volume")+ylim(0,0.80)
      
        densityJY<- ggarrange(fIn, fOut,
                               ncol = 2, nrow = 1)
        plot(densityJY)}
    
    else if (input$densityPlotJY == "Y" && input$radioDensityJY == "Original"){
      fIn <- ggplot(data = pass_central, aes(x=(frequencyIn))) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Tap In Volume")
      
      fOut <-ggplot(data = pass_central, aes(x=(frequencyOut))) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Tap Out Volume")
      
      densityJY1<- ggarrange(fIn, fOut,
                            ncol = 2, nrow = 1)
      plot(densityJY1)}
    # end of output$yDensity_jy
  
  
    else if(input$densityPlotJY=='X' && input$radioDensityJY == "Standard"){
      # Density Plot for x variables with standard y limit
      fBetween <- ggplot(data = pass_central, aes(x=between)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Betweeness Centrality")+ylim(0,40)
      
      fCloseness <- ggplot(data = pass_central, aes(x=closeness)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Closeness Centrality")+ylim(0,40)
      
      fEigen <- ggplot(data = pass_central, aes(x=eigen)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Eigenvalue Centrality")+ylim(0,40)
      
      fDegree <- ggplot(data = pass_central, aes(x=(degree))) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Degree Centrality")+ylim(0,40)
      
      xDensity_jy <-ggarrange(fBetween, fCloseness,fEigen,fDegree, 
                              ncol = 4, nrow = 1)
      plot(xDensity_jy)}
    else if(input$densityPlotJY=='X' && input$radioDensityJY == "Original"){
      # Density Plot for x variables without standard y limit
      fBetween <- ggplot(data = pass_central, aes(x=between)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Betweeness Centrality")
      
      fCloseness <- ggplot(data = pass_central, aes(x=closeness)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Closeness Centrality")
      
      fEigen <- ggplot(data = pass_central, aes(x=eigen)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Eigenvalue Centrality")
      
      fDegree <- ggplot(data = pass_central, aes(x=(degree))) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666") + labs(x = "Degree Centrality")
      
      xDensity_jy <-ggarrange(fBetween, fCloseness,fEigen,fDegree, 
                                     ncol = 4, nrow = 1)
      
      plot(xDensity_jy)}
  }) # end of output$xDensity_jy_plot
  
  ## Correlation Matrix Plot
  corr_central_jy <- cor(pass_central[c("frequencyIn","frequencyOut","between","degree","eigen","closeness")])
  output$corrplot_jy <- renderPlot({
    corr <- round(cor(corr_central_jy), 1)
    #p.mat <- cor_pmat(corr_central_jy)
    correlation <- ggcorrplot(corr,
                              colors = c("#6D9EC1", "white", "#E46726"), 
                              legend.title = "correlation matrix",
                              method = input$shape, type = input$typeShape)
     
    plot(correlation)
  })
  # http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
  
  
  ## Bivariate analysis
  output$bivariate_jy <- renderPlotly({
    axis = list(showline=FALSE,
                zeroline=FALSE,
                gridcolor='#ffff',
                ticklen=4,
                titlefont=list(size=13))
    
    p <- pass_central %>%
      plot_ly() %>%
      add_trace(
        type = 'splom',
        dimensions = list(
          list(label='betweenness', values=~`between`),
          list(label='closeness', values=~`closeness`),
          list(label='eigenvalue', values=~`eigen`),
          list(label='degree', values=~`degree`),
          list(label='frequencyIn', values=~`frequencyIn`),
          list(label='frequencyOut', values=~`frequencyOut`)
        ),
        #text=~factor(type), #, labels=c("red","white")
        diagonal=list(visible=F),
        marker = list(
          color = 'blue',
          size = 5,
          line = list(
            width = 1,
            color = 'rgb(230,230,230)'
          )
        )
      ) %>%
      layout(
        title = "Interactive Scatterplot Matrix for Centrality data",
        hovermode='closest',
        dragmode = 'select',
        plot_bgcolor='rgba(240,240,240, 0.95)',
        xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4, titlefont=list(size=13,tickangle = 45)),
        yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4, titlefont=list(size=13)),
        xaxis2=axis,
        xaxis3=axis,
        xaxis4=axis,
        xaxis5=axis,
        xaxis6=axis,
        xaxis7=axis
        
      )
    
    lp <- p %>% style(showupperhalf = F )
    lp
    ggplotly(lp)
  })
  
  ############################################ Gravity Model Jia Yi #############################################
  # For X variable selections
  selectX <- c("closeness","between","degree","eigen")
  
  # X chosen
  observe({
    updateSelectizeInput(session, 'selectXvar', choices=selectX, selected = selectX, server=TRUE)
  })  
  
  ############
  
  # reactive linear model inputs from y and x from UI
  # linear1 <- reactive({
  #   if (is.null(input$selectXvar)==FALSE && is.null(input$yVar)==FALSE){
  #     
  #     lis_new <-paste(input$selectXvar, collapse = "+")
  #     fml = as.formula(sprintf('%s ~ %s ', input$yVar, lis_new))
  #     fit = lm(fml, data=pass_central)
  #     fit}})
  modelInOut = reactive({
    if (is.null(input$selectXvar)==FALSE && is.null(input$yVar)==FALSE){
      
      lis_new <-paste(input$selectXvar, collapse = "+")
      fml = as.formula(sprintf('%s ~ %s ', input$yVar, lis_new))
      fit = lm(fml, data=pass_central)
      step <- stepAIC(fit, direction = "both")
      step
    } 
  }) # end of modelInOut which gives the "stepAIC"
  
  observe({modelInOut()})
  # for the UI
  
  output$AICtest <- renderPlot({
    plottStats(modelInOut())
    
  })
  
  ###############
  
  # for the UI
  output$yVarUI <- renderUI({
    selectInput("yVar", label = h4("Select dependent variable...."),
                choices = list("Total Tap In Volume" = "frequencyIn", "Total Tap Out Volume" = "frequencyOut"),
                selected = "frequencyIn")})
  
  #Plot the t statistics based on the y and x variable inputs
  output$AIC <- renderPlotly({
    plottStats(modelInOut())
    
  })
  output$Y <- renderText({input$yVar})
  output$seeX <- renderText({input$selectXvar})
  # https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html
  
  output$pred1p <- renderText({min(anova(modelInOut())$'Pr(>F)'[1:length(input$selectXvar)])})  # call col name
  output$pred1RSqAdj <- renderText({summary(modelInOut())[[9]][1]}) # adjusted R sq
  
  output$linearModel <- renderPrint({summary(modelInOut())[4]})  #
  
  output$conclude1 <- renderPrint({
    if(as.numeric(anova(modelInOut())$'Pr(>F)'[1])<1-input$num){
      'Statistically Significant. Reject H0'}
    else {
      'Statistically not significant. Do not reject H0'
    }
  })
  
  #################################### plot residuals###################################
  output$resid <- renderPlot({
    par(mfrow = c(2, 2))
    plot(modelInOut())
    
  })
  
  ######################## type math formual for Durbin Watson test########################
  output$HypoDurbin <- renderUI({
    withMathJax(helpText('$$H_0 : \\sigma_d = 0$$'),helpText('$$H_0 : \\sigma_d \\ne 0$$'))
  })
  # plot Durbin watson test
  output$DurbinStat <- renderText({durbinWatsonTest(modelInOut())[[2]][1]
  })
  
  output$DurbinProb <-   renderText({durbinWatsonTest(modelInOut())[[3]][1]
  })
  output$DurbinConclude <- renderPrint({
    if(as.numeric(durbinWatsonTest(modelInOut())[[3]][1])<1-input$num){
      'Statistically Significant. Reject H0'}
    else {
      'Statistically not significant. Do not reject H0'
    }
  })
  
  ################# Mengyong Plot Proportionate Symbol Map##############
  
  #https://mastering-shiny.org/action-dynamic.html
  sz_filter_my <- reactive({
    filter(busstops_my, planning_area == input$pa_my_1)
  })
  
  observeEvent(sz_filter_my(), {
    #choices <- sort(unique(sz_filter_my()$subzone_name))
    updateSelectInput(session, "sz_filter_my1", choices = sort(unique(sz_filter_my()$subzone_name))) 
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
    
  
  ####################### Mengyong Plot Centrality Map########################################
  sz_filter_my_2 <- reactive({
    dplyr::filter(busstops_my, planning_area == input$pa_my_2)
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
  
  
}
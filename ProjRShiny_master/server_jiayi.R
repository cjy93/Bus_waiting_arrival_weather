######################### Define SERVER logic ----##################

server <- function(input, output, session) {
    ############################################## Jia Yi ####################################################
    ## Num of commuters in Jan 2020 between any 2 busstops
    
    ### Planning Area Dropdown
    
    # output$ex_out <- renderPrint({
    #     str(sapply(sprintf('e%d', 0:7), function(id) {
    #         input[[id]]
    #     }, simplify = FALSE))
    # })
    ###################################################### For Aggregate_Filter Tab JY ######################################################
    # read in data (this data will change so we put in server)
    node_SZ <- read_csv("data/node_flowmap_SZ.csv")
    node_SZ$id = as.character(as.numeric(node_SZ$id))
    #edge_id2_SZ <- read_csv("data/edge_flowmap_SZ_passenger.csv")
    
    # Load in edges file, which will change between selection of "passenger" or "n()"
    edges_data <- reactive({ if (input$radio_flowsize == "passenger"){
      read_csv("data/origin_dest_Full_Aggregated_BusTrips.csv")}
      else {
        read_csv("data/origin_dest_Full_Aggregated_n_weight.csv")
      }})
    
    edges_full<- reactive({ if (input$radio=='SZ'){
      edges <- edges_data()
      #%>%
      #   select(-c('YEAR_MONTH','PT_TYPE','ORIGIN_PT_CODE','DESTINATION_PT_CODE','RoadName_Origin','Description_Origin','RoadName_Destination','Description_Destination'))
      # we need to append "subzone_origin" and "subzone_dest" so we can calculate weights based on busstops in these subzones
      edges_join <- merge(edges, busstops, by.x='BusStopCode_x', by.y = 'BusStopCode') %>% 
        rename(subzone_ori = subzone_name) %>% rename(planning_area_origin = planning_area) #%>% select(-c("subzone_origin","subzone_destination"))
      edges_join2 <- merge(edges_join, busstops, by.x='BusStopCode_y', by.y = 'BusStopCode') %>% 
        rename(subzone_dest = subzone_name)  %>% rename(planning_area_dest = planning_area)
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
        edges <- edges_data()
        #%>%
        #   select(-c('YEAR_MONTH','PT_TYPE','ORIGIN_PT_CODE','DESTINATION_PT_CODE','RoadName_Origin','Description_Origin','RoadName_Destination','Description_Destination'))
        # we need to append "subzone_origin" and "subzone_dest" so we can calculate weights based on busstops in these subzones
        edges_join <- merge(edges, busstops, by.x='BusStopCode_x', by.y = 'BusStopCode') %>% 
          rename(subzone_ori = subzone_name) %>% rename(planning_area_origin = planning_area) #%>% select(-c("subzone_origin","subzone_destination"))
        edges_join2 <- merge(edges_join, busstops, by.x='BusStopCode_y', by.y = 'BusStopCode') %>% 
          rename(subzone_dest = subzone_name)  %>% rename(planning_area_dest = planning_area)
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
    
    
    get_index = node_SZ %>% dplyr::select(c('id','name'))
    
    edge_id2_SZ <- reactive({
      edge_id <- merge(edges_full(), get_index, by.x = "from", by.y = "name") %>%
        dplyr::select(-c("from")) %>%
        rename(from=id) 
      edge_id2 <- merge(edge_id, get_index, by.x = "to", by.y = "name") %>%
        dplyr::select(-c("to")) %>%
        rename(to=id)
      
      edge_id2$to <- as.character(edge_id2$to)
      edge_id2$from <- as.character(edge_id2$from)
      edge_id2_SZ <- edge_id2 %>%
        dplyr::select(c('from','to','DAY_TYPE','weight','category'))
    }) # end of reactive edge_id2_SZ
    
    
    # not to delete this is dummy test 
    output$ex_out <- reactive({
        str(c(input$radio,input$pa_from, input$pa_to, input$sz_from, input$sz_to)
        )
    })
    
    outputOptions(output, "ex_out", suspendWhenHidden = FALSE)      
    
  
    
    # plot the FROM Table
    ## Filter by district then by PA/SZ. Need not do this for renderPlot since Subzone is already unique by the District
    observe({
      print("before")
      print(input$district_from)
      if (is.null(input$district_from)== FALSE){
        pa_sub_from <-  busstops %>% filter(district %in% input$district_from)
        print("middle")
        updateSelectizeInput(session, 'pa_from', choices=pa_sub_from$planning_area, server=TRUE)
        print("after")}
    })
    observe({
      if (is.null(input$district_to)== FALSE){
        pa_sub_to <-  busstops %>% filter(district %in% input$district_to)
        updateSelectizeInput(session, 'pa_to', choices=pa_sub_to$planning_area, server=TRUE)}
    })
    observe({
      if (is.null(input$district_from)== FALSE){
        sz_sub_from <-  busstops %>% filter(district %in% input$district_from)
        updateSelectizeInput(session, 'sz_from', choices=sz_sub_from$subzone_name, server=TRUE)}
    })
    observe({
      if (is.null(input$district_to)== FALSE){
        sz_sub_to <-  busstops %>% filter(district %in% input$district_to)
        updateSelectizeInput(session, 'sz_to', choices=sz_sub_to$subzone_name, server=TRUE)}
    })
    
    # render tables
    output$mytableFrom = DT::renderDataTable({
      print("input_pafrom")
      print(input$pa_from)
        if (input$radio=="PA"){
            busstops%>% filter(busstops$planning_area %in% input$pa_from)
        }
        else{
            busstops%>% filter(busstops$subzone_name %in% input$sz_from)
        }
    }) # end of renderDataTable FROM
    # https://community.rstudio.com/t/evaluation-error-operation-not-allowed-without-an-active-reactive-context/18468/3
    
    
 
    
    # plot the TO Table
    output$mytableTo = DT::renderDataTable({
        if (input$radio=="PA"){
          ## KIV
            busstops%>% filter(busstops$planning_area %in% input$pa_to)
        }
        else{
            busstops%>% filter(busstops$subzone_name %in% input$sz_to)
        }
    }) # end of renderDataTable TO
    
    ### Very Important: To update the "To" list based on edges from the "From"
    sz_selection_from <- reactive({
      nodeidFrom <- node_SZ[which(node_SZ$name %in% input$sz_from) ,] 
      edgeid <- edge_id2_SZ()[which(edge_id2_SZ()$from %in% nodeidFrom$id),] 
      nodeidTo <- node_SZ[which(node_SZ$id %in% edgeid$to) ,]
      nodeNameTo <- unique(nodeidTo$name)
      nodeNameTo #grab the last variable
    })
    
    # https://shiny.rstudio.com/articles/selectize.html
    # https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
    observe({
      updateSelectizeInput(session, 'sz_to', choices=sz_selection_from(), server=TRUE)
    })
    ### end of update "To" list based on edges available
    
    # ## to affect the district option in flow map
    # ### Very Important: To update the "from" list in district based on the from in Edges
    # sz_selection_district_from <- reactive({
    #   nodeidFrom <- node_SZ[which(node_SZ$name %in% input$sz_from) ,] 
    #   edgeid <- edge_id2_SZ[which(edge_id2_SZ$from %in% nodeidFrom$id),] 
    #   nodeidTo <- node_SZ[which(node_SZ$id %in% edgeid$from) ,]
    #   nodeDistrictFrom <- unique(nodeidTo$district)
    #   nodeDistrictFrom #grab the last variable
    # })
    # observe({
    #   updateSelectizeInput(session, 'district_from', choices=sz_selection_district_from(), selected = sz_selection_district_from(), server=TRUE) # not work did not grab only the available district
    # })
    # 
    # # to affect the district option in flow map
    # ### Very Important: To update the "from" list in district based on the To in Edges
    # sz_selection_district_to <- reactive({
    #   nodeidTo <- node_SZ[which(node_SZ$name %in% input$sz_to) ,] 
    #   edgeid <- edge_id2_SZ[which(edge_id2_SZ$to %in% nodeidTo$id),] 
    #   nodeidTo <- node_SZ[which(node_SZ$id %in% edgeid$to) ,]
    #   nodeDistrictFrom <- unique(nodeidTo$district)
    #   nodeDistrictFrom #grab the last variable
    # })
    # observe({
    #   updateSelectizeInput(session, 'district_to', choices=sz_selection_district_to(), selected = sz_selection_district_to(),server=TRUE) # not work did not grab only the available district
    # })
    # ### end of update district
    
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
    
    
    # Download table
    #output$downloadFrom <- downloadHandler(
    #    filename = function(){"source.csv"}, 
    #    content = function(fname){
    #        write.csv(mytableFrom , fname)
    #    }
    #)
    
    # https://shiny.rstudio.com/articles/layout-guide.html
    # https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table
    
    
   
    ########################################################### Map FLow Jia Yi ######################################################################
    
 
    # Subzone Flow map
    ## plot of graph starts here
    
    
    #########
    ## Filter node and edges by the selected subzone name in aggregate filter
    # node_SZ <- node_SZ %>% filter(name == dataInput_From_SZ())
    # 
    # node_SZ_reactive <- reactive({
    #   node_SZ %>% 
    #     filter(name == input$sz_to)
    # })
    #node_SZ_reactive()
    
    # Create Graph object starts here
    
    # qwerty <- reactive({
    #   print(node_SZ_reactive())
    #   print('asd')
    #   abc_rb <- 'aaaaaa'
    #   print(abc_rb)
    #   abc_rb
    # })
    # 
    # observe({
    #   print(qwerty)
    # })
    # 
    ##### Map Flow App depends on the selection output from AggregateFilter
    dataInput_From_SZ <- reactive({ 
      if (input$radio=="SZ"){
        input$sz_from
      }
    })
    
    dataInput_To_SZ <- reactive({
      if (input$radio=="SZ"){  
      input$sz_to
      }
    })
    
    dataInput_From_SZ_district <- reactive({
      if (input$radio=="SZ"){
        input$district_from
      }
    })

    dataInput_To_SZ_district <- reactive({
      if (input$radio=="SZ"){
        input$district_to
      }
    })
    
    dataInput_SZ_nodeWeight <- reactive({
      if (input$radio=="SZ"){
        input$flowsize
      }
    })
    ############################ for Map flow and aggregate filters in map flow ##############################

    edge_id2_SZ_filtered_reactive <- reactive({
      
      if (input$radio=="SZ"){
        # filter the Edges with temporary variable "node2" and "node3"
        node2 <- node_SZ %>% filter(name %in% dataInput_From_SZ() ) #%>% filter(district %in% dataInput_From_SZ_district())
        edge_id2_SZ_filtered <- edge_id2_SZ()[which(edge_id2_SZ()$from %in% node2$id) ,] 
        
        node3 <- node_SZ %>% filter(name %in% dataInput_To_SZ())  #%>% filter(district %in% dataInput_To_SZ_district())
        print("edges before day type")
        print(edge_id2_SZ_filtered)
        print(input$checkGroup)
        edge_id2_SZ_filtered <- edge_id2_SZ_filtered[which(edge_id2_SZ_filtered$to %in% node3$id) ,] %>% filter(DAY_TYPE %in% input$checkGroup)
        print("edgess after day type")
        print(edge_id2_SZ_filtered)
        edge_id2_SZ_filtered
      }
    })
    
    node_SZ_filtered_reactive <- reactive({
      if (input$radio=="SZ"){
        # filter nodes 
        node_SZ_filtered <- node_SZ %>% filter(name %in% dataInput_From_SZ() | name %in% dataInput_To_SZ()) #%>%
          #filter(district %in% dataInput_From_SZ_district() | district %in% dataInput_To_SZ_district())
        node_SZ_filtered
      }
      
    })
    
    observe({
      print('asdadasdsad')
      print(edge_id2_SZ_filtered_reactive())
      print('zczxc')
      print(node_SZ_filtered_reactive())
    })
    
    #edges_for_plot_SZ_reactive <- reactive({
    #  edges_for_plot_SZ <- edge_id2_SZ_filtered_reactive() %>% 
    #      inner_join(node_SZ_filtered_reactive() %>% select(id, x, y), by = c('from' = 'id')) %>%
    #      inner_join(node_SZ_filtered_reactive() %>% select(id, x, y), by = c('to' = 'id'))
    #  edges_for_plot_SZ
    #})
    
    edges_for_plot4_SZ_reactive <- reactive({
      edges_for_plot_SZ <- edge_id2_SZ_filtered_reactive() %>% 
        inner_join(node_SZ_filtered_reactive() %>% dplyr::select(id, x, y), by = c('from' = 'id')) %>%
        inner_join(node_SZ_filtered_reactive() %>% dplyr::select(id, x, y), by = c('to' = 'id'))
      
      
      edges_for_plot4_SZ <- edges_for_plot_SZ %>%
        dplyr::select(c('from','to','DAY_TYPE','weight','category','x.x','y.x','x.y','y.y')) %>%
        rename(x = x.x) %>%
        rename(y = y.x) %>%
        rename(xend = x.y) %>%
        rename(yend = y.y)
      
      # change columns from numerical to numeric then back to character (due to error msgs)
      edges_for_plot4_SZ$x    <- as.numeric(as.character(edges_for_plot4_SZ$x))
      edges_for_plot4_SZ$y    <- as.numeric(as.character(edges_for_plot4_SZ$y))
      edges_for_plot4_SZ$xend <- as.numeric(as.character(edges_for_plot4_SZ$xend))
      edges_for_plot4_SZ$yend <- as.numeric(as.character(edges_for_plot4_SZ$yend))
      
      edges_for_plot4_SZ
    })
    
    ################ Ploting MapJy##########
    output$map_jy <- #renderPlotly({
      renderPlot({
      # validate(
      #   need(input$district_from != '', 'Please aggregate filters from the previous tab'),
      #   need(input$district_to != '', 'Please aggregate filters from the previous tab')
      # )
    # filter based on "Aggregate Filter" tab
    #if (input$radio=="SZ"){
    #  # filter the Edges with temporary variable "node2" and "node3"
    #  node2 <- node_SZ %>% filter(name %in% dataInput_From_SZ() )
    #  edge_id2_SZ <- edge_id2_SZ[which(edge_id2_SZ$from %in% node2$id) ,] 
    #  
    #  node3 <- node_SZ %>% filter(name %in% dataInput_To_SZ())
    #  edge_id2_SZ <- edge_id2_SZ[which(edge_id2_SZ$to %in% node3$id) ,]
    #  # filter nodes 
    #  node_SZ <- node_SZ %>% filter(name %in% dataInput_From_SZ() | name %in% dataInput_To_SZ())
    #}

    node_SZ_filtered = node_SZ_filtered_reactive()
    #edge_id2_SZ_filtered = edge_id2_SZ_filtered_reactive()
    edge_id2_SZ_filtered = edges_for_plot4_SZ_reactive()
    
    # filter nodes based on slider
    #print("filterNode")
    #print(node_SZ_filtered)
    node_SZ_filtered <- node_SZ_filtered %>% filter(weight <= input$flowsize[2]) %>% filter(weight >=input$flowsize[1])
    #print("after filtered")
    #print(node_SZ_filtered)
    #print("ghagdjha")
    #print("inputflowsize")
    #print(input$flowsize)
    # filter edges based on nodes left
    #print("edges befpre")
    #print(edge_id2_SZ_filtered)
    edge_id2_SZ_filtered = edge_id2_SZ_filtered[which(edge_id2_SZ_filtered$from %in% node_SZ_filtered$id) ,]
    edge_id2_SZ_filtered = edge_id2_SZ_filtered[which(edge_id2_SZ_filtered$to %in% node_SZ_filtered$id) ,]
    #print("edges after")
    #print(edge_id2_SZ_filtered)
    #print("hjshkjha")
    
    # filter edge based on slider
    print(edge_id2_SZ_filtered)
    print('flowEdge')
    print(input$flowEdge)
    edge_id2_SZ_filtered <- edge_id2_SZ_filtered %>% filter(weight >= input$flowEdge[1]) %>% filter(weight <= input$flowEdge[2])
    print('edge_id2_SZ_filtered')
    print(edge_id2_SZ_filtered)
    
    g_SZ <- graph_from_data_frame(edge_id2_SZ_filtered, directed = TRUE, vertices = node_SZ_filtered)
    #edges_for_plot_SZ <- edges_for_plot_SZ_reactive()
    #edges_for_plot_SZ <- edge_id2_SZ %>% 
    #  inner_join(node_SZ_filtered %>% select(id, x, y), by = c('from' = 'id')) %>%
    #  inner_join(node_SZ_filtered %>% select(id, x, y), by = c('to' = 'id')) 
    
    #print(edges_for_plot_SZ)
    
    print("hjshkjha2")
    
    node_SZ_filtered$weight = degree(g_SZ)
    print("hjshkjha3")
    print(edge_id2_SZ_filtered)
    
    #print(degree(g_SZ))
    
    maptheme <- theme(panel.grid = element_blank()) +
      theme(axis.text = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(axis.title = element_blank()) +
      theme(legend.position = "bottom") +
      theme(panel.grid = element_blank()) +
      theme(panel.background = element_rect(fill = "#596673")) +
      theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
    
    print('node_SZ_filtered')
    print(node_SZ_filtered)
    
    
    node_pos_SZ <- node_SZ_filtered %>%
      dplyr::select(x, y)  # node positions must be called x, y
    
    
    print('node_pos_SZ')
    print(node_pos_SZ)
    print('asdasd')
    print(count(node_pos_SZ))
    
    validate(
      need(count(node_pos_SZ) > 0 , 'There are no node available, please change your settings')
    )
    
    lay_SZ <- create_layout(g_SZ, 'manual',
                            node.positions = node_pos_SZ)
    
    
    print("hjshkjha4")
    print(node_pos_SZ)
    
    lay_SZ <- lay_SZ[ -c(1,2) ]
    # add node degree for scaling the node sizes
    lay_SZ$weight <- degree(g_SZ)
    # We pass the layout lay and use ggraph's geoms geom_edge_arc and geom_node_point for plotting:
    
    # convert all columns to numeric
    # https://stackoverflow.com/questions/19146354/batch-convert-columns-to-numeric-type
    
    #edges_for_plot4_SZ <- edges_for_plot4_SZ_reactive()
    
    # # filter the edges on the map to show it can print correctly
    # edges_for_plot4_SZ <- unique(edges_for_plot4_SZ[which(edges_for_plot4_SZ$from %in% node_SZ_filtered$id) ,])
    # edges_for_plot4_SZ <- unique(edges_for_plot4_SZ[which(edges_for_plot4_SZ$to %in% node_SZ_filtered$id) ,])
    
    # filter the edges weight based on edges filter
    print("filtering edge plot4")
    print(edge_id2_SZ_filtered)
    print("after filtering edge plot4")
    print(edge_id2_SZ_filtered)
    
    # filter the nodes based on edges filter
    
    
    # Change error msg
    print(count(edge_id2_SZ_filtered))
    print(count(node_SZ_filtered))
    print(count(edge_id2_SZ_filtered) == 0)
    print(count(node_SZ_filtered) != 0)
    print(count(edge_id2_SZ_filtered) > 0 & count(node_SZ_filtered) > 0)
    validate(
      need(count(edge_id2_SZ_filtered) > 0 & count(node_SZ_filtered) > 0, 'There are no edge flow available, please change your settings')
    )
    
    #edges_for_plot4_SZ <- edges_for_plot_SZ %>%
    #  select(c('from','to','DAY_TYPE','weight','category','x.x','y.x','x.y','y.y')) %>%
    #  rename(x = x.x) %>%
    #  rename(y = y.x) %>%
    #  rename(xend = x.y) %>%
    #  rename(yend = y.y)
    
    ## change columns from numerical to numeric then back to character (due to error msgs)
    #edges_for_plot4_SZ$x    <- as.numeric(as.character(edges_for_plot4_SZ$x))
    #edges_for_plot4_SZ$y    <- as.numeric(as.character(edges_for_plot4_SZ$y))
    #edges_for_plot4_SZ$xend <- as.numeric(as.character(edges_for_plot4_SZ$xend))
    #edges_for_plot4_SZ$yend <- as.numeric(as.character(edges_for_plot4_SZ$yend))
    
    lay_SZ$x <-  as.numeric(as.character(lay_SZ$x))
    lay_SZ$y <-  as.numeric(as.character(lay_SZ$y))
    
    
    
    # # create bins for colours  ## not working for
    # edges_for_plot4_SZ <- edges_for_plot4_SZ %>%
    #   mutate(
    #     bin = cut(weight, seq(0,max(edges_for_plot4_SZ$weight),2), na.rm=TRUE)) %>%
    #   ungroup()
    
    
    map_jy <- ggplot(lay_SZ) + map_gg2_SZ+ map_gg3_SZ +# ggraph(lay) 
        geom_edge_arc(aes(edge_width = weight,   # draw edges as arcs
                          circular = FALSE,show.legend = TRUE),
                      data = edge_id2_SZ_filtered, curvature = 0.33,
                      alpha = 0.5) +
        scale_edge_width_continuous(range = c(0.5,5),             # scale for edge widths
                                    guide = FALSE) +
        geom_node_point(aes(size = weight, color= as.factor(district),alpha=0.5,stroke=0.5),show.legend = TRUE,     # draw node # size = weight # color outside geom is color of 
                        #outer boundary of marker # fill is internal shading of point marker # outside is constant color, aes is mapping variables, eg for color
        ) +
        scale_size_continuous(range = c(1, 10), guide = FALSE) +    # scale for node sizes
        geom_node_text(aes(label = name),show.legend = FALSE, repel = TRUE, size = 3,
                       color = "white", fontface = "bold") +
        maptheme
    
        plot(map_jy)
    }, height = 600, width = 1600) # 
    
    # create slider widget based on size of the node flows
    output$flowsize_slider_ui <- renderUI({
      sliderInput("flowsize", 
                  label = "Range of interest for node weights:",
                  min = min(node_SZ_filtered_reactive()$weight), max = max(node_SZ_filtered_reactive()$weight), value = c(0, 100))
    })
    
    # create slider widget based on size of the edges flows
    output$flowEdge_slider_ui_passenger <- renderUI({
      sliderInput("flowEdge",
                  label = "Range of interest for edge weights:",
                  min = min(edges_for_plot4_SZ_reactive()$weight), max = max(edges_for_plot4_SZ_reactive()$weight), value = c(min(edges_for_plot4_SZ_reactive()$weight), max(edges_for_plot4_SZ_reactive()$weight)))
    })

    
    ############################## HeatMap Jiayi ####################################
    ## Preparing the Origin Destination matrix to be in the form "from","to","frequency"
    flow_ori_dest <- reactive({
      if(is.null(edge_id2_SZ_filtered_reactive())==FALSE & (input$radio=="SZ")){
        a <- edge_id2_SZ_filtered_reactive() %>%
          dplyr::select('from','to','weight') %>%
        unite(from_to, from,to, sep = "_", remove=FALSE) %>%
        group_by(from_to) %>%
        summarise(Frequency = sum(weight))%>%
        separate(from_to, c("from", "to"))
        a
      }
    })
    
    observe({
      print('1qtyrtyr')
      print(edge_id2_SZ_filtered_reactive())
      print(flow_ori_dest())
      #
      print('1q end')
    })
    
    ## Create the myflows
    #myflows <- reactive({
    #  a <- prepflows(mat = flow_ori_dest(), i = "from", j = "to", fij = "Frequency")
    #  ## Remove the matrix diagonal
    #  diag(a) <- 0
    #  a
    #})
    
    
    
    
    
    # Combine selections for first heatmap based on absolute numbers
    flowSel <- reactive({
      if(is.null(edge_id2_SZ_filtered_reactive())==FALSE && (input$radio=="SZ")){
        ## Select flows that represent at least 20% of the sum of outgoing flows for 
        # each urban area. ( can select other methods )
        print('flow_ori_dest()')
        print(flow_ori_dest())
        print(is.null(flow_ori_dest()))
        print('here9')
        if (nrow(flow_ori_dest()) >0 ) { 
          myflows_local <- prepflows(mat = flow_ori_dest(), i = "from", j = "to", fij = "Frequency")
          ## Remove the matrix diagonal
          diag(myflows_local) <- 0
          # https://www.rdocumentation.org/packages/flows/versions/1.1.1/topics/firstflows
          
          flowSel1 <- firstflows(mat = myflows_local, method = input$firstflows_ui, 
                                 k = input$kvalue)
          # Select the dominant flows (incoming flows criterion)
          ##flowSel2 <- domflows(mat = myflows_local, w = colSums(myflows_local), k = 1)
          flowSel_local <- myflows_local * flowSel1
          print('flowSel_local')
          print('myflow_local')
          print(myflows_local)
          print('flowSel1')
          print(flowSel1)
          print('flowSel')
          print(flowSel_local)
          node_SZ_local = node_SZ_filtered_reactive()
          print('node_SZ_local')
          print(node_SZ_local)
          col_name_flow <- colnames(flowSel_local)
          print('col_name_flow ')
          print(col_name_flow )
          
          
          df_col_name_flow <- as.data.frame(col_name_flow)
          print('a')
          print(df_col_name_flow)
          df_col_name_flow <- df_col_name_flow %>% rename(id =col_name_flow)
          print('b')
          print(df_col_name_flow)
          
          df_col_name_flow$id =  as.character(df_col_name_flow$id) 
          print('c')
          print(df_col_name_flow)
          
          print("node_SZ_local")
          print(node_SZ_local)
          heatmap_name <- inner_join(df_col_name_flow, node_SZ_local, by = 'id') 
          print('heatmap_name')
          print(heatmap_name)
          
          print('before rename')
          print(flowSel_local)
          print('len')
          print(length(flowSel_local))
          print('len2')
          print(length(heatmap_name$name))
          
          ## rename
          colnames(flowSel_local) <- heatmap_name$name
          rownames(flowSel_local) <- heatmap_name$name
          print('after rename')
          print(flowSel_local)
          
          flowSel_local
        }
      }
    })
    
    observe({
      print('cbcxvb')
      print(is.null(edge_id2_SZ_filtered_reactive()))
      if(is.null(edge_id2_SZ_filtered_reactive())==FALSE && (input$radio=="SZ")){
        print('observe asdxpijp')
        flowSel()
      }
    })
    
    
    # Combine selections for 2nd  heatmap based on absolute numbers
    flowSel2 <- reactive({
      if(is.null(edge_id2_SZ_filtered_reactive())==FALSE && (input$radio=="SZ")){
        ## Select flows that represent at least 20% of the sum of outgoing flows for 
        # each urban area. ( can select other methods )
        print('flow_ori_dest()')
        print(flow_ori_dest())
        print(is.null(flow_ori_dest()))
        print('here9')
        if (nrow(flow_ori_dest()) >0 ) { 
          myflows_local <- prepflows(mat = flow_ori_dest(), i = "from", j = "to", fij = "Frequency")
          ## Remove the matrix diagonal
          diag(myflows_local) <- 0
          # https://www.rdocumentation.org/packages/flows/versions/1.1.1/topics/firstflows
          
          flowSel1 <- firstflows(mat = myflows_local/rowSums(myflows_local) * 100, method = input$firstflows2_ui, 
                                 k = input$kvalue2)
          # Select the dominant flows (incoming flows criterion)
          ##flowSel2 <- domflows(mat = myflows_local, w = colSums(myflows_local), k = 1)
          flowSel_local <- myflows_local * flowSel1
          print('flowSel_local')
          print('myflow_local')
          print(myflows_local)
          print('flowSel1')
          print(flowSel1)
          print('flowSel')
          print(flowSel_local)
          node_SZ_local = node_SZ_filtered_reactive()
          print('node_SZ_local')
          print(node_SZ_local)
          col_name_flow <- colnames(flowSel_local)
          print('col_name_flow ')
          print(col_name_flow )
          
          
          df_col_name_flow <- as.data.frame(col_name_flow)
          print('a')
          print(df_col_name_flow)
          df_col_name_flow <- df_col_name_flow %>% rename(id =col_name_flow)
          print('b')
          print(df_col_name_flow)
          
          df_col_name_flow$id =  as.numeric(as.character(df_col_name_flow$id))
          print('c')
          print(df_col_name_flow)
          
          heatmap_name <- inner_join(df_col_name_flow, node_SZ_local, by = 'id') 
          print('heatmap_name')
          print(heatmap_name)
          
          print('before rename')
          print(flowSel_local)
          print('len')
          print(length(flowSel_local))
          print('len2')
          print(length(heatmap_name$name))
          
          ## rename
          colnames(flowSel_local) <- heatmap_name$name
          rownames(flowSel_local) <- heatmap_name$name
          print('after rename')
          print(flowSel_local)
          
          flowSel_local
        }
      }
    })
    
    observe({
      print('cbcxvb')
      print(is.null(edge_id2_SZ_filtered_reactive()))
      if(is.null(edge_id2_SZ_filtered_reactive())==FALSE && (input$radio=="SZ")){
        print('observe asdxpijp')
        flowSel()
      }
    })
    
    # Node weights
    #inflows <- data.frame(id = colnames(myflows), w = colSums(myflows))
    
    
      output$ori_dest <- renderPlotly({
        if( is.null(edge_id2_SZ_filtered_reactive())==FALSE && 
            (input$radio=="SZ") && 
            (nrow(flow_ori_dest()) >0) ) {
          heatmaply(
            flowSel()
          )
        }
      })
      
      output$ori_dest2 <- renderPlotly({
        if( is.null(edge_id2_SZ_filtered_reactive())==FALSE && 
            (input$radio=="SZ") && 
            (nrow(flow_ori_dest()) >0) ) {
          heatmaply(percentize(flowSel2())
            
          )
        }
      })
      
    ############################################ Gravity Model Jia Yi #############################################
    # For X variable selections
    selectX <- c("closeness","between","degree","eigen")
    
    # what to show on drop down
     observe({
       updateSelectizeInput(session, 'select2_1', choices=selectX, selected = selectX[1], server=TRUE)
     })  
     
     # left "select2_2" be dependent on output from 'select2_1'
     observe({
       selectXLess <- selectX[selectX!=input$select2_1]
       updateSelectizeInput(session, 'select2_2', choices=selectXLess, selected = selectXLess[1], server=TRUE)
     })
     
     
     # https://www.statsandr.com/blog/a-shiny-app-for-simple-linear-regression-by-hand-and-in-r/
     
     
     # 2 variable
     #model <- lm(input$yVar ~ input$select2_1 + input$select2_2, data = pass_central)
     ## Function to extract X
     extract_X <- function(selectId) {
       if (input$selectId == "closeness"){
         X <- closeness}
       else if (input$selectId == "between"){
         X <- between}
       else if (input$selectId == "degree"){
         X <- degree}
       else{
         X <- eigen
       }
       X #picks up the last item
     }
     
     # reactive linear model inputs from y and x from UI
     modelInOut = reactive({
       # fml = as.formula(sprintf('%s ~ %s + %s +%s + %s', input$yVar, input$select2_1, "between", "degree", "eigen"))
       fml = as.formula(sprintf('%s ~ %s + %s ', input$yVar, input$select2_1, input$select2_2))
       fit = lm(fml, data=pass_central)
       #model_lm  <- lm(frequencyIn~ closeness + between + degree + eigen, data = pass_central)
       steptest <- stepAIC(fit, direction = "both")
       steptest
     })
     #modelOut = lm(frequencyOut~ closeness + between + degree + eigen, data = pass_central)
     
     # for the UI
     
     
     #steptest <- stepAIC(modelIn(), direction = "both")
     #steptest <- stepAIC(modelOut, direction = "both")
     output$AICtest <- renderPlot({
       plottStats(modelInOut())
       #{if(input$models=="frequencyIn"){plottStats(steptest)
       #} else if(input$models=="frequencyOut"){plottStats(steptest2)
       #} 
       #}
       #plottStats(step3)
     })
     
     
     ###############
 
    # for the UI
    output$yVarUI <- renderUI({
      selectInput("yVar", label = h4("Select dependent variable...."),
                  choices = list("Total Tap In Volume" = "frequencyIn", "Total Tap Out Volume" = "frequencyOut"),
                  selected = "frequencyIn")})
    # if only 2 element
    
    # if 4 element
    #y_tap <- lm(frequencyIn ~ closeness + between + degree + eigen, data = pass_central)
    #step <- stepAIC(y_tap, direction = "both")
    model1 <- lm(frequencyIn ~ closeness + between + degree + eigen, data = pass_central)
    model2 <- lm(frequencyIn ~ closeness + between + degree,  data = pass_central)
    model3 <- lm(frequencyIn ~ closeness + between ,  data = pass_central)
    model4 <- lm(frequencyIn ~ closeness ,  data = pass_central)
    model5 <- lm(frequencyIn ~ closeness + between + degree*eigen + degree*between + degree*closeness , data = pass_central)

    # AIC step wise to find best model
    step1 <- stepAIC(model1, direction = "both") # linear terms only
    #summary(step1)
    #anova(step1)

    step2 <- stepAIC(model2, direction = "both") # linear + quadratic terms
    #summary(step2)
    #anova(step2)

    step3 <- stepAIC(model3, direction = "both") # linear + quadratic + interactive terms

    
    
    ##### above is KIV
    output$AIC <- renderPlot({
      input$go
      {if(input$models=='Model1'){plottStats(step1)
      } else if(input$models=='Model2'){plottStats(step2)
      } else if(input$models=='Model3'){plottStats(step3)
      }
      }
      #plottStats(step3)
    })
    # https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html
    #input$newplot

    # texts output
    output$pred1p <- renderText({if(input$models=='Model1'){anova(step1)$'Pr(>F)'[1]}})  # call col name
    output$pred2p <- renderText({if(input$models=='Model2'){anova(step2)$'Pr(>F)'[1]}})
    output$pred3p <- renderText({if(input$models=='Model3'){anova(step3)$'Pr(>F)'[1]}})

    output$pred1slope <- renderText({if(input$models=='Model1'){step1[[1]][2]}})
    output$pred2slope <- renderText({if(input$models=='Model2'){step2[[1]][2]}})
    output$pred3slope <- renderText({if(input$models=='Model3'){step3[[1]][2]}})

    output$pred1intercept <- renderText({if(input$models=='Model1'){step1[[1]][1]}})
    output$pred2intercept <- renderText({if(input$models=='Model2'){step2[[1]][1]}})
    output$pred3intercept <- renderText({if(input$models=='Model3'){step3[[1]][1]}})

    output$pred1RSq <- renderText({if(input$models=='Model1'){summary(step1)[[8]][1]}})
    output$pred2RSq <- renderText({if(input$models=='Model2'){summary(step2)[[8]][1]}})
    output$pred3RSq <- renderText({if(input$models=='Model3'){summary(step3)[[8]][1]}})




    # previous
    pushButton <- eventReactive(input$go,{
      runif(input$num)
    })# take dependency on the input$go

    output$conclude1 <- renderPrint({
      if(input$models=='Model1' & as.numeric(anova(step1)$'Pr(>F)'[1])<1-input$num){
        'Statistically Significant. Reject H0'}
      else if(input$models=='Model2' & as.numeric(anova(step2)$'Pr(>F)'[1])<1-input$num){
        'Statistically Significant. Reject H0'}
      else if(input$models=='Model3' & as.numeric(anova(step3)$'Pr(>F)'[1])<1-input$num){
        'Statistically Significant. Reject H0'}
      else {
        'Statistically not significant. Do not reject H0'
      }
    })

    #################################### plot residuals###################################
    output$resid <- renderPlot({
      {if(input$models=='Model1'){par(mfrow = c(2, 2))
        plot(step1)
      } else if(input$models=='Model2'){par(mfrow = c(2, 2))
        plot(step2)
      } else if(input$models=='Model3'){par(mfrow = c(2, 2))
        plot(step3)
      }
      }
    })

    ######################## type math formual for Durbin Watson test########################
    output$HypoDurbin <- renderUI({
      withMathJax(helpText('$$H_0 : \\sigma_d = 0$$'),helpText('$$H_0 : \\sigma_d \\ne 0$$'))
    })
    # plot Durbin watson test
    output$DurbinStat <- renderText({if(input$models=='Model1'){durbinWatsonTest(step1)[[2]][1]
    }else if(input$models=='Model2'){durbinWatsonTest(step2)[[2]][1]
    }else if(input$models=='Model3'){durbinWatsonTest(step3)[[2]][1]
    }
    })

    output$DurbinProb <-   renderText({if(input$models=='Model1'){durbinWatsonTest(step1)[[3]][1]
    }else if(input$models=='Model2'){durbinWatsonTest(step2)[[3]][1]
    }else if(input$models=='Model3'){durbinWatsonTest(step3)[[3]][1]
    }
    })
    output$DurbinConclude <- renderPrint({
      if(input$models=='Model1' & as.numeric(durbinWatsonTest(step1)[[3]][1])<1-input$num){
        'Statistically Significant. Reject H0'}
      else if(input$models=='Model2' & as.numeric(durbinWatsonTest(step2)[[3]][1])<1-input$num){
        'Statistically Significant. Reject H0'}
      else if(input$models=='Model3' & as.numeric(durbinWatsonTest(step3)[[3]][1])<1-input$num){
        'Statistically Significant. Reject H0'}
      else {
        'Statistically not significant. Do not reject H0'
      }
    })
    
    
    ############ Testing Gravity model with other stuff
    ## To extract an input without the "" as string
  
    
    ############################################# Meng Yong ########################################################
    
    
    
    
    ################################################ Yong Shan #####################################################



}
# https://shiny.rstudio.com/gallery/file-upload.htmls
# https://cran.r-project.org/web/packages/ggraph/vignettes/tidygraph.html
# https://rstudio.github.io/shinydashboard/structure.html#background-shiny-and-html
# https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html

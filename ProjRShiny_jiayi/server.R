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
    # node<- read_csv("data/node_flowmap_SZ.csv")
    # node$district <- toupper(node$district)
    # node$id = as.character(as.numeric(node$id))
    #edge_id2_SZ <- read_csv("data/edge_flowmap_SZ_passenger.csv")
    
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
        
        #%>%
        #mutate(district= toupper(district))
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
        #node_SZ$id = as.character(as.numeric(node_SZ$id))
        node_SZ
      }
    })  # end of creating node
    
    
    edges_data <- read_csv("data/origin_dest_Full_Aggregated_BusTrips.csv")
    #data2 <- read_csv("data/origin_dest_Full_Aggregated_n_weight.csv")
    # # Load in edges file, which will change between selection of "passenger" or "n()"
    # edges_data <- reactive({ if (input$radio_flowsize == "passenger"){
    #   data1}
    #   else {
    #     data2
    #   }})
    
    ## convert to upper case for Sankey
    edges_data_edit <- edges_data %>%
      mutate(subzone_origin = toupper(subzone_origin)) %>%
      mutate(subzone_destination = toupper(subzone_destination))
   
    ###
    
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
        #%>%
        #   select(-c('YEAR_MONTH','PT_TYPE','ORIGIN_PT_CODE','DESTINATION_PT_CODE','RoadName_Origin','Description_Origin','RoadName_Destination','Description_Destination'))
        # we need to append "subzone_origin" and "subzone_dest" so we can calculate weights based on busstops in these subzones
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
    
 # not to delete this is dummy test 
    
    # output$ex_out <- reactive({
    #     str(c(input$radio,input$pa_from, input$pa_to, input$sz_from, input$sz_to)
    #     )
    # })
    # 
    # outputOptions(output, "ex_out", suspendWhenHidden = FALSE)      
    
  
    
    # plot the FROM Table
	# render tables
    output$mytableFrom = DT::renderDataTable({
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
      updateSelectizeInput(session, 'pa_to', choices=pa_selection_from(), server=TRUE)
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
      updateSelectizeInput(session, 'sz_to', choices=sz_selection_from(), server=TRUE)
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
    
    
    
    
    #dataInput_From_SZ_district <- reactive({
    #  if (input$radio=="SZ"){
    #    input$district_from
    #  }
    #})

    #dataInput_To_SZ_district <- reactive({
    #  if (input$radio=="SZ"){
    #    input$district_to
    #  }
    #})
    
    
    ############################ for Map flow and aggregate filters in map flow ##############################

    edge_id2_filtered_reactive <- reactive({
      
      #if (input$radio=="SZ"){
        # filter the Edges with temporary variable "node2" and "node3"
        node2 <- node() %>% filter(name %in% dataInput_From() ) #%>% filter(district %in% input$district_from)
        edge_id2_filtered <- edge_id2()[which(edge_id2()$from %in% node2$id) ,] 
        
        node3 <- node() %>% filter(name %in% dataInput_To())  #%>% filter(district %in% input$district_to)
        print("edges before day type")
        #print(edge_id2_filtered)
        print(input$checkGroup)
        edge_id2_filtered <- edge_id2_filtered[which(edge_id2_filtered$to %in% node3$id) ,] %>% filter(DAY_TYPE %in% input$checkGroup)
        print("edgess after day type")
        #print(edge_id2_filtered)
        edge_id2_filtered
      #}
    })
    
    node_filtered_reactive <- reactive({
      if (is.null(dataInput_From())== FALSE && is.null(dataInput_To())== FALSE){
        # filter nodes 
        node_filtered <- node() %>% filter(name %in% dataInput_From() | name %in% dataInput_To())# %>%
          #filter(district %in% input$district_from | district %in% input$district$to)  ### trick to making sure 1 to 1 node edge exists
        node_filtered
      }
      
    })
    
    node_filtered_reactive_heatmap <- reactive({
      #if (input$radio=="SZ"){
      # filter nodes 
      node_filtered <- node() %>% filter(name %in% dataInput_From() | name %in% dataInput_To()) %>%
        mutate(id = as.character(id))
      node_filtered
      #}
      
    })
    
    observe({
      print('asdadasdsad')
      print(edge_id2_filtered_reactive())
      print('zczxc')
      print(node_filtered_reactive())
    })
    
    #edges_for_plot_SZ_reactive <- reactive({
    #  edges_for_plot_SZ <- edge_id2_SZ_filtered_reactive() %>% 
    #      inner_join(node_SZ_filtered_reactive() %>% select(id, x, y), by = c('from' = 'id')) %>%
    #      inner_join(node_SZ_filtered_reactive() %>% select(id, x, y), by = c('to' = 'id'))
    #  edges_for_plot_SZ
    #})
    
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

    node_filtered = node_filtered_reactive()
    #edge_id2_SZ_filtered = edge_id2_SZ_filtered_reactive()
    edge_id2_filtered = edges_for_plot4_reactive()
    
    # filter nodes based on slider
    #print("filterNode")
    #print(node_SZ_filtered)
    node_filtered <- node_filtered %>% filter(weight <= input$flowsize[2]) %>% filter(weight >=input$flowsize[1])
    #print("after filtered")
    #print(node_SZ_filtered)
    #print("ghagdjha")
    #print("inputflowsize")
    #print(input$flowsize)
    # filter edges based on nodes left
    #print("edges befpre")
    #print(edge_id2_SZ_filtered)
    edge_id2_filtered = edge_id2_filtered[which(edge_id2_filtered$from %in% node_filtered$id) ,]
    edge_id2_filtered = edge_id2_filtered[which(edge_id2_filtered$to %in% node_filtered$id) ,]
    #print("edges after")
    #print(edge_id2_SZ_filtered)
    #print("hjshkjha")
    
    # filter edge based on slider
    print(edge_id2_filtered)
    print('flowEdge')
    print(input$flowEdge)
    edge_id2_filtered <- edge_id2_filtered %>% filter(weight >= input$flowEdge[1]) %>% filter(weight <= input$flowEdge[2])
    print('edge_id2_SZ_filtered')
    print(edge_id2_filtered)
    
    
    g <- graph_from_data_frame(edge_id2_filtered, directed = TRUE, vertices = node_filtered)
    #edges_for_plot_SZ <- edges_for_plot_SZ_reactive()
    #edges_for_plot_SZ <- edge_id2_SZ %>% 
    #  inner_join(node_SZ_filtered %>% select(id, x, y), by = c('from' = 'id')) %>%
    #  inner_join(node_SZ_filtered %>% select(id, x, y), by = c('to' = 'id')) 
    
    #print(edges_for_plot_SZ)
    
    print("hjshkjha2")
    
    ######node_filtered$weight = degree(g) ########### Could be culprit
    print("hjshkjha3")
    
    #print(degree(g))
    
    maptheme <- theme(panel.grid = element_blank()) +
      theme(axis.text = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(axis.title = element_blank()) +
      theme(legend.position = "bottom") +
      theme(panel.grid = element_blank()) +
      theme(panel.background = element_rect(fill = "#596673")) +
      theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
    
    print('node_filtered')
    print(node_filtered)
    
    
    node_pos <- node_filtered %>%
      dplyr::select(x, y)  # node positions must be called x, y
    
    
    print('node_pos')
    print(node_pos)
    print('asdasd')
    print(count(node_pos))
    
    validate(
      need(count(node_pos) > 0 , 'There are no node available with your current filters, please change your settings')
    )
    
    #lay <- create_layout(g, 'manual',
    #                        node.positions = node_pos)
     lay  <- create_layout(g, layout='drl')
    
    print("hjshkjha4")
    print(node_pos)
    
    #lay <- lay[ -c(1,2) ]
    # add node degree for scaling the node sizes
    #lay$weight <- degree(g)
    # We pass the layout lay and use ggraph's geoms geom_edge_arc and geom_node_point for plotting:
    
    # convert all columns to numeric
    # https://stackoverflow.com/questions/19146354/batch-convert-columns-to-numeric-type
    
    #edges_for_plot4_SZ <- edges_for_plot4_SZ_reactive()
    
    # # filter the edges on the map to show it can print correctly
    # edges_for_plot4_SZ <- unique(edges_for_plot4_SZ[which(edges_for_plot4_SZ$from %in% node_SZ_filtered$id) ,])
    # edges_for_plot4_SZ <- unique(edges_for_plot4_SZ[which(edges_for_plot4_SZ$to %in% node_SZ_filtered$id) ,])
    
    # filter the edges weight based on edges filter
    print("filtering edge plot4")
    print(edge_id2_filtered)
    print("after filtering edge plot4")
    print(edge_id2_filtered)
	# solve the missing edge.id error
    edge_id2_filtered <- edge_id2_filtered %>% mutate(edge.id = row_number())
    
    # filter the nodes based on edges filter
    
    
    # Change error msg
    print(count(edge_id2_filtered))
    print(count(node_filtered))
    print(count(edge_id2_filtered) == 0)
    print(count(node_filtered) != 0)
    print(count(edge_id2_filtered) > 0 & count(node_filtered) > 0)
    validate(
      need(count(edge_id2_filtered) > 0 & count(node_filtered) > 0, 'There are no edge flow available, please change your settings')
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
    
    #lay$x <-  as.numeric(as.character(lay$x))
    #lay$y <-  as.numeric(as.character(lay$y))
    
    
    
    # # create bins for colours  ## not working for
    # edges_for_plot4_SZ <- edges_for_plot4_SZ %>%
    #   mutate(
    #     bin = cut(weight, seq(0,max(edges_for_plot4_SZ$weight),2), na.rm=TRUE)) %>%
    #   ungroup()
    
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
      print('sankey_fn')
      print(input$radio)
      print('edge pa in pa_to')
      print(input$pa_to)
      #edge_id2_local <- edge_id2()
      #print('edge_id2_local')
      #print(edge_id2_local)
      #print(edges_data$planning_area.x %in% input$pa_from)
      if (input$radio == 'PA' ){
        node_local_from = node() %>% filter(name %in% input$pa_from ) %>% dplyr:: select('name')
        node_local_to = node() %>% filter(name %in% input$pa_to ) %>% dplyr:: select('name')
        #print("node_id_from")
        #print(node_id_local)
        print("namevec")
        print(node_local_from$name)
        print(node_local_to$name)
        print("hghggg")
        name_vec <- c(node_local_from$name, node_local_to$name)
        name_vec <- unique(name_vec)
        print(name_vec)
        
      } else if (input$radio == 'SZ' ){
        node_local_from = node() %>% filter(name %in% input$sz_from ) %>% dplyr:: select('name')
        node_local_to = node() %>% filter(name %in% input$sz_to ) %>% dplyr:: select('name')
        #print("node_id_from")
        #print(node_id_local)
        print("namevec")
        name_vec <- c(node_local_from$name, node_local_to$name)
        name_vec <- unique(name_vec)
        print(name_vec)
      }
      
      nodes_sank <- data.frame(name = name_vec, id = 0:(length(name_vec)-1)) %>%
        mutate(id = as.numeric(id)) %>% mutate(name = as.character(name))
     
      
      #print('edges_sankey')
      #print(edges_sankey)
      #print('nodes_from()')
      #print(node_from())
      # from sankey
      print('sankey_fn_node')
      print(class(nodes_sank))
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
        
        
        print("edges_data2")
        #edges_data2
        print(edges_data2)
        
        sank_edge <- edges_data2 %>%
          left_join(nodes_sank,edges_data2, by=c('subzone_origin'='name')) %>%
          rename(from_id = id) %>%
          left_join(nodes_sank,edges_data2, by=c('subzone_destination'='name')) %>%
          rename(to_id = id)
        
        print("sankedge")
        print(sank_edge)
        sank_left <- sank_edge %>% filter(subzone_origin %in% input$sz_from)  %>% 
          filter(subzone_destination %in% input$sz_to) 
        #sank_left   # left edge
      }
      
      print("sankleft")
      print(sank_left)
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
    
    observe({
      print('1qtyrtyr')
      print(edge_id2_filtered_reactive())
      print('flow-ori')
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
      if(is.null(edge_id2_filtered_reactive())==FALSE ){ # && (input$radio=="SZ")
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
                                 k = as.numeric(input$kvalue))
          # Select the dominant flows (incoming flows criterion)
          flowSel2 <- domflows(mat = myflows_local, w = colSums(myflows_local), k = 1)
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
          node_local = node_filtered_reactive_heatmap()
          print('node_local')
          print(node_local)
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
          
          heatmap_name <- inner_join(df_col_name_flow, node_local, by = 'id') 
          print('heatmap_name')
          print(heatmap_name)
          
          print('before rename')
          print(flowSel_local)
          print('len')
          print(length(flowSel_local))
          print('len2')
          print(length(heatmap_name$name))
          
          ## rename
          print('here2')
          print(heatmap_name)
          print('flowsel')
          print(flowSel_local)
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
      print(is.null(edge_id2_filtered_reactive()))
      if(is.null(edge_id2_filtered_reactive())==FALSE){ #  && (input$radio=="SZ")
        print('observe asdxpijp')
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
                                 k = as.numeric(input$kvalue2))
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
          node_local = node_filtered_reactive_heatmap()
          print('node_local')
          print(node_local)
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
          
          heatmap_name <- inner_join(df_col_name_flow, node_local, by = 'id') 
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
      print(is.null(edge_id2_filtered_reactive()))
      if(is.null(edge_id2_filtered_reactive())==FALSE){ #  && (input$radio=="SZ")
        print('observe asdxpijp')
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
    ############################################ Gravity Model Jia Yi #############################################
    # For X variable selections
    selectX <- c("closeness","between","degree","eigen")
    
    # X chosen
     observe({
       updateSelectizeInput(session, 'selectXvar', choices=selectX, selected = selectX, server=TRUE)
     })  
     
     ############
     
   
     
     
     # https://www.statsandr.com/blog/a-shiny-app-for-simple-linear-regression-by-hand-and-in-r/
     
     
     # 2 variable
     #model <- lm(input$yVar ~ input$select2_1 + input$select2_2, data = pass_central)
     ## Function to extract X
     
     
     # reactive linear model inputs from y and x from UI
	linear1 <- reactive({
       if (is.null(input$selectXvar)==FALSE && is.null(input$yVar)==FALSE){
         print("input$selectXvar")
         print(input$selectXvar)
         print(class(input$selectXvar))
         lis_new <-paste(input$selectXvar, collapse = "+")
         print(lis_new)
         print('input$yVar')
         print(input$yVar)
         print('input$yVar done')
         fml = as.formula(sprintf('%s ~ %s ', input$yVar, lis_new))
         fit = lm(fml, data=pass_central)
         fit}})
     modelInOut = reactive({
       if (is.null(input$selectXvar)==FALSE && is.null(input$yVar)==FALSE){
       print("input$selectXvar")
       print(input$selectXvar)
       print(class(input$selectXvar))
       lis_new <-paste(input$selectXvar, collapse = "+")
       print(lis_new)
       print('input$yVar')
       print(input$yVar)
       print('input$yVar done')
       fml = as.formula(sprintf('%s ~ %s ', input$yVar, lis_new))
       fit = lm(fml, data=pass_central)
       #model_lm  <- lm(frequencyIn~ closeness + between + degree + eigen, data = pass_central)
       step <- stepAIC(fit, direction = "both")
       step
       } 
     }) # end of modelInOut which gives the "stepAIC"
     #modelOut = lm(frequencyOut~ closeness + between + degree + eigen, data = pass_central)
     
     observe({modelInOut()})
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
    
   
    #Plot the t statistics based on the y and x variable inputs
    output$AIC <- renderPlotly({
      plottStats(modelInOut())
     
    })
    output$Y <- renderText({input$yVar})
    output$seeX <- renderText({input$selectXvar})
    # https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html
    #input$newplot

    # texts output
     observe({
      print("class linear")
      print(linear1())
      print("end of class linear")})
    output$pred1p <- renderText({min(anova(linear1())$'Pr(>F)'[1:length(input$selectXvar)])})  # call col name


    output$pred1RSqAdj <- renderText({summary(modelInOut())[[9]][1]}) # adjusted R sq
    #output$pred1RSq <- renderText({summary(modelInOut())[[8]][1]}) # non adjusted R sq
    
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
    
  
    ############################################# Meng Yong ########################################################
    
    
    
    
    ################################################ Yong Shan #####################################################



}
# https://shiny.rstudio.com/gallery/file-upload.htmls
# https://cran.r-project.org/web/packages/ggraph/vignettes/tidygraph.html
# https://rstudio.github.io/shinydashboard/structure.html#background-shiny-and-html
# https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html

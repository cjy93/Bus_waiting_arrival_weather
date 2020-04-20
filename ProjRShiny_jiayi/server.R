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
    # Show PA options when 
    # datasetInput <- reactive({
    #     switch(input$radio,
    #            "Planning Area" = 'PA')
    # })
    # read in data (this data will change so we put in server)
    node_SZ <- read_csv("data/node_flowmap.csv")
    edge_id2_SZ <- read_csv("data/edge_flowmap.csv")
    
    output$ex_out <- reactive({
        str(c(input$radio,input$pa_from, input$pa_to, input$sz_from, input$sz_to)
        )
    })
    
    outputOptions(output, "ex_out", suspendWhenHidden = FALSE)      
    
  
    
    # plot the FROM Table
    output$mytableFrom = DT::renderDataTable({
        if (input$radio=="PA"){
            busstops%>% filter(busstops$planning_area %in% input$pa_from)
        }
        else{
            busstops%>% filter(busstops$subzone_name %in% input$sz_from)
        }
    }) # end of renderDataTable FROM
    # https://community.rstudio.com/t/evaluation-error-operation-not-allowed-without-an-active-reactive-context/18468/3
    
    # # make sure the To table has available locations the From pick can go to
    # availableTo <- reactive({
    #   if (input$radio=="PA"){
    #     ## KIV
    #     busstops%>% filter(busstops$planning_area %in% input$pa_to)
    #   }
    #   else{
    #     busstops <- busstops%>% filter(busstops$subzone_name %in% node_SZ)
    #   }
    # }) #End of reactive
    # reference
    # edge_id2_SZ <- edge_id2_SZ[which(edge_id2_SZ$from %in% node2$id) ,] 
    
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
      edgeid <- edge_id2_SZ[which(edge_id2_SZ$from %in% nodeidFrom$id),] 
      nodeidTo <- node_SZ[which(node_SZ$id %in% edgeid$to) ,]
      nodeNameTo <- unique(nodeidTo$name)
      nodeNameTo #grab the last variable
    })
    
    # https://shiny.rstudio.com/articles/selectize.html
    observe({
      updateSelectizeInput(session, 'sz_to', choices=sz_selection_from(), server=TRUE)
    })
    ###
    
  
    
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
    dataInput_From_SZ <- reactive({ # unused at the moment
      if (input$radio=="SZ"){
        input$sz_from
      }
    })
    
    dataInput_To_SZ <- reactive({
      if (input$radio=="SZ"){  # unused at the moment
      input$sz_to
      }
    })
    
    #################
    # 
    # filtered_df <- reactive({
    #   
    #   res <- df %>% filter(current_grade >= input$current)
    #   res <- res %>% filter(projected_grade >= input$projected)
    #   res <- res %>% filter(age >= input$age[1] & age <= input$age[2])
    #   res <- res %>% filter(ethnicity %in% input$ethnicity | is.null(input$ethnicity))
    #   
    #   if(input$previous == TRUE)
    #     res <- res %>% filter(previous_sale == 1)
    #   
    #   if(input$warm == TRUE)
    #     res <- res %>% filter(warm_lead == 1)
    #   
    #   res
    #   
    # })
    output$map_jy <- renderPlot({
    # filter based on "Aggregate Filter" tab
    if (input$radio=="SZ"){
      # filter the Edges with temporary variable "node2" and "node3"
      node2 <- node_SZ %>% filter(name %in% dataInput_From_SZ() )
      edge_id2_SZ <- edge_id2_SZ[which(edge_id2_SZ$from %in% node2$id) ,] 
      
      node3 <- node_SZ %>% filter(name %in% dataInput_To_SZ())
      edge_id2_SZ <- edge_id2_SZ[which(edge_id2_SZ$to %in% node3$id) ,]
      # filter nodes 
      node_SZ <- node_SZ %>% filter(name %in% dataInput_From_SZ() | name %in% dataInput_To_SZ())
    }

   
    #node_SZ_new <- node_SZ
  
    
    g_SZ <- graph_from_data_frame(edge_id2_SZ, directed = TRUE, vertices = node_SZ)
    edges_for_plot_SZ <- edge_id2_SZ %>% 
      inner_join(node_SZ %>% select(id, x, y), by = c('from' = 'id')) %>%
      inner_join(node_SZ %>% select(id, x, y), by = c('to' = 'id')) 
    
    print(edges_for_plot_SZ)
    
    node_SZ$weight = degree(g_SZ)
    
    maptheme <- theme(panel.grid = element_blank()) +
      theme(axis.text = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(axis.title = element_blank()) +
      theme(legend.position = "bottom") +
      theme(panel.grid = element_blank()) +
      theme(panel.background = element_rect(fill = "#596673")) +
      theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
    
    node_pos_SZ <- node_SZ %>%
      select(x, y)  # node positions must be called x, y
    lay_SZ <- create_layout(g_SZ, 'manual',
                            node.positions = node_pos_SZ)
    
    lay_SZ <- lay_SZ[ -c(1,2) ]
    # add node degree for scaling the node sizes
    lay_SZ$weight <- degree(g_SZ)
    # We pass the layout lay and use ggraph's geoms geom_edge_arc and geom_node_point for plotting:
    
    # convert all columns to numeric
    # https://stackoverflow.com/questions/19146354/batch-convert-columns-to-numeric-type
    
    edges_for_plot4_SZ <- edges_for_plot_SZ %>%
      select(c('from','to','DAY_TYPE','weight','category','x.x','y.x','x.y','y.y')) %>%
      rename(x = x.x) %>%
      rename(y = y.x) %>%
      rename(xend = x.y) %>%
      rename(yend = y.y)
    
    # change columns from numerical to numeric then back to character (due to error msgs)
    edges_for_plot4_SZ$x    <- as.numeric(as.character(edges_for_plot4_SZ$x))
    edges_for_plot4_SZ$y    <- as.numeric(as.character(edges_for_plot4_SZ$y))
    edges_for_plot4_SZ$xend <- as.numeric(as.character(edges_for_plot4_SZ$xend))
    edges_for_plot4_SZ$yend <- as.numeric(as.character(edges_for_plot4_SZ$yend))
    
    lay_SZ$x <-  as.numeric(as.character(lay_SZ$x))
    lay_SZ$y <-  as.numeric(as.character(lay_SZ$y))
    
    
    
    # create bins for colours
    edges_for_plot4_SZ <- edges_for_plot4_SZ %>%
      mutate(
        bin = cut(weight, seq(0,max(edges_for_plot4_SZ$weight),2), na.rm=TRUE)) %>%
      ungroup()
    
    
    
    
    
    
    
 
    
    map_jy <- ggplot(lay_SZ) + map_gg2_SZ+ map_gg3_SZ +# ggraph(lay) 
        geom_edge_arc(aes(edge_width = weight,   # draw edges as arcs
                          circular = FALSE,show.legend = TRUE),
                      data = edges_for_plot4_SZ, curvature = 0.33,
                      alpha = 0.5) +
        scale_edge_width_continuous(range = c(0.5,5),             # scale for edge widths
                                    guide = FALSE) +
        geom_node_point(aes(size = weight, color= as.factor(district)),show.legend = FALSE         # draw node
        ) +
        scale_size_continuous(range = c(1, 10), guide = FALSE) +    # scale for node sizes
        geom_node_text(aes(label = name),show.legend = FALSE, repel = TRUE, size = 3,
                       color = "white", fontface = "bold") +
        maptheme
    
        plot(map_jy)
    }, height = 600, width = 2000)
    
    ################## HeatMap JY ####################################
    # # Remove the matrix diagonal
    # 
    # # Selection of flows > 500
    # flowSel1 <- firstflowsg(mat = myflows, method = "xfirst", k = 5000)
    # # Selection of flows > 1000
    # flowSel2 <- firstflowsg(mat = myflows, method = "xfirst", k = 10000)
    # # table output where flowSel1 ( k <=5000))
    # flowtable1 <- compmat(mat1 = myflows, mat2 = myflows * flowSel1, digits = 1)
    # # table output where flowSel1 ( k <=10000))
    # flowstable2 <- compmat(mat1 = myflows, mat2 = myflows * flowSel2, digits = 1)
    # 
    # output$flow_jy <- renderPrint({ 
    #   {if(input$flow_input== 5000){flowtable1
    #   }else if(input$flow_input == 10000){flowstable2
    #   }
    #   }
    # }) # end of renderPrint Statistics
    # 
    # # Plot Flows diagram
    # ## Remove the matrix diagonal
    # diag(myflows) <- 0
    # 
    # ## Select flows that represent at least 20% of the sum of outgoing flows for 
    # ## each urban area. ( can select other methods )
    # flowSel1 <- firstflows(mat = myflows/rowSums(myflows)*100, method = "xfirst", 
    #                        k = 20)
    # 
    # 
    # ## Select the dominant flows (incoming flows criterion)
    # flowSel2 <- domflows(mat = myflows, w = colSums(myflows), k = 1)
    # 
    # ## Combine selections
    # flowSel <- myflows * flowSel1 * flowSel2
    # 
    # ## Node weights
    # inflows <- data.frame(id = colnames(myflows), w = colSums(myflows))
    # 
    # ## OLD: Plot dominant flows map
    # output$polygon_jy <- renderImage({plot(mpsz, col = "#cceae7", border = NA)})
    # #output$flow
    # opar <- par(mar = c(0,0,2,0))
    # ## Ploting the points
    # # output$flowDom_jy <- leaflet() %>% sp::plot(mpsz, col = "#cceae7", border = NA) %>%
    # # 
    # # 
    # # plotMapDomFlows(mat = flowSel, spdf = mpbus, spdfid = "BUS_STOP_N", w = inflows, wid = "id",
    # #                 wvar = "w", wcex = 0.05, add = TRUE,
    # #                 legend.flows.pos = "topright",
    # #                 legend.flows.title = "Nb. of commuters") %>%
    # # title("Dominant Flows of Commuters") %>%
    # # mtext(text = "singapore bus,2020", side = 4, line = -1, adj = 0.01, cex = 0.8)   ############## not work with map separated
    # 
    # 
    # 
    # 



    
    ############################################# Meng Yong ########################################################
    
    
    
    
    ################################################ Yong Shan #####################################################



}
# https://shiny.rstudio.com/gallery/file-upload.htmls
# https://cran.r-project.org/web/packages/ggraph/vignettes/tidygraph.html
# https://rstudio.github.io/shinydashboard/structure.html#background-shiny-and-html
# https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html

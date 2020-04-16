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
        location_my %>% dplyr::filter(planning_area == input$planning_region_my)%>% 
            dplyr::filter(TIME_PER_HOUR == input$time_my)%>%
            dplyr::filter(DAY_TYPE == input$week_my)
    })
    
    output$map_my <- renderLeaflet({
        map_data <- location_my_map()
        map_data %>% leaflet(width = 1600, height = 1200)%>%
            addProviderTiles("CartoDB", group = "CartoDB") %>% 
            addCircleMarkers(lng = ~lon, 
                             lat = ~lat,
                             radius = ~tap_in_out_radius,
                             fillOpacity = 0.75,
                             label = ~Description, 
                             popup = ~paste0('Tap in Volume: ', TOTAL_TAP_IN_VOLUME, "<br/>", 'Tap out volume: ', TOTAL_TAP_OUT_VOLUME),
                             color =  ~pal(planning_area)
                             ) %>%
            setMaxBounds(lng1 = 103.801959 + .25, 
                         lat1 = 1.32270 + .25, 
                         lng2 = 103.801959 - .25, 
                         lat2 = 1.32270 - .25)%>%
            addLayersControl(baseGroups =unique(planning_area_list_my)
            )
    })
    
    output$tbl_my <- DT::renderDataTable({
        DT::datatable(
            location_my_map(),
            extensions = "Scroller",
            style = "bootstrap",
            class = "compact",
            width = "100%",
            options = list(
                deferRender = TRUE,
                scrollY = 300,
                scroller = TRUE,
                dom = 'tp'
            )
        )
    })

}

#location_my['BusStopCode', 'Description', 'RoadName', 'planning_area', 'TOTAL_TAP_IN', 'TOTAL_TAP_OUT_VOLUME', 'TIME_PER_HOUR', 'lon', 'lat', 'tap_in_out_radius']%>%
#    rename(c(TapIns = TOTAL_TAP_IN, TapOuts = TOTAL_TAP_OUT_VOLUME, Time = TIME_PER_HOUR, PlanningArea = planning_area)) %>% 
#    dplyr::filter(planning_area %in% input$planning_region_my)

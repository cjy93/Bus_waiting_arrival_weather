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
    ## Ploting the points
    # output$flowDom_jy <- leaflet() %>% sp::plot(mpsz, col = "#cceae7", border = NA) %>%
    # 
    # 
    # plotMapDomFlows(mat = flowSel, spdf = mpbus, spdfid = "BUS_STOP_N", w = inflows, wid = "id",
    #                 wvar = "w", wcex = 0.05, add = TRUE,
    #                 legend.flows.pos = "topright",
    #                 legend.flows.title = "Nb. of commuters") %>%
    # title("Dominant Flows of Commuters") %>%
    # mtext(text = "singapore bus,2020", side = 4, line = -1, adj = 0.01, cex = 0.8)   ############## not work with map separated
    
}



# https://shiny.rstudio.com/gallery/file-upload.htmls
# https://cran.r-project.org/web/packages/ggraph/vignettes/tidygraph.html
# https://rstudio.github.io/shinydashboard/structure.html#background-shiny-and-html
# https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html

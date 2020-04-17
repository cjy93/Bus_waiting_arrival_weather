# UI Shiny
################### Define UI ----############################
css <- HTML(" body {
    background-color: #000000;
}")

ui <- dashboardPage(
    skin = 'blue',
    dashboardHeader(title = "Bus Travel in Singapore",titleWidth = 450),
    dashboardSidebar(width = 200,
                     sidebarMenu(id = 'sbm',
                                 menuItem('Home', tabName = 'Home', icon=icon("home")),
                                 menuItem('Flow', tabName = 'Flow_Diagrams', icon = icon("bus")),
                                 menuItem('Proportional_Symbol_Map', tabName = 'ProportionalSymbolMap_Diagrams', icon = icon("connectdevelop")),
                                 menuItem('Centrality', tabName = 'Centrality', icon = icon("globe")),
                                 menuItem('GravityModel', tabName = 'Gravity_Model', icon = icon("chart-line")),
                                 menuItem('Download', tabName = 'Download', icon = icon("download"))# tabs are here!
                                )
                    ), # end of DashboardSiderBar
    dashboardBody(
        tabItems(
            tabItem(tabName = 'Home',
                    fluidPage(
                        titlePanel("Overview"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Create demographic maps with 
               information from the 2010 US Census."),
                                
                                selectInput("var", 
                                            label = "Choose a variable to display",
                                            choices = c("Percent White", 
                                                        "Percent Black",
                                                        "Percent Hispanic", 
                                                        "Percent Asian"),
                                            selected = "Percent White"),
                                
                                sliderInput("range", 
                                            label = "Range of interest:",
                                            min = 0, max = 100, value = c(0, 100))
                            ),
                            
                            mainPanel(
                                textOutput("selected_var")
                            )
                        )
                    )
            ), #end of tabname "home" 
            
            tabItem(tabName = 'Flow_Diagrams',
                    fluidPage(
                        
                        titlePanel("Flow Maps"),
                        fluidRow(
                            
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("Select passenger flow size"),
                                    
                                    selectInput("flow_input", 
                                                label = "flow k",
                                                choices = c(5000, 
                                                            10000
                                                            ),
                                                selected = 10000),
                                    
                                    sliderInput("range", 
                                                label = "Range of interest:",
                                                min = 0, max = 100, value = c(0, 100))
                                ),
                                column(
                                    mainPanel(
                                        verbatimTextOutput("flow_jy"),
                                        #plotOutput("ploygon_jy"),  # not working
                                        plotOutput("map_jy"),
                                        visNetworkOutput("vizNW_jy"),
                                        sankeyNetworkOutput("sankey_jy", width = "100%", height = "500px")
                                        #plotOutput("flowDom_jy")
                                        #leafletOutput("flowDom_jy"),
                                    ), width = 10
                                ) # of column
                            )
                            
                        ) # end of fluidRow
                    ) # end of fluidPage
            ), #end of tabname "flow"  
            
            tabItem(tabName = 'ProportionalSymbolMap_Diagrams',
                    fluidPage(
                        titlePanel("Proportional Symbol Map Diagrams"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                            ),
                            
                            mainPanel(
                                
                            )
                        )
                    )
            ), #end of tabname "ProportionalSymbolMap_Diagrams" 
            
            tabItem(tabName = 'Centrality',
                    fluidPage(
                        titlePanel("Centrality"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                            ),
                            
                            mainPanel(
                                
                            )
                        )
                    )
            ), #end of tabname "Centrality" 
            
            tabItem(tabName = 'Gravity_Model',
                    fluidPage(
                        titlePanel("Gravity_Model"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                            ),
                            
                            mainPanel(
                                
                            )
                        )
                    )
            ), #end of tabname "Gravity_Model" 
            
            tabItem(tabName = 'Download',
                    fluidPage(
                        titlePanel("Download"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                            ),
                            
                            mainPanel(
                                
                            )
                        )
                    )
            ) #end of tabname "Download" 
        
        )# end of TabItems
    ) #end of dashboardBody
)

#https://shiny.rstudio.com/gallery/soccer-player-similarity.html
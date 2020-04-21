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
                                 menuItem('Flow', tabName = 'Flow_Diagrams', icon = icon("bus"),startExpanded= TRUE,
                                    menuSubItem("Aggregation Filters", tabName = "Aggregation_Filters"),
                                    menuSubItem("Flow Map", tabName = "Flow_Map"),
                                    menuSubItem("Sankey Diagram", tabName = "Sankey_Diagram")),
                                 menuItem('Proportional Symbol_Map', tabName = 'ProportionalSymbolMap_Diagrams', icon = icon("connectdevelop")),
                                 menuItem('Centrality', tabName = 'Centrality', icon = icon("globe")),
                                 menuItem('Gravity Model', tabName = 'Gravity_Model', icon = icon("chart-line")),
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
            
            tabItem(tabName = 'Aggregation_Filters',
                    fluidPage(

                        titlePanel("Aggregation Filters"),
                        fluidRow(

                            sidebarLayout(
                                
                                    sidebarPanel(
                                        helpText("Select passenger flow size"),
                                        verbatimTextOutput('ex_out'),
                                        # I() indicates it is raw JavaScript code that should be evaluated, instead
                                        # of a normal character string
                                        ## Select dropdown of either filter by Planning Area or Filter by Subzone
                                        radioButtons("radio", h3("Choose type of filter"),
                                                     choices = list("Planning Area" = 'PA', 
                                                                    "Subzone" = 'SZ'),selected = 'PA'),
                                        #switch view to PA
                                        conditionalPanel( condition = "input.radio=='PA'", #input.radio == 'PA'
                                            # if you select planning area, then show this
                                            selectizeInput(
                                                'pa_from', 'From Planning Area', choices = unique(busstops$planning_area) ,
                                                multiple = TRUE,
                                                options = list(maxItems = 10,
                                                   placeholder = 'Please select one or more options below',
                                                   onInitialize = I('function() { this.setValue(""); }')
                                                )
                                            ), #end of selectizeInput from Planning Area
                                            
                                            selectizeInput(
                                                'pa_to', 'To Planning Area', choices = unique(busstops$planning_area) ,
                                                multiple = TRUE,
                                                options = list(maxItems = 10,
                                                    placeholder = 'Please select one or more options below',
                                                    onInitialize = I('function() { this.setValue(""); }')
                                                )
                                            )#end of selectizeInput to Planning Area
                                        ), # end of conditionalPanel
                                        
                                        #switch view to SZ
                                        conditionalPanel( condition = "input.radio=='SZ'", 
                                                          # if you select subzone, then show this
                                                          selectizeInput(
                                                              'sz_from', 'From SubZone', choices = unique(busstops$subzone_name) ,
                                                              multiple = TRUE,
                                                              options = list(maxItems = 10,
                                                                  placeholder = 'Please select one or more options below',
                                                                  onInitialize = I('function() { this.setValue(""); }')
                                                              )
                                                          ), #end of selectizeInput from Planning Area
                                                          
                                                          selectizeInput(
                                                              'sz_to', 'To SubZone', choices = "initialisation"  , #initialise
                                                              #'sz_to', 'To SubZone', choices = unique(busstops$subzone_name) ,
                                                              multiple = TRUE,
                                                              options = list(maxItems=10,
                                                                  placeholder = 'Please select one or more options below',
                                                                  onInitialize = I('function() { this.setValue(""); }')
                                                              )
                                                          )#end of selectizeInput to Planning Area
                                        ) # end of conditionalPanel
                                        #submitButton("Apply changes")
                                ) ,
                               
                                mainPanel(
                                    tabsetPanel(
                                        tabPanel("Source Busstops", withSpinner(DT::dataTableOutput("mytableFrom"), type=2),downloadButton('downloadFrom',"Download the table")), 
                                        tabPanel("Destination Busstops", withSpinner(DT::dataTableOutput("mytableTo"), type=2),downloadButton('downloadTo',"Download the table"))
                                    )# https://www.listendata.com/2019/07/add-loader-for-shiny-r.html
                                    

                                ) # end of mainPanel
                            ) # end of sidebarlayout

                        ) # end of fluidRow
                    ) # end of fluidPage
            ), #end of tabname "MainAggregate"
            
            tabItem(tabName = 'Flow_Map',
                    fluidPage(
                        title = "Flow Maps",
                        #titlePanel("Flow Maps"),
                        h1("Flow Maps"),
                        h3("For filtering of Planning Area OR Subzone:"),
                        h5("Please filter them at 'Aggregate Filter' tab"),
                        plotOutput("map_jy"),
                        hr(),
                        fluidRow(
                          column(3,  
                            helpText("Select passenger flow size"),
                            radioButtons("radio_flowsize", h3("Choose type of selection criteria"),
                                         choices = list("By Passenger Volume" = 'passenger', 
                                                        "By Number of Bus Services" = 'bus'),selected = 'passenger'),
                            
                           
                            conditionalPanel( condition = "input.radio_flowsize=='passenger'",         
                                sliderInput("flowsize_passenger", 
                                            label = "Range of interest:",
                                            min = 0, max = 10000, value = c(0, 100))
                                ),
                            conditionalPanel( condition = "input.radio_flowsize=='bus'", 
                                sliderInput("busstopNetworksize", 
                                            label = "Range of interest:",
                                            min = 0, max = 100, value = c(0, 100))
                                )), # end of first column 3
                          column(3,  
                            selectizeInput(
                                'district_from', 'From District', choices = "initialisation"  , #initialise
                                multiple = TRUE,
                                options = list(maxItems=10,
                                               placeholder = 'Please select one or more options below',
                                               onInitialize = I('function() { this.setValue(""); }')
                                )
                            ), #end of selectizeInput to District from
                            selectizeInput(
                                'district_to', 'To District', choices = "initialisation"  , #initialise
                                multiple = TRUE,
                                options = list(maxItems=10,
                                               placeholder = 'Please select one or more options below',
                                               onInitialize = I('function() { this.setValue(""); }')
                                )
                            )),
                          column(3,
                                 uiOutput("flowsize_slider_ui")   
                                 #sliderInput("flowsize", 
                                 #            label = "Range of interest:",
                                 #            min = 0, max = 100000, value = c(0, 100))
                                 )
                                 #end of selectizeInput to district To
                                    #),width = 2
                                #), #end of column for sidebarpanel
                                #column(
                                    #mainPanel(
                                        #verbatimTextOutput("flow_jy"),
                                        
                                        # visNetworkOutput("vizNW_jy"),
                                        # sankeyNetworkOutput("sankey_jy", width = "100%", height = "500px")
                                        #plotOutput("flowDom_jy")
                                        #leafletOutput("flowDom_jy"),
                                    #), width = 10
                                #) # of column
                            #)
                            
                        ) # end of fluidRow
                    ) # end of fluidPage
            ), #end of tabname "flowMap"  
            
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
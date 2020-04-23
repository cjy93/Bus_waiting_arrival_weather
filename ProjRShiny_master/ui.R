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
                                          menuSubItem("Origin-Destination Matrix", tabName = "Origin-Destination")),
                                 menuItem('Passenger Volume', tabName = 'Passenger_volume', icon = icon("connectdevelop")),
                                 menuItem('Busstop Centrality', tabName = 'Centrality', icon = icon("globe")),
                                 menuItem('GravityModel', tabName = 'Gravity_Model', icon = icon("chart-line"))
                     )
    ), # end of DashboardSiderBar
    
    dashboardBody(
        tabItems(
            tabItem(tabName = 'Home',
                    fluidPage(
                        # titlePanel("Home"),
                        fluidRow(
                            column(12,
                                   img(src = "SBS-Transit-bus-5.jpg", height = 300),
                                   h1('Re-imaginging Bus Transport Network in Singapore'),
                                   p("Singapore's public transport use rose to hit a record high in 2018, with a total of 7.54 million trips made on buses or trains each day."),
                                   p("Here's what may come across your mind: Do you ever have experiences where a bus ride that is supposed to be short and quick took way longer 
                                   than expected? Are you frustrated that the bus stops at every stop even though there's nobody boarding or alighting? And why do we have so many 
                                   bus stops that almost nobody uses?"),
                                   p("What if we can reimagine the public bus network in Singapore through data?"),
                                   p("In this project, we will use use data visualization techniques to map out all transportation nodes in Singapore and re-propose a different way 
                                     of organizing our bus services, which include bus stops, bus routes, and connectivity within region and from regions to regions."),
                                   h2("Main Packages Used"),
                                   p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
                                   code('install.packages(shiny)'),
                                   br(),
                                   code('install.packages(dplyr)'),
                                   br(),
                                   code('install.packages(tidyverse)'),
                                   br(),
                                   code('install.packages(leaflet)'),
                                   br(),
                                   code('install.packages(tidygraph)'),
                                   br(),
                                   code('install.packages(flows)'),
                                   br(),
                                   code('install.packages(sf)'),
                                   br(),
                                   h2("Links"),
                                   a("Github", href = "https://github.com/cjy93/LTA_bus_analysis"),
                                   br(),
                                   a("Wikipedia", href = "https://wiki.smu.edu.sg/1920t2isss608/Group08_proposal"),
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
                                    h3("Choose type of selection criteria"),
                                    h4("You can have multiple selection"),
                                    selectizeInput(
                                        'district_from', 'From District', choices = busstops$district  , #initialise
                                        multiple = TRUE,
                                        options = list(maxItems=10,
                                                       placeholder = 'Please select one or more options below'
                                        )
                                    ), #end of selectizeInput to District from
                                    selectizeInput(
                                        'district_to', 'To District', choices = busstops$district  , #initialise
                                        multiple = TRUE,
                                        options = list(maxItems=10,
                                                       placeholder = 'Please select one or more options below'
                                        )
                                    ),
                                    
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
                                                                         placeholder = 'Please select one or more options below',selected=busstops$planning_area[1],
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
                        #plotlyOutput("map_jy"),
                        plotOutput("map_jy"),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        fluidRow(
                            column(4,  
                                   #helpText("Select passenger flow size"),
                                   radioButtons("radio_flowsize", h3("Choose edge type and edge weight"),
                                                choices = list("By Passenger Volume" = 'passenger', 
                                                               "By Number of Bus Services" = 'bus'),selected = 'passenger'),
                                   
                                   
                                   conditionalPanel( condition = "input.radio_flowsize=='passenger'",
                                                     uiOutput("flowEdge_slider_ui_passenger")   ### slider for node weights
                                   )
                                   # conditionalPanel( condition = "input.radio_flowsize=='bus'", 
                                   #     sliderInput("busstopNetworksize", 
                                   #                 label = "Range of interest:",
                                   #                 min = 0, max = 100, value = c(0, 100))
                            ), # end of first column 3
                            
                            column(4,
                                   h3("Choose Node weights"),
                                   uiOutput("flowsize_slider_ui"),   ### slider for node weights
                                   #sliderInput("flowsize", 
                                   #            label = "Range of interest:",
                                   #            min = 0, max = 100000, value = c(0, 100))
                                   
                                   checkboxGroupInput("checkGroup", 
                                                      h3("Type of Day"), 
                                                      choices = list("Weekdays" = "WEEKDAY", 
                                                                     "Weekends/Holidays" = "WEEKENDS/HOLIDAY"),
                                                      selected = c("WEEKDAY","WEEKENDS/HOLIDAY")) # end of select input for day type
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
            
            
            tabItem(tabName = 'Origin-Destination',
                    fluidPage(
                        titlePanel("Origin-Destination Matrix"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Origin Destination Matrix by Absolute numbers"),
                                # Copy the line below to make a select box 
                                selectInput("firstflows_ui", label = h3("First Flow Methods"), 
                                            choices = list("nfirst" = "nfirst", "xfirst" = "xfirst", "xsumfirst" = "xsumfirst"), 
                                            selected = "xfirst"),
                                selectInput("kvalue", label = h3("select min flow size"),
                                            choices = list("10" = 10, "20" = 20, "50" = 50,"100"=100,"200"=200,"500"=500), 
                                            selected = 10),
                                helpText("Origin Destination Matrix by flows that represent at least % of the outputs"),
                                # Copy the line below to make a select box 
                                selectInput("firstflows2_ui", label = h3("First Flow Methods"), 
                                            choices = list("nfirst" = "nfirst", "xfirst" = "xfirst"), 
                                            selected = "xfirst"),
                                selectInput("kvalue2", label = h3("select min flow size"), 
                                            choices = list( "5" = 0.05 , "10" = 0.10, "20" = 0.20, "50" = 0.50,"90"= 90), 
                                            selected = 10)
                            ),
                            
                            mainPanel(
                                helpText("Origin Destination Matrix by Absolute numbers"),
                                plotlyOutput("ori_dest"),
                                helpText("Origin Destination Matrix by flows that represent at least % of the outputs"),
                                plotlyOutput("ori_dest2")
                            )
                        )
                    )
            ), #end of tabname "Origin-Destination" 
            
            ### Passenger Volume by Busstop ##
            tabItem(tabName = 'Passenger_volume',
                    fluidPage(
                        titlePanel("Passenger Volume"),
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("radio_my1", h3("Choose Region Filter"),
                                             choices = list("Singapore" = "SG", "Planning Area" = 'PA', 
                                                            "Subzone" = 'SZ'), selected = 'SG'),
                                
                                conditionalPanel(condition = "input.radio_my1=='SZ' | input.radio_my1=='PA'", 
                                                 selectizeInput(
                                                     'pa_my_1', 'Planning Area', choices = sort(unique(busstops$planning_area)),
                                                     selected = 'Ang Mo Kio'
                                                 )
                                ), 
                                
                                conditionalPanel(condition = "input.radio_my1=='SZ'", 
                                                 selectizeInput(
                                                     'sz_filter_my1', 'SubZone', choices = sort(unique(busstops$subzone_name))
                                                 )
                                ), 
                                selectInput(inputId = "week_my", 
                                            label = "Weekday / Weekend",
                                            choices = c('Weekday' = 'WEEKDAY',
                                                        'Weekend' = 'WEEKENDS/HOLIDAY'),
                                            selected = "WEEKDAY"),
                                sliderInput(inputId = 'time_my',
                                            label = 'Time of the Day',
                                            min = 6,
                                            max = 23,
                                            step = 1,
                                            value = 8),
                                
                            ),
                            
                            mainPanel(
                                leafletOutput("map_my", height = 800),                                   
                                br(),
                                plotlyOutput("trendPlot_my", , height = 400),                                 
                                br(),
                                DT::dataTableOutput("tbl_my")
                                
                                # https://stackoverflow.com/questions/50128349/filtering-leaflet-map-data-in-shiny
                            )
                        )
                    )
            ), #end of tabname Passenger Volume by Busstop 
            
            ### Centrality ###
            tabItem(tabName = 'Centrality',
                    fluidPage(
                        titlePanel("Busstop Centrality"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("radio_my2", h3("Choose Region Filter"),
                                             choices = list("Singapore" = "SG", "Planning Area" = 'PA', 
                                                            "Subzone" = 'SZ'), selected = 'PA'),
                                
                                conditionalPanel(condition = "input.radio_my2=='SZ' | input.radio_my2=='PA'", 
                                                 selectizeInput(
                                                     'pa_my_2', 'Planning Area', choices = sort(unique(busstops$planning_area)),
                                                     selected = 'Ang Mo Kio'
                                                 )
                                ), 
                                
                                conditionalPanel(condition = "input.radio_my2=='SZ'", 
                                                 selectizeInput(
                                                     'sz_filter_my_2', 'SubZone', choices = sort(unique(busstops$subzone_name))
                                                 )
                                ), 
                                ## 
                                
                                radioButtons("radio_my3", h3("Choose Centrality filter"),
                                             choices = list("Betweenness Centrality" = "between_my_filter",
                                                            "Degree Centrality" = "degree_my_filter",
                                                            "Closeness Centrality" = "closeness_my_filter",
                                                            "Eigenvalue Centrality" = "eigen_my_filter"), selected = "degree_my_filter"),
                                
                                sliderInput(
                                    inputId = "centrality_filter", 
                                    label = "Centrality", 
                                    min = 0, 
                                    max = 1, 
                                    value = c(0.05, 1))
                                
                            ),
                            
                            mainPanel(
                                #put in viz here
                                leafletOutput("map_my_centrality", height = 900)
                                #DT::dataTableOutput("tbl_my_2")
                            )
                        )
                    )
            ), #end of tabname "Centrality" 
            
            tabItem(tabName = 'Gravity_Model',
                    fluidPage(
                        titlePanel("Gravity_Model"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("yVar", label = h4("Select dependent variable...."),
                                            choices = list("Total Tap In Volume" = "frequencyIn", "Total Tap Out Volume" = "frequencyOut"),
                                            selected = "frequencyIn"),
                                radioButtons("numOfVar", h3("Choose Number of Variables"),
                                             choices = list("2_var" = '2', 
                                                            "4_var" = '4'),selected = '2'),
                                #switch view to PA
                                conditionalPanel( condition = "input.numOfVar=='2'", #input.radio == 'PA'
                                                  # if you select planning area, then show this
                                                  selectizeInput(
                                                      'select2_1', 'First item', choices = "initialisation"  , #initialise
                                                      multiple = FALSE,
                                                      options = list(maxItems=1,
                                                                     placeholder = 'Please select one or more options below',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                      )
                                                  ), #end of selectizeInput to District from
                                                  
                                                  selectizeInput(
                                                      'select2_2', 'Second Item', choices = "initialisation"  , #initialise
                                                      multiple = FALSE,
                                                      options = list(maxItems=1,
                                                                     placeholder = 'Please select one or more options below',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                      )
                                                  ) #end of selectizeInput to District from
                                ), # end of conditionalPanel
                                
                                #switch view to SZ
                                conditionalPanel( condition = "input.numOfVar=='4'", 
                                                  selectizeInput(
                                                      'select4_1', 'First item', choices = "initialisation"  , #initialise
                                                      multiple = FALSE,
                                                      options = list(maxItems=1,
                                                                     placeholder = 'Please select one or more options below',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                      )
                                                  ), #end of selectizeInput to District from
                                                  
                                                  selectizeInput(
                                                      'select4_2', 'Second Item', choices = "initialisation"  , #initialise
                                                      multiple = FALSE,
                                                      options = list(maxItems=1,
                                                                     placeholder = 'Please select one or more options below',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                      )
                                                  ), #end of selectizeInput to District from
                                                  selectizeInput(
                                                      'select4_3', 'Third Item', choices = "initialisation"  , #initialise
                                                      multiple = FALSE,
                                                      options = list(maxItems=1,
                                                                     placeholder = 'Please select one or more options below',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                      )
                                                  ), #end of selectizeInput to District from
                                                  
                                                  selectizeInput(
                                                      'select4_4', 'Fourth Item', choices = "initialisation"  , #initialise
                                                      multiple = FALSE,
                                                      options = list(maxItems=1,
                                                                     placeholder = 'Please select one or more options below',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                      )
                                                  ) #end of selectizeInput to District from
                                ), # end of conditionalPanel
                                selectInput(inputId = "models", 
                                            label = "Choose a variable to display",
                                            choices = c("Model1", 
                                                        "Model2",
                                                        "Model3"),
                                            selected = "Model3"),
                                
                                numericInput(inputId = "num", 
                                             label = ("confidence level"), 
                                             value = 0.95)
                                #submitButton("Apply changes")
                                
                            ),
                            
                            mainPanel(
                                h5("The polynomial regression model (bivariate analysis) is calculated for each demographic variable and shown below:"),
                                h4("Slope"),
                                verbatimTextOutput("pred1slope"),
                                verbatimTextOutput("pred2slope"),
                                verbatimTextOutput("pred3slope"),
                                h4("Intercept"),
                                verbatimTextOutput("pred1intercept"),
                                verbatimTextOutput("pred2intercept"),
                                verbatimTextOutput("pred3intercept"),
                                h4("R Square"),
                                verbatimTextOutput("pred1RSq"),
                                verbatimTextOutput("pred2RSq"),
                                verbatimTextOutput("pred3RSq"),
                                h5("If p value is < 1-(confidence level), we conclude that the relationship between the independent variables and medal average is statistically significant"),
                                h4("P value of regression model"),
                                verbatimTextOutput("pred1p"),
                                verbatimTextOutput("pred2p"),
                                verbatimTextOutput("pred3p"),
                                verbatimTextOutput("conclude1"),
                                verbatimTextOutput("conclude2"),
                                verbatimTextOutput("conclude3"),
                                #plottStats("finalCoef")
                                plotOutput("AIC"),
                                h4("In order for our regression model to be more accurate, our group has to check if the assumption of error terms having constant variance is satisfied."),
                                plotOutput("resid"),
                                column(6,
                                       h4("Now we check if the error terms are independent at 5% significance level using the Durbin Watson test."),
                                       h4("The Durbin_Watson Test"),
                                       h5("The value of Durbin-Watson Statistics ranges from 0 to 4.
                                         As a general Rule of thumb, the residuals are not correlated if the DW statistic is approximately 2 ,
                                         and an acceptable range for the DW statistic is 1.50 to 2.50"),
                                       uiOutput("HypoDurbin")
                                ),
                                column(6,
                                       h4('Durbin-Watson Statistic'),
                                       verbatimTextOutput("DurbinStat"),
                                       h4('Durbin-Watson P-value'),
                                       verbatimTextOutput("DurbinProb"),
                                       h4('Durbin-Watson conclusion'),
                                       verbatimTextOutput("DurbinConclude")
                                )
                            )
                        )
                    )
            ) #end of tabname "Gravity_Model" 
            
            
        )# end of TabItems
    ) #end of dashboardBody
)

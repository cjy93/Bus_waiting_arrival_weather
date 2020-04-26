# UI Shiny
################### Define UI ----############################
css <- HTML(" body {
    background-color: #000000;
}")

ui <- dashboardPage(
    skin = 'blue',
    dashboardHeader(title = "Network Analysis of Singapore Bus Transport", titleWidth = 450),
    dashboardSidebar(width = 200,
                     sidebarMenu(id = 'sbm',
                                 menuItem('Home', tabName = 'Home', icon=icon("home")),
                                 menuItem('  Passenger Volume', tabName = 'Passenger_volume', icon = icon("connectdevelop")),
                                 menuItem('Bus Stop Centrality', tabName = 'Centrality', icon = icon("globe")),
                                 menuItem('Flow', tabName = 'Flow_Diagrams', icon = icon("bus"),startExpanded= TRUE,
                                          menuSubItem("Aggregation Filters", tabName = "Aggregation_Filters"),
                                          menuSubItem("Flow Map", tabName = "Flow_Map"),
                                          menuSubItem("Trip Generator and Receiver", tabName = "flow_Bar_Graph"),
                                          menuSubItem("Origin-Destination Matrix", tabName = "Origin-Destination")),
                                 menuItem('EDA', tabName = 'EDA_Model', icon = icon("wpexplorer")),
                                 menuItem('Gravity Model', tabName = 'Gravity_Model', icon = icon("chart-line"))
                     )
    ), # end of DashboardSiderBar
    
    dashboardBody(
        tabItems(
            tabItem(tabName = 'Home',
                    fluidPage(
                        # titlePanel("Home"),
                        fluidRow(
                            column(12,
                                   img(src = "Group_logo1.png", height = 250),
                                  
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
                                   a("Wikipedia", href = "https://wiki.smu.edu.sg/1920t2isss608/Group08_proposal")
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
                                    radioButtons("radio", h3("Choose type of filter"),
                                                 choices = list("Planning Area" = 'PA', 
                                                                "Subzone" = 'SZ'),selected = 'PA'),
                                    #switch view to PA
                                    conditionalPanel( condition = "input.radio=='PA'", #input.radio == 'PA'
                                                      # if you select planning area, then show this
                                                      selectizeInput(
                                                          'pa_from', 'From Planning Area', choices = sort(unique(busstops$planning_area)) ,
                                                          multiple = TRUE,
                                                          options = list(maxItems = 20,
                                                                         placeholder = 'Please select one or more options below',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                          )
                                                      ), #end of selectizeInput from Planning Area
                                                      
                                                      selectizeInput(
                                                          'pa_to', 'To Planning Area', choices = sort(unique(busstops$planning_area)) ,
                                                          multiple = TRUE,
                                                          options = list(maxItems = 20,
                                                                         placeholder = 'Please select one or more options below',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                          )
                                                      )#end of selectizeInput to Planning Area
                                    ), # end of conditionalPanel
                                    
                                    #switch view to SZ
                                    conditionalPanel( condition = "input.radio=='SZ'", 
                                                      # if you select subzone, then show this
                                                      selectizeInput(
                                                          'sz_from', 'From SubZone', choices = toupper(sort(unique(busstops$subzone_name))) ,
                                                          multiple = TRUE,
                                                          options = list(maxItems = 20,
                                                                         placeholder = 'Please select one or more options below',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                          )
                                                      ), #end of selectizeInput from Planning Area
                                                      
                                                      selectizeInput(
                                                          #'sz_to', 'To SubZone', choices = "initialisation"  , #initialise
                                                          'sz_to', 'To SubZone', choices = sort(unique(busstops$subzone_name)) ,
                                                          multiple = TRUE,
                                                          options = list(maxItems=20,
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

                        h5("For filtering of Planning Area OR Subzone:Please filter them at 'Aggregate Filter' tab"),
                        #plotlyOutput("map_jy"),
                        plotOutput("map_jy"),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(), hr(),hr(),hr(),
                        hr(),
                        
                        fluidRow(
                            column(4,  
                                   div(style="height: 10px;"),
                                   h4("Choose weight of edges (By Total Trips"), 
                                   conditionalPanel( condition = TRUE,
                                                     uiOutput("flowEdge_slider_ui_passenger")   ### slider for node weights
                                   )

                            ), # end of first column 3
                            column(4,  
                                   h4("Choose type of selection criteria"),
                                   h5("You can have multiple selection"),
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
                            column(4,
                                   h4("Choose Node weights"),
                                   uiOutput("flowsize_slider_ui"),   ### slider for node weights

                                   checkboxGroupInput("checkGroup", 
                                                      h4("Type of Day"), 
                                                      choices = list("Weekdays" = "WEEKDAY", 
                                                                     "Weekends/Holidays" = "WEEKENDS/HOLIDAY"),
                                                      selected = c("WEEKDAY","WEEKENDS/HOLIDAY")) # end of select input for day type
                            )
                        ) # end of fluidRow
                    ) # end of fluidPage
            ), #end of tabname "flowMap"  
            
            tabItem(tabName = 'flow_Bar_Graph',
                    fluidPage(
                        title = "Trip Generator and Receiver",
                        h3("Trip Generator and Receiver"),
                        h4("To aggregate by Planning Area or Subzones:"),
                        h5("Please filter at 'Aggregate Filter' Tab"),
                        h5("Nodes on the left are the source and nodes on the right are the destination"),
                        sankeyNetworkOutput("sankey_from")
                        
                    )
            ), #end of tabname "flow_Bar_Graph" 
            
            tabItem(tabName = 'Origin-Destination',
                    fluidPage(
                        titlePanel("Origin-Destination Matrix"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                h3("Origin Destination Matrix by Absolute numbers"),
                                # Copy the line below to make a select box 
                                selectInput("firstflows_ui", label = h4("First Flow Methods"), 
                                            choices = list("nfirst" = "nfirst", "xfirst" = "xfirst", "xsumfirst" = "xsumfirst"), 
                                            selected = "xfirst"),
                                numericInput("kvalue", label = h4("select min flow size for firstflows"), value = 1),
                                
                                # Copy the line below to make a select box 
                                numericInput("kvalue_dom", label = h4("select min flow size for dominance matrix (default k = 1)"), value = 1),
                                hr(),
                                hr(),
                                h3("Origin Destination Matrix by flows that represent at least % of the outputs"),
                                # Copy the line below to make a select box 
                                selectInput("firstflows2_ui", label = h4("First Flow Methods"), 
                                            choices = list("nfirst" = "nfirst", "xfirst" = "xfirst"), 
                                            selected = "xfirst"),
                                sliderInput("kvalue2", 
                                            label = h4("select min flow size for firstflows (in %)"),
                                            min = 0, max = 1, value = 0.05),
                                numericInput("kvalue2_dom", label = h4("select min flow size for dominance matrix (default k = 1)"), value = 1)
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Flow Select without Dominance",
                                             helpText("Origin Destination Matrix by Absolute numbers"),
                                             plotlyOutput("ori_dest"),
                                             helpText("Origin Destination Matrix by flows that represent at least % of the outputs"),
                                             plotlyOutput("ori_dest2")), # end of tabPanel "flowSel"
                                    tabPanel("Flow Select with Dominance",
                                             helpText("Origin Destination Matrix by Absolute numbers"),
                                             plotlyOutput("ori_dest_dom"),
                                             helpText("Origin Destination Matrix by flows that represent at least % of the outputs"),
                                             plotlyOutput("ori_dest2_dom")) # end of tabPanel "dominance"
                                    
                                )
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
                                
                                conditionalPanel(condition = "input.radio_my1=='SZ' or input.radio_my1=='PA'", 
                                                 selectizeInput(
                                                     'pa_my_1', 'Planning Area', choices = sort(unique(busstops_my$planning_area)),
                                                     selected = 'Ang Mo Kio'
                                                 )
                                ), 
                                conditionalPanel(condition = "input.radio_my1=='SZ'", 
                                                 selectizeInput(
                                                     'sz_filter_my1', 'SubZone', choices = sort(unique(busstops_my$subzone_name_my))
                                                 )
                                ), 
                                radioButtons("radio_my_taps", h3("Tap Ins / Tap Outs"),
                                             choices = list("Tap Ins" = "tap_ins", 
                                                            "Tap Outs" = 'tap_outs'), selected = 'tap_ins'),
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
                                            value = 8)
                                
                            ),
                            
                            mainPanel(
                                leafletOutput("map_my", height = 700),                                   
                                br(),
                                plotlyOutput("trendPlot_my",height = 400),                                 
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
                                                     'pa_my_2', 'Planning Area', choices = sort(unique(busstops_my$planning_area)),
                                                     selected = 'Ang Mo Kio'
                                                 )
                                ), 
                                
                                conditionalPanel(condition = "input.radio_my2=='SZ'", 
                                                 selectizeInput(
                                                     'sz_filter_my_2', 'SubZone', choices = sort(unique(busstops_my$subzone_name_my))
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
            
            tabItem(tabName = 'EDA_Model',
                    fluidPage(
                        titlePanel("EDA on Centrality Measures"),
                        sidebarLayout(
                            sidebarPanel(
                                h3("For Density Plots"),
                                radioButtons("densityPlotJY", label = h4("Choose Y or X density Plots"),
                                             choices = list("X" ="X" , "Y" = "Y"), 
                                             selected = "X"),
                             
                                conditionalPanel(
                                    condition = "input.densityPlotJY=='X'",
                                    radioButtons("radioDensityJY", label = h4("Standise axis or Original Axis for density plot"),
                                                 choices = list("Standard" ="Standard" , "Original" = "Original"), 
                                                 selected = "Original")#,
                                    # condition = "input.densityPlotJY == 'Y'",
                                    # helpText("No selection for limits")
                                ),
                                h3("For Correlation Matrix"),
                                selectInput("shape", label = h4("Select Shape"), 
                                            choices = list("square" ="square", "circle" = "circle"), 
                                            selected = "circle"),
                                selectInput("typeShape", label = h4("Select type"), 
                                            choices = list("full" ="full", "lower" = "lower", "upper" = "upper"), 
                                            selected = "full")
                               
                                
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Density Plots",plotOutput("Density_jy") ),
                                    tabPanel("Correlation Matrix",plotOutput("corrplot_jy")),
                                    tabPanel("Bivariate analysis", plotlyOutput("bivariate_jy"))
                                )
                                
                            )
                        )
                    )
            ), # end of tab 'EDA_model'
            
            tabItem(tabName = 'Gravity_Model',
                    fluidPage(
                        titlePanel("Gravity Model"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                uiOutput("yVarUI"),
                                selectizeInput(
                                    'selectXvar', 'X variables', choices = "initialisation"  , #initialise
                                    multiple = TRUE,
                                    options = list(maxItems=6,  # by the way you only have 4 centrality options
                                                   placeholder = 'Please select one or more options below',
                                                   onInitialize = I('function() { this.setValue(""); }')
                                    )
                                ),
                                
                                numericInput(inputId = "num", 
                                             label = ("confidence level"), 
                                             value = 0.95)
                                #submitButton("Apply changes")
                                
                            ),
                            
                            # https://dss.princeton.edu/online_help/analysis/interpreting_regression.htm
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Regression Model",
                                             h5("The polynomial regression model is calculated for each demographic variable and shown below:"),
                                             h4("X variables chosen"),
                                             verbatimTextOutput("seeX"),
                                             h4("Adjusted R Square for Regression Model"),
                                             verbatimTextOutput("pred1RSqAdj"),
                                             h5("If p value is < 1-(confidence level), we conclude that the relationship between the independent variables and medal average is statistically significant"),
                                             h4("P value of regression model"),
                                             verbatimTextOutput("pred1p"),
                                             verbatimTextOutput("conclude1"),
                                             #plottStats("finalCoef"),
                                             h4("After StepAIC 'both' directions"),
                                             withSpinner(plotlyOutput("AIC")),
                                             h5("t statistic is the coefficient divided by its standard error"),
                                             verbatimTextOutput("linearModel")),
                                    tabPanel("Model Assumptions",
                                             h4("In order for our regression model to be more accurate, our group has to check if the assumption of error terms having constant variance is satisfied."),
                                             withSpinner(plotOutput("resid")),
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
                                    ) # end of tabsetpanel
                                )
                            )
                        )
                    )
            ) #end of tabname "Gravity_Model" 
            

        )# end of TabItems
    ) #end of dashboardBody
)

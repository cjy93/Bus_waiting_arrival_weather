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
                                 h3("Choose type of selection criteria"),
                                 h4("You can have multiple selection"),
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
                                uiOutput("yVarUI"),
                                radioButtons("numOfVar", h3("Choose Number of Variables"),
                                             choices = list("2_var" = '2', 
                                                            "4_var" = '4'),selected = '2'),
                                #switch view to PA
                                conditionalPanel( condition = "input.numOfVar=='2'", #input.radio == 'PA'
                                                  # if you select planning area, then show this
                                                  selectInput("select21", label = h4("Select box 1"), 
                                                              choices = list("closeness" = "closeness", "closeness" = "closeness", "degree" = "degree", "between" = "between", "eigen"="eigen"), 
                                                              selected = "closeness"),
                                                  
                                                  selectInput("select22", label = h4("Select box 2"), 
                                                              choices = list("closeness" = "closeness", "closeness" = "closeness", "degree" = "degree", "between" = "between", "eigen"="eigen"), 
                                                              selected = "closeness")
                                ), # end of conditionalPanel
                                
                                #switch view to SZ
                                conditionalPanel( condition = "input.numOfVar=='4'", 
                                                  selectInput("select41", label = h4("Select box 1"), 
                                                              choices = list("closeness" = "closeness", "closeness" = "closeness", "degree" = "degree", "between" = "between", "eigen"="eigen"), 
                                                              selected = "closeness"),
                                                  
                                                  selectInput("select42", label = h4("Select box 2"), 
                                                              choices = list("closeness" = "closeness", "closeness" = "closeness", "degree" = "degree", "between" = "between", "eigen"="eigen"), 
                                                              selected = "closeness"),
                                                  selectInput("select43", label = h4("Select box 3"), 
                                                              choices = list("closeness" = "closeness", "closeness" = "closeness", "degree" = "degree", "between" = "between", "eigen"="eigen"), 
                                                              selected = "closeness"),
                                                  
                                                  selectInput("select44", label = h4("Select box 4"), 
                                                              choices = list("closeness" = "closeness", "closeness" = "closeness", "degree" = "degree", "between" = "between", "eigen"="eigen"), 
                                                              selected = "closeness")
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
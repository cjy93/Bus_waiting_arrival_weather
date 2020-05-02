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
                                 menuItem('Passenger Volume', tabName = 'Passenger_volume', icon = icon("connectdevelop")),
                                 menuItem('Bus Stop Centrality', tabName = 'Centrality', icon = icon("globe")),
                                 menuItem('GravityModel', tabName = 'Gravity_Model', icon = icon("chart-line")),
                                 menuItem('Download', tabName = 'Download', icon = icon("download"))# tabs are here!
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
                                   a("Wikipedia", href = "https://wiki.smu.edu.sg/1920t2isss608/Group08_proposal"),
                            )
                            
                        )
                    )
            ), #end of tabname "home" 
            
            tabItem(tabName = 'Flow_Diagrams',
                    fluidPage(
                        titlePanel("Flow Maps"),
                        
                        sidebarLayout(
                            sidebarPanel(width = 2,
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
                            
                            mainPanel(
                               # verbatimTextOutput("flow_jy"),
                            #    plotOutput("ploygon_jy")

                            )
                        )
                    )
            ), #end of tabname "flow"  
            
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
                                                     'sz_filter_my1', 'SubZone', choices = sort(unique(busstops$subzone_name_my))
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
                                            value = 8),
                                
                            ),
                            
                            mainPanel(
                                leafletOutput("map_my", height = 700),                                   
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
                                                     'sz_filter_my_2', 'SubZone', choices = sort(unique(busstops$subzone_name_my))
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

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
                                 menuItem('Busstop Centrality', tabName = 'Centrality', icon = icon("globe")),
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
                            
                            mainPanel(
                                verbatimTextOutput("flow_jy"),
                                plotOutput("ploygon_jy")
                                #plotOutput("flowDom_jy")
                                #leafletOutput("flowDom_jy"),
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
                                             choices = list("Planning Area" = 'PA', 
                                                            "Subzone" = 'SZ'), selected = 'PA'),
                                
                                conditionalPanel(condition = "input.radio_my1=='PA'", 
                                                  selectizeInput(
                                                      'pa_my_1', 'Planning Area', choices = sort(unique(busstops$planning_area)),
                                                      selected = 'Ang Mo Kio'
                                                  ) 
                                                 ), 
                                
                                conditionalPanel(condition = "input.radio_my1=='SZ'", 
                                                  selectizeInput(
                                                      'sz_my_1', 'SubZone', choices = sort(unique(busstops$subzone_name)),
                                                      selected = 'Admiralty'
                                                      )
                                                    ), 
                                selectInput(inputId = "week_my", 
                                            label = "Weekday / Weekend",
                                            choices = c('Weekday' = 'WEEKDAY',
                                                        'Weekend' = 'WEEKENDS/HOLIDAY'),
                                            selected = "WEEKDAY"),
                                radioButtons(inputId = 'time_my',
                                             label = 'Time of the Day',
                                             choices = c('06:00' = 6,'07:00' = 7,'08:00' = 8,'09:00' = 9,'10:00' = 10,'11:00' = 11,'12:00' = 12,'13:00' = 13,
                                                         '14:00' = 14,'15:00' = 15,'16:00' = 16,'17:00' = 17,'18:00' = 18,'19:00' = 19,'20:00' = 20, '21:00' = 21,
                                                         '22:00' = 22,'23:00' = 23),
                                             selected = 8),
                            ),
                            
                            mainPanel(
                                leafletOutput("map_my"),
                                DT::dataTableOutput("tbl_my")
                                # https://stackoverflow.com/questions/50128349/filtering-leaflet-map-data-in-shiny
                            )
                        )
                    )
            ), #end of tabname Passenger Volume by Busstop 
            
            ### Centrality ###
            tabItem(tabName = 'Centrality',
                    fluidPage(
                        titlePanel("Centrality"),

                        sidebarLayout(
                          sidebarPanel(
                              radioButtons("radio_my2", h3("Choose Region Filter"),
                                           choices = list("Planning Area" = 'PA', 
                                                          "Subzone" = 'SZ'), selected = 'PA'),
                              
                              conditionalPanel(condition = "input.radio_my2=='PA'", 
                                               selectizeInput(
                                                   'pa_my_2', 'Planning Area', choices = sort(unique(busstops$planning_area)),
                                                   selected = 'Ang Mo Kio'
                                               ) 
                              ), 
                              
                              conditionalPanel(condition = "input.radio_my2=='SZ'", 
                                               selectizeInput(
                                                   'sz_my_2', 'SubZone', choices = sort(unique(busstops$subzone_name)),
                                                   selected = 'Admiralty'
                                               )
                              ), 
                              
                              radioButtons("radio_my3", h3("Choose Centrality filter"),
                                           choices = list("Betweenness Centrality" = "between_my_filter",
                                                          "Closeness Centrality" = "closeness_my_filter",
                                                          "Degree Centrality" = "degree_my_filter",
                                                          "Eigenvalue Centrality" = "eigen_my_filter"), selected = "between_my_filter"),
                              
                              sliderInput(
                                  inputId = "centrality_filter", 
                                  label = "Centrality", 
                                  min = 0, 
                                  max = 1, 
                                  value = c(0, 1))
                              
                          ),
                          
                          mainPanel(
                            #put in viz here
                            leafletOutput("map_my_centrality"),
                            DT::dataTableOutput("tbl_my_2")
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

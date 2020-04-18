## Library packages 

# Used packages
pacotes = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","plotly","FNN",'flows','sp',
            'tidyverse','st','sf','maptools','networkD3','gganimate','leaflet','RColorBrewer',
            'dplyr','ggplot2','reshape2','tidyverse','plotly','igraph','ggraph','tidygraph',
            'visNetwork', 'lubridate', 'ggmap','visNetwork', 
            'ggiraph', 'sf', 'tmap',
            'flows','sp'
            # added in by mengyong
            ,'leaflet.extras', 'geosphere', 'RColorBrewer'
            
            )

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

##################################################### Import data here #########################################################

# busstop volume
busstop_volume <- read.csv("data/passenger volume by busstop.csv")
colnames(busstop_volume)[5] = "BusStopCode"

# busstop information
busstop_information <- read.csv("data/busstops_with_planning_area.csv")[3:8]
busstop_information$planning_area = str_to_title(busstop_information$planning_area)
busstop_information <- filter(busstop_information, planning_area != "Invalid")
busstop_information$planning_area <- as.character(busstop_information$planning_area)
busstop_information$planning_area[busstop_information$planning_area %in% c('Central Water Catchment', 'Mandai', 'Marina South', 'Museum', 'Newton', 'Orchard', 'Outram', 
                                                                           'Seletar', 'Rochor', 'Singapore River', 'Tanglin', 'Southern Islands', 'River Valley', 'Paya Lebar', 
                                                                           'Straits View', 'Tengah')] <- "Others"
#busstop_information <- filter(busstop_information, planning_area %in% c("Ang Mo Kio", 'Bishan')) #to reduce size of the problem, to delete later


## Origin Destination data
data<- read.csv("data/origin_subset_10000.csv")

##################################################### Jia Yi #########################################################

# statistics
flow <- data %>%
  select('DAY_TYPE','TIME_PER_HOUR','BusStopCode_x','BusStopCode_y','TOTAL_TRIPS') %>%
  unite(from_to, BusStopCode_x,BusStopCode_y, sep = "_", remove=FALSE) %>%
  group_by(from_to) %>% 
  summarise(Frequency = sum(TOTAL_TRIPS))%>%
  separate(from_to, c("from", "to"))
myflows <- prepflows(mat = flow, i = "from", j = "to", fij = "Frequency")
myflows[1:4,1:4]

## Get statistics about the matrix
statmat_jy <-  statmat(mat = myflows, output = "none", verbose = TRUE)
## Plot Lorenz curve only
lorenz_jy <- statmat(mat = myflows, output = "lorenz", verbose = FALSE)
## Graphics only
graphic_jy <- statmat(mat = myflows, output = "all", verbose = FALSE)
## Statistics only
mystats <- statmat(mat = myflows, output = "none", verbose = FALSE)
mystats_jy <- str(mystats)
## Sum of flows
sumflow_jy <- mystats$sumflows

# Plot Flows diagram
mpsz <- readShapeSpatial("data/geospatial/MP14_SUBZONE_WEB_PL.shp") # plot singapore shape
mpbus <- readShapeSpatial("data/BusStopLocation_Jan2020/BusStop.shp") # plot busstop

##################################################### Mengyong #########################################################

busstop_volume_lat_long_my <- dplyr::inner_join(busstop_volume, busstop_information, by ='BusStopCode')
location_my <- busstop_volume_lat_long_my %>%
  #dplyr::filter(DAY_TYPE == c('WEEKDAY'))%>%
  #dplyr::filter(TIME_PER_HOUR == 10)%>%
  dplyr::group_by(BusStopCode)%>%
  dplyr::arrange(desc(BusStopCode))%>%
  rename(c(lat = Latitude, lon = Longitude))

location_my$tap_in_out_radius <- (location_my$TOTAL_TAP_IN_VOLUME + location_my$TOTAL_TAP_OUT_VOLUME)**(1/2)/6
location_my <- location_my[c('planning_area', 'DAY_TYPE', 'TIME_PER_HOUR', 'BusStopCode', 'Description', 'RoadName', 'TOTAL_TAP_IN_VOLUME', 'TOTAL_TAP_OUT_VOLUME', 'lon', 'lat', 'tap_in_out_radius')]%>%
  rename(c(Day = DAY_TYPE, TapIns = TOTAL_TAP_IN_VOLUME, TapOuts = TOTAL_TAP_OUT_VOLUME, Time = TIME_PER_HOUR, PlanningArea = planning_area)) 

planning_area_list_my <-sort(unique(location_my$PlanningArea))
pal <- colorFactor(palette = 'Set3', domain = planning_area_list_my)
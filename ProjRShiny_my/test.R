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
busstop_information <- read.csv("data/busstops_with_planning_area.csv")[3:8]%>%
  filter(planning_area != "Invalid")
busstop_information$planning_area <- as.character(busstop_information$planning_area)
busstop_information$BusStopCode <- as.character(busstop_information$BusStopCode)
busstop_information$planning_area[busstop_information$planning_area %in% c('Central Water Catchment', 'Mandai', 'Marina South', 'Museum', 'Newton', 'Orchard', 'Outram', 
                                                                           'Seletar', 'Rochor', 'Singapore River', 'Tanglin', 'Southern Islands', 'River Valley', 'Paya Lebar', 
                                                                           'Straits View', 'Tengah')] <- "Others"

#bus route
busroute <- read_csv('data/bus_route_overall.csv')
busroute$BusStopCode <- as.character(busroute$BusStopCode)
busroute <- busroute[c('BusStopCode', 'Direction', 'Distance', 'ServiceNo', 'StopSequence')]
busroute <- busroute[busroute$BusStopCode %in% as.list(unique(busstop_information['BusStopCode']))[['BusStopCode']], ] 

## Origin Destination data
data<- head(read.csv("data/origin_subset_10000.csv"),1000)

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

##################################################### Mengyong Proportionate symbol map#########################################################

busstop_volume_lat_long_my <- dplyr::inner_join(busstop_volume, busstop_information, by ='BusStopCode')
location_my <- busstop_volume_lat_long_my %>%
  dplyr::group_by(BusStopCode)%>%
  dplyr::arrange(desc(BusStopCode))%>%
  rename(c(lat = Latitude, lon = Longitude))

location_my$tap_in_out_radius <- (location_my$TOTAL_TAP_IN_VOLUME + location_my$TOTAL_TAP_OUT_VOLUME)**(1/2)/6
location_my <- location_my[c('planning_area', 'DAY_TYPE', 'TIME_PER_HOUR', 'BusStopCode', 'Description', 'RoadName', 'TOTAL_TAP_IN_VOLUME', 'TOTAL_TAP_OUT_VOLUME', 'lon', 'lat', 'tap_in_out_radius')]%>%
  rename(c(Day = DAY_TYPE, TapIns = TOTAL_TAP_IN_VOLUME, TapOuts = TOTAL_TAP_OUT_VOLUME, Time = TIME_PER_HOUR, PlanningArea = planning_area)) 

planning_area_list_my <-sort(unique(location_my$PlanningArea))
pal <- colorFactor(palette = 'Set3', domain = planning_area_list_my)


##################################################### Mengyong Centrality#########################################################

busroute_2 <- busroute
busroute_2['StopSequence'] = busroute_2['StopSequence']-1
busroute_2['BusStopCode_dest'] = busroute_2['BusStopCode']
busroute_2 <- busroute_2[c('BusStopCode_dest', 'Direction', 'ServiceNo', 'StopSequence')]
busstops_from_to <- dplyr::inner_join(busroute, busroute_2, by =c('StopSequence', 'ServiceNo', 'Direction'))

#join the two tables together
busroute_busstop <- dplyr::inner_join(busstops_from_to, busstop_information, by ='BusStopCode')
keeps <- c('BusStopCode', 'BusStopCode_dest')
busroute_busstop <- busroute_busstop[, keeps, drop = FALSE] %>% 
  rename(from = BusStopCode) %>%
  rename(to = BusStopCode_dest)

#groupby
busroute_busstop_aggregated <- busroute_busstop %>%
  #group_by(from, to, planning_area) %>%
  group_by(from, to) %>%
  summarise(Weight = n()) %>%
  filter(from!=to) %>%
  filter(Weight > 1) %>%
  ungroup()
busroute_busstop_aggregated$from <- as.character(busroute_busstop_aggregated$from)
busroute_busstop_aggregated$to <- as.character(busroute_busstop_aggregated$to)

#nodes
nodes_my <- busstop_information %>%
  rename(id = BusStopCode)
nodes_my$id <- as.character(nodes_my$id)

#create graph structure
bus_graph <- tbl_graph(nodes = nodes_my, edges = busroute_busstop_aggregated, directed = TRUE)

#extract centrality
bus_graph=bus_graph%>%mutate(betweenness_centrality = centrality_betweenness(normalized = TRUE)) %>%mutate(closeness_centrality = centrality_closeness(normalized = TRUE)) %>%mutate(degree_centrality=centrality_degree(mode='out',normalized = TRUE))%>%mutate(eigen_centrality=centrality_eigen(weights=bus_graph$Weight,directed=TRUE))

#get edge table
plot_vector2<- as.data.frame(cbind(V(bus_graph)$Longitude,V(bus_graph)$Latitude))

edgelist <- get.edgelist(bus_graph)
edgelist[,1]<-as.numeric(match(edgelist[,1],V(bus_graph)))
edgelist[,2]<-as.numeric(match(edgelist[,2],V(bus_graph)))

node1=data.frame(plot_vector2[edgelist[,1],])
node2=data.frame(plot_vector2[edgelist[,2],])
node3=data.frame(cbind(node1,node2))

edge_table <- node3 %>%
  rename(c(long.f = V1, lat.f = V2, long.t = V1.1, lat.t = V2.1))%>%
  dplyr::left_join(busstop_information, by =c("long.f"= "Longitude", "lat.f" = "Latitude")) 

keeps <- c("long.f","lat.f","long.t","lat.t", "planning_area")
edge_table <- edge_table[ , (names(edge_table) %in% keeps)]

# get node table
plot_vector2<- as.data.frame(cbind(V(bus_graph)$Longitude,V(bus_graph)$Latitude,V(bus_graph)$betweenness_centrality,V(bus_graph)$closeness_centrality,
                                   V(bus_graph)$eigen_centrality,V(bus_graph)$degree_centrality))
map_table <- plot_vector2 %>%
  rename(c(long.f = V1, lat.f = V2, between.f = V3, closeness.f = V4, eigen.f = V5, degree.f = V6))%>%
  dplyr::left_join(busstop_information, by =c("long.f"= "Longitude", "lat.f" = "Latitude")) 

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

map_table$between.f <-range01(map_table$between.f)
map_table$closeness.f <-range01(map_table$closeness.f)
map_table$eigen.f <-range01(map_table$eigen.f)
map_table$degree.f <-range01(map_table$degree.f)

map_table$combined.f = log(map_table$between.f+1) + log(map_table$closeness.f+1) + log(map_table$eigen.f+1) + log(map_table$degree.f)

print('hello')
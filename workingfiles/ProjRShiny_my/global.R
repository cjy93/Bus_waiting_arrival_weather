## Library packages 


library(shiny)
library(dplyr)
library(tidyverse)
library(shinydashboard)
library(flows)
library(maptools)
library(st)
library(leaflet)
library(reshape2)
library(igraph)
library(ggraph)
library(tidygraph)
library(tmap)
library(flows)
library(sp)
library(RColorBrewer)
library(plotly)
library(ggthemes)

suppressWarnings(as.numeric(c("1", "2", "X")))
##################################################### Import data here #########################################################

# busstop volume
busstop_volume <- read.csv("data/passenger volume by busstop.csv")
colnames(busstop_volume)[5] = "BusStopCode"
busstop_volume$BusStopCode <- as.character(busstop_volume$BusStopCode)


# busstop information
busstops <- read.csv("data/busstop_lonlat_subzone_District.csv")%>%
  dplyr::filter(planning_area != "Invalid")
busstops$subzone_name_my <- busstops$subzone_name
busstops$BusStopCode <- as.integer(busstops$BusStopCode)
busstops$BusStopCode <- as.character(busstops$BusStopCode)
busstops$planning_area <- as.character(busstops$planning_area)
busstops$planning_area[busstops$planning_area %in% c('Central Water Catchment', 'Mandai', 'Marina South', 'Museum', 'Newton', 'Orchard', 'Outram', 
                                                                           'Seletar', 'Rochor', 'Singapore River', 'Tanglin', 'Southern Islands', 'River Valley', 'Paya Lebar', 
                                                                           'Straits View', 'Tengah')] <- "Others"

#bus route
busroute <- read_csv('data/bus_route_overall.csv')
busroute$BusStopCode <- as.integer(busroute$BusStopCode)
busroute$BusStopCode <- as.character(busroute$BusStopCode)
busroute <- busroute[c('BusStopCode', 'Direction', 'Distance', 'ServiceNo', 'StopSequence')]
busroute <- busroute[busroute$BusStopCode %in% as.list(unique(busstops['BusStopCode']))[['BusStopCode']], ] 

## Origin Destination data

##################################################### Mengyong Proportionate symbol map#########################################################

busstop_volume_lat_long_my <- dplyr::inner_join(busstop_volume, busstops, by ='BusStopCode')
location_my <- busstop_volume_lat_long_my %>%
  dplyr::group_by(BusStopCode)%>%
  dplyr::arrange(desc(BusStopCode))
location_my <- plyr::rename(location_my, c('Latitude'= 'lat' , 'Longitude'= 'lon' ))

#location_my$tap_in_out_radius <- (location_my$TOTAL_TAP_IN_VOLUME + location_my$TOTAL_TAP_OUT_VOLUME)**(1/2)/6
location_my <- location_my[c('planning_area', 'subzone_name_my', 'DAY_TYPE', 'TIME_PER_HOUR', 'BusStopCode', 'Description', 'RoadName', 'TOTAL_TAP_IN_VOLUME', 'TOTAL_TAP_OUT_VOLUME', 'lon', 'lat')]
location_my <- plyr::rename(location_my, c("DAY_TYPE" = "Day" , "TOTAL_TAP_IN_VOLUME"= "TapIns" , "TOTAL_TAP_OUT_VOLUME"= "TapOuts" ,"TIME_PER_HOUR"= "Time" , "planning_area"=  "PlanningArea" ))
location_my <- location_my %>% dplyr::filter(Time >=6 & Time <= 23)

planning_area_list_my <-sort(unique(location_my$PlanningArea))

#pal <- colorNumeric(palette = "RdPu", domain = location_my$TapIns*2)

##################################################### Mengyong Centrality#########################################################

busroute_2 <- busroute
busroute_2['StopSequence'] = busroute_2['StopSequence']-1
busroute_2['BusStopCode_dest'] = busroute_2['BusStopCode']
busroute_2 <- busroute_2[c('BusStopCode_dest', 'Direction', 'ServiceNo', 'StopSequence')]
busstops_from_to <- dplyr::inner_join(busroute, busroute_2, by =c('StopSequence', 'ServiceNo', 'Direction'))

#join the two tables together
busroute_busstop <- dplyr::inner_join(busstops_from_to, busstops, by ='BusStopCode')
keeps <- c('BusStopCode', 'BusStopCode_dest')
busroute_busstop <- busroute_busstop[, keeps, drop = FALSE]
  busroute_busstop <- plyr::rename(busroute_busstop, c("BusStopCode" = "from")) 
  busroute_busstop <- plyr::rename(busroute_busstop,c("BusStopCode_dest" = "to"))

#groupby
busroute_busstop_aggregated <- busroute_busstop %>%
  #group_by(from, to, planning_area) %>%
  dplyr::group_by(from, to) %>%
  dplyr::summarise(Weight = n()) %>%
  dplyr::filter(from!=to) %>%
  dplyr::filter(Weight > 0) %>%
  dplyr::ungroup()
busroute_busstop_aggregated$from <- as.character(busroute_busstop_aggregated$from)
busroute_busstop_aggregated$to <- as.character(busroute_busstop_aggregated$to)

#nodes
nodes_my <- busstops
nodes_my <- plyr::rename(nodes_my,c("BusStopCode" = "id" ))
nodes_my$id <- as.character(nodes_my$id)

#create graph structure
bus_graph <- tbl_graph(nodes = nodes_my, edges = busroute_busstop_aggregated, directed = TRUE)


#extract centrality
bus_graph=bus_graph%>%mutate(betweenness_centrality = centrality_betweenness(normalized = TRUE)) %>%mutate(closeness_centrality = centrality_closeness(normalized = TRUE)) %>%
  dplyr::mutate(degree_centrality=centrality_degree(mode='out',normalized = TRUE))
bus_graph = bus_graph %>% dplyr::mutate(eigen_centrality=centrality_eigen(weight = bus_graph$betweenness_centrality, directed = TRUE, scale = FALSE))


#get edge table
plot_vector2<- as.data.frame(cbind(V(bus_graph)$Longitude,V(bus_graph)$Latitude,V(bus_graph)$betweenness_centrality,V(bus_graph)$closeness_centrality,
                                   V(bus_graph)$eigen_centrality,V(bus_graph)$degree_centrality))

edgelist <- get.edgelist(bus_graph)
edgelist[,1]<-as.numeric(match(edgelist[,1],V(bus_graph)))
edgelist[,2]<-as.numeric(match(edgelist[,2],V(bus_graph)))

node1=data.frame(plot_vector2[edgelist[,1],])
node2=data.frame(plot_vector2[edgelist[,2],])
node3=data.frame(cbind(node1,node2))

edge_table <- 
  plyr::rename(node3,c("V1" = "long.f" , "V2" = "lat.f" , "V1.1" = "long.t" , "V2.1"= "lat.t" , "V3" = "between.f" , "V4"= "closeness.f","V5"= "eigen.f" , "V6" = "degree.f" ))
edge_table<- edge_table %>%  dplyr::left_join(busstops, by =c("long.f"= "Longitude", "lat.f" = "Latitude")) 

keeps <- c("long.f","lat.f","long.t","lat.t", "planning_area", 'subzone_name_my', "between.f", "closeness.f", "eigen.f","degree.f" )
edge_table <- edge_table[ , (names(edge_table) %in% keeps)]

#range01 <- function(x){(x-min(x))/(max(x)-min(x))}
range01 <- function(x) trunc(rank(x))/length(x)

edge_table$between.f <-range01(edge_table$between.f)
edge_table$closeness.f <-range01(edge_table$closeness.f)
edge_table$eigen.f <-range01(edge_table$eigen.f)
edge_table$degree.f <-range01(edge_table$degree.f)

# get node table
map_table <- 
  plyr::rename(plot_vector2,c("V1"="long.f" , "V2"="lat.f" , "V3"="between.f" , "V4"="closeness.f" , "V5"="eigen.f" ,"V6"= "degree.f" ))
map_table <- map_table %>%  dplyr::left_join(busstops, by =c("long.f"= "Longitude", "lat.f" = "Latitude")) 

map_table$between.f <-round(range01(map_table$between.f),3)
map_table$closeness.f <-round(range01(map_table$closeness.f),3)
map_table$eigen.f <-round(range01(map_table$eigen.f),3)
map_table$degree.f <-round(range01(map_table$degree.f),3)

#write.csv(map_table,"Path where you'd like to export the DataFrame\\File Name.csv", row.names = FALSE)

#get the radius of the bubbles
map_table$combined.f = (map_table$between.f*3+1)**(3/4) + (map_table$closeness.f*3+1)**(3/4) + (map_table$eigen.f*3+1)**(3/4) + (map_table$degree.f*3+1)**(3/4)
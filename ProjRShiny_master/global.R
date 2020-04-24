## Library packages 


library(shiny)
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
library(dplyr)
library(shinycssloaders)
library(FNN)
library(ggplot2)
library(reshape2)
library(lubridate)
library(sf)
library(heatmaply)
library(MASS)
library(ERSA)
library(car)
library(rgdal)     # R wrapper around GDAL/OGR
#library(ggmap)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


##################################################### Import data here #########################################################

# Busstop Volume (Mengyong)
busstop_volume <- read.csv("data/passenger volume by busstop.csv")%>% 
  rename(BusStopCode = PT_CODE)
busstop_volume$BusStopCode <- as.character(busstop_volume$BusStopCode)

# Passenger Volume by Busstop (Jiayi)
passVol <- busstop_volume %>% 
  group_by(BusStopCode) %>% summarise(frequencyIn = sum(TOTAL_TAP_IN_VOLUME),frequencyOut = sum(TOTAL_TAP_OUT_VOLUME))

# Busstop
busstops <- read.csv("data/busstop_lonlat_subzone_District.csv")%>%
  mutate(subzone_name = toupper(subzone_name))%>% #capitalise the column in subzone
  dplyr::filter(planning_area != "Invalid")
busstops$BusStopCode <- as.integer(busstops$BusStopCode)
busstops$BusStopCode <- as.character(busstops$BusStopCode)

# Bus Route
busroute <- read_csv('data/bus_route_overall.csv')
busroute$BusStopCode <- as.integer(busroute$BusStopCode)
busroute$BusStopCode <- as.character(busroute$BusStopCode)
busroute <- busroute[c('BusStopCode', 'Direction', 'Distance', 'ServiceNo', 'StopSequence')]
busroute <- busroute[busroute$BusStopCode %in% as.list(unique(busstops['BusStopCode']))[['BusStopCode']], ] 

# Centrality dataset (Removed, using from dataset Mengyong created, see code chunk at end of global.R)
#central <- read_csv("data/centralityTable.csv")
#central$BusStopCode <- as.character(central$BusStopCode)
#pass_central <- inner_join(passVol, central, by = "BusStopCode") %>%
#  rename(closeness=closeness.f, between=between.f, eigen=eigen.f, degree=degree.f) 

## Analysing by Subzones or PA
SZ <- read_csv("data/subzoneData.csv") %>%
  rename(subzone_name = SUBZONE_N) 
PA <- read_csv("data/PAData.csv") %>%
  rename(planning_area = PLN_AREA_N)

#####################################################  Apps Jia Yi #########################################################
############################### Flow Map #####################################
## Create node
#busstops <- busstops %>% dplyr::select(c('planning_area','subzone_name','BusStopCode','district'))

# ShapeFile for SZ
# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
shapefile_SZ <- readOGR("data/geospatial", "MP14_SUBZONE_WEB_PL")

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df_SZ <- fortify(shapefile_SZ)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.

## This is to draw the shape file of Singapore outline
map_gg2_SZ <- geom_polygon(data = shapefile_df_SZ, 
                           aes(x = long, y = lat, group = group),
                           color = 'gray', fill = 'gray', size = .2) 
map_gg3_SZ <- geom_path(data = shapefile_df_SZ, 
                        aes(x = long, y = lat, group = group),
                        color = 'red', fill = 'red', size = .2)
map_gg4_SZ <- ggplot() + map_gg2_SZ + map_gg3_SZ +geom_point() +
  annotate("point", x = 31596, y = 29220, colour = "blue")


##################################################### Mengyong Proportionate symbol map#########################################################

busstop_volume_lat_long_my <- dplyr::inner_join(busstop_volume, busstops, by ='BusStopCode')
location_my <- busstop_volume_lat_long_my %>%
  dplyr::group_by(BusStopCode)%>%
  dplyr::arrange(desc(BusStopCode))%>%
  rename(c(lat = Latitude, lon = Longitude))

location_my$tap_in_out_radius <- (location_my$TOTAL_TAP_IN_VOLUME + location_my$TOTAL_TAP_OUT_VOLUME)**(1/2)/6
location_my <- location_my[c('planning_area', 'subzone_name', 'DAY_TYPE', 'TIME_PER_HOUR', 'BusStopCode', 'Description', 'RoadName', 'TOTAL_TAP_IN_VOLUME', 'TOTAL_TAP_OUT_VOLUME', 'lon', 'lat', 'tap_in_out_radius')]%>%
  rename(c(Day = DAY_TYPE, TapIns = TOTAL_TAP_IN_VOLUME, TapOuts = TOTAL_TAP_OUT_VOLUME, Time = TIME_PER_HOUR, PlanningArea = planning_area)) %>%
  dplyr::filter(Time >=6 & Time <= 23)

planning_area_list_my <-sort(unique(location_my$PlanningArea))

pal <- colorNumeric(palette = "RdPu", domain = location_my$tap_in_out_radius)

##################################################### Mengyong Centrality #########################################################

busroute_2 <- busroute
busroute_2['StopSequence'] = busroute_2['StopSequence']-1
busroute_2['BusStopCode_dest'] = busroute_2['BusStopCode']
busroute_2 <- busroute_2[c('BusStopCode_dest', 'Direction', 'ServiceNo', 'StopSequence')]
busstops_from_to <- dplyr::inner_join(busroute, busroute_2, by =c('StopSequence', 'ServiceNo', 'Direction'))

#join the two tables together
busroute_busstop <- dplyr::inner_join(busstops_from_to, busstops, by ='BusStopCode')
keeps <- c('BusStopCode', 'BusStopCode_dest')
busroute_busstop <- busroute_busstop[, keeps, drop = FALSE] %>% 
  rename(from = BusStopCode) %>%
  rename(to = BusStopCode_dest)

#groupby
busroute_busstop_aggregated <- busroute_busstop %>%
  #group_by(from, to, planning_area) %>%
  group_by(from, to) %>%
  summarise(Weight = n()) %>%
  dplyr::filter(from!=to) %>%
  dplyr::filter(Weight > 0) %>%
  ungroup()
busroute_busstop_aggregated$from <- as.character(busroute_busstop_aggregated$from)
busroute_busstop_aggregated$to <- as.character(busroute_busstop_aggregated$to)

#nodes
nodes_my <- busstops %>%
  rename(id = BusStopCode)
nodes_my$id <- as.character(nodes_my$id)

#create graph structure
bus_graph <- tbl_graph(nodes = nodes_my, edges = busroute_busstop_aggregated, directed = TRUE)


#extract centrality
bus_graph=bus_graph%>%mutate(betweenness_centrality = centrality_betweenness(normalized = TRUE)) %>%mutate(closeness_centrality = centrality_closeness(normalized = TRUE)) %>%
  mutate(degree_centrality=centrality_degree(mode='out',normalized = TRUE))
bus_graph = bus_graph %>% mutate(eigen_centrality=centrality_eigen(weight = bus_graph$betweenness_centrality, directed = TRUE, scale = FALSE))


#get edge table
plot_vector2<- as.data.frame(cbind(V(bus_graph)$Longitude,V(bus_graph)$Latitude,V(bus_graph)$betweenness_centrality,V(bus_graph)$closeness_centrality,
                                   V(bus_graph)$eigen_centrality,V(bus_graph)$degree_centrality))

edgelist <- get.edgelist(bus_graph)
edgelist[,1]<-as.numeric(match(edgelist[,1],V(bus_graph)))
edgelist[,2]<-as.numeric(match(edgelist[,2],V(bus_graph)))

node1=data.frame(plot_vector2[edgelist[,1],])
node2=data.frame(plot_vector2[edgelist[,2],])
node3=data.frame(cbind(node1,node2))

edge_table <- node3 %>%
  rename(c(long.f = V1, lat.f = V2, long.t = V1.1, lat.t = V2.1, between.f = V3, closeness.f = V4, eigen.f = V5, degree.f = V6))%>%
  dplyr::left_join(busstops, by =c("long.f"= "Longitude", "lat.f" = "Latitude")) 

keeps <- c("long.f","lat.f","long.t","lat.t", "planning_area", 'subzone_name', "between.f", "closeness.f", "eigen.f","degree.f" )
edge_table <- edge_table[ , (names(edge_table) %in% keeps)]

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

edge_table$between.f <-range01(edge_table$between.f)
edge_table$closeness.f <-range01(edge_table$closeness.f)
edge_table$eigen.f <-round(range01(log(edge_table$eigen.f+1)**0.15),3)
edge_table$degree.f <-range01(edge_table$degree.f)

# get node table
map_table <- plot_vector2 %>%
  rename(c(long.f = V1, lat.f = V2, between.f = V3, closeness.f = V4, eigen.f = V5, degree.f = V6))%>%
  dplyr::left_join(busstops, by =c("long.f"= "Longitude", "lat.f" = "Latitude")) 

map_table$between.f <-round(range01(map_table$between.f),3)
map_table$closeness.f <-round(range01(map_table$closeness.f),3)
map_table$eigen.f <-round(range01(log(map_table$eigen.f+1)**0.15),3)
map_table$degree.f <-round(range01(map_table$degree.f),3)

#get the radius of the bubbles
map_table$combined.f = (map_table$between.f*3+1)**(3/4) + (map_table$closeness.f*3+1)**(3/4) + (map_table$eigen.f*3+1)**(3/4) + (map_table$degree.f*3+1)**(3/4)

##################################################### Jiayi Part 2 - Centrality #########################################################

### Jiayi's centrality dataset
central <- map_table
central$BusStopCode <- as.character(central$BusStopCode)
pass_central <- inner_join(passVol, central, by = "BusStopCode") %>%
  rename(closeness=closeness.f, between=between.f, eigen=eigen.f, degree=degree.f) 
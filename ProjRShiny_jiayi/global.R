## Library packages 

# Used packages
pacotes = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","plotly","FNN",'flows','sp',
            'tidyverse','st','sf','maptools','networkD3','gganimate','leaflet','RColorBrewer',
            'dplyr','ggplot2','reshape2','tidyverse','plotly','igraph','ggraph','tidygraph',
            'visNetwork', 'lubridate', 'ggmap','visNetwork', 
            'ggiraph', 'sf', 'tmap','rgdal',
            'flows','sp')

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})


# BusStops
busstops <- read_csv('data/busstop_lonlat_subzone_District.csv') %>%
  mutate(subzone_name = toupper(subzone_name)) #capitalise the column in subzone
busstops$BusStopCode <- as.character(busstops$BusStopCode) 

#filter(planning_area == 'QUEENSTOWN')
# busstops <- busstops[busstops$BusStopCode %in% as.list(unique(busroute['BusStopCode']))[['BusStopCode']], ] 

busstops


# BusRoute.csv
busroute <- read_csv('data/bus_route_overall.csv')
busroute$BusStopCode <- as.character(busroute$BusStopCode)
busroute <- busroute[c('BusStopCode', 'Direction', 'Distance', 'ServiceNo', 'StopSequence')] 
#%>%dplyr::filter(ServiceNo %in% top_bus)
busroute <- busroute[busroute$BusStopCode %in% as.list(unique(busstops['BusStopCode']))[['BusStopCode']], ] 

busroute

# create the busstop sequence for later
busroute_2 <- busroute
busroute_2['StopSequence'] = busroute_2['StopSequence']-1
busroute_2['BusStopCode_dest'] = busroute_2['BusStopCode']
busroute_2 <- busroute_2[c('BusStopCode_dest', 'Direction', 'ServiceNo', 'StopSequence')]

busstops_from_to <- dplyr::inner_join(busroute, busroute_2, by =c('StopSequence', 'ServiceNo', 'Direction'))
busstops_from_to


#join the two tables together
# to from to the id

busroute_busstop <- dplyr::inner_join(busstops_from_to, busstops, by ='BusStopCode')
keeps <- c('BusStopCode', 'BusStopCode_dest', 'Direction', 'Distance', 'ServiceNo', 'Latitude', 'Longitude', 'planning_area', 'subzone_name','district')
busroute_busstop <- busroute_busstop[, keeps, drop = FALSE] %>% 
  rename(from = BusStopCode) %>%
  rename(to = BusStopCode_dest)

head(busroute_busstop)

# weight weight of busstops to busstops
busroute_busstop_aggregated <- busroute_busstop %>%
  group_by(from, to) %>%  # , planning_area
  summarise(Weight = n()) %>%
  filter(from!=to) %>%
  filter(Weight > 1) %>%
  ungroup()

busroute_busstop_aggregated$from <- as.character(busroute_busstop_aggregated$from)
busroute_busstop_aggregated$to <- as.character(busroute_busstop_aggregated$to)
busroute_busstop_aggregated

# rename ID and make character
total <- busstops %>%
  rename(id = BusStopCode)
total$id <- as.character(total$id)




## Origin Destination data
data<- read.csv("data/origin_subset_10000.csv")  ## Jia Yi-- The original one is too big for debugging, use this temporarily
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

###** JiaYi Flow Diagram Starts here
## Analysing by Subzones
SZ <- read_csv("data/subzoneData.csv") %>%
  rename(subzone_name = SUBZONE_N)

## Create node
busstops <- read_csv('data/busstop_lonlat_subzone_district.csv') %>%
  mutate(subzone_name = toupper(subzone_name)) #capitalise the column in subzone
busstops <- busstops %>% select(c('subzone_name','BusStopCode','district'))

# Now inner join the 2 tables so i can find the weights
node <- dplyr::inner_join(busstops, SZ, by =c('subzone_name')) %>%
  select(c('OBJECTID','district','subzone_name','X_ADDR','Y_ADDR'))%>%
  group_by(OBJECTID,subzone_name,district,X_ADDR,Y_ADDR) %>%  
  summarise(weight = n()) %>%
  # filter(weight > 1) %>%
  ungroup()  %>%
  rename(id = OBJECTID) %>%
  rename(name = subzone_name)%>%
  rename(x = X_ADDR) %>%
  rename(y = Y_ADDR)

# Create Edges
## For weights for subzone we change to TOTAL_TRIPS and DAY_TYPE as column
edges <- read_csv("data/origin_subset_10000.csv") %>%
  select(-c('YEAR_MONTH','PT_TYPE','ORIGIN_PT_CODE','DESTINATION_PT_CODE','RoadName_Origin','Description_Origin','RoadName_Destination','Description_Destination'))
# we need to append "subzone_origin" and "subzone_dest" so we can calculate weights based on busstops in these subzones
edges_join <- merge(edges, busstops, by.x='BusStopCode_x', by.y = 'BusStopCode') %>% 
  rename(subzone_ori = subzone_name)
edges_join2 <- merge(edges_join, busstops, by.x='BusStopCode_y', by.y = 'BusStopCode') %>% 
  rename(subzone_dest = subzone_name) 
# remove the intermediate dataframe from cache
remove(edges_join) 
remove(edges)
edges <- edges_join2 %>%
  #select('subzone_ori','subzone_dest') %>%
  rename(from =subzone_ori) %>%
  rename(to = subzone_dest) %>%
  # Group by "from","to" and add the "DAY_TYPE" so we can choose in RShiny
  group_by(from,to,DAY_TYPE) %>%
  arrange(X1) %>%
  # make weights to be according to "TOTAL_TRIPS"
  summarise(weight = sum(TOTAL_TRIPS))%>%
  # filter(weight>1) %>%
  ungroup() 

# add a column for category so it fit into the online model
edges <- cbind(edges,category=1)



# change the from and to in "edges" to match with the "node" id

get_index = node %>% select(c('id','name'))

edge_id <- merge(edges, get_index, by.x = "from", by.y = "name") %>%
  select(-c("from")) %>%
  rename(from=id) 
edge_id2 <- merge(edge_id, get_index, by.x = "to", by.y = "name") %>%
  select(-c("to")) %>%
  rename(to=id)

## rearrange dataframe for edge_id such that "from" and "to" is in first 2 columns
edge_id2$to <- as.character(edge_id2$to)
edge_id2$from <- as.character(edge_id2$from)
edge_id2 <- edge_id2 %>%
  select(c('from','to','DAY_TYPE','weight','category'))

# change node id also to as.character
node$id <- as.character(node$id)

# ShapeFile for SZ
# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
shapefile <- readOGR("data/geospatial", "MP14_SUBZONE_WEB_PL")

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(shapefile)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.

## This is to draw the shape file of Singapore outline
map_gg2 <- geom_polygon(data = shapefile_df, 
                        aes(x = long, y = lat, group = group),
                        color = 'gray', fill = 'gray', size = .2) 
map_gg3 <- geom_path(data = shapefile_df, 
                     aes(x = long, y = lat, group = group),
                     color = 'red', fill = 'red', size = .2)
map_gg4 <- ggplot() + map_gg2+ map_gg3 +geom_point() +
  annotate("point", x = 31596, y = 29220, colour = "blue")




# Create Graph object starts here
g <- graph_from_data_frame(edge_id2, directed = TRUE, vertices = node)
edges_for_plot <- edge_id2 %>%
  inner_join(node %>% select(id, x, y), by = c('from' = 'id')) %>%
  inner_join(node %>% select(id, x, y), by = c('to' = 'id')) 


node$weight = degree(g)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

node_pos <- node %>%
  select(x, y)  # node positions must be called x, y
lay <- create_layout(g, 'manual',
                     node.positions = node_pos)

lay <- lay[ -c(1,2) ]
# add node degree for scaling the node sizes
lay$weight <- degree(g)
# We pass the layout lay and use ggraph's geoms geom_edge_arc and geom_node_point for plotting:

# convert all columns to numeric
# https://stackoverflow.com/questions/19146354/batch-convert-columns-to-numeric-type

edges_for_plot4 <- edges_for_plot %>%
  select(c('from','to','DAY_TYPE','weight','category','x.x','y.x','x.y','y.y')) %>%
  rename(x = x.x) %>%
  rename(y = y.x) %>%
  rename(xend = x.y) %>%
  rename(yend = y.y)

# change columns from numerical to numeric then back to character (due to error msgs)
edges_for_plot4$x    <- as.numeric(as.character(edges_for_plot4$x))
edges_for_plot4$y    <- as.numeric(as.character(edges_for_plot4$y))
edges_for_plot4$xend <- as.numeric(as.character(edges_for_plot4$xend))
edges_for_plot4$yend <- as.numeric(as.character(edges_for_plot4$yend))

lay$x <-  as.numeric(as.character(lay$x))
lay$y <-  as.numeric(as.character(lay$y))

edge_id2$to <- as.character(edge_id2$to)
edge_id2$from <- as.character(edge_id2$from)
edge_id2 <- edge_id2 %>%
  select(c('from','to','DAY_TYPE','weight','category'))

# create bins for colours
edges_for_plot4 <- edges_for_plot4 %>%
  mutate(
    bin = cut(weight, seq(0,max(edges_for_plot4$weight),2), na.rm=TRUE)) %>%
  ungroup()

#******* VizNetwork*****************
# VisNetwork Interactive Graphs
#To make my interactive graph have a dropdown menu, they always make us select by id, in my case of `node$id`, it does not show any relevant information. So we have to make tne `node$name` the column of id
# Edit Node data to make id = name
node_inter <- node %>% 
  select(-c("id")) %>%
  rename(id = name)

# make the `from` and `to` of edges link to the newly created `id` in `node_inter`
edges_inter <- edges_join2 %>%
  #select('subzone_ori','subzone_dest') %>%
  rename(from =subzone_ori) %>%
  rename(to = subzone_dest) %>%
  # Group by "from","to" and add the "DAY_TYPE" so we can choose in RShiny
  group_by(from,to,DAY_TYPE) %>%
  arrange(X1) %>%
  # make weights to be according to "TOTAL_TRIPS"
  summarise(weight = sum(TOTAL_TRIPS))%>%
  filter(weight>1) %>%
  ungroup() %>% 
  drop_na() 


#Make sure "weights" is called "value" to invoke weights on flow edges
edges_inter <- edges_inter %>%
  rename(value = weight)


#Make sure 'weights' is called 'value' to invoke wights on nodes
node_inter <- node_inter %>%
  rename(value = weight) %>%
  rename(group = district)






#################################################### Meng Yong ########################################################











###################################################### Yong Shan ##########################################################
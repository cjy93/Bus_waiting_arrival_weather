## Library packages 

# Used packages
pacotes = c("shiny", "shinydashboard",  "plotly", "shinycssloaders","tidyverse",
            "scales", "kableExtra", "ggfortify","dplyr","plotly","FNN",'flows','sp',
            'tidyverse','st','sf','maptools','networkD3','gganimate','leaflet','RColorBrewer',
            'dplyr','ggplot2','reshape2','tidyverse','plotly','igraph','ggraph','tidygraph',
             'lubridate', 'ggmap','visNetwork', 
            'ggiraph', 'sf', 'tmap','rgdal',
            'flows','shinycssloaders','heatmaply',
            'MASS','ERSA','car')
            # 
#pkg removed cos cannot load. "shinythemes"
# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

library("shiny")
library("shinydashboard")
library("plotly")
library("shinycssloaders")
library("tidyverse")
library("dplyr")
library("plotly")
library("FNN")
library('flows')
library('sp')
library('dplyr')
library('ggplot2')
library('reshape2')
library('tidyverse')
library('igraph')
library('ggraph')
library('tidygraph')
library('lubridate')
library('sf')
library('flows')
library('shinycssloaders')
library('heatmaply')
library('MASS')
library('ERSA')
library('car')



#Load lib
# https://gist.github.com/RobertMyles/96665be044540e9ac668ee697dac6db7
# lapply(pacotes, require, character.only = TRUE)


########################### Data Prep JY #############################
# passenger volume per busstops
passVol <- read_csv("data/passenger volume by busstop.csv") 
passVol <- passVol %>% rename(BusStopCode = PT_CODE) %>%
  group_by(BusStopCode) %>% summarise(frequencyIn = sum(TOTAL_TAP_IN_VOLUME),frequencyOut = sum(TOTAL_TAP_OUT_VOLUME))
# centrality dataset
central <- read_csv("data/centralityTable.csv")
central$BusStopCode <- as.character(central$BusStopCode)
pass_central <- inner_join(passVol, central, by = "BusStopCode") %>%
  rename(closeness=closeness.f, between=between.f,eigen=eigen.f,degree=degree.f) 

  


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



## Jia Yi-- The original one is too big for debugging, use this temporarily
## Origin Destination data
# Bus flow
#data<- read.csv("data/origin_dest_cleaned_jy_subset.csv") 

# Plot Flows diagram
# mpsz <- readShapeSpatial("data/geospatial/MP14_SUBZONE_WEB_PL.shp") # plot singapore shape
# mpbus <- readShapeSpatial("data/BusStopLocation_Jan2020/BusStop.shp") # plot busstop

## Analysing by Subzones or PA
SZ <- read_csv("data/subzoneData.csv") %>%
  rename(subzone_name = SUBZONE_N) 
PA <- read_csv("data/PAData.csv") %>%
  rename(planning_area = PLN_AREA_N)


#####################################################  Apps Jia Yi #########################################################
############################### Flow Map #####################################
## Create node
busstops <- busstops %>% dplyr::select(c('planning_area','subzone_name','BusStopCode','district'))



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

# prepared the file outside and call them in for app to run faster
########### this is still the subset file ##########################











#################################################### Meng Yong ########################################################











###################################################### Yong Shan ##########################################################
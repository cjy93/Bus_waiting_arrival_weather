## Library packages 

# Used packages
pacotes = c("shiny", "shinydashboard", "shinythemes", "plotly", "shinycssloaders","tidyverse",
            "scales", "knitr", "kableExtra", "ggfortify","dplyr","plotly","FNN",'flows','sp',
            'tidyverse','st','sf','maptools','networkD3','gganimate','leaflet','RColorBrewer',
            'dplyr','ggplot2','reshape2','tidyverse','plotly','igraph','ggraph','tidygraph',
            'visNetwork', 'lubridate', 'ggmap','visNetwork', 
            'ggiraph', 'sf', 'tmap',
            'flows','sp')

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

# BusRoute.csv
top_bus <- c('51','61','961','66','67','14','30','143','10','147','178','197','154','167','166','174','196','171','2','12','21','170','970','13')

busroute <- read_csv('data/bus_route_overall.csv')
busroute$BusStopCode <- as.character(busroute$BusStopCode)
busroute <- busroute[c('BusStopCode', 'Direction', 'Distance', 'ServiceNo', 'StopSequence')] %>%
  dplyr::filter(ServiceNo %in% top_bus)

busNode <- read_csv("data/busstops.csv")


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

#################################################### Meng Yong ########################################################











###################################################### Yong Shan ##########################################################
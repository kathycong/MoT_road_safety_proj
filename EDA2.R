#' ## EDA 2
#' https://saferactive.github.io/trafficalmr/articles/report1.html
#' 
#' According to saferactive the KSI/bkm for cycle casualties has been obtain by the ff:
#' 
#' - estimated the number of kilometers cycled in each London Borough (local authority district or area such as Auckland central/Northcote/Papatoetoe/Albany ?) . 
#' - Used census data for travel to work and cyclestreets.net 'fast' routing option, ability to create a route network
#' - Each road segment within the route network is assigned to a London Borough based on its centroid location. Multiplying the number of cyclist using each segment by the length of the segment this gives the number of km cycled within each Borough
#' 
#' - Since the km cycled used only covers one-way commuter journey on a single day in 2011. The project decided to filter the data to only include accidents taking place during the rush hour peak periods (7:30-9:30 and 16:30-18:30) on weekdays.
#' - Then they divided the number of casualties by two (to reprent one-way jourbeys), then by five (to represent a single year) and again by 261 (to represent a single day)
#' 
## -----------------------------------------------------------------------------
setwd("/shared/projects/nzt/MoT_road_safety_proj")

## -----------------------------------------------------------------------------
##loading libraries

library(sf)
library(dplyr)
#library(spDataLarge)
library(stplanr)      # geographic transport data package
library(tmap)



## -----------------------------------------------------------------------------
##load journey to work data
jtw <-  read.csv("Data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv", fileEncoding="UTF-8-BOM")

## loading clipped SA2 shape file
sa2.clipped <- read_sf("Data/statistical-area-2-2021-clipped-generalised.shp")

##converting this to int used for joining df and shape file
sa2.clipped$SA22021_V1 <- as.integer(sa2.clipped$SA22021_V1)


## -----------------------------------------------------------------------------
#Usual Residence/Origin data

jtw_origin = jtw %>% 
  group_by(SA2_code_usual_residence_address) %>% 
  select(SA2_code_usual_residence_address, Total) %>% 
  summarise(Total =sum(Total))


## -----------------------------------------------------------------------------
#Plotting usual residence against density using total column
o <-  left_join(sa2.clipped, jtw_origin, by = c("SA22021_V1" = "SA2_code_usual_residence_address" ))

#plotting new zealand
qtm(o, c("Total")) + tm_layout(panel.labels = c("New Zealand Usual Residence (2018)"))

#plotting Auckland City
qtm(o, c("Total"), bbox = "Auckland New Zealand") + tm_layout(panel.labels = c("Auckland Region Usual Residence (2018)"))


## -----------------------------------------------------------------------------
#Usual Work/Destination data

jtw_dest = jtw %>% 
  group_by(SA2_code_workplace_address) %>% 
  select(SA2_code_workplace_address, Total) %>% 
  summarise(Total =sum(Total))


## -----------------------------------------------------------------------------

d <-  left_join(sa2.clipped, jtw_dest, by = c("SA22021_V1" = "SA2_code_workplace_address" ))


#plotting new zealand
qtm(d, c("Total")) + tm_layout(panel.labels = c("New Zealand Workplace (2018)"))

#plotting Auckland City
qtm(d, c("Total"), bbox = "Auckland New Zealand") + tm_layout(panel.labels = c("Auckland Region Work place (2018)"))


## -----------------------------------------------------------------------------
##loading crash data
if (!file.exists("Data/Crash_Analysis_System_(CAS)_data.rds")) {
  cas.data <- iotools::read.csv.raw("Data/Crash_Analysis_System_(CAS)_data.csv")
  saveRDS(cas.data, file="Data/Crash_Analysis_System_(CAS)_data.rds")
} else cas.data <- readRDS("Data/Crash_Analysis_System_(CAS)_data.rds")

colnames(cas.data)[1] <- "X"

## -----------------------------------------------------------------------------

# function that converts geospatial data 
nzp = function(x, inverse=FALSE) loc = proj4::project(x, "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", inverse=inverse)

#https://cengel.github.io/R-spatial/intro.html
#head(cas.data)


## -----------------------------------------------------------------------------

#missing data after conversion
(missing_data = sum(is.na(cas.data[1])))
missing_data/nrow(cas.data) #percentage

## removing NAs
## 10% of the data is missing
cas.data2 <- cas.data[!is.na(cas.data$X), ]



## -----------------------------------------------------------------------------
#Noting that the spatial reference for the crash data is 2193
head(cas.data2[1:2])

## -----------------------------------------------------------------------------
## According to metadata in stats nz, spatial reference is 2171
str(sa2.clipped)

## -----------------------------------------------------------------------------
##converting dataframe into an sf object with st_as_sf()
cas.data_sf <- st_as_sf(cas.data2, coords = c("X", "Y"))


## -----------------------------------------------------------------------------
#checking projection for crash data
st_crs(cas.data_sf) 

#checking projection for Statistical area 2 clipped
st_crs(sa2.clipped)

#As noted by simon, it is NZGD2000 but that's not always supported so avoid mismatch

#matching the crs for both dataset
st_crs(cas.data_sf) = st_crs(sa2.clipped) 

## -----------------------------------------------------------------------------
## Points in Polygon
#test <- cas.data_sf %>%
#  st_join(sa2.clipped)

#getting the intersects
i <- st_intersects(cas.data_sf, sa2.clipped)
i


## -----------------------------------------------------------------------------
i_table = table(unlist(i))

cas_sa2 = sa2.clipped
cas_sa2$cas = c(i_table[as.character(seq_len(nrow(cas_sa2)))]) 

#plotting new zealand
qtm(cas_sa2, "cas") + tm_layout(panel.labels = "New Zealand CAS")

#plotting Auckland City
qtm(cas_sa2, "cas", bbox = "Auckland New Zealand") + tm_layout(panel.labels = "Auckland Region CAS")



## -----------------------------------------------------------------------------
rt

## -----------------------------------------------------------------------------
#routing 

#function to project geo spatial data 
nzp = function(x, inverse=FALSE) loc = proj4::project(x, "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", inverse=inverse)

#routing example
route_example = jtw[jtw$SA2_code_usual_residence_address == 116700 & jtw$SA2_code_workplace == 147900, c(3,4,7,8)]


## project the journey locations from NZGD to lat/lon
src = nzp(route_example[,1:2], inverse=TRUE)
dst = nzp(route_example[,3:4], inverse=TRUE)

## -----------------------------------------------------------------------------
cas_ex = data.frame(lat=174.7455,lon=36.8309)
cas_ex

src

## -----------------------------------------------------------------------------
cas_ex$lat

## -----------------------------------------------------------------------------
library(osrm)

rt = osrmRoute(c(src$x[1], src$y[1]), c(dst$x[1], dst$y[1]))

cas_ex = data.frame(lon = 174.7455, lat = -36.8309)



library(leaflet)
l <- addTiles(leaflet()) %>%
  setView(mean(range(rt$lon)), mean(range(rt$lat)), 11) %>%
  addPolylines(rt$lon, rt$lat) %>%
    addCircleMarkers(cas_ex$lon, cas_ex$lat, color = 'black', radius = 7)

l


#' ### Assumptions
#' 
#' 1. CAS data and journey to work data did not explicitly specify the time of crash and travel to work, respectively. The assumption that crash and journey to work takes place at the same time. Noting that in the SaferActive/Trafficalmr research they made an assumption that the journey to work data took place during peak hours (7:30-9:30 and 16:30-18:30) thus filtering the crash data during this period. 
#' 
#' 2. SA2 clipped is used to designate JTW and CAS to a specific area. Thus, we might not have the exact location. 
#' 
#' 3. There are different routes from usual residence to workplace for JTW data, we are assuming that the routes in osrm or r5r is the route travelled by the commuter. 
#' 
#' 
#' ### Methodology
#' 
#' 1. In the first iteration, we will be getting the routes of each JTW instance and simulating this. We will also be focusing on type of mode of travel, vehicle. 
#' 
#' 2. Similar to SaferActive/Trafficalmr research, each road segment will be assigned to a SA2 area centroird where a weight is assigned to each edge/road segment according to the number of commuters that have traversed teh said edge/road segment.
#' 
#' 3. We can then quantify a rough measure 
#' 
#'      Rough Risk Measure =   `No. of crashes in an SA2 area` / `Total number of commuters that traversed the same SA2 area as the crash site`
#'      
#'      
#' ## Next steps:
#'  - Scaling up the routes
#'  - Include distance travelled by commuters for KSI/bKm
#'  - Take into consideration demographics and meshblock to extrapolate JTW 
#'  - Comparing the stats provided by Shriv/Jenny 
#' 
#' 
## -----------------------------------------------------------------------------

#' 
## -----------------------------------------------------------------------------
options(java.parameters = "-Xmx2G")
library(r5r)

## -----------------------------------------------------------------------------
## r5r fails to detect its own location, so make sure
## the system library comes first
.libPaths("/shared/rcloud/Rlib")
r5 = setup_r5("/shared/projects/osm", verbose=FALSE)

## -----------------------------------------------------------------------------
it = detailed_itineraries(r5,
    origins=data.frame(id=1, lat=-36.8496255, lon=174.835279),
    destinations=data.frame(id=2, lat=-36.8520715, lon=174.7680502),
    mode="CAR")

## -----------------------------------------------------------------------------
it

## -----------------------------------------------------------------------------
library(leaflet)
ll = sf::st_coordinates(it$geometry[1])
addTiles(leaflet()) %>% addPolylines(ll[,1], ll[,2])


## -----------------------------------------------------------------------------
it = detailed_itineraries(r5,
    origins=data.frame(id=1, lat=-36.8496255, lon=174.835279),
    destinations=data.frame(id=2, lat=-36.8520715, lon=174.7680502),
    mode=c("WALK","TRANSIT"),
    departure_datetime = as.POSIXct("2021-08-24 08:00"),
    max_walk_dist = 5000)

## -----------------------------------------------------------------------------
it

## -----------------------------------------------------------------------------
library(leaflet)
l = addTiles(leaflet())
for (i in seq_len(nrow(it))) {
    ll = sf::st_coordinates(it$geometry[i])
    l = addPolylines(l, ll[,1], ll[,2], color=ifelse(it$mode[i] == "WALK", "red", "blue"))
}
l

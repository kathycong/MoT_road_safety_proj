##Functions in one file

#' Getting the routes of multiple geographic coordinate system (lat/lon) points
#'
#' This function returns a dataframe of where each row an sf linesegment object from the lat/lon points
#'
#' @param lat_source The latitude coordinates of the source point, needs to be a vector
#' @param lng_aource The longitude coodinates of the source point, needs to be a vector
#' @param lat_dest The latitude coordinates of the destination point, needs to be a vector
#' @param lng_dest The longitude coodinates of the destination point, needs to be a vector

get_routes <- function(lat_source, lng_source, lat_dest, lng_dest, osm_dir){
  ##note that gh::route uses the parameter "vehicle" to specify the transportation
  ##mode
  
 
  ##Error handling on inputs
  ##check that lengths for all inputs are the same
  if (!all(sapply(list(length(lng_source), length(lat_dest),
                       length(lng_dest)), FUN = identical,
                  length(lat_source))))
    stop("input lengths are not the same")
  
  ##check that all input are numeric
  if (!all(sapply(list(is.numeric(lng_source), is.numeric(lat_dest),
                       is.numeric(lng_dest)), FUN = identical,
                  is.numeric(lat_source))))
    stop("inputs must be a numeric vector")
  
  ##check that all inputs are not NAs
  if (!all(sapply(list(is.na(lng_source), is.na(lat_dest),
                       is.na(lng_dest)), FUN = identical,
                  is.na(lat_source))))
    stop("inputs must not contain NAs")
  
  ##get list of routes
  routes_list <- lapply(seq.int(length(lat_source)), function(i) {
    
    ##change the route to an sf linesegment and bundle it into a list
    tryCatch(ghroute::route(lat_source[i],
                            lng_source[i],
                            lat_dest[i],
                            lng_dest[i])[[1]][[1]][ ,2:1],
             error = function(e) NA)})
  
  ##transforms the nested list of sf linesegments or routes into a dataframe
  
  output <- purrr::map_dfr(routes_list, function(x){ sf::st_as_sf(
    
    if(any(is.na(x))){
      sf::st_sfc(x, crs = 4326)}
    else {
      sf::st_sfc(sf::st_linestring(x), crs = 4326)})
  })
  
  #https://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
  gc()
  rJava::.jcall("java/lang/System", method = "gc")
  
  output
}


#' Getting the polygons the route intersected
#'
#' This function gets the vector of the polygon ids or codes where a given route
#' is intersected
#'
#' @param routes An sf linesegment object representing routes of multiple
#' source-destination points
#' @param polygon The polygon as geographic coordinate system we wanted to find
#' whether the routes have intersected or not
#' @return A list of vectors where a polygon has intersected with a given route.
#' Each vector in the list represents the series of polygons the route has
#' intersected.
#'

get_route_intersects <- function(routes, polygon, inverse = FALSE){
  
  if(inverse) {
    
    lapply(seq.int(nrow(polygon)),
           function(i) which(lengths(sf::st_intersects(routes,
                                                       polygon[i, ])) > 0))
  }
  
  else {
    lapply(seq.int(nrow(routes)),
           function(i) which(lengths(sf::st_intersects(polygon, routes[i, ])) > 0))
  }
}


#' Calculating the risk from diving the total crashes in a given polygon by
#' the total distance travelled by travellers in th esame polygon
#'
#' This function will calculate the risk measure as indicated in the research
#' paper
#'
#' @param person_travelled The distance travelled by the person aggregated
#'
#' @param crash_lat The latitude coordinate of crash point. Needs to be a vector.
#'
#' @param crash_lng The longitude coordinate of crash point. Needs to be a vector.
#'
#' @param crash_weight The number of crashes in a given lat/lng points.
#'
#' @return This function returns a dataframe with new column called
#' risk_per_m as
#'
#'

get_risk <- function(crash_lat, crash_lng, crash_weight, polygon){
  
  #create a dataframe
  crash_data <-  data.frame(total_crash_weight = crash_weight,
                            crash_lat = crash_lat,
                            crash_lng = crash_lng)
  
  #change lat and lng as sf objects
  crash_data_sf <- st_as_sf(crash_data, coords = c("crash_lng", "crash_lat"),
                            crs = 4326)
  
  #calculating the risk exposure
  output <- st_join(polygon, crash_data_sf)
  
  output <- output  %>%
    group_by_at(vars(-total_crash_weight, -geometry)) %>%
    summarise(total_crash_weight = sum(total_crash_weight))
  
  return(output)

}

#' Getting the distance travelled by a traveller per polygon
#'
#' This function returns a dataframe of the total distance travelled by
#' travellers  for each polygon.
#'
#' @param routes The routes travelled by the traveller with the associated
#' weight or number of travellers.
#'
#' @param polygon An sfc class object. The polygon we are trying to intersect
#' with teh routes.
#'
#' @param polygon_id A unique polygon identifier used for groupby and
#' joining geometry data.
#'
#' @param weight The weight for each line segment or route. This is optional.

get_dist_travel <- function(polygon, routes, weight){
  
  #weight is an optional argument
  
  #if weight param is missing then default to 1
  if(missing(weight)){
    weight <- rep(1, nrow(routes))
  } else weight
  
  
  #############checks#########
  # 1. input has the correct class
  # 2. input has the correct length i.e. length(routes) == weight
  # 3. Data has no NaNs
  
  #binding routes and weight as its easier for aggregation later on
  routes <- cbind(routes, weight = weight)
  
  #avoiding errors
  sf::st_agr(routes) <- "constant"
  sf::st_agr(polygon) <- "constant"
  
  # getting the intersection of polygons and routes provided
  output <- sf::st_intersection(polygon, routes)
  
  #getting distance travelled
  output$total_dist<- sf::st_length((output)) * output$weight
  
  output <- sf::st_join(polygon, output[, c('total_dist', 'weight', 'geometry')])
  
  #grouping by aggregated data
  output <- output  %>%
    group_by_at( vars(-total_dist, -weight, -geometry)) %>%
    summarise(total_dist  = sum(total_dist),
              weight = sum(weight))
  
  return(output)
  
  
  output %>% mapview(zcol = 'total_dist',
                     layer.name = "total distance travelled")
  
}


route_risk <- function(routes, crash_lat, crash_lng, crash_weight){
  
  #create a dataframe
  crash_data <-  data.frame(total_crash_weight = crash_weight,
                            crash_lat = crash_lat,
                            crash_lng = crash_lng)
  
  #change lat and lng as sf objects
  crash_data_sf <- st_as_sf(crash_data, coords = c("crash_lng", "crash_lat"),
                            crs = 4326)
  
  #sum the number of crash points within 5 meters from the route
  output <- lapply(seq.int(nrow(routes)), function(i){
    
    sum(crash_data_sf[lengths(st_is_within_distance(crash_data_sf,
                                                    routes[i, ],
                                                    dist = 5)) >0,
    ]$total_crash_weight)
  })
  
  cbind(routes, total_crash_weight = unlist(output))
  
}




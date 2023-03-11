helper.function <- function()
{
  return(1)
}

# function that takes the enrolment data and plots an interactive map showing users per country
create_map_interactive <- function(enrollments){
  total_users_per_country <- data.frame(table(filter(enrollments, !(detected_country == "--"))$detected_country))
  
  names(total_users_per_country)[1] = "region"
  names(total_users_per_country)[2] = "users"
  
  world_map <- map_data(map = "world")
  world_map$region <- iso.alpha(world_map$region)
  
  world_map <- left_join(world_map, total_users_per_country, by="region")
  world_map <- world_map %>%
    mutate(users = coalesce(users, 0))
  
  map1 <- ggplot(world_map, aes( x = long, y = lat, group=group)) +
    geom_polygon(aes(fill = users), color = "black") +
    scale_fill_gradient(low = "yellow", high =  "red", na.value = "grey50")
  #map1
  
  ggplotly(map1) %>%
    highlight(
      "plotly_hover",
      selected = attrs_selected(line = list(color = "black"))
    ) %>%
    frameWidget()
}

# function that takes the enrolment data and plots a map showing users per country
create_map <- function(enrollments){
  total_users_per_country <- data.frame(table(filter(enrollments, !(detected_country == "--"))$detected_country))
  
  names(total_users_per_country)[1] = "region"
  names(total_users_per_country)[2] = "users"
  
  world_map <- map_data(map = "world")
  world_map$region <- iso.alpha(world_map$region)
  
  world_map <- left_join(world_map, total_users_per_country, by="region")
  world_map <- world_map %>%
    mutate(users = coalesce(users, 0))
  
  map1 <- ggplot(world_map, aes( x = long, y = lat, group=group)) +
    geom_polygon(aes(fill = users), color = "black") +
    scale_fill_gradient(low = "cornsilk", high =  "red", na.value = "grey50")
  return(map1)
}
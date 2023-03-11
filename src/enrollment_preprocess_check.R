library('ProjectTemplate')
load.project()


cyber.security.1_enrolments <- cyber.security.1_enrolments %>%
  mutate(detected_country = ifelse((detected_country == "--" | detected_country != country) & country != "Unknown", country, detected_country))

total_users_per_country <- data.frame(table(filter(cyber.security.1_enrolments, !(detected_country == "--"))$detected_country))
head(total_users_per_country)
sum(total_users_per_country$Freq)
nrow(cyber.security.1_enrolments)

names(total_users_per_country)[1] = "region"
names(total_users_per_country)[2] = "users"

create_map(cyber.security.1_enrolments)

world_map <- map_data(map = "world")
world_map$region <- iso.alpha(world_map$region)

world_map <- left_join(world_map, total_users_per_country, by="region")
world_map <- world_map %>%
  mutate(users = coalesce(users, 0))

map1 <- ggplot(world_map, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = users), color = "black") +
  scale_fill_gradient(low = "cyan", high =  "red", na.value = "grey50")
map1


ggplotly(map1) %>%
  highlight(
    "plotly_hover",
    selected = attrs_selected(line = list(color = "black"))
  ) %>%
  frameWidget()


cyber.security.1_enrolments <- 
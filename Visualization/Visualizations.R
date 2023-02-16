#### maps

## Circles around a parcel and near new constructions

parcels <- read_csv("/Users/saratonazzi/Documents/Polimi/Non Parametric/Parcels_augmented.csv")
centroid_point <- st_as_sf(parcels[60000:60010,7:8], coords = c(x = "Centroid_Long", y = "Centroid_Lat"), crs = 4326)


# Buffer circles by 100m
dat_circles_100 <- st_buffer(centroid_point, dist = 100)
poly_100 <- st_transform(dat_circles_100$geometry, 4326)
# Buffer circles by 500m
dat_circles_500 <- st_buffer(centroid_point, dist = 500)
poly_500 <- st_transform(dat_circles_500$geometry, 4326)
# Buffer circles by 1000m
dat_circles_1000 <- st_buffer(centroid_point, dist = 1000)
poly_1000 <- st_transform(dat_circles_1000$geometry, 4326)
# Buffer circles by 2000m
dat_circles_2000 <- st_buffer(centroid_point, dist = 2000)
poly_2000 <- st_transform(dat_circles_2000$geometry, 4326)

new_construction <- read_csv("/Users/saratonazzi/Documents/Polimi/Non Parametric/New_construction_clean_geocoded_nh.csv")
points_nb <- new_construction %>% 
  as.data.frame %>% 
  st_as_sf(coords = c(x = "lon", y = "lat")) %>%
  st_set_crs(4326)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(data= poly_2000[1],
              fillOpacity = 0.5,
              color = "yellow", weight = 1, opacity = 1) %>%
  addPolygons(data= poly_1000[1],
              fillOpacity = 0.5,
              color = "gold", weight = 1, opacity = 1) %>%
  addPolygons(data= poly_500[1],
              fillOpacity = 0.5,
              color = "orange", weight = 1, opacity = 1) %>%
  addPolygons(data= poly_100[1],
              fillOpacity = 0.5,
              color = "red", weight = 1, opacity = 1) %>%
  addCircles(data = points_year,
             opacity = 0.7, color = "blue", weight = 4,
  )


# map of neighborhoods

SFNeighborhoods <- read_sf("SFNeighborhoods_new.geojson")
center <- st_centroid(SFNeighborhoods)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(data=SFNeighborhoods,
              fillOpacity = 0,
              color = "black", weight = 1, opacity = 1) %>%
  addLabelOnlyMarkers(data = center,
                      label = ~nhood,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = FALSE))


# map of google bus stops

gb <- read_csv("google_busses.csv")
gb_points <- gb %>%
  as.data.frame %>% 
  st_as_sf(coords = c(x = "lon", y = "lat")) %>%
  st_set_crs(4326)

busIcon = makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/6395/6395324.png",
  iconWidth = 30, iconHeight = 30)

leaflet(data=gb_points) %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addMarkers(icon = busIcon)

# mean rent global

rent <- read_csv("rent_clean_nh.csv")
rent <- rent %>% filter(nhood != 'San Francisco' & nhood !='Treasure Island')

rent_nh <- merge(rent,subset(SFNeighborhoods, select=c("nhood", "geometry")), by.x="nhood", by.y="nhood") %>% 
  select(nhood,geometry,price_mq)

rent_nh <- st_as_sf(rent_nh)

mean_rent <- rent_nh %>% 
  group_by(nhood) %>% 
  summarise(mean_price = mean(price_mq), .groups = "drop") %>% 
  mutate(
    label = paste0("<b>", nhood, ":</b> ", mean_price)
  ) %>% 
  select(nhood, mean_price, label)

pal <- leaflet::colorNumeric(
  "YlOrRd",
  domain = mean_rent$mean_price
)

leaflet(mean_rent) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(
    color = "#222", weight = 2, opacity = 1,
    fillColor = ~pal(mean_rent$mean_price), fillOpacity = 0.7,
    label = ~lapply(mean_rent$label, htmltools::HTML),
    labelOptions = labelOptions(direction = "top"),
    highlight = highlightOptions(
      color = "#FFF", bringToFront = TRUE
    )
  ) %>%
  addLegend_decreasing(
    pal = pal, values = mean_rent$mean_price, opacity = 2,
    title = "Rent price", position = "bottomright", decreasing = TRUE
    #,labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )

# mean rent for video

rent <- read_csv("rent_clean_nh.csv")
rent <- rent %>% filter(nhood != 'San Francisco' & nhood !='Treasure Island')

rent <- rent %>% filter(year==2019)

rent[nrow(rent) + 1,'nhood'] <- 'Lakeshore'
rent[nrow(rent),'price_mq'] <- 26

rent[nrow(rent) + 1,'nhood'] <- 'Oceanview/Merced/Ingleside'
rent[nrow(rent),'price_mq'] <- 28

rent[nrow(rent) + 1,'nhood'] <- 'Western Addition'
rent[nrow(rent),'price_mq'] <- 45

rent[nrow(rent) + 1,'nhood'] <- 'Outer Richmond'
rent[nrow(rent),'price_mq'] <- 36

rent[nrow(rent) + 1,'nhood'] <- 'Twin Peaks'
rent[nrow(rent),'price_mq'] <- 41

domain = c(7,71)

rent_nh <- merge(rent,subset(SFNeighborhoods, select=c("nhood", "geometry")), by.x="nhood", by.y="nhood") %>% 
  select(nhood,geometry,price_mq)

rent_nh <- st_as_sf(rent_nh)

mean_rent <- rent_nh %>% 
  group_by(nhood) %>% 
  summarise(mean_price = mean(price_mq), .groups = "drop") %>% 
  mutate(
    label = paste0("<b>", nhood, ":</b> ", mean_price)
  ) %>% 
  select(nhood, mean_price, label)

pal <- leaflet::colorNumeric(
  "YlOrRd",
  domain = domain)

leaflet(mean_rent) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(
    color = "#222", weight = 2, opacity = 1,
    fillColor = ~pal(mean_rent$mean_price), fillOpacity = 0.7,
    label = ~lapply(mean_rent$label, htmltools::HTML),
    labelOptions = labelOptions(direction = "top"),
    highlight = highlightOptions(
      color = "#FFF", bringToFront = TRUE
    )
  ) %>%
  addLegend_decreasing(
    pal = pal, values = domain, opacity = 2,
    title = "Rent price", position = "bottomright", decreasing = TRUE
  )
  

## evictions in neighborhhood

evictions <- read_csv("Eviction_Notices_Clean_nh.csv")
#evictions <- evictions %>% filter(year==2019)
evictions_nh <- merge(evictions,subset(SFNeighborhoods, select=c("nhood", "geometry","area")), by.x="nhood", by.y="nhood") %>% 
  select(nhood,geometry,area,dummy)
evictions_nh <- st_as_sf(evictions_nh)

mean_evictions <- evictions_nh %>% 
  group_by(nhood) %>% 
  summarise(mean_num = sum(dummy), area=mean(area), .groups = "drop") %>% 
  mutate(
    label = paste0("<b>", nhood, ":</b> ", mean_evictions$mean_num/mean_evictions$area*1000000)
  ) %>% 
  select(nhood, mean_num, label, area)

mean_evictions$density <- mean_evictions$mean_num/mean_evictions$area*1000000

pal <- leaflet::colorNumeric(
  "YlOrRd",
  domain = mean_evictions$density )

leaflet(mean_evictions) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(
    color = "#222", weight = 2, opacity = 1,
    fillColor = ~pal(mean_evictions$density), fillOpacity = 0.7,
    label = ~lapply(mean_evictions$label, htmltools::HTML),
    labelOptions = labelOptions(direction = "top"),
    highlight = highlightOptions(
      color = "#FFF", bringToFront = TRUE
    )
  ) %>%
  addLegend_decreasing(
    pal = pal, values = mean_evictions$density , opacity = 2, decreasing = TRUE,
    position = "bottomright",
  )




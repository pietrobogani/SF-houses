library(leaflet)

SFNeighborhoods <- read_sf("Documents/Polimi/Non Parametric/SFNeighborhoods_new.geojson")
evictions <- read_csv("Documents/Polimi/Non Parametric/Eviction_Notices_Clean_nh.csv")

evictions_nh <- merge(evictions,subset(SFNeighborhoods, select=c("nhood", "geometry")), by.x="nhood", by.y="nhood") %>% 
  select(nhood,geometry)

evictions_nh <- st_as_sf(evictions_nh)

evictions_count <- evictions_nh %>% 
  group_by(nhood) %>% 
  summarise(total_evictions = n(), .groups = "drop") %>% 
  mutate(
     label = paste0("<b>", nhood, ":</b> ", total_evictions)
   ) %>% 
  select(nhood, total_evictions, label)

pal <- leaflet::colorNumeric(
  "YlOrRd",
  domain = evictions_count$total_evictions
)

leaflet(evictions_count) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(
    #data=SFNeighborhoods,
    color = "#222", weight = 2, opacity = 1,
    fillColor = ~pal(evictions_count$total_evictions), fillOpacity = 0.7,
    label = ~lapply(evictions_count$label, htmltools::HTML),
    labelOptions = labelOptions(direction = "top"),
    highlight = highlightOptions(
      color = "#FFF", bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal, values = ~total_evictions, opacity = 0.7,
    title = "Number of evictions", position = "topleft"
  )

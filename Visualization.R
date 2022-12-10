library(leaflet)

SFNeighborhoods <- read_sf("Documents/Polimi/Non Parametric/SFNeighborhoods_new.geojson")
evictions <- read_csv("Documents/Polimi/Non Parametric/Eviction_Notices_Clean_nh.csv")

evictions_count <- evictions %>% 
  group_by(nhood) %>% 
  summarise(total_evictions = n(), .groups = "drop")
  # mutate(
  #   label = paste0("<b>", nhood, ":</b> ", total_evictions)
  # ) %>% 
  # select(nhood, total_evictions)

pal <- leaflet::colorNumeric(
  "YlOrRd",
  domain = evictions_count$total_evictions
)

leaflet(evictions_count) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>% 
  addPolygons(
    data=SFNeighborhoods,
    color = "#222", weight = 2, opacity = 1,
    fillColor = ~pal(evictions_count$total_evictions), fillOpacity = 0.7,
    #label = ~lapply(label, htmltools::HTML),
    labelOptions = labelOptions(direction = "top"),
    highlight = highlightOptions(
      color = "#FFF", bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal, values = ~total_evictions, opacity = 0.7,
    title = "# evictions", position = "topleft"
  )

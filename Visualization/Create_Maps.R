install.packages("maps")
install.packages("leaflet")
install.packages("dplyr")
install.packages("shiny")
# will not install them as I already have them
# and then load those libraries

library(maps)
library(leaflet)
library(dplyr)
library(shiny)
library(viridis)
library(sf)


#leggo e imparo da questo link "https://www.r-bloggers.com/2021/01/visualizing-geospatial-data-in-r-part-3-making-interactive-maps-with-leaflet/"
#https://leafletjs.com/examples/choropleth/ : da questo link si impara come dare colori diversi in base ad una certa covariate
#----------------------------------------------------------------------------------------------------------------------------------------------





#--------------- Create a map displaying the nhoods with different colors

geoJson <- read_sf("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/SFNeighborhoods_new.geojson")

#options(digits=20)
{
#    (nhood_poly1<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly1.csv" ))
#    (nhood_poly2<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly2.csv" ))
#    (nhood_poly3<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly3.csv" ))
#    (nhood_poly4<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly4.csv" ))
#    (nhood_poly5<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly5.csv" ))
#    (nhood_poly6<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly6.csv" ))
#    (nhood_poly7<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly7.csv" ))
#    (nhood_poly8<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly8.csv" ))
#    (nhood_poly9<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly9.csv" ))
#    (nhood_poly10<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly10.csv" ))
#    (nhood_poly11<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly11.csv" ))
#    (nhood_poly12<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly12.csv" ))
#    (nhood_poly13<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly13.csv" ))
#    (nhood_poly14<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly14.csv" ))
#    (nhood_poly15<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly15.csv" ))
#    (nhood_poly16<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly16.csv" ))
#    (nhood_poly17<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly17.csv" ))
#    (nhood_poly18<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly18.csv" ))
#    (nhood_poly19<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly19.csv" ))
#    (nhood_poly20<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly20.csv" ))
#    (nhood_poly21<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly21.csv" ))
#    (nhood_poly22<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly22.csv" ))
#    (nhood_poly23<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly23.csv" ))
#    (nhood_poly24<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly24.csv" ))
#    (nhood_poly25<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly25.csv" ))
#    (nhood_poly26<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly26.csv" ))
#    (nhood_poly27<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly27.csv" ))
#    (nhood_poly28<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly28.csv" ))
#    (nhood_poly29<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly29.csv" ))
#    (nhood_poly30<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly30.csv" ))
#    (nhood_poly31<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly31.csv" ))
#    (nhood_poly32<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly32.csv" ))
#    (nhood_poly33<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly33.csv" ))
#    (nhood_poly34<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly34.csv" ))
#    (nhood_poly35<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly35.csv" ))
#    (nhood_poly36<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly36.csv" ))
#    (nhood_poly37<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly37.csv" ))
#    (nhood_poly38<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly38.csv" ))
#    (nhood_poly39<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly39.csv" ))
#    (nhood_poly40<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly40.csv" ))
#    (nhood_poly41<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly41.csv" ))
} #carico i poligoni


{

ui <- fluidPage(
  titlePanel("San Francisco Nhoods"),
  leafletOutput("mymap"))



#color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
#col=sample(color, 37)
#pal <- colorFactor(palette = color, unique(dataset$neighborhoods)) #questa palette non è un granchè per la mia mappa perchè ha anche colori chiari

unique(dataset$nhood) #41 nhoods
# palette = c(#96CDCD","#8B8378", "aquamarine3","azure4","#0000CD","#53868B","#D2691E","#7FFF00","#CDC8B1","#00FFFF","#FFB90F",
#             "#556B2F","darkorchid1","#C1FFC1","#FF1493","#FFD700","#C1CDC1","#CD6090","khaki3","#EEA2AD","#B3EE3A",
#             "orange", "#98FB98", "#FFA500","#8B2500",,"#551A8B","#EEEE00","black","navy","#FF8C69","#9FB6CD",
#             "#EED2EE","#8B7E66","#00C5CD","#00EE76","#8B0000","#EECFA1","#CD5C5C","#00B2EE","#FF69B4","#FF00FF",
#             "#FFA500")
pal2 <- colorFactor(palette = c("orange","#EED2EE", "aquamarine3","azure4","#0000CD","#53868B","#D2691E","#7FFF00","#CD5C5C","#00008B",
                                "#FF1493","#00FFFF","#FFB90F","#556B2F","darkorchid1","#C1FFC1","#FFD700","#EE6AA7","#CD6090","khaki3"
                                ,"#EEA2AD", "orange", "#8B2500","#8B4726","#551A8B","#EEEE00","#B3EE3A","black","navy","#FF8C69",
                                "#98FB98","#00F5FF","#8B7E66","#00C5CD","#00EE76","#8B0000","#EECFA1","#00B2EE"),unique(geoJson$nhood))
#palette costruita a mano per avere solo cori scuri che risaltano sulla mappa, usiamo questa

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(data = geoJson,fillColor = ~pal2(geoJson$nhood),color = ~pal2(geoJson$nhood),weight = 5,fill = TRUE
                                                                   ,fillOpacity = 0.8)
  # addPolygons( data = as.matrix(nhood_poly1),color = palette[1])  %>%
  # addPolygons( data = as.matrix(nhood_poly2),color = palette[2])%>%
  # addPolygons( data = as.matrix(nhood_poly3),color = palette[3])%>%
  # addPolygons( data = as.matrix(nhood_poly4),color = palette[4])%>%
  # addPolygons( data = as.matrix(nhood_poly5),color = palette[5])%>%
  # addPolygons( data = as.matrix(nhood_poly6),color = palette[6])%>%
  # addPolygons( data = as.matrix(nhood_poly7),color = palette[7])%>%
  # addPolygons( data = as.matrix(nhood_poly8),color = palette[8])%>%
  # addPolygons( data = as.matrix(nhood_poly9),color = palette[9])  %>%         #problematico, errore nei dati iniziali
  #  addPolygons( data = as.matrix(nhood_poly10),color = palette[10])%>%
  # addPolygons( data = as.matrix(nhood_poly11),color = palette[11]) %>%
  #  addPolygons( data = as.matrix(nhood_poly12),color = palette[12]) %>%
  #  addPolygons( data = as.matrix(nhood_poly13),color = palette[13])%>%
  #  addPolygons( data = as.matrix(nhood_poly14),color = palette[14]) %>%
  # addPolygons( data = as.matrix(nhood_poly15),color = palette[15])%>%
  # addPolygons( data = as.matrix(nhood_poly16),color = palette[16])   %>%         #problematico, errore nei dati iniziali
  #   addPolygons( data = as.matrix(nhood_poly17),color = palette[17]) %>%
  #  addPolygons( data = as.matrix(nhood_poly18),color = palette[18]) %>%
  # addPolygons( data = as.matrix(nhood_poly19),color = palette[19])%>%
  # addPolygons( data = as.matrix(nhood_poly20),color = palette[20]) %>%
  #  addPolygons( data = as.matrix(nhood_poly21),color = palette[21]) %>%
  #  addPolygons( data = as.matrix(nhood_poly22),color = palette[22]) %>%       #problematico,, errore nei dati iniziali
  # addPolygons( data = as.matrix(nhood_poly23),color = palette[23]) %>%
  # addPolygons( data = as.matrix(nhood_poly24),color = palette[24]) %>%
  #     addPolygons( data = as.matrix(nhood_poly25),color = palette[25]) %>%
  #   addPolygons( data = as.matrix(nhood_poly26),color = palette[26]) %>%
  #  addPolygons( data = as.matrix(nhood_poly27),color = palette[27])%>%
  # addPolygons( data = as.matrix(nhood_poly28),color = palette[28]) %>%
  # addPolygons( data = as.matrix(nhood_poly29),color = palette[29]) %>%
  # addPolygons( data = as.matrix(nhood_poly30),color = palette[30])%>%
  # addPolygons( data = as.matrix(nhood_poly31),color = palette[31]) %>%
  # addPolygons( data = as.matrix(nhood_poly32),color = palette[32]) %>%
  # addPolygons( data = as.matrix(nhood_poly33),color = palette[33])%>%
  # addPolygons( data = as.matrix(nhood_poly34),color = palette[34]) %>%
  # addPolygons( data = as.matrix(nhood_poly35),color = palette[35]) %>%
  # addPolygons( data = as.matrix(nhood_poly36),color = palette[36])  %>%       #problematico
  # addPolygons( data = as.matrix(nhood_poly37),color = palette[37])%>%
  #  addPolygons( data = as.matrix(nhood_poly38),color = palette[38]) %>%
  #   addPolygons( data = as.matrix(nhood_poly39),color = palette[39]) %>%
  # addPolygons( data = as.matrix(nhood_poly40),color = palette[40]) %>%
  # addPolygons( data = as.matrix(nhood_poly41),color = palette[41])
  # 

  })
  
  
}


# finally, we need to call the shinyapp function with the ui and server as arguments

shinyApp(ui, server)



}




#--------------------- Create a map of "New_Constructions" with an automatic Slider on year and selecting the nhood I want to visualize

New_construction_clean_geocoded_nh <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/New_construction_clean_geocoded_nh.csv", header=TRUE)
dataset <- New_construction_clean_geocoded_nh[,c(11,13,14,15,16)] 

head(dataset)




{


ui <- fluidPage(
  titlePanel("San Francisco Constructions by year and nhood"),
  leafletOutput("mymap"),
  fluidRow(column(12, 
                  sliderInput("slider1", h3("Select the year"),
                              min = 2007, max = 2018, value = 2007, step=1, animate = TRUE),
                  # here the slider allows to keep a max, min and a set value to start the app with
                 radioButtons("radio", h3("Select the nhood"),
                               choices = unique(dataset$neighborhoods), selected = "Mission")
                  # and here the rado button allows to add a list with the element it will contain
                  # in this case Poland contains Poland but it can contain 1, that depends on 
                  # your attributes, and also you have the selected one like in the case of the value
                  # there are a lot of options so if you are interested just keep checking
                  # see you
  )
  )
)


server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    leaflet(dataset %>%
              dplyr::filter(
              neighborhoods == input$radio,
              year == input$slider1
              )) %>%
      addTiles() %>%
      addCircleMarkers(lat = ~lat, lng= ~lon, radius = ~10,color = "green" ) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    
  })
  
  
}

# finally, we need to call the shinyapp function with the ui and server as arguments

shinyApp(ui, server)
}


#--------------------- Create a map of "New_Constructions" with an automatic Slider on year and painting each nhood with a different color


New_construction_clean_geocoded_nh <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/New_construction_clean_geocoded_nh.csv", header=TRUE)
dataset <- New_construction_clean_geocoded_nh[,c(11,13,14,15,16)] 

head(dataset)


{
  
  
  ui <- fluidPage(
    titlePanel("San Francisco Constructions by year"),
    leafletOutput("mymap"),
    fluidRow(column(12, 
                    sliderInput("slider1", h3("Select the year"),
                                min = 2007, max = 2018, value = 2007,step=1, animate = TRUE),
                    # here the slider allows to keep a max, min and a set value to start the app with
    )
    )
  )
  
  
  #color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][2:433] #rimuovo bianco
  #col=sample(color, 37)
  #pal <- colorFactor(palette = color, unique(dataset$neighborhoods)) #questa palette non è un granchè per la mia mappa perchè ha anche colori chiari
  
  pal2 <- colorFactor(palette = c("#8B8378", "aquamarine3","azure4","#0000CD","#53868B","#D2691E","#7FFF00","#CDC8B1","#00FFFF","#FFB90F",
                                  "#556B2F","darkorchid1","#C1FFC1","#FF1493","#FFD700","#C1CDC1","#CD6090","khaki3","#EEA2AD","#B3EE3A",
                                  "orange", "#98FB98", "#8B2500","#96CDCD","#551A8B","#EEEE00","black","navy","#FF8C69","#9FB6CD",
                                  "#EED2EE","#8B7E66","#00C5CD","#00EE76","#8B0000","#EECFA1","#CD5C5C"),unique(dataset$neighborhoods))
    #palette costruita a mano per avere solo cori scuri che risaltano sulla mappa, usiamo questa
  
  server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
      
      leaflet(dataset %>%
                dplyr::filter(
                  year == input$slider1
                )) %>%
        addTiles() %>%
        addCircleMarkers(lat = ~lat, lng= ~lon, radius = ~5 ,stroke = TRUE, fillOpacity = TRUE, weight = 1, color = ~pal2(neighborhoods) ) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      })
    
    }
  
  # finally, we need to call the shinyapp function with the ui and server as arguments
  
  shinyApp(ui, server)
}





#--------Create a map of "Evictions" painting each nhood with a different color based on the number of total constructions across the years

{ 
  SFNeighborhoods <- read_sf("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/SFNeighborhoods_new_nh.geojson")
  #evictions <- readcsv("Documents/Polimi/Non Parametric/Eviction_Notices_Clean_nh.csv")
  evictions<- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices_clean_nh.csv", header=TRUE)
  
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
}




#--------------------- Create a map of "Parcels" selecting the nhood I want to visualize

Parcels_final_nh <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Parcels_final_nh.csv", header=TRUE)
dataset <- Parcels_final_nh[,6:8]

{
  
  
  ui <- fluidPage(
    titlePanel("San Francisco Parcels"),
    leafletOutput("mymap"),
    fluidRow(column(12, 
                    radioButtons("radio", h3("Select the nhood"),
                                 choices = unique(dataset$neighborhoods), selected = "Mission")
    )))

  
  
  #color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  #col=sample(color, 37)
  #pal <- colorFactor(palette = color, unique(dataset$neighborhoods)) #questa palette non è un granchè per la mia mappa perchè ha anche colori chiari
  
  unique(dataset$neighborhoods) #39 nhoods
  
  pal2 <- colorFactor(palette = c("#8B8378", "aquamarine3","azure4","#0000CD","#53868B","#D2691E","#7FFF00","#CDC8B1","#00FFFF","#FFB90F",
                                  "#556B2F","darkorchid1","#C1FFC1","#FF1493","#FFD700","#C1CDC1","#CD6090","khaki3","#EEA2AD","#B3EE3A",
                                  "orange", "#98FB98", "#8B2500","#96CDCD","#551A8B","#EEEE00","black","navy","#FF8C69","#9FB6CD",
                                  "#EED2EE","#8B7E66","#00C5CD","#00EE76","#8B0000","#EECFA1","#CD5C5C","#00B2EE","#FF69B4"),unique(dataset$neighborhoods))
  #palette costruita a mano per avere solo cori scuri che risaltano sulla mappa, usiamo questa
  
  server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
      
      leaflet(dataset %>%
                dplyr::filter(
                  neighborhoods == input$radio))  %>%
              addTiles() %>%
              addCircleMarkers(lat = ~Centroid_Lat, lng= ~Centroid_Long, radius = ~5 ,stroke = TRUE, fillOpacity = TRUE, weight = 1, color = ~pal2(neighborhoods) ) %>%
              addProviderTiles(providers$CartoDB.Positron)
      
    })
    
  }
  
  # finally, we need to call the shinyapp function with the ui and server as arguments
  
  shinyApp(ui, server)
}



#--------------------- Create a map of "Parcels"  painting each nhood with a different color

{
  
  
  ui <- fluidPage(
    titlePanel("San Francisco Parcels"),
    leafletOutput("mymap"),
    )
  
  
  
  #color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  #col=sample(color, 37)
  #pal <- colorFactor(palette = color, unique(dataset$neighborhoods)) #questa palette non è un granchè per la mia mappa perchè ha anche colori chiari
  
  unique(dataset$neighborhoods) #39 nhoods
  
  pal2 <- colorFactor(palette = c("#8B8378", "aquamarine3","azure4","#0000CD","#53868B","#D2691E","#7FFF00","#CDC8B1","#00FFFF","#FFB90F",
                                  "#556B2F","darkorchid1","#C1FFC1","#FF1493","#FFD700","#C1CDC1","#CD6090","khaki3","#EEA2AD","#B3EE3A",
                                  "orange", "#98FB98", "#8B2500","#96CDCD","#551A8B","#EEEE00","black","navy","#FF8C69","#9FB6CD",
                                  "#EED2EE","#8B7E66","#00C5CD","#00EE76","#8B0000","#EECFA1","#CD5C5C","#00B2EE","#FF69B4"),unique(dataset$neighborhoods))
  #palette costruita a mano per avere solo cori scuri che risaltano sulla mappa, usiamo questa
  
  server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
      
      leaflet(dataset)%>%
        addTiles() %>%
        addCircleMarkers(lat = ~Centroid_Lat, lng= ~Centroid_Long, radius = ~5 ,stroke = TRUE, fillOpacity = TRUE, weight = 1, color = ~pal2(neighborhoods) ) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
    })
    
  }
  
  # finally, we need to call the shinyapp function with the ui and server as arguments
  
  shinyApp(ui, server) #plot molto pesante
} #plot molto pesante





#--------------------- Create a map of "Eviction_notices" painting each nhood with a different color based on the amount of evictions

Eviction_Notices_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices_clean.csv", header=TRUE)
sum_eviction_by_nhood = aggregate(Eviction_Notices_clean$dummy, by = list(Eviction_Notices_clean$nhood), FUN = sum)
names(sum_eviction_by_nhood)[names(sum_eviction_by_nhood) == 'Group.1'] <- 'nhood'
names(sum_eviction_by_nhood)[names(sum_eviction_by_nhood) == 'x'] <- 'Evictions_number'
temp <- Eviction_Notices_clean[,c(4,12,13)]
temp <- temp[!duplicated(temp),]
temp<- temp[order(temp$nhood),]
sum_eviction_by_nhood<-sum_eviction_by_nhood[order(sum_eviction_by_nhood$nhood),]
sum_eviction_by_nhood$centroid_lat <- temp$centroid_lat
sum_eviction_by_nhood$centroid_long <- temp$centroid_long

{
  ui <- fluidPage(
    titlePanel("San Francisco Evictions"),
    leafletOutput("mymap"),
  )
  
  
  
  #color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  #col=sample(color, 37)
  #pal <- colorFactor(palette = color, unique(dataset$neighborhoods)) #questa palette non è un granchè per la mia mappa perchè ha anche colori chiari
  
  unique(sum_eviction_by_nhood$nhood) #41 nhoods
  
  pal2 <- colorFactor(palette = c("#8B8378", "aquamarine3","azure4","#0000CD","#53868B","#D2691E","#7FFF00","#CDC8B1","#00FFFF","#FFB90F",
                                  "#556B2F","darkorchid1","#C1FFC1","#FF1493","#FFD700","#C1CDC1","#CD6090","khaki3","#EEA2AD","#B3EE3A",
                                  "orange", "#98FB98", "#8B2500","#96CDCD","#551A8B","#EEEE00","black","navy","#FF8C69","#9FB6CD",
                                  "#EED2EE","#8B7E66","#00C5CD","#00EE76","#8B0000","#EECFA1","#CD5C5C","#00B2EE","#FF69B4","#FF00FF",
                                  "#FFA500"),unique(dataset$neighborhoods)) #prendo i colori da "https://r-charts.com/colors/"
  #palette costruita a mano per avere solo cori scuri che risaltano sulla mappa, usiamo questa
  
  server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
      
      leaflet(dataset)%>%
        addTiles() %>%
        addCircleMarkers(lat = ~Centroid_Lat, lng= ~Centroid_Long, radius = ~ifelse() ,stroke = TRUE, fillOpacity = TRUE, weight = 1, color = ~pal2(neighborhoods) ) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
    })
    
  }
  
  # finally, we need to call the shinyapp function with the ui and server as arguments
  
  shinyApp(ui, server) #plot molto pesante
} #plot molto pesante

Parcels <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Parcels.csv", header=TRUE)


Parcels <- Parcels[which(Parcels$RESUNITS > 0), ] #elimino quelle dove non abita nessuno
#Parcels <- Parcels[which(Parcels$LANDUSE == 'RESIDENT'), ] #elemino quelle che non sono a fini abitativi
Parcels <- Parcels[,-c(14,16:21) ]

###ADD NEIGHBORHOODS TO PARCEL


Parcels_final <- parcel

points <- Parcels_final[,6:7] %>% 
  as.data.frame %>% 
  st_as_sf(coords = c(x = "Centroid_Long", y = "Centroid_Lat")) %>%
  st_set_crs(4326)

Parcels_final$number <- st_within(points, poly)

check <- mapply(function(x) !identical(x, integer(0)), Parcels_final$number)
Parcels_final <- Parcels_final[check,]

Parcels_final$neighborhoods <- mapply(function(x) SFNeighborhoods[[1]][[x]], Parcels_final$number)
Parcels_final <- subset(Parcels_final, select=-c(8))
write.csv(Parcels_final,"/Users/saratonazzi/Documents/Polimi/Non Parametric/Parcels_final_nh.csv", row.names = FALSE)
                                      
                                      #####PARCEL WITH POLYGONS

SFNeighborhoods <- read_sf("/Users/saratonazzi/Documents/Polimi/Non Parametric/SFNeighborhoods_new.geojson")
parcel <- read_sf("Documents/Polimi/Non Parametric/Land Use.geojson")

poly <- st_transform(SFNeighborhoods$geometry, 4326)
poly_parcels <- st_transform(parcel$geometry, 4326)
parcel$Centroid <- st_centroid(poly_parcels, of_largest_polygon=FALSE)

parcel$number <- st_within(parcel$Centroid, poly)

check <- mapply(function(x) !identical(x, integer(0)), parcel$number)
parcel <- parcel[check,]

parcel$neighborhoods <- mapply(function(x) SFNeighborhoods[[1]][[x]], parcel$number)
                               
#####FIND NEW BUILDINGS NEAR PARCELS

#build circles

parcels <- read_csv("/Users/saratonazzi/Documents/Polimi/Non Parametric/Parcels_augmented.csv")
centroid_point <- st_as_sf(parcels[,7:8], coords = c(x = "Centroid_Long", y = "Centroid_Lat"), crs = 4326)


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

for (year_f in 2007:2018) {
  
  points_year <- points_nb %>% filter(year==year_f)
  
  # Circles of 100m
  number100 <- st_contains(poly_100,points_year)
  parcels[,paste("newc_100m_", as.character(year_f), sep="")] <- lengths(number100)
  
  # Circles of 500m
  number500 <- st_contains(poly_500,points_year)
  parcels[,paste("newc_500m_", as.character(year_f), sep="")] <- lengths(number500)-lengths(number100)
  
  # Circles of 1000m
  number1000 <- st_contains(poly_1000,points_year)
  parcels[,paste("newc_1000m_", as.character(year_f), sep="")] <- lengths(number1000)-lengths(number500)
  
  # Circles of 2000m
  number2000 <- st_contains(poly_2000,points_year)
  parcels[,paste("newc_2000m_", as.character(year_f), sep="")] <- lengths(number2000)-lengths(number1000)
  save.image("~/Documents/Polimi/Non Parametric/temp.RData")

}

#distanza da Financial District e Caltrain Station

centroid_point <- st_as_sf(parcels[,7:8], coords = c(x = "Centroid_Long", y = "Centroid_Lat"), crs = 4326)

lat <- c(37.793157,37.77638)
lon <- c(-122.396575,-122.394444)

poi <- data.frame(lat,lon)

poi <- poi %>%
  as.data.frame %>% 
  st_as_sf(coords = c(x = "lon", y = "lat")) %>%
  st_set_crs(4326)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addCircles(data = poi,
             opacity = 0.7, color = "#222", weight = 3,
  )

fd_distance <- st_distance(x = centroid_point, y = poi[1,])
cs_distance <- st_distance(centroid_point, poi[2,])

parcels[,"FinDistr_distance"] <- as.numeric(gsub(" [m]", "", fd_distance))
parcels[,"CaltStat_distance"] <- as.numeric(gsub(" [m]", "", cs_distance))

#distanza da google bus stop più vicina

gb <- read_csv("Documents/Polimi/Non Parametric/google_busses.csv")
gb_points <- gb %>%
  as.data.frame %>% 
  st_as_sf(coords = c(x = "lon", y = "lat")) %>%
  st_set_crs(4326)

dist <- st_distance(centroid_point, gb_points)
min_dist <- apply(dist,1,min)
parcels[,'MinDistGB'] <- min_dist

write.csv(parcels,"/Users/saratonazzi/Documents/Polimi/Non Parametric/Parcels_augmented_complete.csv", row.names = FALSE)
  
#aggiungo centroidi con comando da libreria sf a parcel --> calcolo nh dei centroidi !!
  
Parcels_poly <- read_csv("Documents/Polimi/Non Parametric/Parcels_poly.csv")
SFNeighborhoods <- read_sf("Documents/Polimi/Non Parametric/SFNeighborhoods_new.geojson")
#Parcels_poly <- Parcels_poly[Parcels_poly$STREET!="UNKNOWN",c(1,3)]
#parcels <- read_csv("Documents/Polimi/Non Parametric/Parcels_new.csv")

poly <- st_as_sfc(Parcels_poly$the_geom)
centroids <- st_centroid(poly, of_largest_polygon=FALSE)

for(i in 1:136852) {
  Parcels_poly[i,"Centroid_Long"] <- centroids[[i]][1]
  Parcels_poly[i,"Centroid_Lat"] <- centroids[[i]][2]
}

points <- Parcels_poly[,8:9] %>%
  as.data.frame %>% 
  st_as_sf(coords = c(x = "Centroid_Long", y = "Centroid_Lat")) %>%
  st_set_crs(4326)

poly_nh <- st_transform(SFNeighborhoods$geometry, 4326)
Parcels_poly$number <- st_within(points, poly_nh)

check <- mapply(function(x) !identical(x, integer(0)), Parcels_poly$number)
Parcels_poly <- Parcels_poly[check,]

Parcels_poly$neighborhoods <- mapply(function(x) SFNeighborhoods[[1]][[x]], Parcels_poly$number)
Parcels_poly <- Parcels_poly[,-10]
Parcels_polygons <- Parcels_poly[,c(1:2,8:9)]
Parcels_augmented <- Parcels_poly[,-2]
write.csv(Parcels_augmented,"/Users/saratonazzi/Documents/Polimi/Non Parametric/Parcels_augmented.csv", row.names = FALSE)
write.csv(Parcels_polygons,"/Users/saratonazzi/Documents/Polimi/Non Parametric/Parcels_polygons.csv", row.names = FALSE)               


# 
# 
# #                          CALCULATION OF CENTROID OF THE PARCELS
# 
# 
# for( i in 1:length(Parcels[,1])) {       #estraggo coordinate, codice lento da eseguire, non fatelo
#   temp <- strsplit(Parcels[i,3], ", ")
#   for (j in 1:length(temp[[1]])){
#     Parcels[i,16+j] <- temp[[1]][j]
#   }
# }
# 
# 
# #HP CONSIDERO UN MASSIMO DI 50 PUNTI => TENGO SOLO 67 COLONNE DI "PARCELS"
# 
# Parcels <- Parcels[,-c(50:2941)]
# Complete_Parcels_not_to_modify <- Parcels
# 
# 
# 
# for( i in 1:length(Parcels[,1])) {        # sistemo il primo punto che ha anche "MULTIPOLYGON..." da togliere
#   temp <- strsplit(Parcels[i,17], "\\(" )
#   Parcels[i,17] <- temp[[1]][4]
# }
# 
# 
# j = 18
# for( i in 1:length(Parcels[,1])) {   #sistemo l'ultimo punto che contiene ")))" da togliere length(Parcels[,1])
#   while( j < length(Parcels[1,])) { 
#     if(!(is.na(Parcels[i,j])))  {
#       j = j + 1
#     }
#     
#     else {
#       temp = strsplit(Parcels[i,j-1], "\\)" )
#       Parcels[i,j-1] = temp[[1]][1]
#       j = length(Parcels[1,]) + 1
#     } 
#     
#     
#   }
#   j = 18
# }
# 
# 
# Parcels <- Parcels_prova
# 
# 
# Parcels_prova[90948,32] = "-122.41039847930617 37.726766468046975" #Fix di errori già presenti nella creazione del dataset
# Parcels_prova[90948,33] = "-122.41056570544711 37.72627503015284"
# Parcels_prova[105516,23] = "-122.43679266986896 37.73394341984941"
# Parcels_prova[105516,24] = "-122.43757072924244 37.7340980056725"
# 
# 
# 
# sum_long = 0
# sum_lat = 0
# counter = 0
# j = 17
# 
# for( i in 1:length(Parcels[,1])) {              #poichè le coordinate sono molto vicine tra loro le considero cartesiane e faccio una media campionaria
#   while (j < length(Parcels[1,])-1){
#     if(!(is.na(Parcels_prova[i,j]))) {
#       temp <- strsplit(Parcels_prova[i,j], " " )
#       sum_long = sum_long + as.numeric(temp[[1]][1])
#       sum_lat = sum_lat + as.numeric(temp[[1]][2])
#       counter = counter + 1
#       j = j+1
#     }
#     else {
#       j = 2942   #se trovo anche solo un NA, ho finito di guardare quella riga, quindi esco dal ciclo sulle colonne
#     }
#   }
#   
#   Parcels_prova$Centroid_Lat[i] = sum_lat/counter
#   Parcels_prova$Centroid_Long[i] = sum_long/counter
#   sum_lat = 0
#   sum_long = 0
#   counter = 0
#   j = 17
# }
# 
# 
# 
# CENTROID_LAT =  Parcels_prova$Centroid_Lat
# CENTROID_LONG =  Parcels_prova$Centroid_Long
# 
# flag = 0
# j=1
# index=0
# for (i in 1:length(CENTROID_LONG)){
#   if((is.na(CENTROID_LAT[i]))) {
#     index[j]=i
#     j=j+1
#     flag = flag+1           #controllo se ci sono degli NA e dove. Se flag=0 allora è tutto giusto
#   }
# }
# 
# options(digits=9)

#Parcels_final = Parcels[,-c(2,4:6,12:16)]   #tolgo colonne che non servono
# save(Parcels_final, file = 'Parcels_finall.RData')
# write.csv(Parcels_final,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Parcels_final.csv")
# Parcels_final <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Parcels_final.csv", header=TRUE)
#Parcels_final <- Parcels_final[,-c(1:6,11:16)]
#write.csv(Parcels_final,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Parcels_poly.csv",row.names=FALSE)

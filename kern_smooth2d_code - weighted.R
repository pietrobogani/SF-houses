library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)
library(magrittr)
library(KernSmooth)
library(readr)
library(sf)
library(rgl)
library(lattice)


geo = read_sf('SFNeighborhoods_new.geojson')
#rent_yearly <- read_csv("rent_nhood_yearly_nh.csv") #me lo costruisco io, non dovrebbe servire
Parcels_final_nh <- read_csv("Parcels_final_nh.csv")
rent_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean_nh.csv", header=TRUE)


#---I want to create a new rent_nhood_yearly s.t. the observations with(lat,long) are separated from the rest

#I'll exclude rent observations that have wrong coordinates
ind_lat = which(is.na(rent_clean$lat) == F)
ind_lon = which(is.na(rent_clean$lon) == F)
ind_loc = intersect(ind_lon,ind_lat) #653 osservazioni
range_lon = range(Parcels_final_nh$Centroid_Long)
range_lat = range(Parcels_final_nh$Centroid_Lat)
ind_lon_ok = which(rent_clean$lon > range_lon[1] & rent_clean$lon < range_lon[2])
ind_lat_ok = which(rent_clean$lat > range_lat[1] & rent_clean$lat < range_lat[2])
ind_loc_ok = intersect(ind_loc,intersect(ind_lon_ok,ind_lat_ok)) #614 osservazioni
rent_geoloc <- rent_clean[ind_loc_ok,]#Qui ho le osservazioni di rent geoloc e che sono corrette

index <- which(!(is.na(rent_clean$lat)) & !(is.na(rent_clean$lon)))
rent_clean <- rent_clean[-index,] #Lascio in rent_clean solo le osservazioni non geolocalizzate

vect_year = paste(rent_clean$year)
vect_nhood = paste(rent_clean$nhood)
vect_aus2 = paste(vect_year,vect_nhood)
rent_clean$year_nhood = vect_aus2
rm(vect_aus2,vect_year,vect_nhood)


length(unique(rent_clean$year_nhood))
rent_nhood_yearly = aggregate(rent_clean$price_mq, by = list(rent_clean$year_nhood), FUN = mean)
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'x'] <- 'avg_rent.sq'
rent_yearly <- rent_nhood_yearly #Così uso lo stesso nome che usa il codice di tom da qui in avanti
rm(rent_nhood_yearly)

#Sistemo gli address di rent_yearly
{
for (i in 1:length(rent_yearly[,1])) {   #preparo l'address
  temp <- strsplit(rent_yearly[i,1], " ")
  for (j in 1:length(temp[[1]])){
    rent_yearly[i,2+j] <- temp[[1]][j]
  }
}


for( i in 1:length(rent_yearly[,1])){  #preparo l'address
  j = 5
  while (j < 8) {
    if(!(is.na(rent_yearly[i,j]))) {
      rent_yearly[i,4] = paste(rent_yearly[i,4], rent_yearly[i,j], sep=" ")
      j = j+1
    }
    else
      j = 13
    
  }
}
rent_yearly <- rent_yearly[, -c(1,5:11)]
colnames(rent_yearly) <- c( 'avg_rent.mq','year', 'nhood')
rent_yearly <- rent_yearly[-which(rent_yearly$nhood == 'San Francisco'),]
} 

#Aggiungo a rent_yearly le coordinate dei nhood
{rent_yearly$lat = rep(0,dim(rent_yearly)[1])
rent_yearly$lon = rep(0,dim(rent_yearly)[1])
list_nhood = unique(rent_yearly$nhood)
for(nh in list_nhood){
  ind_nh_rent = which(rent_yearly$nhood == nh)
  ind_nh_geo = which(geo$nhood == nh)
  if(length(ind_nh_geo) > 0 & length(ind_nh_rent) > 0){
    rent_yearly[ind_nh_rent,]$lon = geo[ind_nh_geo,]$lon
    rent_yearly[ind_nh_rent,]$lat = geo[ind_nh_geo,]$lat
  }
}
ind_treasure_isl = which(rent_yearly$nhood == 'Treasure Island')
rent_yearly = rent_yearly[-ind_treasure_isl,]
}

#Unisco i rent singoli e le medie nei nhood
rent_geoloc <- as.data.frame(cbind(rent_geoloc$price_mq,rent_geoloc$year,rent_geoloc$nhood,rent_geoloc$lat,rent_geoloc$lon))
colnames(rent_geoloc) <- c( 'avg_rent.mq','year', 'nhood','lat','lon')
rent_smooth <- as.data.frame(rbind(rent_yearly,rent_geoloc))
rent_yearly <-rent_smooth
rm(rent_smooth)

rent_yearly$lat = as.numeric(rent_yearly$lat)
rent_yearly$lon = as.numeric(rent_yearly$lon)
rent_yearly$avg_rent.mq = as.numeric(rent_yearly$avg_rent.mq)
#Provo a plottare i dati dei prezzi in 3d- NON FUNZIONA
library(rgl)
plot3d(rent_yearly$lon, rent_yearly$lat,  rent_yearly$avg_rent.mq, 
       xlim = as.numeric(range(rent_yearly$lon)) , ylim = as.numeric(range(rent_yearly$lat)), col = as.factor(rent_yearly$year))


#Costruisco uno smoothing per un anno fissato come prova
year = 2015
ind_year = which(rent_yearly$year == 2015)
# m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
#               y = rent_yearly[ind_year,]$lon,
#               z = rent_yearly[ind_year,]$avg_rent.mq,
#               n = 15)
m_loc = npreg(avg_rent.mq  ~ lat + lon,
              ckertype = 'gaussian', #or gaussian, epanechnikov
              bws = c(0.007,0.007), # bandwidth
              data = rent_yearly[ind_year,]
              )


#Valuto sulle parcel
parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
parcels$price = predict(m_loc, newdata = parcels)

#Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
colormap <- colorRampPalette(c( 'yellow','red'))
parcels = parcels[order(parcels$price),]
plot3d(parcels$lon, parcels$lat, parcels$price, 
       xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))


#Valuto sul quadrato corrispondente a sf 
grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
mesh_coord$price = predict(m_loc, newdata = mesh_coord)
price = predict(m_loc, newdata = mesh_coord)
mesh_coord = mesh_coord[order(mesh_coord$price),]
#Plot della superficie ma senza gradiente dei colori
#Non riesco a mettere i colori perchÃ¨ riordinando i prezzi, si scombinano le corrispondenze
# con grid_lon e grid_lat! L'unica soluzione che ho trovato Ã¨ quella di riunire coordinate
# e prezzi in un unico dataset (ie mesh_coord) e plottare tutti i punti
persp3d(grid_lon,grid_lat, price, smooth = F, col = 'red', alpha = 0.5)
plot3d(parcels$lon,parcels$lat,min(parcels$price), add = T, size = 0.1)
plot3d(parcels$lon,parcels$lat,parcels$price, add = T, size = 0.1, alpha = 0.3)
#Plot della griglia fitta con gradiente colori
plot3d(mesh_coord$lon,mesh_coord$lat, mesh_coord$price, smooth = F, col = heat.colors(dim(mesh_coord)[1]), alpha = 0.5,cex = 2)
plot3d(parcels$lon,parcels$lat,min(parcels$price), add = T, size = 0.1)
plot3d(parcels$lon,parcels$lat,parcels$price, add = T, size = 0.5, alpha = 0.3)


#Plot in 2d con gradiente colori
library(lattice)
x11()
levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
          colorkey = T,
          xlab = "Longitude", ylab = "Latitude", zlab = "Price",
          aspect = 1,
          col.regions = heat.colors(rev = T, n = 60),
          at = seq(10,70,1.5),
          panel = function(...) {
            panel.levelplot(...)
            panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
          }, 
          main = 'Prices smoothed over SF')







#----- NOW I'D LIKE TO THE SAME BUT WITH DIFFERENT WEIGHT. E.G. IF THE MEAN IN 2015 IN FINANCIAL DISTRICT IS
#      BUILT WITH 100 OBSERVATIONS, THE WEIGHT WILL BE 100 => I'LL PUT 100 TIMES THAT OBSERVATION IN THE DATASET

geo = read_sf('SFNeighborhoods_new.geojson')
#rent_yearly <- read_csv("rent_nhood_yearly_nh.csv") #me lo costruisco io, non dovrebbe servire
Parcels_final_nh <- read_csv("Parcels_final_nh.csv")
rent_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean_nh.csv", header=TRUE)

#---I want to create a new rent_nhood_yearly s.t. the observations with(lat,long) are separated from the rest

#I'll exclude rent observations that have wrong coordinates
ind_lat = which(is.na(rent_clean$lat) == F)
ind_lon = which(is.na(rent_clean$lon) == F)
ind_loc = intersect(ind_lon,ind_lat) #653 osservazioni
range_lon = range(Parcels_final_nh$Centroid_Long)
range_lat = range(Parcels_final_nh$Centroid_Lat)
ind_lon_ok = which(rent_clean$lon > range_lon[1] & rent_clean$lon < range_lon[2])
ind_lat_ok = which(rent_clean$lat > range_lat[1] & rent_clean$lat < range_lat[2])
ind_loc_ok = intersect(ind_loc,intersect(ind_lon_ok,ind_lat_ok)) #614 osservazioni
rent_geoloc <- rent_clean[ind_loc_ok,]#Qui ho le osservazioni di rent geoloc e che sono corrette

index <- which(!(is.na(rent_clean$lat)) & !(is.na(rent_clean$lon)))
rent_clean <- rent_clean[-index,] #Lascio in rent_clean solo le osservazioni non geolocalizzate
rent_clean <- rent_clean[-which(rent_clean$nhood == 'San Francisco'),]
rent_clean <- rent_clean[-which(rent_clean$nhood == 'Treasure Island'),]
vect_year = paste(rent_clean$year)
vect_nhood = paste(rent_clean$nhood)
vect_aus2 = paste(vect_year,vect_nhood)
rent_clean$year_nhood = vect_aus2
rent_clean$dummy = 1 #Così poi posso contare su quanti sto facendo la media
rm(vect_aus2,vect_year,vect_nhood)


length(unique(rent_clean$year_nhood))
rent_nhood_yearly = aggregate(rent_clean$price_mq, by = list(rent_clean$year_nhood), FUN = mean)
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'x'] <- 'avg_rent.sq'
rent_yearly <- rent_nhood_yearly #Così uso lo stesso nome che usa il codice di tom da qui in avanti


#Sistemo gli address di rent_yearly
{
  for (i in 1:length(rent_yearly[,1])) {   #preparo l'address
    temp <- strsplit(rent_yearly[i,1], " ")
    for (j in 1:length(temp[[1]])){
      rent_yearly[i,2+j] <- temp[[1]][j]
    }
  }
  
  
  for( i in 1:length(rent_yearly[,1])){  #preparo l'address
    j = 5
    while (j < 8) {
      if(!(is.na(rent_yearly[i,j]))) {
        rent_yearly[i,4] = paste(rent_yearly[i,4], rent_yearly[i,j], sep=" ")
        j = j+1
      }
      else
        j = 13
      
    }
  }
  rent_yearly <- rent_yearly[, -c(1,5:11)]
  colnames(rent_yearly) <- c( 'avg_rent.mq','year', 'nhood')
} 

#Aggiungo a rent_yearly le coordinate dei nhood
{rent_yearly$lat = rep(0,dim(rent_yearly)[1])
  rent_yearly$lon = rep(0,dim(rent_yearly)[1])
  list_nhood = unique(rent_yearly$nhood)
  for(nh in list_nhood){
    ind_nh_rent = which(rent_yearly$nhood == nh)
    ind_nh_geo = which(geo$nhood == nh)
    if(length(ind_nh_geo) > 0 & length(ind_nh_rent) > 0){
      rent_yearly[ind_nh_rent,]$lon = geo[ind_nh_geo,]$lon
      rent_yearly[ind_nh_rent,]$lat = geo[ind_nh_geo,]$lat
    }
  }
}

rent_yearly$count = (aggregate(rent_clean$dummy, by = list(rent_clean$year_nhood), FUN = sum))[,2] #Qui dentro conto con quanti elementi ho fatto la media


library(splitstackshape)
library(data.table)
rent_yearly <- setDT(expandRows(rent_yearly, "count"))[,][] #Così replico ogni colonna tanto volte quanto il valore di count

#Unisco i rent singoli e le medie nei nhood
rent_geoloc <- as.data.frame(cbind(rent_geoloc$price_mq,rent_geoloc$year,rent_geoloc$nhood,rent_geoloc$lat,rent_geoloc$lon))
colnames(rent_geoloc) <- c( 'avg_rent.mq','year', 'nhood','lat','lon')
rent_smooth <- as.data.frame(rbind(rent_yearly,rent_geoloc))
rent_yearly <-rent_smooth
rm(rent_smooth)

rent_yearly$lat = as.numeric(rent_yearly$lat)
rent_yearly$lon = as.numeric(rent_yearly$lon)
rent_yearly$avg_rent.mq = as.numeric(rent_yearly$avg_rent.mq)


#-------------Costruisco uno smoothing per ogni anno
{
  year = 2011
  ind_year = which(rent_yearly$year == 2011)
  # m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
  #               y = rent_yearly[ind_year,]$lon,
  #               z = rent_yearly[ind_year,]$avg_rent.mq,
  #               n = 15)
  m_loc = npreg(avg_rent.mq  ~ lat + lon,
                ckertype = 'gaussian', #or gaussian, epanechnikov
                bws = c(0.007,0.007), # bandwidth
                data = rent_yearly[ind_year,]
  )
  
  
  #Valuto sulle parcel
  parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
  parcels$price = predict(m_loc, newdata = parcels)
  
  
  #Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
  colormap <- colorRampPalette(c( 'yellow','red'))
  parcels = parcels[order(parcels$price),]
  plot3d(parcels$lon, parcels$lat, parcels$price, 
         xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))
  
  #Plot in 2d con gradiente colori
  grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
  grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
  mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
  mesh_coord$price = predict(m_loc, newdata = mesh_coord)
  price = predict(m_loc, newdata = mesh_coord)
  mesh_coord = mesh_coord[order(mesh_coord$price),]
  
  library(lattice)
  x11()
  levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
            colorkey = T,
            xlab = "Longitude", ylab = "Latitude", zlab = "Price",
            aspect = 1,
            col.regions = heat.colors(rev = T, n = 60),
            at = seq(10,70,1.5),
            panel = function(...) {
              panel.levelplot(...)
              panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
            }, 
            main = '2011')
} #2011
{
  year = 2012
  ind_year = which(rent_yearly$year == 2012)
  # m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
  #               y = rent_yearly[ind_year,]$lon,
  #               z = rent_yearly[ind_year,]$avg_rent.mq,
  #               n = 15)
  m_loc = npreg(avg_rent.mq  ~ lat + lon,
                ckertype = 'gaussian', #or gaussian, epanechnikov
                bws = c(0.007,0.007), # bandwidth
                data = rent_yearly[ind_year,]
  )
  
  
  #Valuto sulle parcel
  parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
  parcels$price = predict(m_loc, newdata = parcels)
  
  
  #Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
  colormap <- colorRampPalette(c( 'yellow','red'))
  parcels = parcels[order(parcels$price),]
  plot3d(parcels$lon, parcels$lat, parcels$price, 
         xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))
  
  
  #Plot in 2d con gradiente colori
  grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
  grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
  mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
  mesh_coord$price = predict(m_loc, newdata = mesh_coord)
  price = predict(m_loc, newdata = mesh_coord)
  mesh_coord = mesh_coord[order(mesh_coord$price),]
  library(lattice)
  x11()
  levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
            colorkey = T,
            xlab = "Longitude", ylab = "Latitude", zlab = "Price",
            aspect = 1,
            col.regions = heat.colors(rev = T, n = 60),
            at = seq(10,70,1.5),
            panel = function(...) {
              panel.levelplot(...)
              panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
            }, 
            main = '2012')
} #2012
{
  year = 2013
  ind_year = which(rent_yearly$year == 2013)
  # m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
  #               y = rent_yearly[ind_year,]$lon,
  #               z = rent_yearly[ind_year,]$avg_rent.mq,
  #               n = 15)
  m_loc = npreg(avg_rent.mq  ~ lat + lon,
                ckertype = 'gaussian', #or gaussian, epanechnikov
                bws = c(0.007,0.007), # bandwidth
                data = rent_yearly[ind_year,]
  )
  
  
  #Valuto sulle parcel
  parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
  parcels$price = predict(m_loc, newdata = parcels)
  
  
  #Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
  colormap <- colorRampPalette(c( 'yellow','red'))
  parcels = parcels[order(parcels$price),]
  plot3d(parcels$lon, parcels$lat, parcels$price, 
         xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))
  
  #Plot in 2d con gradiente colori
  library(lattice)
  grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
  grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
  mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
  mesh_coord$price = predict(m_loc, newdata = mesh_coord)
  price = predict(m_loc, newdata = mesh_coord)
  mesh_coord = mesh_coord[order(mesh_coord$price),]
  x11()
  levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
            colorkey = T,
            xlab = "Longitude", ylab = "Latitude", zlab = "Price",
            aspect = 1,
            col.regions = heat.colors(rev = T, n = 60),
            at = seq(10,70,1.5),
            panel = function(...) {
              panel.levelplot(...)
              panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
            }, 
            main = '2013')
} #2013
{
  year = 2014
  ind_year = which(rent_yearly$year == 2014)
  # m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
  #               y = rent_yearly[ind_year,]$lon,
  #               z = rent_yearly[ind_year,]$avg_rent.mq,
  #               n = 15)
  m_loc = npreg(avg_rent.mq  ~ lat + lon,
                ckertype = 'gaussian', #or gaussian, epanechnikov
                bws = c(0.007,0.007), # bandwidth
                data = rent_yearly[ind_year,]
  )
  
  
  #Valuto sulle parcel
  parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
  parcels$price = predict(m_loc, newdata = parcels)
  
  
  #Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
  colormap <- colorRampPalette(c( 'yellow','red'))
  parcels = parcels[order(parcels$price),]
  plot3d(parcels$lon, parcels$lat, parcels$price, 
         xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))
  
  #Plot in 2d con gradiente colori
  library(lattice)
  grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
  grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
  mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
  mesh_coord$price = predict(m_loc, newdata = mesh_coord)
  price = predict(m_loc, newdata = mesh_coord)
  mesh_coord = mesh_coord[order(mesh_coord$price),]
  x11()
  levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
            colorkey = T,
            xlab = "Longitude", ylab = "Latitude", zlab = "Price",
            aspect = 1,
            col.regions = heat.colors(rev = T, n = 60),
            at = seq(10,70,1.5),
            panel = function(...) {
              panel.levelplot(...)
              panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
            }, 
            main = '2014')
} #2014
{
year = 2015
ind_year = which(rent_yearly$year == 2015)
# m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
#               y = rent_yearly[ind_year,]$lon,
#               z = rent_yearly[ind_year,]$avg_rent.mq,
#               n = 15)
m_loc = npreg(avg_rent.mq  ~ lat + lon,
              ckertype = 'gaussian', #or gaussian, epanechnikov
              bws = c(0.007,0.007), # bandwidth
              data = rent_yearly[ind_year,]
)


#Valuto sulle parcel
parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
parcels$price = predict(m_loc, newdata = parcels)

#Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
colormap <- colorRampPalette(c( 'yellow','red'))
parcels = parcels[order(parcels$price),]
plot3d(parcels$lon, parcels$lat, parcels$price, 
       xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))

#Plot in 2d con gradiente colori
library(lattice)
grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
mesh_coord$price = predict(m_loc, newdata = mesh_coord)
price = predict(m_loc, newdata = mesh_coord)
mesh_coord = mesh_coord[order(mesh_coord$price),]
x11()
levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
          colorkey = T,
          xlab = "Longitude", ylab = "Latitude", zlab = "Price",
          aspect = 1,
          col.regions = heat.colors(rev = T, n = 60),
          at = seq(10,70,1.5),
          panel = function(...) {
            panel.levelplot(...)
            panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
          }, 
          main = '2015')
} #2015
{
  year = 2016
  ind_year = which(rent_yearly$year == 2016)
  # m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
  #               y = rent_yearly[ind_year,]$lon,
  #               z = rent_yearly[ind_year,]$avg_rent.mq,
  #               n = 15)
  m_loc = npreg(avg_rent.mq  ~ lat + lon,
                ckertype = 'gaussian', #or gaussian, epanechnikov
                bws = c(0.007,0.007), # bandwidth
                data = rent_yearly[ind_year,]
  )
  
  
  #Valuto sulle parcel
  parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
  parcels$price = predict(m_loc, newdata = parcels)
  
  #Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
  colormap <- colorRampPalette(c( 'yellow','red'))
  parcels = parcels[order(parcels$price),]
  plot3d(parcels$lon, parcels$lat, parcels$price, 
         xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))
  
  #Plot in 2d con gradiente colori
  library(lattice)
  grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
  grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
  mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
  mesh_coord$price = predict(m_loc, newdata = mesh_coord)
  price = predict(m_loc, newdata = mesh_coord)
  mesh_coord = mesh_coord[order(mesh_coord$price),]
  x11()
  levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
            colorkey = T,
            xlab = "Longitude", ylab = "Latitude", zlab = "Price",
            aspect = 1,
            col.regions = heat.colors(rev = T, n = 60),
            at = seq(10,70,1.5),
            panel = function(...) {
              panel.levelplot(...)
              panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
            }, 
            main = '2016')
} #2016
{
  year = 2017
  ind_year = which(rent_yearly$year == 2017)
  # m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
  #               y = rent_yearly[ind_year,]$lon,
  #               z = rent_yearly[ind_year,]$avg_rent.mq,
  #               n = 15)
  m_loc = npreg(avg_rent.mq  ~ lat + lon,
                ckertype = 'gaussian', #or gaussian, epanechnikov
                bws = c(0.007,0.007), # bandwidth
                data = rent_yearly[ind_year,]
  )
  
  
  #Valuto sulle parcel
  parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
  parcels$price = predict(m_loc, newdata = parcels)
  
  #Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
  colormap <- colorRampPalette(c( 'yellow','red'))
  parcels = parcels[order(parcels$price),]
  plot3d(parcels$lon, parcels$lat, parcels$price, 
         xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))
  
  #Plot in 2d con gradiente colori
  library(lattice)
  grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
  grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
  mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
  mesh_coord$price = predict(m_loc, newdata = mesh_coord)
  price = predict(m_loc, newdata = mesh_coord)
  mesh_coord = mesh_coord[order(mesh_coord$price),]
  x11()
  levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
            colorkey = T,
            xlab = "Longitude", ylab = "Latitude", zlab = "Price",
            aspect = 1,
            col.regions = heat.colors(rev = T, n = 60),
            at = seq(10,70,1.5),
            panel = function(...) {
              panel.levelplot(...)
              panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
            }, 
            main = '2017')
} #2017
{
  year = 2018
  ind_year = which(rent_yearly$year == 2018)
  # m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
  #               y = rent_yearly[ind_year,]$lon,
  #               z = rent_yearly[ind_year,]$avg_rent.mq,
  #               n = 15)
  m_loc = npreg(avg_rent.mq  ~ lat + lon,
                ckertype = 'gaussian', #or gaussian, epanechnikov
                bws = c(0.007,0.007), # bandwidth
                data = rent_yearly[ind_year,]
  )
  
  
  #Valuto sulle parcel
  parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
  parcels$price = predict(m_loc, newdata = parcels)
  
  #Plotto i prezzi sulle parcel ( non sono ancora riuscito a mettere i colori)
  colormap <- colorRampPalette(c( 'yellow','red'))
  parcels = parcels[order(parcels$price),]
  plot3d(parcels$lon, parcels$lat, parcels$price, 
         xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))
  
  #Plot in 2d con gradiente colori
  library(lattice)
  grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
  grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
  mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
  mesh_coord$price = predict(m_loc, newdata = mesh_coord)
  price = predict(m_loc, newdata = mesh_coord)
  mesh_coord = mesh_coord[order(mesh_coord$price),]
  x11()
  levelplot(mesh_coord$price ~ mesh_coord$lon * mesh_coord$lat, 
            colorkey = T,
            xlab = "Longitude", ylab = "Latitude", zlab = "Price",
            aspect = 1,
            col.regions = heat.colors(rev = T, n = 60),
            at = seq(10,70,1.5),
            panel = function(...) {
              panel.levelplot(...)
              panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
            }, 
            main = '2018')
} #2018







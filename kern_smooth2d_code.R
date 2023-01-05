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
rent_yearly <- read_csv("rent_nhood_yearly_nh.csv")
Parcels_final_nh <- read_csv("Parcels_final_nh.csv")


#La parte commentata qua è stata già messa nella creazione del dataset!
# #Aggiungo a geo lat e long dei nhood (in due colonne e non unite!)
# geo$lon = rep(0,dim(geo)[1])
# geo$lat = rep(0,dim(geo)[1])
# for(i in 1:dim(geo)[1]){
#   point = st_centroid(geo[i,]$geometry)
#   geo[i,]$lon = point[[1]][1]
#   geo[i,]$lat = point[[1]][2]
# }


#Aggiungo a rent_yearly le coordinate dei nhood
rent_yearly$lat = rep(0,dim(rent_yearly)[1])
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


#Provo a plottare i dati dei prezzi in 3d
library(rgl)
plot3d(rent_yearly$lon, rent_yearly$lat,  rent_yearly$avg_rent.mq, 
       xlim = range(rent_yearly$lon) , ylim = range(rent_yearly$lat), col = as.factor(rent_yearly$year))


#Costruisco uno smoothing per ogni anno
year = 2015
ind_year = which(rent_yearly$year == 2015)
# m_loc = kde2d(x = rent_yearly[ind_year,]$lat,
#               y = rent_yearly[ind_year,]$lon,
#               z = rent_yearly[ind_year,]$avg_rent.mq,
#               n = 15)
m_loc = npreg(avg_rent.mq  ~ lat + lon,
              ckertype = 'gaussian', #or gaussian, epanechnikov
              bws = c(0.009,0.009), # bandwidth
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
grid_lat = seq(range(parcels$lat), length.out = 100)
grid_lon = seq(range(parcels$lon), length.out = 100)
mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
mesh_coord$price = predict(m_loc, newdata = mesh_coord)
price = predict(m_loc, newdata = mesh_coord)
mesh_coord = mesh_coord[order(mesh_coord$price),]
#Plot della superficie ma senza gradiente dei colori
#Non riesco a mettere i colori perchè riordinando i prezzi, si scombinano le corrispondenze
# con grid_lon e grid_lat! L'unica soluzione che ho trovato è quella di riunire coordinate
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



#Procedo ora a farlo per ogni anno ############################################# 
rm(list = ls())

#Importo i dataset e creo le varie griglie necessarie
geo = read_sf('SFNeighborhoods_new.geojson')
rent_yearly <- read_csv("rent_nhood_yearly_nh.csv")
Parcels_final_nh <- read_csv("Parcels_final_nh.csv")
parcels = data.frame(lat = Parcels_final_nh$Centroid_Lat, lon = Parcels_final_nh$Centroid_Long)
#Aggiungo a rent_yearly le coordinate dei nhood
rent_yearly$lat = rep(0,dim(rent_yearly)[1])
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
rm(nh,list_nhood,ind_nh_geo,ind_nh_rent,ind_treasure_isl)
grid_lat = seq(range(parcels$lat)[1],range(parcels$lat)[2], length.out = 100)
grid_lon = seq(range(parcels$lon)[1],range(parcels$lon)[2], length.out = 100)
mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
#Inizializzo 8 colonne vuote in parcels e mesh_coord per mettere poi i prezzi
mesh_coord$price_2011 = rep(0,dim(mesh_coord)[1])
mesh_coord$price_2012 = rep(0,dim(mesh_coord)[1])
mesh_coord$price_2013 = rep(0,dim(mesh_coord)[1])
mesh_coord$price_2014 = rep(0,dim(mesh_coord)[1])
mesh_coord$price_2015 = rep(0,dim(mesh_coord)[1])
mesh_coord$price_2016 = rep(0,dim(mesh_coord)[1])
mesh_coord$price_2017 = rep(0,dim(mesh_coord)[1])
mesh_coord$price_2018 = rep(0,dim(mesh_coord)[1])
parcels$price_2011 = rep(0,dim(parcels)[1])
parcels$price_2012 = rep(0,dim(parcels)[1])
parcels$price_2013 = rep(0,dim(parcels)[1])
parcels$price_2014 = rep(0,dim(parcels)[1])
parcels$price_2015 = rep(0,dim(parcels)[1])
parcels$price_2016 = rep(0,dim(parcels)[1])
parcels$price_2017 = rep(0,dim(parcels)[1])
parcels$price_2018 = rep(0,dim(parcels)[1])

#Calcolo il modello per ogni anno e valuto sul parcel e griglia per ciascun anno
years = unique(rent_yearly$year)
for(ind_year in 1:length(years)){
  ind_rent_year = which(rent_yearly$year == years[ind_year])
  m_loc = npreg(avg_rent.mq  ~ lat + lon,
                ckertype = 'gaussian', #or gaussian, epanechnikov
                bws = c(0.009,0.009), # bandwidth
                data = rent_yearly[ind_rent_year,]
  )
  
  mesh_coord[,ind_year+2] = predict(m_loc, newdata = mesh_coord[,c(1,2)])
  parcels[,ind_year+2] = predict(m_loc, newdata = parcels[,c(1,2)])
}
rm(ind_year,ind_rent_year,m_loc)


x11()
for(ind_year in 1:length(years)){
  levelplot(mesh_coord[,ind_year+2] ~ mesh_coord$lon * mesh_coord$lat, 
            colorkey = T,
            xlab = "Longitude", ylab = "Latitude", zlab = "Price",
            aspect = 1,
            col.regions = heat.colors(rev = T, n = 60),
            at = seq(10,70,1.5),
            panel = function(...) {
              panel.levelplot(...)
              panel.points(parcels$lon, parcels$lat, col = 'black', cex = 0.2, alpha = 0.1, pch = 19)
            }, 
            main = paste(years[ind_year]))
}









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


ind_year = 4
years = unique(rent_yearly$year)
ind_rent_year = which(rent_yearly$year == years[ind_year])
length(ind_rent_year)
#Provo con thin plate spline basis 
model_gam=gam(avg_rent.mq ~ s(lon,bs='cr') + s(lat,bs='cr')+ s(lon,lat,bs='tp'),data = rent_yearly[ind_rent_year,])
model_gam=gam(avg_rent.mq ~ s(lon,lat,bs='tp'),data = rent_yearly[ind_rent_year,])
#Provo qualcosa di più semplice dato che non ho abbastanza dati ....
model_gam=gam(avg_rent.mq ~ s(lon,bs='cr') + s(lat,bs='cr'),data = rent_yearly[ind_rent_year,])
model_gam=gam(avg_rent.mq ~ s(I(lat*lon), bs = 'cr'),data = rent_yearly[ind_rent_year,])

#Plot dei risultati (spoiler: sono osceni)
mesh_coord[,ind_year+2] = predict(model_gam, newdata = mesh_coord[,c(1,2)])
parcels[,ind_year+2] = predict(model_gam, newdata = parcels[,c(1,2)])

price = predict(model_gam, newdata = mesh_coord[,c(1,2)])
persp3d(grid_lon,grid_lat, price, smooth = F, col = 'red', alpha = 0.5)

colormap <- colorRampPalette(c( 'yellow','red'))
parcels = parcels[order(parcels[,ind_year+2]),]
plot3d(parcels$lon, parcels$lat, parcels[, ind_year+2], 
       xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(dim(parcels)[1]))

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



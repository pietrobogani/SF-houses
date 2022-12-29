library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)
library(magrittr)
library(KernSmooth)
library(readr)
library(sf)
geo = read_sf('SFNeighborhoods_new.geojson')
rent_yearly <- read_csv("rent_nhood_yearly_nh.csv")
Parcels_final_nh <- read_csv("Parcels_final_nh.csv")


#Aggiungo a geo lat e long dei nhood (in due colonne e non unite!)
geo$lon = rep(0,dim(geo)[1])
geo$lat = rep(0,dim(geo)[1])
for(i in 1:dim(geo)[1]){
  point = st_centroid(geo[i,]$geometry)
  geo[i,]$lon = point[[1]][1]
  geo[i,]$lat = point[[1]][2]
}

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
ind_zero = which(rent_yearly$lat == 0)
rent_yearly = rent_yearly[-ind_zero,]
ind_treasure_isl = which(rent_yearly$nhood == 'Treasure Island')
rent_yearly = rent_yearly[-ind_treasure_isl,]


#Provo a plottare i dati dei prezzi in 3d
library(rgl)
x11()
plot3d(rent_yearly$lat, rent_yearly$lon, rent_yearly$avg_rent.mq, 
       xlim = range(rent_yearly$lat) , ylim = range(rent_yearly$lon), col = as.factor(rent_yearly$year))


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
colormap <- colorRampPalette(c("red", "yellow"))
x11()
plot3d(parcels$lon, parcels$lat, parcels$price, 
       xlim = range(parcels$lon) , ylim = range(parcels$lat), col = colormap(length(parcels$price)))


#Valuto sul quadrato corrispondente a sf 
range_lat = range(parcels$lat)
range_lon = range(parcels$lon)
grid_lat = seq(range_lat[1], range_lat[2], 0.0003)
grid_lon = seq(range_lon[1], range_lon[2], 0.0003)


mesh_coord = expand.grid(lon = grid_lon,lat = grid_lat)
price = predict(m_loc, newdata = mesh_coord)
mesh_coord <- mesh_coord[order(mesh_coord$lon, mesh_coord$lat),]
persp3d(mesh_coord$lon,mesh_coord$lat, price)










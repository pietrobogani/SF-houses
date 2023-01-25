library(readr)
library(sf)
geo = read_sf('SFNeighborhoods_new.geojson')
eviction_nhood_monthly <- read_csv("eviction_monthly_nh.csv")
eviction_nhood_monthly = eviction_nhood_monthly[-2251,] #tolgo la riga con il count totale
vect_year = eviction_nhood_monthly$year
vect_month = eviction_nhood_monthly$month
vect_day = rep(1,length(vect_year))
date = paste(vect_year,vect_month,vect_day, sep = '-')
eviction_nhood_monthly$date = date
rm(vect_day,vect_month,vect_year,date)
eviction_nhood_monthly$date = as.Date(eviction_nhood_monthly$date, tryFormats = '%Y-%m-%d')
#typeof(eviction_nhood_monthly$date)
eviction_nhood_monthly$date_num = as.numeric(eviction_nhood_monthly$date)
list_nhood = unique(eviction_nhood_monthly$nhood)
for(nh in list_nhood){
  print(nh)
  print(dim(eviction_nhood_monthly[which(eviction_nhood_monthly$nhood == nh),]))
}
# Rimuovo McLaren Park , Treasure Island e Lincoln Park dato che ho 1 osservazione ...
# Rimuovo anche Golden Gate Park perchè ha una parcel!
nh_multiple_osservations = c('McLaren Park', 'Treasure Island', 'Lincoln Park', 'Golden Gate Park')
for(nh in nh_multiple_osservations){
  ind = which(eviction_nhood_monthly$nhood == nh)
  eviction_nhood_monthly = eviction_nhood_monthly[-ind,]
}
rm(nh,nh_multiple_osservations, ind)
#Aggiungo l'area di ciascun nhood
eviction_nhood_monthly$area = rep(0,dim(eviction_nhood_monthly)[1])
list_nhood = unique(geo$nhood)
for(i in list_nhood){
  ind_eviction_nhood = which(eviction_nhood_monthly$nhood == i) 
  if(length(ind_eviction_nhood) > 0){
    eviction_nhood_monthly[ind_eviction_nhood,]$area = geo[which(geo$nhood == i),]$area
  }
}
#Aggiungo il numero di parcels di ciascun nhood
parcels <- read_csv("Parcels_augmented.csv")
parcels$count = 1
parcels_for_nhood = aggregate(parcels$count , by = list(parcels$neighborhoods), FUN = sum)
list_nhood = unique(parcels_for_nhood$Group.1)
eviction_nhood_monthly$parcels = rep(0,dim(eviction_nhood_monthly)[1])
for(i in list_nhood){
  ind_eviction_nhood = which(eviction_nhood_monthly$nhood == i) 
  if(length(ind_eviction_nhood) > 0){
    eviction_nhood_monthly[ind_eviction_nhood,]$parcels = parcels_for_nhood[which(parcels_for_nhood$Group.1 == i),]$x
  }
}
#Aggiungo il numero di residents units di ciasun nhood
resunits_for_nhood = aggregate(parcels$RESUNITS , by = list(parcels$neighborhoods), FUN = sum)
list_nhood = unique(resunits_for_nhood$Group.1)
eviction_nhood_monthly$resunits = rep(0,dim(eviction_nhood_monthly)[1])
for(i in list_nhood){
  ind_eviction_nhood = which(eviction_nhood_monthly$nhood == i) 
  if(length(ind_eviction_nhood) > 0){
    eviction_nhood_monthly[ind_eviction_nhood,]$resunits = resunits_for_nhood[which(resunits_for_nhood$Group.1 == i),]$x
  }
}

#NB: da qui in poi (per non dover riscrivere tutto il codice!) si avrà:
#    - count = #evictions/area
#    - count_not_norm = #evictions
#    - count_parcels = #evictions/#parcels
#    - count_resunits = #evictions/#resunits


eviction_nhood_monthly$count_not_norm = eviction_nhood_monthly$count
eviction_nhood_monthly$count = eviction_nhood_monthly$count / eviction_nhood_monthly$area
eviction_nhood_monthly$count_parcels = eviction_nhood_monthly$count / eviction_nhood_monthly$parcels
eviction_nhood_monthly$count_resunits = eviction_nhood_monthly$count / eviction_nhood_monthly$resunits

#Plot delle raw evictions (ie non smoothed)
x11()
plot(eviction_nhood_monthly$date,eviction_nhood_monthly$count,
     xlab = 'Year', ylab = 'Number of evictions', main = 'Raw number of evictions')

#Creo modello di kern smoothing e preparo i dataset funzionali
first_date = min(eviction_nhood_monthly$date)
final_date = max(eviction_nhood_monthly$date)
grid_time = seq(first_date,final_date,by = 1)
grid_time_num = data.frame(date_num = as.numeric(grid_time))
funz_evictions = data.frame(row.names = grid_time)
list_nhood = unique(eviction_nhood_monthly$nhood)
for(nh in list_nhood){
  ind_nh = which(eviction_nhood_monthly$nhood == nh)
  data = eviction_nhood_monthly[ind_nh,]
  #bw = npregbw(formula = count ~ date_num, bws = 365*6/12, data = data)
  m_loc = npreg( count ~ date_num,
                 ckertype = 'gaussian', 
                 bws = 356*6/12,  #bw$bw, # bandwidth di 6 mesi oppure bw$bw
                 data = data)
  preds=predict(m_loc,newdata=grid_time_num,se=T)
  se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
  funz_evictions = cbind(funz_evictions,preds$fit)
}
rm(data,nh,ind_nh,m_loc,preds,se.bands)
colnames(funz_evictions) = list_nhood
library(roahd)
funct_data = funz_evictions
funct_data = fData(grid_time,t(funct_data))
#Calcolo derivate prime e creo i dataset
diff_evictions = funz_evictions[2:dim(funz_evictions)[1],] - funz_evictions[1:dim(funz_evictions)[1]-1,]
dim(funz_evictions)
dim(diff_evictions)
funct_data_diff = fData(grid_time[2:length(grid_time)], t(diff_evictions))
#Plot di funzioni e derivate delle evictions
x11()
plot(funct_data, xlab = 'Year', ylab = 'Number of evictions', main = 'Smoothed functions of evictions')
x11()
plot(funct_data_diff, xlab = 'Year', ylab = 'd/dt(Number of evictions)', main = 'Approximation of first derivative')
}
# funz_evictions  : table evictions
# diff_evictions  : table derivate differenze all'indietro evictions
# funct_data      : evictions funzioanli (f_data type)
# funct_data_diff : derivate evictions funzionali (f_data type)

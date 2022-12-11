#Code for GAM model (yearly)

#Preparo dataset con numero di costruzioni per year-nhood ######################

library(readr)
new_constr <- read_csv("New_construction_clean_geocoded_nh.csv")
unique(new_constr$neighborhoods)
unique(new_constr$year) #ho degli NA nelle date da rimuovere dopo!

new_constr$date = as.Date(new_constr$date, tryFormats = '%d/%m/%Y')
aus_df <- data.frame(year = as.numeric(format(new_constr$date, format = "%Y")),
                     month = as.numeric(format(new_constr$date, format = "%m")),
                     day = as.numeric(format(new_constr$date, format = "%d")))
new_constr = cbind(new_constr,aus_df)
rm(aus_df)

vect_nhood = new_constr$neighborhoods
vect_year = new_constr$year
vect_aus = paste(vect_year,vect_nhood,sep = '-')
new_constr$year_nhood = vect_aus
rm(vect_nhood,vect_year,vect_aus)

new_constr = cbind(new_constr, count = rep(1,dim(new_constr)[1]))
num_constr =aggregate(new_constr[,c(11,21)], by = list(new_constr$year_nhood), FUN = sum)
colnames(num_constr)[1] = 'year_nhood'
num_constr
rm(new_constr)
num_constr$year = rep(NA,dim(num_constr)[1])
num_constr$nhood = rep(NA,dim(num_constr)[1])
for(i in 1:dim(num_constr)[1]){
  splitted = strsplit(num_constr[i,]$year_nhood,split = '-')
  num_constr[i,]$year = splitted[[1]][1]
  num_constr[i,]$nhood = splitted[[1]][2]
}
rm(splitted,i)
num_constr = num_constr[-which(num_constr$year == 'NA'),]

#Breve analisi esplorativa su num_constr:
num_constr$units_per_constr = num_constr$new_units_built/num_constr$count
ind = which(num_constr$units_per_constr <= 100)
hist(num_constr$new_units_built)
hist(num_constr$units_per_constr, breaks = c(0,1,5,15,50,100,500),plot = F)
hist(num_constr$units_per_constr, breaks = c(0,1,5,15,50,100,500),plot = T)


#Carico dataset con coords dei nhood e aggiungo le distanze dai punti di interesse
library(sf)
geo = read_sf('SFNeighborhoods_new.geojson')
caltr = c(-122.394724,37.776734) #NB: tutti i dati sono in long-lat
fin_distr = c(-122.396575,37.793157)
geo$lon = rep(0,dim(geo)[1])
geo$lat = rep(0,dim(geo)[1])
geo$dist_caltr = rep(0,dim(geo)[1])
geo$dist_fin = rep(0,dim(geo)[1])
dist <- function(lat1,lat2,lon1,lon2) {
  lat1 = lat1/(180/pi)
  lat2 = lat2/(180/pi)
  lon1 = lon1/(180/pi)
  lon2 = lon2/(180/pi)
  dist = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1))*6371
  return(dist)
}
for(i in 1:dim(geo)[1]){
  point = st_centroid(geo[i,]$geometry)
  geo[i,]$lon = point[[1]][1]
  geo[i,]$lat = point[[1]][2]
  dist_caltr = dist(lat1 = geo[i,]$lat, lon1 = geo[i,]$lon, lat2 = caltr[2], lon2 = caltr[1])
  dist_fin = dist(lat1 = geo[i,]$lat, lon1 = geo[i,]$lon, lat2 = fin_distr[2], lon2 = fin_distr[1])
  geo[i,]$dist_caltr = dist_caltr
  geo[i,]$dist_fin = dist_fin
}


#Aggiungo aree e distanze ai nhood a num_constr
num_constr$area = rep(0,dim(num_constr)[1])
num_constr$dist_caltr = rep(0,dim(num_constr)[1])
num_constr$dist_fin = rep(0,dim(num_constr)[1])
list_nhood = unique(geo$nhood)
for(i in list_nhood){
  ind = which(num_constr$nhood == i) 
  if(length(ind) > 0){
    num_constr[ind,]$area = geo[which(geo$nhood == i),]$area
    num_constr[ind,]$dist_caltr = geo[which(geo$nhood == i),]$dist_caltr
    num_constr[ind,]$dist_fin = geo[which(geo$nhood == i),]$dist_fin
  }
}
rm(dist_caltr,dist_fin,caltr,fin_distr,i,ind,list_nhood,dist,geo)

#Metto tutto dentro rent per poi fare il gam 
rent_clean <- read_csv("rent_clean.csv")
unique(rent_clean$nhood)
unique(num_constr$nhood)

rent_clean$num_constr = rep(0,dim(rent_clean)[1])
rent_clean$num_units = rep(0,dim(rent_clean)[1])
rent_clean$dist_caltr = rep(0,dim(rent_clean)[1])
rent_clean$dist_fin = rep(0,dim(rent_clean)[1])
for(i in 1:dim(num_constr)[1]){
  ind_nhood_year = which(rent_clean$nhood == num_constr[i,]$nhood & rent_clean$year == num_constr[i,]$year)
  rent_clean[ind_nhood_year,]$num_constr = num_constr[i,]$count
  rent_clean[ind_nhood_year,]$num_units = num_constr[i,]$new_units_built
  rent_clean[ind_nhood_year,]$dist_caltr = num_constr[i,]$dist_caltr
  rent_clean[ind_nhood_year,]$dist_fin = num_constr[i,]$dist_fin
}
#rent_clean = rent_clean[,-c(1,2,4,5)] #serve a togliere le colonne che non interessano
rm(num_constr,i,ind_nhood_year)



# GAM model ####################################################################
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)


lin_mod = lm(price_mq ~ nhood:year + num_units , data = rent_clean)
summary(lin_mod)

{#GAM with natural splines
  model_gam_ns <-lm(price_mq ~ ns(num_units, df = 3) + ns(year, df = 3), data = rent_clean)
  summary(model_gam_ns)
  gam::plot.Gam(model_gam_ns, se=TRUE)
  #plot(model_gam_ns$residuals,model_gam$residuals)
  #cor(model_gam_ns$residuals,model_gam$residuals) #to compare the residuals with "GAM with smoothing cubic splines"
}



{#GAM with smoothing cubic splines basis (NOT natural!)
  model_gam=gam(price_mq ~ s(num_units,bs='cr') + nhood + year,data = rent_clean)
  summary(model_gam)
  hist(model_gam$residuals)
  qqnorm(model_gam$residuals)
  qqline(model_gam$residuals,col = 'red', lwd = 2)
  aus = sample(1:length(model_gam$residuals),5000)
  shapiro.test(model_gam$residuals[aus])
  plot(model_gam)
}






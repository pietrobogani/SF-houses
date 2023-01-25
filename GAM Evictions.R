library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(readr)
library(sf)

#Carico datasets

eviction_nhood_yearly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_yearly_nh.csv", header=TRUE)
rent_nhood_yearly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_nhood_yearly_nh.csv", header=TRUE)
new_constr <- read_csv("New_construction_clean_geocoded_nh.csv")
geo = read_sf('SFNeighborhoods_new.geojson')



#Calcolo num. costr. in ogni nhood e in ogni anno

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



#Calcolo la distanza da Caltrain Station e Financial District

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



#Omogeinizzo i nhood tra eviction_nhood_yearly e num_constr e rent_nhood_monthly

unique(eviction_nhood_yearly$nhood)
unique(num_constr$nhood)
unique(rent_nhood_yearly$nhood)
setdiff(unique(rent_nhood_yearly$nhood),unique(num_constr$nhood)) # "Treasure Island"
setdiff(unique(eviction_nhood_yearly$nhood),unique(rent_nhood_yearly$nhood)) #"Chinatown" "Golden Gate Park" "Japantown" "Mission Bay"  "Lincoln Park" "McLaren Park"     "Presidio"
setdiff(unique(eviction_nhood_yearly$nhood),unique(num_constr$nhood)) #"Golden Gate Park" "McLaren Park" "Presidio" "Treasure Island" 

ind1 = which(eviction_nhood_yearly$nhood == 'Treasure Island')
ind2 = which(eviction_nhood_yearly$nhood == 'McLaren Park')
ind3 = which(eviction_nhood_yearly$nhood == 'Golden Gate Park')
ind4 = which(eviction_nhood_yearly$nhood == 'Presidio')
ind5 = which(eviction_nhood_yearly$nhood == 'Lincoln Park')
ind6 = which(eviction_nhood_yearly$nhood == 'Chinatown')
ind7 = which(eviction_nhood_yearly$nhood == 'Japantown')
ind8 = which(eviction_nhood_yearly$nhood == 'Mission Bay')
eviction_nhood_yearly = eviction_nhood_yearly[-c(ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8),]

ind1.1 = which(num_constr$nhood == 'Lincoln Park')
ind2.1 = which(num_constr$nhood == 'Chinatown')
ind3.1 = which(num_constr$nhood == 'Japantown')
ind4.1 = which(num_constr$nhood == 'Mission Bay')
num_constr = num_constr[-c(ind1.1,ind2.1,ind3.1,ind4.1),]

ind1.2 = which(rent_nhood_yearly$nhood == 'Treasure Island')
rent_nhood_yearly = rent_nhood_yearly[-c(ind1.2),]



#Sposto le covariate da num_constr a eviction_nhood_yearly

eviction_nhood_yearly$dist_caltr = rep(0,dim(eviction_nhood_yearly)[1])
eviction_nhood_yearly$dist_fin = rep(0,dim(eviction_nhood_yearly)[1])
eviction_nhood_yearly$area = rep(0,dim(eviction_nhood_yearly)[1])
eviction_nhood_yearly$num_constr = rep(0,dim(eviction_nhood_yearly)[1])
eviction_nhood_yearly$num_units_0 = rep(0,dim(eviction_nhood_yearly)[1])
eviction_nhood_yearly$num_units_1 = rep(0,dim(eviction_nhood_yearly)[1])
eviction_nhood_yearly$num_units_2 = rep(0,dim(eviction_nhood_yearly)[1])
eviction_nhood_yearly$num_units_3 = rep(0,dim(eviction_nhood_yearly)[1])
eviction_nhood_yearly$num_units_4 = rep(0,dim(eviction_nhood_yearly)[1])


list_nhood = unique(num_constr$nhood)
for(nh in list_nhood){                      #Aggiungo area, dist fin. distr. e dist. caltr. stat.
  ind_nhood_constr = which(num_constr$nhood == nh)[1]
  ind_nhood_rent = which(eviction_nhood_yearly$nhood == nh)
  
  if(length(ind_nhood_rent) > 0 ){
    eviction_nhood_yearly[ind_nhood_rent,]$dist_caltr = num_constr[ind_nhood_constr,]$dist_caltr
    eviction_nhood_yearly[ind_nhood_rent,]$dist_fin = num_constr[ind_nhood_constr,]$dist_fin
    eviction_nhood_yearly[ind_nhood_rent,]$area = num_constr[ind_nhood_constr,]$area
  }
}

for(i in 1:dim(num_constr)[1]){  #aggiungo le costruzioni nei 4 anni passati
  
  ind_nhood_year = which(eviction_nhood_yearly$nhood == num_constr[i,]$nhood & eviction_nhood_yearly$year == num_constr[i,]$year)
  ind_nhood_year1 = which(eviction_nhood_yearly$nhood == num_constr[i,]$nhood & (eviction_nhood_yearly$year-1) == num_constr[i,]$year)
  ind_nhood_year2 = which(eviction_nhood_yearly$nhood == num_constr[i,]$nhood & (eviction_nhood_yearly$year-2) == num_constr[i,]$year)
  ind_nhood_year3 = which(eviction_nhood_yearly$nhood == num_constr[i,]$nhood & (eviction_nhood_yearly$year-3) == num_constr[i,]$year)
  ind_nhood_year4 = which(eviction_nhood_yearly$nhood == num_constr[i,]$nhood & (eviction_nhood_yearly$year-4) == num_constr[i,]$year)
  
  if(length(ind_nhood_year) > 0 ){
    eviction_nhood_yearly[ind_nhood_year,]$num_constr = num_constr[i,]$count
    eviction_nhood_yearly[ind_nhood_year,]$num_units_0 = num_constr[i,]$new_units_built
  }
  
  if(length(ind_nhood_year1) > 0){
    eviction_nhood_yearly[ind_nhood_year1,]$num_units_1 = num_constr[i,]$new_units_built
  }
  if(length(ind_nhood_year2) > 0){
    eviction_nhood_yearly[ind_nhood_year2,]$num_units_2 = num_constr[i,]$new_units_built
  }
  if(length(ind_nhood_year3) > 0){
    eviction_nhood_yearly[ind_nhood_year3,]$num_units_3 = num_constr[i,]$new_units_built
  }
  if(length(ind_nhood_year4) > 0){
    eviction_nhood_yearly[ind_nhood_year4,]$num_units_4 = num_constr[i,]$new_units_built
  }
  
}


#Sposto le covariate da rent_nhood_yearly a eviction_nhood_yearly (l'unica è il rent/mq)

eviction_nhood_yearly$rent = rep(0,dim(eviction_nhood_yearly)[1])

for(i in 1:dim(rent_nhood_yearly)[1]){  #aggiungo le costruzioni nei 4 anni passati
  
  ind_nhood_year = which(eviction_nhood_yearly$nhood == rent_nhood_yearly[i,]$nhood & eviction_nhood_yearly$year == rent_nhood_yearly[i,]$year)

  if(length(ind_nhood_year) > 0 ){
    eviction_nhood_yearly[ind_nhood_year,]$rent = rent_nhood_yearly[i,]$avg_rent.mq
  }
}


#Per alcune combinazioni di nhood-anno, non abbiamo dati e quindi l'avg-rent è 0. Sostituisco il valore all'anno prima. Questo non risolve i problemi nell' anno 2011, però

for(i in 1:dim(eviction_nhood_yearly)[1]){  
  if(eviction_nhood_yearly$rent[i] == 0) {
    for(j in 1:dim(eviction_nhood_yearly)[1]){ 
      if(eviction_nhood_yearly$nhood[j] == eviction_nhood_yearly$nhood[i] & eviction_nhood_yearly$year[j] == (eviction_nhood_yearly$year[i]-1)){
        eviction_nhood_yearly$rent[i] = eviction_nhood_yearly$rent[j]
      }
    }
  }
}

#Risolvo lo stesso problema ma per il 2011. In questo caso, mettiamo il valore nello stesso nhood nel 2012

for(i in 1:dim(eviction_nhood_yearly)[1]){  
  if(eviction_nhood_yearly$rent[i] == 0) {
    for(j in 1:dim(eviction_nhood_yearly)[1]){ 
      if(eviction_nhood_yearly$nhood[j] == eviction_nhood_yearly$nhood[i] & eviction_nhood_yearly$year[j] == (eviction_nhood_yearly$year[i]+1)){
        eviction_nhood_yearly$rent[i] = eviction_nhood_yearly$rent[j]
      }
    }
  }
}

#Provo a normalizzare alcune covariate:
{
  #Normalizzo il numero di construzioni sull' area
eviction_nhood_yearly$num_units_0 = eviction_nhood_yearly$num_units_0/eviction_nhood_yearly$area * 1e6
eviction_nhood_yearly$num_units_1 = eviction_nhood_yearly$num_units_1/eviction_nhood_yearly$area * 1e6
eviction_nhood_yearly$num_units_2 = eviction_nhood_yearly$num_units_2/eviction_nhood_yearly$area * 1e6
eviction_nhood_yearly$num_units_3 = eviction_nhood_yearly$num_units_3/eviction_nhood_yearly$area * 1e6
eviction_nhood_yearly$num_units_4 = eviction_nhood_yearly$num_units_4/eviction_nhood_yearly$area * 1e6

  #Normalizzo il numero di evictions sull'area
eviction_nhood_yearly$count = eviction_nhood_yearly$count/eviction_nhood_yearly$area * 1e6
}

#Pulizia finale di eviction_nhood_yearly prima del GAM
eviction_nhood_yearly <- eviction_nhood_yearly[,-c(1,2,8)]


#PROCEDO A FARE IL GAM

model_gam=gam(count ~  +nhood   + s(year,bs='cr',k = 8) + s(rent,bs='cr')  #s(dist_caltr,bs='cr') +s(dist_fin,bs='cr')  # manca s(I(nhood * year)
               + s(num_units_0,bs='cr') + s(num_units_1,bs='cr') 
              + s(num_units_2,bs='cr') + s(num_units_3,bs='cr')+ s(num_units_4,bs='cr') 
              ,data = eviction_nhood_yearly) 
# "s" basically creates smoothing splines out of x1 or x2#'cr' -> cubic splines , 'tp' -> thin plate splines
#model_gam <- lm(y ~ ns(x1, df = 3) + ns(x2, df = 3), data = dataset) # same but now we're using natural cubic splines
summary(model_gam)


#Controllo normalità dei residui
qqnorm(model_gam$residuals)
shapiro.test(model_gam$residuals)

plot(model_gam)

#NORMALIZZO NUM_CONSTR CON AREA E NON #EVICTIONS
#Grande dipendenza da tempo (year) e luogo (nhood/dist to caltr & fin distr). Debole ma presente dipendenza con gli affitti
#Dipendenza dal numero di costruzioni solo ad anno 1 e 4.
#Si può concludere che la dinamica del numero di evictions dipende da una componente geografica legata ai vari quartieri e
#una componente temporale che ha visto un generale aumento e picco tra il 2014 e 2015. Tuttavia non si trova una relazione
#diretta e chiara con il costo degli affitti e con il numero di costruzioni nel quartiere.

#NORMALIZZO #EVICTIONS CON AREA E NON NUM_CONSTR
#R^2 sale rispetto a prima a 0.885. Nhood e year sempre molto influenti. Cresce anche influenza di num_constr, tutte molto
#significative tranne num_units_2. Tuttavia num_units_1 e num_units_4 puntano verso l'alto in maniera controintuitiva

#NORMALIZZO ENTRAMBE
#R^2 sale ancora  a 0.923. Stessi ragionamenti. Tuttavia ora TUTTE le 4 covariate legate a num_constr puntano verso l'alto,
#che è il contrario di quello che vorremmo 


#Altre considerazioni:
#- se consideriamo entrambe normalizzate, forse si può concludere dicendo che dal nostro studio evictions e rents hanno 
#  comportamenti diversi e non possono essere usati in modo intercambiabile come proxy per la gentrification


#Cose che si possono cambiare:  - Normalizzare su #parcel?
#                               - provare ad aggiungere anche construzioni ad anni più vecchi?
#Futuri sviluppi del progetto potrebbero essere di trovare dati evictions geoloc così da poter studiare a granularità
#parcel oppure ottenere dati sulla popolazione per ogni nhood così da poter normalizzare rispetto alla popolazione
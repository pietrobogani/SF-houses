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
#num_constr = num_constr[-which(num_constr$year == 'NA'),]

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
rent_clean <- read_csv("rent_clean_nh.csv")

unique(rent_clean$nhood)
unique(num_constr$nhood)

ind_tr_isl = which(rent_clean$nhood == 'Treasure Island')
ind_sf = which(rent_clean$nhood == 'San Francisco')
rent_clean = rent_clean[-c(ind_tr_isl,ind_sf),]

rent_clean$dist_caltr = rep(0,dim(rent_clean)[1])
rent_clean$dist_fin = rep(0,dim(rent_clean)[1])
rent_clean$area = rep(0,dim(rent_clean)[1])
rent_clean$num_constr = rep(0,dim(rent_clean)[1])
rent_clean$num_units_0 = rep(0,dim(rent_clean)[1])
rent_clean$num_units_1 = rep(0,dim(rent_clean)[1])
rent_clean$num_units_2 = rep(0,dim(rent_clean)[1])
rent_clean$num_units_3 = rep(0,dim(rent_clean)[1])
rent_clean$num_units_4 = rep(0,dim(rent_clean)[1])



list_nhood = unique(num_constr$nhood)
for(nh in list_nhood){
  ind_nhood_constr = which(num_constr$nhood == nh)[1]
  ind_nhood_rent = which(rent_clean$nhood == nh)
  
  if(length(ind_nhood_rent) > 0 ){
    rent_clean[ind_nhood_rent,]$dist_caltr = num_constr[ind_nhood_constr,]$dist_caltr
    rent_clean[ind_nhood_rent,]$dist_fin = num_constr[ind_nhood_constr,]$dist_fin
    rent_clean[ind_nhood_rent,]$area = num_constr[ind_nhood_constr,]$area
  }
}


for(i in 1:dim(num_constr)[1]){

  ind_nhood_year = which(rent_clean$nhood == num_constr[i,]$nhood & rent_clean$year == num_constr[i,]$year)
  ind_nhood_year1 = which(rent_clean$nhood == num_constr[i,]$nhood & (rent_clean$year-1) == num_constr[i,]$year)
  ind_nhood_year2 = which(rent_clean$nhood == num_constr[i,]$nhood & (rent_clean$year-2) == num_constr[i,]$year)
  ind_nhood_year3 = which(rent_clean$nhood == num_constr[i,]$nhood & (rent_clean$year-3) == num_constr[i,]$year)
  ind_nhood_year4 = which(rent_clean$nhood == num_constr[i,]$nhood & (rent_clean$year-4) == num_constr[i,]$year)
  
  if(length(ind_nhood_year) > 0 ){
    rent_clean[ind_nhood_year,]$num_constr = num_constr[i,]$count
    rent_clean[ind_nhood_year,]$num_units_0 = num_constr[i,]$new_units_built
  }
  
  if(length(ind_nhood_year1) > 0){
    rent_clean[ind_nhood_year1,]$num_units_1 = num_constr[i,]$new_units_built
  }
  if(length(ind_nhood_year2) > 0){
    rent_clean[ind_nhood_year2,]$num_units_2 = num_constr[i,]$new_units_built
  }
  if(length(ind_nhood_year3) > 0){
    rent_clean[ind_nhood_year3,]$num_units_3 = num_constr[i,]$new_units_built
  }
  if(length(ind_nhood_year4) > 0){
    rent_clean[ind_nhood_year4,]$num_units_4 = num_constr[i,]$new_units_built
  }
  
}
#rent_clean = rent_clean[,-c(1,2,4,5)] #serve a togliere le colonne che non interessano
rm(num_constr,i,ind_nhood_year,ind_nhood_year1,ind_nhood_year2,ind_nhood_year3,ind_nhood_year4,point)



# GAM model ####################################################################
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)


lin_mod = lm(price_mq ~     nhood + year + nhood:year + num_units_0 + num_units_1 + num_units_2 +
               num_units_3 + num_units_4 + dist_fin + dist_caltr , data = rent_clean)

lin_mod = lm(price_mq ~  nhood:year + num_units_0 + num_units_1 + num_units_2 +
               num_units_3 + num_units_4, data = rent_clean)
summary(lin_mod)


{#GAM with natural splines
  
  rent_clean$year_fct = as.factor(rent_clean$year)
  #Gam con numero di costruzioni (e non densità!):
  model_gam_ns <-lm(price_mq ~ ns(num_units_0, df = 3) + ns(num_units_1, df = 3) + 
                      ns(num_units_2, df = 3) + ns(num_units_3, df = 3) + 
                      ns(num_units_4, df = 3) + year_fct +
                      ns(dist_fin, df = 3) + ns(dist_caltr, df = 3)  , data = rent_clean)
  summary(model_gam_ns)
  gam::plot.Gam(model_gam_ns, se=TRUE)
  model_chosen = model_gam_ns
  #Numero di costruzioni non è mai significativo con alpha=0.05!
  
  
  #Gam con numero di costruzioni (e non densità!) e nhood:
  model_gam_ns <-lm(price_mq ~ ns(num_units_0, df = 3) + ns(num_units_1, df = 3) + 
                      ns(num_units_2, df = 3) + ns(num_units_3, df = 3) + 
                      ns(num_units_4, df = 3) + year_fct +
                      ns(dist_fin, df = 3) + ns(dist_caltr, df = 3) + nhood  , data = rent_clean)
  summary(model_gam_ns)
  gam::plot.Gam(model_gam_ns, se=TRUE)
  #Non ha senso mettere sia distanze che nhood!O uno o l'altro!
  
  
  #Gam con numero di costruzioni (e non densità!) e nhood(ma non distanze):
  model_gam_ns <-lm(price_mq ~ ns(num_units_0, df = 3) + ns(num_units_1, df = 3) + 
                      ns(num_units_2, df = 3) + ns(num_units_3, df = 3) + 
                      ns(num_units_4, df = 3) + year_fct + nhood  , data = rent_clean)
  summary(model_gam_ns)
  gam::plot.Gam(model_gam_ns, se=TRUE)
  #Così le costruzioni dell'anno corrente diventano significative...
  
  #Tra i modelli con numero di costruzioni userei quello con distanze e non nhood così 
  # non c'è significatività di nessuna costruzione! In più l'informazione dei nhood
  # non si perde così tanto tenendo le due distanze

  
  #Passo a usare le densità e non il numero di costruzioni
  rent_clean$num_units_0 = rent_clean$num_units_0/rent_clean$area 
  rent_clean$num_units_1 = rent_clean$num_units_1/rent_clean$area
  rent_clean$num_units_2 = rent_clean$num_units_2/rent_clean$area 
  rent_clean$num_units_3 = rent_clean$num_units_3/rent_clean$area 
  rent_clean$num_units_4 = rent_clean$num_units_4/rent_clean$area 
  
  rent_clean$num_units_0_adj = rent_clean$num_units_0 * 1e6
  rent_clean$num_units_1_adj = rent_clean$num_units_1 * 1e6
  rent_clean$num_units_2_adj = rent_clean$num_units_2 * 1e6
  rent_clean$num_units_3_adj = rent_clean$num_units_3 * 1e6
  rent_clean$num_units_4_adj = rent_clean$num_units_4 * 1e6

  model_gam_ns <-lm(price_mq ~ ns(num_units_0, df = 3) + ns(num_units_1, df = 3) + 
                    ns(num_units_2, df = 3) + ns(num_units_3, df = 3) + 
                    ns(num_units_4, df = 3) + year_fct +
                    ns(dist_fin, df = 3) + ns(dist_caltr, df = 3)  , data = rent_clean)
  summary(model_gam_ns)
  gam::plot.Gam(model_gam_ns, se=TRUE)
  model_chosen_dens = model_gam_ns
  #Le costruzioni in anno4 sono significative, ma solo un elemento su tre della base
  
  
  #Uso un rescaling per vedere se ci sono problemi a livello computazionale con densità molto piccole
  model_gam_ns <-lm(price_mq ~ ns(num_units_0_adj, df = 3) + ns(num_units_1_adj, df = 3) + 
                      ns(num_units_2_adj, df = 3) + ns(num_units_3_adj, df = 3) + 
                      ns(num_units_4_adj, df = 3) + year_fct +
                      ns(dist_fin, df = 3) + ns(dist_caltr, df = 3)  , data = rent_clean)
  summary(model_gam_ns)
  gam::plot.Gam(model_gam_ns, se=TRUE)
  #Non cambia nulla rispetto senza riscaling ... è inutile?
  
  
  
  #Provo ad usare il giorno come unità di misura del tempo e non gli anni:
  rent_clean$d_num = as.numeric(rent_clean$d)
  rent_clean$d_num = rent_clean$d_num - min(rent_clean$d_num)
  
  #Gam con giorni come unità di tempo con ns a 5 df: 
  model_gam_ns <-lm(price_mq ~ ns(num_units_0, df = 3) + ns(num_units_1, df = 3) + 
                      ns(num_units_2, df = 3) + ns(num_units_3, df = 3) + 
                      ns(num_units_4, df = 3) + ns(d_num, df = 5) +
                      ns(dist_fin, df = 3) + ns(dist_caltr, df = 3)  , data = rent_clean)
  summary(model_gam_ns)
  gam::plot.Gam(model_gam_ns, se=TRUE)
  #Modello cambia leggermente e costruzioni a 2 e 4 anni hanno un elemento della base significativo

  
  #Si potrebbe provare con smoothing splines?
  #Io terrei o modello con distanze,numero di constr e year_fct oppure distanze,dens_constr e giorni
  #Magari quello con year_fct dove si hanno meno cose significative (?)
}








library(readxl)
library("writexl")
library(stringr)

Buyout_Agreements <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Original Datasets/Buyout_Agreements.csv", header=TRUE)
rent <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Original Datasets/rent.csv", header=TRUE)
Eviction_Notices <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Original Datasets/Eviction_Notices.csv", header=TRUE)
New_construction <-  read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Original Datasets/New_construction.csv", header=TRUE, sep = ";")
Parcels <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Original Datasets/Parcels.csv", header=TRUE)


# ------------------------------------------------- Eviction Notices -----------------------------------------------------------------------




{
Eviction_Notices <- Eviction_Notices[,-c(1,3:5,7:27,30:45)] #selezione covariate di interesse
Eviction_Notices <- Eviction_Notices[!(Eviction_Notices[,3]==""), ] #elimino gli sfratti senza quartiere
Eviction_Notices <- Eviction_Notices[!duplicated(Eviction_Notices),] #elimino gli sfratti duplici (fatti lo stesso giorno nello stesso indirizzo)
for (i in 1:length(Eviction_Notices[,1])) {   #preparo bene le coordinate
  if(Eviction_Notices[i,4] != ""){
    temp <- strsplit(Eviction_Notices[i,4], ", ")
    Eviction_Notices[i,5] <- temp[[1]][1]
    Eviction_Notices[i,6] <- temp[[1]][2]
  }
  else{
    Eviction_Notices[i,5] <- ""
    Eviction_Notices[i,6] <- ""
  }
}

for (i in 1:length(Eviction_Notices[,1])) {   #preparo bene le coordinate
  if(Eviction_Notices[i,4] != ""){
    temp <- strsplit(Eviction_Notices[i,5], "\\(")
    temp1 <- strsplit(Eviction_Notices[i,6], "\\)")
    Eviction_Notices[i,5] <- temp[[1]][2]
    Eviction_Notices[i,6] <- temp1[[1]][1]
    Eviction_Notices[i,5] <- as.double(Eviction_Notices[i,5])
    Eviction_Notices[i,6] <- as.double(Eviction_Notices[i,6])
  }
}
Eviction_Notices <-  Eviction_Notices[,-c(4)]
for (i in 1:length(Eviction_Notices[,1])) {   #preparo la data
  temp <- strsplit(Eviction_Notices[i,2], "/")
  Eviction_Notices[i,6] <- temp[[1]][1]
  Eviction_Notices[i,7] <- temp[[1]][3]
  Eviction_Notices[i,7] <- as.double(Eviction_Notices[i,7])
  Eviction_Notices[i,6] <- as.double(Eviction_Notices[i,6])
}
Eviction_Notices <-  Eviction_Notices[,-c(2)]
colnames(Eviction_Notices) <- c('address', 'nhood', 'lat', 'long','month','year')

for (i in 1:length(Eviction_Notices[,1])) {   #preparo l'address
  temp <- strsplit(Eviction_Notices[i,1], " ")
  for( j in 1: length(temp[[1]])){
    Eviction_Notices[i,6+j] <- temp[[1]][j] 
  }
}
Eviction_Notices <- Eviction_Notices[,-c(8,9)]
for( i in 1:length(Eviction_Notices[,1])){  #preparo l'address
  j = 9
  while (Eviction_Notices[i,j] != "" & !(is.na(Eviction_Notices[i,j]))) {
    Eviction_Notices[i,8] = paste(Eviction_Notices[i,8], Eviction_Notices[i,j], sep=" ")
    j = j+1
  }
}
for( i in 1:length(Eviction_Notices[,1])){ 
  j = 9
  while (Eviction_Notices[i,j] != "" & !(is.na(Eviction_Notices[i,j]))){
    j=j+1
  }
  if (!(is.na(Eviction_Notices[i,j]))){
    Eviction_Notices[i,9] = Eviction_Notices[i,j+1]
  }
}
Eviction_Notices <- Eviction_Notices[,-c(10:14)]
colnames(Eviction_Notices) <- c('address', 'nhood', 'lat', 'long','month','year','block','street_name', 'street_type')
Eviction_Notices <- Eviction_Notices[Eviction_Notices$year > 2010 & Eviction_Notices$year < 2019,]  #butto via gli anni prima perchè per fare il modello di regr devo usare gli stessi anni di 'rent'
write.csv(Eviction_Notices,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices_clean.csv")
Eviction_Notices_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices_clean.csv", header=TRUE)
} # Prime modifiche

{

{
    #Calcolo i df con numero di evictions per nhood per anno e per mese
vect_year = paste(Eviction_Notices_clean$year)
vect_month = paste(Eviction_Notices_clean$month)
vect_nhood = paste(Eviction_Notices_clean$nhood)
vect_aus = paste(vect_month,vect_year,vect_nhood)
vect_aus2 = paste(vect_year,vect_nhood)
Eviction_Notices_clean$year_nhood = vect_aus2
Eviction_Notices_clean$month_year_nhood = vect_aus
rm(vect_aus,vect_aus2,vect_year,vect_nhood,vect_month)

length(unique(Eviction_Notices_clean$year_nhood)) #Dalle 973 osservazioni non sono state tolte quelle dal 1997 al 2007!
Eviction_Notices_clean$dummy = 1
eviction_nhood_yearly = aggregate(Eviction_Notices_clean$dummy, by = list(Eviction_Notices_clean$year_nhood), FUN = sum)
names(eviction_nhood_yearly)[names(eviction_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(eviction_nhood_yearly)[names(eviction_nhood_yearly) == 'x'] <- 'Count'

length(unique(Eviction_Notices_clean$month_year_nhood)) #Dalle 8943 osservazioni non sono state tolte quelle dal 97 al 07
eviction_nhood_monthly = aggregate(Eviction_Notices_clean$dummy, by = list(Eviction_Notices_clean$month_year_nhood), FUN = sum)
names(eviction_nhood_monthly)[names(eviction_nhood_monthly) == 'Group.1'] <- 'nhood_month_year'
names(eviction_nhood_monthly)[names(eviction_nhood_monthly) == 'x'] <- 'Count'
#Adesso son da splittare di nuovo mese-anno e nhood e si può plottare tutto (analisi esplorativa)!



for (i in 1:length(eviction_nhood_monthly[,1])) {   #preparo bene le coordinate
  temp <- strsplit(eviction_nhood_monthly[i,1], " ")
  for (j in 1:length(temp[[1]])){
    eviction_nhood_monthly[i,2+j] <- temp[[1]][j]
  }
}


for( i in 1:length(eviction_nhood_monthly[,1])){  #preparo l'address
  j = 6
  while (j < 9) {
    if(!(is.na(eviction_nhood_monthly[i,j]))) {
      eviction_nhood_monthly[i,5] = paste(eviction_nhood_monthly[i,5], eviction_nhood_monthly[i,j], sep=" ")
      j = j+1
    }
    else
      j = 14
    
  }
}
eviction_nhood_monthly <- eviction_nhood_monthly[, -c(6:8)]
colnames(eviction_nhood_monthly) <- c('nhood_month_year', 'count', 'month','year', 'nhood')
write.csv(eviction_nhood_monthly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_nhood_monthly_nh.csv")



for (i in 1:length(eviction_nhood_yearly[,1])) {   #preparo bene le coordinate
  temp <- strsplit(eviction_nhood_yearly[i,1], " ")
  for (j in 1:length(temp[[1]])){
    eviction_nhood_yearly[i,2+j] <- temp[[1]][j]
  }
}


for( i in 1:length(eviction_nhood_yearly[,1])){  #preparo l'address
  j = 5
  while (j <8 ) {
    if(!(is.na(eviction_nhood_yearly[i,j]))) {
      eviction_nhood_yearly[i,4] = paste(eviction_nhood_yearly[i,4], eviction_nhood_yearly[i,j], sep=" ")
      j = j+1
    }
    else
      j = 13
  }  
  }

eviction_nhood_yearly <- eviction_nhood_yearly[, -c(5:7)]
colnames(eviction_nhood_yearly) <- c('nhood_month_year', 'count','year', 'nhood')
write.csv(eviction_nhood_yearly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_nhood_yearly_nh.csv")
} #Vecchio codice
  
  eviction_nhood_yearly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_yearly_nh.csv", header=TRUE)
  eviction_nhood_monthly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_monthly_nh.csv", header=TRUE)
  
} # Preparazione di eviction_nhood_yearly & eviction_nhood_monthly

#Aggiungo i centri ai nhoods di Eviction_Notices_Clean
{
  Eviction_Notices_clean$centroid_lat <- NA
  Eviction_Notices_clean$centroid_long <- NA
  
  SFNeighborhoods_new <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/SFNeighborhoods_new.csv", header=TRUE)
  
  
  for (i in 1:length(Eviction_Notices_clean[,1])){
    for ( j in 1:length(SFNeighborhoods_new$nhood)){
      if (Eviction_Notices_clean$nhood[i] == SFNeighborhoods_new$nhood[j]){
        Eviction_Notices_clean$centroid_lat[i] <- SFNeighborhoods_new$centroid_lat[j]
        Eviction_Notices_clean$centroid_long[i] <- SFNeighborhoods_new$centroid_long[j]
      }
    }
  }
  
  write.csv(Eviction_Notices_clean,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices_clean.csv")
}

#Aggiungo i centri ai nhoods di eviction_nhood_monthly
{
  eviction_nhood_monthly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_nhood_monthly.csv", header=TRUE)
  eviction_nhood_monthly$centroid_lat <- NA
  eviction_nhood_monthly$centroid_long <- NA
  
  for (i in 1:length(eviction_nhood_monthly[,1])){
    if (eviction_nhood_monthly$nhood[i] == 'Bayview Hunters Point'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.730696
      eviction_nhood_monthly$centroid_long[i] <- -122.388515
    }
    if (eviction_nhood_monthly$nhood[i] == 'Bernal Heights'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.741391
      eviction_nhood_monthly$centroid_long[i] <- -122.414033
    }
    if (eviction_nhood_monthly$nhood[i] == 'Castro/Upper Market'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.758812
      eviction_nhood_monthly$centroid_long[i] <- -122.435644
    }
    if (eviction_nhood_monthly$nhood[i] == 'Chinatown'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.795073
      eviction_nhood_monthly$centroid_long[i] <- -122.406350
    }
    if (eviction_nhood_monthly$nhood[i] == 'Excelsior'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.722300
      eviction_nhood_monthly$centroid_long[i] <- -122.431224
    }
    
    if (eviction_nhood_monthly$nhood[i] == 'Financial District/South Beach'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.793213
      eviction_nhood_monthly$centroid_long[i] <- -122.398187
    }
    if (eviction_nhood_monthly$nhood[i] == 'Glen Park'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.735976
      eviction_nhood_monthly$centroid_long[i] <- -122.434464
    }
    if (eviction_nhood_monthly$nhood[i] == 'Golden Gate Park'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.769283
      eviction_nhood_monthly$centroid_long[i] <- -122.473486
    }
    if (eviction_nhood_monthly$nhood[i] == 'Haight Ashbury'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.769910
      eviction_nhood_monthly$centroid_long[i] <- -122.447678
    }
    if (eviction_nhood_monthly$nhood[i] == 'Hayes Valley'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.776425
      eviction_nhood_monthly$centroid_long[i] <- -122.426235
    }
    if (eviction_nhood_monthly$nhood[i] == 'Inner Richmond'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.778849
      eviction_nhood_monthly$centroid_long[i] <- -122.468955 
    }
    if (eviction_nhood_monthly$nhood[i] == 'Inner Sunset'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.762068
      eviction_nhood_monthly$centroid_long[i] <- -122.467189
    }
    if (eviction_nhood_monthly$nhood[i] == 'Japantown'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.785840
      eviction_nhood_monthly$centroid_long[i] <- -122.429952
    }
    if (eviction_nhood_monthly$nhood[i] == 'Lakeshore'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.718720
      eviction_nhood_monthly$centroid_long[i] <- -122.477916
    }
    if (eviction_nhood_monthly$nhood[i] == 'Lone Mountain/USF'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.777975
      eviction_nhood_monthly$centroid_long[i] <- -122.452816
    }
    if (eviction_nhood_monthly$nhood[i] == 'Marina'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.802877
      eviction_nhood_monthly$centroid_long[i] <- -122.436969
    }
    if (eviction_nhood_monthly$nhood[i] == 'McLaren Park'){ #non molto convinto, l'ho messo in mezzo al parco, le vie sono tutte attorno al parco gigante e non si capiscono i confini su google maps
      eviction_nhood_monthly$centroid_lat[i] <- 37.718407
      eviction_nhood_monthly$centroid_long[i] <- -122.418904
    }
    if (eviction_nhood_monthly$nhood[i] == 'Mission'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.758338
      eviction_nhood_monthly$centroid_long[i] <- -122.414678
    }
    if (eviction_nhood_monthly$nhood[i] == 'Mission Bay'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.770674
      eviction_nhood_monthly$centroid_long[i] <- -122.392015
    }
    if (eviction_nhood_monthly$nhood[i] == 'Nob Hill'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.793044
      eviction_nhood_monthly$centroid_long[i] <- -122.414461
    }
    if (eviction_nhood_monthly$nhood[i] == 'Noe Valley'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.747095
      eviction_nhood_monthly$centroid_long[i] <- -122.431421
    }
    if (eviction_nhood_monthly$nhood[i] == 'North Beach'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.799254
      eviction_nhood_monthly$centroid_long[i] <- -122.407458
    }
    if (eviction_nhood_monthly$nhood[i] == 'Oceanview/Merced/Ingleside'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.718959
      eviction_nhood_monthly$centroid_long[i] <- -122.462039
    }
    if (eviction_nhood_monthly$nhood[i] == 'Outer Mission'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.709585
      eviction_nhood_monthly$centroid_long[i] <- -122.455082
    }
    if (eviction_nhood_monthly$nhood[i] == 'Outer Richmond'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.776683
      eviction_nhood_monthly$centroid_long[i] <- -122.489908
    }
    if (eviction_nhood_monthly$nhood[i] == 'Pacific Heights'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.792182
      eviction_nhood_monthly$centroid_long[i] <- -122.432784
    }
    if (eviction_nhood_monthly$nhood[i] == 'Portola'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.726470
      eviction_nhood_monthly$centroid_long[i] <- -122.407642
    }
    if (eviction_nhood_monthly$nhood[i] == 'Potrero Hill'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.759056
      eviction_nhood_monthly$centroid_long[i] <- -122.398618
    }
    if (eviction_nhood_monthly$nhood[i] == 'Presidio Heights' | eviction_nhood_monthly$nhood[i] == 'Presidio'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.788849
      eviction_nhood_monthly$centroid_long[i] <- -122.452228
    }
    if (eviction_nhood_monthly$nhood[i] == 'Russian Hill'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.800432
      eviction_nhood_monthly$centroid_long[i] <- -122.417609
    }
    if (eviction_nhood_monthly$nhood[i] == 'Seacliff'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.785975
      eviction_nhood_monthly$centroid_long[i] <- -122.489522
    }
    if (eviction_nhood_monthly$nhood[i] == 'South of Market'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.779527
      eviction_nhood_monthly$centroid_long[i] <- -122.405727
    }
    if (eviction_nhood_monthly$nhood[i] == 'Sunset/Parkside'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.749026
      eviction_nhood_monthly$centroid_long[i] <- -122.493742
    }
    if (eviction_nhood_monthly$nhood[i] == 'Tenderloin'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.784141
      eviction_nhood_monthly$centroid_long[i] <- -122.414748
    }
    if (eviction_nhood_monthly$nhood[i] == 'Twin Peaks'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.753375
      eviction_nhood_monthly$centroid_long[i] <- -122.445652
    }
    if (eviction_nhood_monthly$nhood[i] == 'Visitacion Valley'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.713141
      eviction_nhood_monthly$centroid_long[i] <- -122.408825
    }
    if (eviction_nhood_monthly$nhood[i] == 'West of Twin Peaks'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.737955
      eviction_nhood_monthly$centroid_long[i] <- -122.460363
    }
    if (eviction_nhood_monthly$nhood[i] == 'Western Addition'){
      eviction_nhood_monthly$centroid_lat[i] <- 37.780587
      eviction_nhood_monthly$centroid_long[i] <- -122.434510
    }
  }
  
  write.csv(eviction_nhood_monthly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_nhood_monthly.csv")
}

#Aggiungo i centri ai nhoods di eviction_nhood_yearly
{
  eviction_nhood_yearly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_nhood_yearly.csv", header=TRUE)
  eviction_nhood_yearly$centroid_lat <- NA
  eviction_nhood_yearly$centroid_long <- NA
  
  for (i in 1:length(eviction_nhood_yearly[,1])){
    if (eviction_nhood_yearly$nhood[i] == 'Bayview Hunters Point'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.730696
      eviction_nhood_yearly$centroid_long[i] <- -122.388515
    }
    if (eviction_nhood_yearly$nhood[i] == 'Bernal Heights'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.741391
      eviction_nhood_yearly$centroid_long[i] <- -122.414033
    }
    if (eviction_nhood_yearly$nhood[i] == 'Castro/Upper Market'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.758812
      eviction_nhood_yearly$centroid_long[i] <- -122.435644
    }
    if (eviction_nhood_yearly$nhood[i] == 'Chinatown'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.795073
      eviction_nhood_yearly$centroid_long[i] <- -122.406350
    }
    if (eviction_nhood_yearly$nhood[i] == 'Excelsior'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.722300
      eviction_nhood_yearly$centroid_long[i] <- -122.431224
    }
    
    if (eviction_nhood_yearly$nhood[i] == 'Financial District/South Beach'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.793213
      eviction_nhood_yearly$centroid_long[i] <- -122.398187
    }
    if (eviction_nhood_yearly$nhood[i] == 'Glen Park'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.735976
      eviction_nhood_yearly$centroid_long[i] <- -122.434464
    }
    if (eviction_nhood_yearly$nhood[i] == 'Golden Gate Park'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.769283
      eviction_nhood_yearly$centroid_long[i] <- -122.473486
    }
    if (eviction_nhood_yearly$nhood[i] == 'Haight Ashbury'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.769910
      eviction_nhood_yearly$centroid_long[i] <- -122.447678
    }
    if (eviction_nhood_yearly$nhood[i] == 'Hayes Valley'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.776425
      eviction_nhood_yearly$centroid_long[i] <- -122.426235
    }
    if (eviction_nhood_yearly$nhood[i] == 'Inner Richmond'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.778849
      eviction_nhood_yearly$centroid_long[i] <- -122.468955 
    }
    if (eviction_nhood_yearly$nhood[i] == 'Inner Sunset'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.762068
      eviction_nhood_yearly$centroid_long[i] <- -122.467189
    }
    if (eviction_nhood_yearly$nhood[i] == 'Japantown'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.785840
      eviction_nhood_yearly$centroid_long[i] <- -122.429952
    }
    if (eviction_nhood_yearly$nhood[i] == 'Lakeshore'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.718720
      eviction_nhood_yearly$centroid_long[i] <- -122.477916
    }
    if (eviction_nhood_yearly$nhood[i] == 'Lone Mountain/USF'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.777975
      eviction_nhood_yearly$centroid_long[i] <- -122.452816
    }
    if (eviction_nhood_yearly$nhood[i] == 'Marina'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.802877
      eviction_nhood_yearly$centroid_long[i] <- -122.436969
    }
    if (eviction_nhood_yearly$nhood[i] == 'McLaren Park'){ #non molto convinto, l'ho messo in mezzo al parco, le vie sono tutte attorno al parco gigante e non si capiscono i confini su google maps
      eviction_nhood_yearly$centroid_lat[i] <- 37.718407
      eviction_nhood_yearly$centroid_long[i] <- -122.418904
    }
    if (eviction_nhood_yearly$nhood[i] == 'Mission'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.758338
      eviction_nhood_yearly$centroid_long[i] <- -122.414678
    }
    if (eviction_nhood_yearly$nhood[i] == 'Mission Bay'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.770674
      eviction_nhood_yearly$centroid_long[i] <- -122.392015
    }
    if (eviction_nhood_yearly$nhood[i] == 'Nob Hill'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.793044
      eviction_nhood_yearly$centroid_long[i] <- -122.414461
    }
    if (eviction_nhood_yearly$nhood[i] == 'Noe Valley'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.747095
      eviction_nhood_yearly$centroid_long[i] <- -122.431421
    }
    if (eviction_nhood_yearly$nhood[i] == 'North Beach'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.799254
      eviction_nhood_yearly$centroid_long[i] <- -122.407458
    }
    if (eviction_nhood_yearly$nhood[i] == 'Oceanview/Merced/Ingleside'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.718959
      eviction_nhood_yearly$centroid_long[i] <- -122.462039
    }
    if (eviction_nhood_yearly$nhood[i] == 'Outer Mission'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.709585
      eviction_nhood_yearly$centroid_long[i] <- -122.455082
    }
    if (eviction_nhood_yearly$nhood[i] == 'Outer Richmond'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.776683
      eviction_nhood_yearly$centroid_long[i] <- -122.489908
    }
    if (eviction_nhood_yearly$nhood[i] == 'Pacific Heights'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.792182
      eviction_nhood_yearly$centroid_long[i] <- -122.432784
    }
    if (eviction_nhood_yearly$nhood[i] == 'Portola'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.726470
      eviction_nhood_yearly$centroid_long[i] <- -122.407642
    }
    if (eviction_nhood_yearly$nhood[i] == 'Potrero Hill'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.759056
      eviction_nhood_yearly$centroid_long[i] <- -122.398618
    }
    if (eviction_nhood_yearly$nhood[i] == 'Presidio Heights' | eviction_nhood_yearly$nhood[i] == 'Presidio'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.788849
      eviction_nhood_yearly$centroid_long[i] <- -122.452228
    }
    if (eviction_nhood_yearly$nhood[i] == 'Russian Hill'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.800432
      eviction_nhood_yearly$centroid_long[i] <- -122.417609
    }
    if (eviction_nhood_yearly$nhood[i] == 'Seacliff'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.785975
      eviction_nhood_yearly$centroid_long[i] <- -122.489522
    }
    if (eviction_nhood_yearly$nhood[i] == 'South of Market'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.779527
      eviction_nhood_yearly$centroid_long[i] <- -122.405727
    }
    if (eviction_nhood_yearly$nhood[i] == 'Sunset/Parkside'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.749026
      eviction_nhood_yearly$centroid_long[i] <- -122.493742
    }
    if (eviction_nhood_yearly$nhood[i] == 'Tenderloin'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.784141
      eviction_nhood_yearly$centroid_long[i] <- -122.414748
    }
    if (eviction_nhood_yearly$nhood[i] == 'Twin Peaks'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.753375
      eviction_nhood_yearly$centroid_long[i] <- -122.445652
    }
    if (eviction_nhood_yearly$nhood[i] == 'Visitacion Valley'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.713141
      eviction_nhood_yearly$centroid_long[i] <- -122.408825
    }
    if (eviction_nhood_yearly$nhood[i] == 'West of Twin Peaks'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.737955
      eviction_nhood_yearly$centroid_long[i] <- -122.460363
    }
    if (eviction_nhood_yearly$nhood[i] == 'Western Addition'){
      eviction_nhood_yearly$centroid_lat[i] <- 37.780587
      eviction_nhood_yearly$centroid_long[i] <- -122.434510
    }
  }
  
  write.csv(eviction_nhood_yearly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_nhood_yearly.csv")
}

#Calcolo eviction_notices/square feet per ogni nhood. Sommo le quantità di evictions dal 2011 al 2017. Poi divido in clusters i nhood sulla
#base di quante evictions ci sono state
{
  
  Eviction_Notices_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices_clean_nh.csv", header=TRUE)
  sum_eviction_by_nhood = aggregate(Eviction_Notices_clean$dummy, by = list(Eviction_Notices_clean$nhood), FUN = sum)
  names(sum_eviction_by_nhood)[names(sum_eviction_by_nhood) == 'Group.1'] <- 'nhood'
  names(sum_eviction_by_nhood)[names(sum_eviction_by_nhood) == 'x'] <- 'Evictions_number'
  
  SFNeighborhoods_new <- read_sf("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/SFNeighborhoods_new_nh.geojson")
  sum_eviction_by_nhood$area <-NA
  for(i in 1:length(sum_eviction_by_nhood[,1])){
    for(j in 1:length(SFNeighborhoods_new$area)){
      if(sum_eviction_by_nhood$nhood[i] ==SFNeighborhoods_new$nhood[j])
        sum_eviction_by_nhood$area[i]=SFNeighborhoods_new$area[j]
      
    }
  }
  sum_eviction_by_nhood[,2] <- sum_eviction_by_nhood[,2]/sum_eviction_by_nhood[,3]*100000 #senn� numeri troppo piccoli
  
  
  
  
  
  
  #Calcolo due clusters
  m <- median(sum_eviction_by_nhood[,2])
  high_evictions_nhood2 <- NA
  low_evictions_nhood2 <- NA
  j <- 1
  k <- 1
  for(i in 1:length(sum_eviction_by_nhood[,1])){
    if (sum_eviction_by_nhood[i,2] >= m){
      high_evictions_nhood2[j] <- sum_eviction_by_nhood[i,1]
      j =j+1
    }
    else{
      low_evictions_nhood2[k] <- sum_eviction_by_nhood[i,1]
      k = k+1
    }
  
  }
  write.csv(high_evictions_nhood2,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/high_evictions_nhood_nh2.csv")
  write.csv(low_evictions_nhood2,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/low_evictions_nhood_nh2.csv")
  
  
  
  
  #Calcolo tre clusters
  q1 <- quantile(sum_eviction_by_nhood[,2],probs = 0.33)
  q2 <- quantile(sum_eviction_by_nhood[,2],probs = 0.66)
  high_evictions_nhood3 <- NA
  medium_evictions_nhood3 <- NA
  low_evictions_nhood3 <- NA
  j <- 1
  k <- 1
  t <- 1
  for(i in 1:length(sum_eviction_by_nhood[,1])){
    if (sum_eviction_by_nhood[i,2] >= q2){
      high_evictions_nhood3[j] <- sum_eviction_by_nhood[i,1]
      j =j+1
    }
    else if (sum_eviction_by_nhood[i,2] <= q1){
      low_evictions_nhood3[k] <- sum_eviction_by_nhood[i,1]
      k = k+1
    }
    else {
      medium_evictions_nhood3[t] <- sum_eviction_by_nhood[i,1]
      t <- t+1
    }
    
  }
  
  write.csv(high_evictions_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/high_evictions_nhood_nh3.csv")
  write.csv(low_evictions_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/low_evictions_nhood_nh3.csv")
  write.csv(medium_evictions_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/medium_evictions_nhood_nh3.csv")
  
  
}



# ------------------------------------------------- Buyout Agreements -----------------------------------------------------------------------




{
Buyout_Agreements <- Buyout_Agreements[,-c(1,2,5:7,27,9,10,13:27)] #selezione covariate di interesse
Buyout_Agreements <- Buyout_Agreements[!(Buyout_Agreements[,4]==""), ] #elimino buyout senza quartiere
Buyout_Agreements <- Buyout_Agreements[!duplicated(Buyout_Agreements),] #elimino buyout duplici (fatti nello stesso giorno, alla stessa cifra, nello stesso indirizzo)
Buyout_Agreements <- Buyout_Agreements[(Buyout_Agreements[,1] != ""),] #tolgo quelli senza data
for (i in 1:length(Buyout_Agreements[,1])) {   #preparo bene le coordinate
  temp <- strsplit(Buyout_Agreements[i,5], "\\(")
  Buyout_Agreements[i,5] <- temp[[1]][2]
}

for (i in 1:length(Buyout_Agreements[,1])) {   #preparo bene le coordinate
  temp <- strsplit(Buyout_Agreements[i,5], " ")
  Buyout_Agreements[i,6] <- temp[[1]][1]
  Buyout_Agreements[i,7] <- temp[[1]][2]
}

for (i in 1:length(Buyout_Agreements[,1])) {   #preparo bene le coordinate
  temp <- strsplit(Buyout_Agreements[i,7], "\\)")
  Buyout_Agreements[i,7] <- temp[[1]][1]
}
for (i in 1:length(Buyout_Agreements[,1])) {    #preparo le date
  temp <- strsplit(Buyout_Agreements[i,1], "/")
  Buyout_Agreements[i,8] <- temp[[1]][1]
  Buyout_Agreements[i,9] <- temp[[1]][3]
  Buyout_Agreements[i,8] <- as.double(Buyout_Agreements[i,8])
  Buyout_Agreements[i,9] <- as.double(Buyout_Agreements[i,9])
}
Buyout_Agreements <- Buyout_Agreements[,-c(1,5)]
colnames(Buyout_Agreements) <- c('buyout_amount', 'address','nhood', 'long','lat', 'month','year')
write.csv(Buyout_Agreements,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Buyout_Agreements_Clean.csv")
Buyout_Agreements_Clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Buyout_Agreements_Clean.csv", header=TRUE)
} #Prime modifiche di base

{
#Calcolo i df con numero di buyout per nhood per anno e per mese
vect_year = paste(Buyout_Agreements_Clean$year)
vect_month = paste(Buyout_Agreements_Clean$month)
vect_nhood = paste(Buyout_Agreements_Clean$nhood)
vect_aus = paste(vect_month,vect_year,vect_nhood)
vect_aus2 = paste(vect_year,vect_nhood)
Buyout_Agreements_Clean$year_nhood = vect_aus2
Buyout_Agreements_Clean$month_year_nhood = vect_aus
rm(vect_aus,vect_aus2,vect_year,vect_nhood,vect_month)

length(unique(Buyout_Agreements_Clean$year_nhood)) #260 osservazioni
Buyout_Agreements_Clean$dummy = 1
buyout_nhood_yearly = aggregate(Buyout_Agreements_Clean[,c(2,11)], by = list(Buyout_Agreements_Clean$year_nhood), FUN = sum)
names(buyout_nhood_yearly)[names(buyout_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(buyout_nhood_yearly)[names(buyout_nhood_yearly) == 'dummy'] <- 'Count'
buyout_nhood_yearly$avg_buyout = buyout_nhood_yearly$buyout_amount / buyout_nhood_yearly$Count

length(unique(Buyout_Agreements_Clean$month_year_nhood)) #1383 osservazioni 
buyout_nhood_monthly = aggregate(Buyout_Agreements_Clean[,c(2,11)], by = list(Buyout_Agreements_Clean$month_year_nhood), FUN = sum)
names(buyout_nhood_monthly)[names(buyout_nhood_monthly) == 'Group.1'] <- 'nhood_month_year'
names(buyout_nhood_monthly)[names(buyout_nhood_monthly) == 'dummy'] <- 'Count'
buyout_nhood_monthly$avg_buyout = buyout_nhood_monthly$buyout_amount / buyout_nhood_monthly$Count
#Adesso son da splittare di nuovo mese-anno e nhood e si può plottare tutto (analisi esplorativa)!




for (i in 1:length(buyout_nhood_monthly[,1])) {   #preparo bene le coordinate
  temp <- strsplit(buyout_nhood_monthly[i,2], " ")
  for (j in 1:length(temp[[1]])){
    buyout_nhood_monthly[i,5+j] <- temp[[1]][j]
  }
}


for( i in 1:length(buyout_nhood_monthly[,1])){  #preparo l'address
  j = 9
  while (j < 12) {
    if(!(is.na(buyout_nhood_monthly[i,j]))) {
      buyout_nhood_monthly[i,8] = paste(buyout_nhood_monthly[i,8], buyout_nhood_monthly[i,j], sep=" ")
      j = j+1
    }
    else
      j = 14
    
  }
}
buyout_nhood_monthly <- buyout_nhood_monthly[, -c(1,9:11)]
colnames(buyout_nhood_monthly) <- c('nhood_month_year','buyout_amount', 'count','avg_buyout', 'month','year', 'nhood')
write.csv(buyout_nhood_monthly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/buyout_nhood_monthly.csv")



for (i in 1:length(buyout_nhood_yearly[,1])) {   #preparo bene le coordinate
  temp <- strsplit(buyout_nhood_yearly[i,2], " ")
  for (j in 1:length(temp[[1]])){
    buyout_nhood_yearly[i,5+j] <- temp[[1]][j]
  }
}


for( i in 1:length(buyout_nhood_yearly[,1])){  #preparo l'address
  j = 8
  while (j <11 ) {
    if(!(is.na(buyout_nhood_yearly[i,j]))) {
      buyout_nhood_yearly[i,7] = paste(buyout_nhood_yearly[i,7], buyout_nhood_yearly[i,j], sep=" ")
      j = j+1
    }
    else
      j = 13
    
  }
}
buyout_nhood_yearly <- buyout_nhood_yearly[, -c(1,8:10)]
colnames(buyout_nhood_yearly) <- c('year_nhood', 'buyout_amount','count','avg_buyout','year', 'nhood')
write.csv(buyout_nhood_yearly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/buyout_nhood_yearly.csv")
} #non faccio runnare, non mi esce perchè buyout_amount a me si carica come char. Qui si preparano buyout_nhood_monthly & buyout_nhood_yearly
#nhood da aggiornare

#Aggiungo i centri ai nhoods di Buyout_Agreements_Clean
{
Buyout_Agreements_Clean$centroid_lat <- NA
Buyout_Agreements_Clean$centroid_long <- NA

for (i in 1:length(Buyout_Agreements_Clean[,1])){
  if (Buyout_Agreements_Clean$nhood[i] == 'Bayview Hunters Point'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.730696
    Buyout_Agreements_Clean$centroid_long[i] <- -122.388515
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Bernal Heights'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.741391
    Buyout_Agreements_Clean$centroid_long[i] <- -122.414033
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Castro/Upper Market'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.758812
    Buyout_Agreements_Clean$centroid_long[i] <- -122.435644
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Chinatown'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.795073
    Buyout_Agreements_Clean$centroid_long[i] <- -122.406350
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Excelsior'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.722300
    Buyout_Agreements_Clean$centroid_long[i] <- -122.431224
  }
  
  if (Buyout_Agreements_Clean$nhood[i] == 'Financial District/South Beach'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.793213
    Buyout_Agreements_Clean$centroid_long[i] <- -122.398187
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Glen Park'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.735976
    Buyout_Agreements_Clean$centroid_long[i] <- -122.434464
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Haight Ashbury'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.769910
    Buyout_Agreements_Clean$centroid_long[i] <- -122.447678
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Hayes Valley'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.776425
    Buyout_Agreements_Clean$centroid_long[i] <- -122.426235
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Inner Richmond'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.778849
    Buyout_Agreements_Clean$centroid_long[i] <- -122.468955 
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Inner Sunset'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.762068
    Buyout_Agreements_Clean$centroid_long[i] <- -122.467189
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Japantown'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.785840
    Buyout_Agreements_Clean$centroid_long[i] <- -122.429952
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Lakeshore'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.718720
    Buyout_Agreements_Clean$centroid_long[i] <- -122.477916
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Lone Mountain/USF'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.777975
    Buyout_Agreements_Clean$centroid_long[i] <- -122.452816
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Marina'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.802877
    Buyout_Agreements_Clean$centroid_long[i] <- -122.436969
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'McLaren Park'){ #non molto convinto, l'ho messo in mezzo al parco, le vie sono tutte attorno al parco gigante e non si capiscono i confini su google maps
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.718407
    Buyout_Agreements_Clean$centroid_long[i] <- -122.418904
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Mission'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.758338
    Buyout_Agreements_Clean$centroid_long[i] <- -122.414678
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Mission Bay'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.770674
    Buyout_Agreements_Clean$centroid_long[i] <- -122.392015
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Nob Hill'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.793044
    Buyout_Agreements_Clean$centroid_long[i] <- -122.414461
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Noe Valley'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.747095
    Buyout_Agreements_Clean$centroid_long[i] <- -122.431421
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'North Beach'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.799254
    Buyout_Agreements_Clean$centroid_long[i] <- -122.407458
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Oceanview/Merced/Ingleside'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.718959
    Buyout_Agreements_Clean$centroid_long[i] <- -122.462039
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Outer Mission'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.709585
    Buyout_Agreements_Clean$centroid_long[i] <- -122.455082
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Outer Richmond'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.776683
    Buyout_Agreements_Clean$centroid_long[i] <- -122.489908
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Pacific Heights'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.792182
    Buyout_Agreements_Clean$centroid_long[i] <- -122.432784
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Portola'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.726470
    Buyout_Agreements_Clean$centroid_long[i] <- -122.407642
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Potrero Hill'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.759056
    Buyout_Agreements_Clean$centroid_long[i] <- -122.398618
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Presidio Heights' | Buyout_Agreements_Clean$nhood[i] == 'Presidio'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.788849
    Buyout_Agreements_Clean$centroid_long[i] <- -122.452228
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Russian Hill'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.800432
    Buyout_Agreements_Clean$centroid_long[i] <- -122.417609
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Seacliff'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.785975
    Buyout_Agreements_Clean$centroid_long[i] <- -122.489522
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'South of Market'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.779527
    Buyout_Agreements_Clean$centroid_long[i] <- -122.405727
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Sunset/Parkside'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.749026
    Buyout_Agreements_Clean$centroid_long[i] <- -122.493742
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Tenderloin'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.784141
    Buyout_Agreements_Clean$centroid_long[i] <- -122.414748
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Twin Peaks'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.753375
    Buyout_Agreements_Clean$centroid_long[i] <- -122.445652
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Visitacion Valley'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.713141
    Buyout_Agreements_Clean$centroid_long[i] <- -122.408825
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'West of Twin Peaks'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.737955
    Buyout_Agreements_Clean$centroid_long[i] <- -122.460363
  }
  if (Buyout_Agreements_Clean$nhood[i] == 'Western Addition'){
    Buyout_Agreements_Clean$centroid_lat[i] <- 37.780587
    Buyout_Agreements_Clean$centroid_long[i] <- -122.434510
  }
}

write.csv(Buyout_Agreements_Clean,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Buyout_Agreements_Clean.csv")
}
#nhood da aggiornare

#Aggiungo i centri ai nhoods di buyout_nhood_monthly
{
  buyout_nhood_monthly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/buyout_nhood_monthly.csv", header=TRUE)
  buyout_nhood_monthly$centroid_lat <- NA
  buyout_nhood_monthly$centroid_long <- NA
  
  for (i in 1:length(buyout_nhood_monthly[,1])){
    if (buyout_nhood_monthly$nhood[i] == 'Bayview Hunters Point'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.730696
      buyout_nhood_monthly$centroid_long[i] <- -122.388515
    }
    if (buyout_nhood_monthly$nhood[i] == 'Bernal Heights'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.741391
      buyout_nhood_monthly$centroid_long[i] <- -122.414033
    }
    if (buyout_nhood_monthly$nhood[i] == 'Castro/Upper Market'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.758812
      buyout_nhood_monthly$centroid_long[i] <- -122.435644
    }
    if (buyout_nhood_monthly$nhood[i] == 'Chinatown'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.795073
      buyout_nhood_monthly$centroid_long[i] <- -122.406350
    }
    if (buyout_nhood_monthly$nhood[i] == 'Excelsior'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.722300
      buyout_nhood_monthly$centroid_long[i] <- -122.431224
    }
    
    if (buyout_nhood_monthly$nhood[i] == 'Financial District/South Beach'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.793213
      buyout_nhood_monthly$centroid_long[i] <- -122.398187
    }
    if (buyout_nhood_monthly$nhood[i] == 'Glen Park'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.735976
      buyout_nhood_monthly$centroid_long[i] <- -122.434464
    }
    if (buyout_nhood_monthly$nhood[i] == 'Haight Ashbury'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.769910
      buyout_nhood_monthly$centroid_long[i] <- -122.447678
    }
    if (buyout_nhood_monthly$nhood[i] == 'Hayes Valley'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.776425
      buyout_nhood_monthly$centroid_long[i] <- -122.426235
    }
    if (buyout_nhood_monthly$nhood[i] == 'Inner Richmond'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.778849
      buyout_nhood_monthly$centroid_long[i] <- -122.468955 
    }
    if (buyout_nhood_monthly$nhood[i] == 'Inner Sunset'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.762068
      buyout_nhood_monthly$centroid_long[i] <- -122.467189
    }
    if (buyout_nhood_monthly$nhood[i] == 'Japantown'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.785840
      buyout_nhood_monthly$centroid_long[i] <- -122.429952
    }
    if (buyout_nhood_monthly$nhood[i] == 'Lakeshore'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.718720
      buyout_nhood_monthly$centroid_long[i] <- -122.477916
    }
    if (buyout_nhood_monthly$nhood[i] == 'Lone Mountain/USF'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.777975
      buyout_nhood_monthly$centroid_long[i] <- -122.452816
    }
    if (buyout_nhood_monthly$nhood[i] == 'Marina'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.802877
      buyout_nhood_monthly$centroid_long[i] <- -122.436969
    }
    if (buyout_nhood_monthly$nhood[i] == 'McLaren Park'){ #non molto convinto, l'ho messo in mezzo al parco, le vie sono tutte attorno al parco gigante e non si capiscono i confini su google maps
      buyout_nhood_monthly$centroid_lat[i] <- 37.718407
      buyout_nhood_monthly$centroid_long[i] <- -122.418904
    }
    if (buyout_nhood_monthly$nhood[i] == 'Mission'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.758338
      buyout_nhood_monthly$centroid_long[i] <- -122.414678
    }
    if (buyout_nhood_monthly$nhood[i] == 'Mission Bay'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.770674
      buyout_nhood_monthly$centroid_long[i] <- -122.392015
    }
    if (buyout_nhood_monthly$nhood[i] == 'Nob Hill'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.793044
      buyout_nhood_monthly$centroid_long[i] <- -122.414461
    }
    if (buyout_nhood_monthly$nhood[i] == 'Noe Valley'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.747095
      buyout_nhood_monthly$centroid_long[i] <- -122.431421
    }
    if (buyout_nhood_monthly$nhood[i] == 'North Beach'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.799254
      buyout_nhood_monthly$centroid_long[i] <- -122.407458
    }
    if (buyout_nhood_monthly$nhood[i] == 'Oceanview/Merced/Ingleside'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.718959
      buyout_nhood_monthly$centroid_long[i] <- -122.462039
    }
    if (buyout_nhood_monthly$nhood[i] == 'Outer Mission'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.709585
      buyout_nhood_monthly$centroid_long[i] <- -122.455082
    }
    if (buyout_nhood_monthly$nhood[i] == 'Outer Richmond'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.776683
      buyout_nhood_monthly$centroid_long[i] <- -122.489908
    }
    if (buyout_nhood_monthly$nhood[i] == 'Pacific Heights'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.792182
      buyout_nhood_monthly$centroid_long[i] <- -122.432784
    }
    if (buyout_nhood_monthly$nhood[i] == 'Portola'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.726470
      buyout_nhood_monthly$centroid_long[i] <- -122.407642
    }
    if (buyout_nhood_monthly$nhood[i] == 'Potrero Hill'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.759056
      buyout_nhood_monthly$centroid_long[i] <- -122.398618
    }
    if (buyout_nhood_monthly$nhood[i] == 'Presidio Heights' | buyout_nhood_monthly$nhood[i] == 'Presidio'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.788849
      buyout_nhood_monthly$centroid_long[i] <- -122.452228
    }
    if (buyout_nhood_monthly$nhood[i] == 'Russian Hill'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.800432
      buyout_nhood_monthly$centroid_long[i] <- -122.417609
    }
    if (buyout_nhood_monthly$nhood[i] == 'Seacliff'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.785975
      buyout_nhood_monthly$centroid_long[i] <- -122.489522
    }
    if (buyout_nhood_monthly$nhood[i] == 'South of Market'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.779527
      buyout_nhood_monthly$centroid_long[i] <- -122.405727
    }
    if (buyout_nhood_monthly$nhood[i] == 'Sunset/Parkside'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.749026
      buyout_nhood_monthly$centroid_long[i] <- -122.493742
    }
    if (buyout_nhood_monthly$nhood[i] == 'Tenderloin'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.784141
      buyout_nhood_monthly$centroid_long[i] <- -122.414748
    }
    if (buyout_nhood_monthly$nhood[i] == 'Twin Peaks'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.753375
      buyout_nhood_monthly$centroid_long[i] <- -122.445652
    }
    if (buyout_nhood_monthly$nhood[i] == 'Visitacion Valley'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.713141
      buyout_nhood_monthly$centroid_long[i] <- -122.408825
    }
    if (buyout_nhood_monthly$nhood[i] == 'West of Twin Peaks'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.737955
      buyout_nhood_monthly$centroid_long[i] <- -122.460363
    }
    if (buyout_nhood_monthly$nhood[i] == 'Western Addition'){
      buyout_nhood_monthly$centroid_lat[i] <- 37.780587
      buyout_nhood_monthly$centroid_long[i] <- -122.434510
    }
  }
  
  write.csv(buyout_nhood_monthly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/buyout_nhood_monthly.csv")
}
#nhood da aggiornare

#Aggiungo i centri ai nhoods di buyout_nhood_yearly
{
  buyout_nhood_yearly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/buyout_nhood_yearly.csv", header=TRUE)
  buyout_nhood_yearly$centroid_lat <- NA
  buyout_nhood_yearly$centroid_long <- NA
  
  for (i in 1:length(buyout_nhood_yearly[,1])){
    if (buyout_nhood_yearly$nhood[i] == 'Bayview Hunters Point'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.730696
      buyout_nhood_yearly$centroid_long[i] <- -122.388515
    }
    if (buyout_nhood_yearly$nhood[i] == 'Bernal Heights'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.741391
      buyout_nhood_yearly$centroid_long[i] <- -122.414033
    }
    if (buyout_nhood_yearly$nhood[i] == 'Castro/Upper Market'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.758812
      buyout_nhood_yearly$centroid_long[i] <- -122.435644
    }
    if (buyout_nhood_yearly$nhood[i] == 'Chinatown'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.795073
      buyout_nhood_yearly$centroid_long[i] <- -122.406350
    }
    if (buyout_nhood_yearly$nhood[i] == 'Excelsior'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.722300
      buyout_nhood_yearly$centroid_long[i] <- -122.431224
    }
    
    if (buyout_nhood_yearly$nhood[i] == 'Financial District/South Beach'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.793213
      buyout_nhood_yearly$centroid_long[i] <- -122.398187
    }
    if (buyout_nhood_yearly$nhood[i] == 'Glen Park'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.735976
      buyout_nhood_yearly$centroid_long[i] <- -122.434464
    }
    if (buyout_nhood_yearly$nhood[i] == 'Haight Ashbury'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.769910
      buyout_nhood_yearly$centroid_long[i] <- -122.447678
    }
    if (buyout_nhood_yearly$nhood[i] == 'Hayes Valley'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.776425
      buyout_nhood_yearly$centroid_long[i] <- -122.426235
    }
    if (buyout_nhood_yearly$nhood[i] == 'Inner Richmond'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.778849
      buyout_nhood_yearly$centroid_long[i] <- -122.468955 
    }
    if (buyout_nhood_yearly$nhood[i] == 'Inner Sunset'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.762068
      buyout_nhood_yearly$centroid_long[i] <- -122.467189
    }
    if (buyout_nhood_yearly$nhood[i] == 'Japantown'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.785840
      buyout_nhood_yearly$centroid_long[i] <- -122.429952
    }
    if (buyout_nhood_yearly$nhood[i] == 'Lakeshore'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.718720
      buyout_nhood_yearly$centroid_long[i] <- -122.477916
    }
    if (buyout_nhood_yearly$nhood[i] == 'Lone Mountain/USF'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.777975
      buyout_nhood_yearly$centroid_long[i] <- -122.452816
    }
    if (buyout_nhood_yearly$nhood[i] == 'Marina'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.802877
      buyout_nhood_yearly$centroid_long[i] <- -122.436969
    }
    if (buyout_nhood_yearly$nhood[i] == 'McLaren Park'){ #non molto convinto, l'ho messo in mezzo al parco, le vie sono tutte attorno al parco gigante e non si capiscono i confini su google maps
      buyout_nhood_yearly$centroid_lat[i] <- 37.718407
      buyout_nhood_yearly$centroid_long[i] <- -122.418904
    }
    if (buyout_nhood_yearly$nhood[i] == 'Mission'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.758338
      buyout_nhood_yearly$centroid_long[i] <- -122.414678
    }
    if (buyout_nhood_yearly$nhood[i] == 'Mission Bay'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.770674
      buyout_nhood_yearly$centroid_long[i] <- -122.392015
    }
    if (buyout_nhood_yearly$nhood[i] == 'Nob Hill'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.793044
      buyout_nhood_yearly$centroid_long[i] <- -122.414461
    }
    if (buyout_nhood_yearly$nhood[i] == 'Noe Valley'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.747095
      buyout_nhood_yearly$centroid_long[i] <- -122.431421
    }
    if (buyout_nhood_yearly$nhood[i] == 'North Beach'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.799254
      buyout_nhood_yearly$centroid_long[i] <- -122.407458
    }
    if (buyout_nhood_yearly$nhood[i] == 'Oceanview/Merced/Ingleside'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.718959
      buyout_nhood_yearly$centroid_long[i] <- -122.462039
    }
    if (buyout_nhood_yearly$nhood[i] == 'Outer Mission'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.709585
      buyout_nhood_yearly$centroid_long[i] <- -122.455082
    }
    if (buyout_nhood_yearly$nhood[i] == 'Outer Richmond'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.776683
      buyout_nhood_yearly$centroid_long[i] <- -122.489908
    }
    if (buyout_nhood_yearly$nhood[i] == 'Pacific Heights'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.792182
      buyout_nhood_yearly$centroid_long[i] <- -122.432784
    }
    if (buyout_nhood_yearly$nhood[i] == 'Portola'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.726470
      buyout_nhood_yearly$centroid_long[i] <- -122.407642
    }
    if (buyout_nhood_yearly$nhood[i] == 'Potrero Hill'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.759056
      buyout_nhood_yearly$centroid_long[i] <- -122.398618
    }
    if (buyout_nhood_yearly$nhood[i] == 'Presidio Heights' | buyout_nhood_yearly$nhood[i] == 'Presidio'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.788849
      buyout_nhood_yearly$centroid_long[i] <- -122.452228
    }
    if (buyout_nhood_yearly$nhood[i] == 'Russian Hill'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.800432
      buyout_nhood_yearly$centroid_long[i] <- -122.417609
    }
    if (buyout_nhood_yearly$nhood[i] == 'Seacliff'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.785975
      buyout_nhood_yearly$centroid_long[i] <- -122.489522
    }
    if (buyout_nhood_yearly$nhood[i] == 'South of Market'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.779527
      buyout_nhood_yearly$centroid_long[i] <- -122.405727
    }
    if (buyout_nhood_yearly$nhood[i] == 'Sunset/Parkside'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.749026
      buyout_nhood_yearly$centroid_long[i] <- -122.493742
    }
    if (buyout_nhood_yearly$nhood[i] == 'Tenderloin'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.784141
      buyout_nhood_yearly$centroid_long[i] <- -122.414748
    }
    if (buyout_nhood_yearly$nhood[i] == 'Twin Peaks'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.753375
      buyout_nhood_yearly$centroid_long[i] <- -122.445652
    }
    if (buyout_nhood_yearly$nhood[i] == 'Visitacion Valley'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.713141
      buyout_nhood_yearly$centroid_long[i] <- -122.408825
    }
    if (buyout_nhood_yearly$nhood[i] == 'West of Twin Peaks'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.737955
      buyout_nhood_yearly$centroid_long[i] <- -122.460363
    }
    if (buyout_nhood_yearly$nhood[i] == 'Western Addition'){
      buyout_nhood_yearly$centroid_lat[i] <- 37.780587
      buyout_nhood_yearly$centroid_long[i] <- -122.434510
    }
  }
  
  write.csv(buyout_nhood_yearly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/buyout_nhood_yearly.csv")
}
#nhood da aggiornare




# ------------------------------------------------------- Rent ------------------------------------------------------------------------------------




{
rent <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Original Datasets/rent.csv", header=TRUE)
rent$d = as.Date(as.character(rent$date),format = "%Y%m%d") 
rent <- rent[,-c(1:3,15:17)]
rent <- rent[(rent[,2] == 'san francisco'),] #solo s francisco città
#rent <- rent[!(is.na(rent[,7])), ] #rimuovo quelli che non hanno i metri quadri, secondo me sono inutili
#rent<- rent[!(rent[,5])==0,] #rimuovo quelli senza letti, l'idea sarebbe di usare rent/beds
#rent<-rent[!(is.na(rent[,5])),] #rimuovo quelli senza letti, l'idea sarebbe di usare rent/beds
rent <- rent[!(is.na(rent[,4])), ] #rimuovo quelli che non hanno il prezzo, secondo me sono inutili
rent <- rent[!duplicated(rent),] #elimino rent duplici (fatti nello stesso giorno, alla stessa cifra, nello stesso indirizzo)

write.csv(rent,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_pure.csv")



rent_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean.csv", header=TRUE)
for (i in 1:length(rent[,1])) {
  temp <- strsplit(rent_clean[i,13], "-")
  rent[i,13] <- temp[[1]][3]
  rent[i,14] <- temp[[1]][2]
  rent[i,15] <- temp[[1]][1]
}
length(rent[rent$V13 == '2001',15]) #5
length(rent[rent$V13 == '2002',15]) #6
length(rent[rent$V13 == '2003',15]) #7   
length(rent[rent$V13 == '2004',15]) #250
length(rent[rent$V13 == '2005',15]) #28
length(rent[rent$V13 == '2006',15]) #67
length(rent[rent$V13 == '2007',15]) #24
length(rent[rent$V13 == '2008',15]) #8
length(rent[rent$V13 == '2009',15]) #5
length(rent[rent$V13 == '2010',15]) #6
length(rent[rent$V13 == '2011',15]) #1241    #Basandoci su 'rent.csv', gli unici anni in cui studiare sono dal 2011 al 2018
length(rent[rent$V13 == '2012',15]) #2707
length(rent[rent$V13 == '2013',15]) #884
length(rent[rent$V13 == '2014',15]) #623
length(rent[rent$V13 == '2015',15]) #1587
length(rent[rent$V13 == '2016',15]) #2121
length(rent[rent$V13 == '2017',15]) #262
length(rent[rent$V13 == '2018',15]) #354

#I remove them if year < 2011    => Mi servono Construction dal 2007 al 2018!
rent <- rent[as.double(rent$V15) > 2010 ,]

#Aggiungo rent/mq e rimuovo sq feet e price, beds, rooms, address, data completa, city&county( always == san francisco)
rent[,16]= rent[,4]/rent[,7]
rent <- rent[,-c(2:9)]
colnames(rent) <- c('nhood', 'lat', 'lon','date', 'day', 'month','year', 'rent.sq')
write.csv(rent,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean.csv")
rent_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean.csv", header=TRUE)

#uniformo i nomi dei nhood a quelli di tutti gli altri datasets
rent_clean$nhood <- str_replace(rent_clean$nhood, "castro", "Castro/Upper Market")
rent_clean$nhood <- str_replace(rent_clean$nhood, "alamo square", "Haight Ashbury/Hayes Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "bayview", "Bayview Hunters Point")
rent_clean$nhood <- str_replace(rent_clean$nhood, "hunters point", "Bayview Hunters Point")
rent_clean$nhood <- str_replace(rent_clean$nhood, "bernal", "Bernal Heights")
rent_clean$nhood <- str_replace(rent_clean$nhood, "candlestick point", "Bayview Hunters Point")
rent_clean$nhood <- str_replace(rent_clean$nhood, "civic / van ness", "Tenderloin")
rent_clean$nhood <- str_replace(rent_clean$nhood, "cole valley", "Haight Ashbury/Hayes Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "diamond heights", "Glen Park/Noe Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "excelsior / outer mission", "Excelsior/Outer Mission")
rent_clean$nhood <- str_replace(rent_clean$nhood, "lower pac hts", "Pacific Heights")
rent_clean$nhood <- str_replace(rent_clean$nhood, "marina / cow hollow", "Marina")
rent_clean$nhood <- str_replace(rent_clean$nhood, "mission district", "Mission")
rent_clean$nhood <- str_replace(rent_clean$nhood, "north beach / telegraph hill", "North Beach")
rent_clean$nhood <- str_replace(rent_clean$nhood, "parkside", "Sunset/Parkside")
rent_clean$nhood <- str_replace(rent_clean$nhood, "outer sunset", "Sunset/Parkside")
rent_clean$nhood <- str_replace(rent_clean$nhood, "presidio hts / laurel hts / lake st", "Presidio Heights")
rent_clean$nhood <- str_replace(rent_clean$nhood, "sea cliff", "Seacliff")
rent_clean$nhood <- str_replace(rent_clean$nhood, "lower haight", "Haight Ashbury/Hayes Valley")
rent_clean$nhood <- str_replace(rent_clean$nhood, "ingleside", "Oceanview/Merced/Ingleside")
rent_clean$nhood <- mapply(function(x) str_to_title(x), rent_clean$nhood)
rent_clean$nhood <- str_replace(rent_clean$nhood, "Usf / Anza Vista", "Lone Mountain/USF")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Nopa", "Lone Mountain/USF")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Financial District", "Financial District/South Beach/SOMA")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Downtown", "Financial District/South Beach/SOMA")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Soma / South Beach", "Financial District/South Beach/SOMA")
rent_clean$nhood <- str_replace(rent_clean$nhood, "West Portal / Forest Hills", "West of Twin Peaks")
rent_clean$nhood <- str_replace(rent_clean$nhood, "Ccsf", "West of Twin Peaks")
write.csv(rent_clean,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean.csv")




} #preparazione di base

{
#Calcolo i df con avg rent per nhood per anno e per mese
vect_year = paste(rent_clean$year)
vect_month = paste(rent_clean$month)
vect_nhood = paste(rent_clean$nhood)
vect_aus = paste(vect_month,vect_year,vect_nhood)
vect_aus2 = paste(vect_year,vect_nhood)
rent_clean$year_nhood = vect_aus2
rent_clean$month_year_nhood = vect_aus
rm(vect_aus,vect_aus2,vect_year,vect_nhood,vect_month)

length(unique(rent_clean$year_nhood))
rent_nhood_yearly = aggregate(rent_clean$price_mq, by = list(rent_clean$year_nhood), FUN = mean)
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'x'] <- 'avg_rent.sq'

length(unique(rent_clean$month_year_nhood))
rent_nhood_monthly = aggregate(rent_clean$price_mq, by = list(rent_clean$month_year_nhood), FUN = mean)
names(rent_nhood_monthly)[names(rent_nhood_monthly) == 'Group.1'] <- 'nhood_month_year'
names(rent_nhood_monthly)[names(rent_nhood_monthly) == 'x'] <- 'avg_rent.sq'
#Adesso son da splittare di nuovo mese-anno e nhood e si può plottare tutto!

for (i in 1:length(rent_nhood_monthly[,1])) {   #preparo bene le coordinate
  temp <- strsplit(rent_nhood_monthly[i,1], " ")
  for (j in 1:length(temp[[1]])){
  rent_nhood_monthly[i,2+j] <- temp[[1]][j]
  }
}


for( i in 1:length(rent_nhood_monthly[,1])){  #preparo l'address
  j = 6
  while (j < 9) {
    if(!(is.na(rent_nhood_monthly[i,j]))) {
      rent_nhood_monthly[i,5] = paste(rent_nhood_monthly[i,5], rent_nhood_monthly[i,j], sep=" ")
      j = j+1
    }
      else
        j = 14
    
  }
}
rent_nhood_monthly <- rent_nhood_monthly[, -c(6:12)]
colnames(rent_nhood_monthly) <- c('nhood_month_year', 'avg_rent.mq', 'month','year', 'nhood')
rent_nhood_monthly <- rent_nhood_monthly[-which(rent_nhood_monthly$nhood == 'San Francisco'),]
write.csv(rent_nhood_monthly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_nhood_monthly_nh.csv")



for (i in 1:length(rent_nhood_yearly[,1])) {   #preparo bene le coordinate
  temp <- strsplit(rent_nhood_yearly[i,1], " ")
  for (j in 1:length(temp[[1]])){
    rent_nhood_yearly[i,2+j] <- temp[[1]][j]
  }
}


for( i in 1:length(rent_nhood_yearly[,1])){  #preparo l'address
  j = 5
  while (j < 8) {
    if(!(is.na(rent_nhood_yearly[i,j]))) {
      rent_nhood_yearly[i,4] = paste(rent_nhood_yearly[i,4], rent_nhood_yearly[i,j], sep=" ")
      j = j+1
    }
    else
      j = 13
    
  }
}
rent_nhood_yearly <- rent_nhood_yearly[, -c(5:11)]
colnames(rent_nhood_yearly) <- c('nhood_month_year', 'avg_rent.mq','year', 'nhood')
rent_nhood_yearly <- rent_nhood_yearly[-which(rent_nhood_yearly$nhood == 'San Francisco'),]
write.csv(rent_nhood_yearly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_nhood_yearly_nh.csv")

# rent_clean <- rent_clean[,-c(10,11)]
# colnames(rent_clean) <- c('X','nhood', 'lat','lon', 'd','day','month','year', 'price_sqft')
# rent_clean$price_mq <- NA
# rent_clean$price_mq <- (rent_clean$price_sqft)/0.0929
# write.csv(rent_clean,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean.csv")

} # Preparazione di rent_nhood_yearly & rent_nhood_monthly

#Calcolo avg_rent per ogni nhood. Poi divido in clusters i nhood sulla base di quanto sono alti gli affitti medi
{
  avg_rent_by_nhood = aggregate(rent_nhood_yearly$avg_rent.mq, by = list(rent_nhood_yearly$nhood), FUN = mean)
  
  
  #Calcolo due clusters
  m <- median(avg_rent_by_nhood[,2])
  high_rent_nhood2 <- NA
  low_rent_nhood2 <- NA
  j <- 1
  k <- 1
  for(i in 1:length(avg_rent_by_nhood[,1])){
    if (avg_rent_by_nhood[i,2] >= m){
      high_rent_nhood2[j] <- avg_rent_by_nhood[i,1]
      j =j+1
    }
    else{
      low_rent_nhood2[k] <- avg_rent_by_nhood[i,1]
      k = k+1
    }
    
  }
  write.csv(high_rent_nhood2,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/high_rent_nhood2_nh.csv")
  write.csv(low_rent_nhood2,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/low_rent_nhood2_nh.csv")
  
  
  
  
  #Calcolo tre clusters
  q1 <- quantile(avg_rent_by_nhood[,2],probs = 0.33)
  q2 <- quantile(avg_rent_by_nhood[,2],probs = 0.66)
  high_rent_nhood3 <- NA
  medium_rent_nhood3 <- NA
  low_rent_nhood3 <- NA
  j <- 1
  k <- 1
  t <- 1
  for(i in 1:length(avg_rent_by_nhood[,1])){
    if (avg_rent_by_nhood[i,2] >= q2){
      high_rent_nhood3[j] <- avg_rent_by_nhood[i,1]
      j =j+1
    }
    else if (avg_rent_by_nhood[i,2] <= q1){
      low_rent_nhood3[k] <- avg_rent_by_nhood[i,1]
      k = k+1
    }
    else {
      medium_rent_nhood3[t] <- avg_rent_by_nhood[i,1]
      t <- t+1
    }
    
  }
  
  write.csv(high_rent_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/high_rent_nhood3_nh.csv")
  write.csv(low_rent_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/low_rent_nhood3_nh.csv")
  write.csv(medium_rent_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/medium_rent_nhood3_nh.csv")
  
  
}


# ------------------------------------------------------- New Construction ------------------------------------------------------------------------------------




{
New_construction <- New_construction[,-c(1:5,7:11,16,23:36)]
New_construction <- New_construction[((New_construction[,4]=='APARTMENTS') | (New_construction[,4]=='1 FAMILY DWELLING') | 
                                            (New_construction[,4]=='2 FAMILY DWELLING')), ] #Rimuovo i lavori fatti su costruzioni che non mi interessano

New_construction <- New_construction[!(is.na(New_construction[,5])), ] #elimino quelli di cui non le unità abitative finali
New_construction <- New_construction[!(is.na(New_construction[,3])), ] #elimino quelli di cui non le unità abitative iniziali
New_construction <- New_construction[((New_construction[,5]-New_construction[,3]>0)), ] #Rimuovo i lavori che non aumentano le unità abitative


New_construction[,12] <- New_construction[,5]-New_construction[,3]
New_construction <- New_construction[,-c(2:5)] #colonne non utili

for (i in 1:length(New_construction[,1])) {    #preparo le date
  New_construction[i,9] <- New_construction[i,1][[1]]
}

for (i in 1:4226) {    #preparo le date
  temp <- strsplit(as.character(New_construction[i,1][[1]]), "/")
  New_construction[i,9] <- temp[[1]][2]
  New_construction[i,10] <- temp[[1]][3]
}
New_construction <- New_construction[,-c(1)]
colnames(New_construction) <- c('block', 'lot', 'street_number','street_number_suffix', 'street_name','street_suffix','new_units_built','month', 'year')
write.csv(New_construction,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/New_construction_clean.csv")

#totale 43025 unità vengono costruite

} #Prime operazioni

{ 
  New_construction_clean_geocoded_nh <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/New_construction_clean_geocoded_nh.csv", header=TRUE)
  SFNeighborhoods_new <- read_sf("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/SFNeighborhoods_new.geojson")
  constructions_by_nhood = aggregate(New_construction_clean_geocoded_nh$new_units_built, by = list(New_construction_clean_geocoded_nh$neighborhoods), FUN = sum)
  constructions_by_nhood$area <-NA
  for(i in 1:length(constructions_by_nhood[,1])){
    for(j in 1:length(SFNeighborhoods_new$area)){
      if(constructions_by_nhood$Group.1[i] ==SFNeighborhoods_new$nhood[j])
        constructions_by_nhood$area[i]=SFNeighborhoods_new$area[j]
      
    }
  }
  constructions_by_nhood[,2] <- constructions_by_nhood[,2]/constructions_by_nhood[,3]*10000 #senn� numeri troppo piccoli
  #Calcolo due clusters
  m <- median(constructions_by_nhood[,2])
  high_construction_nhood2 <- NA
  low_construction_nhood2 <- NA
  j <- 1
  k <- 1
  for(i in 1:length(constructions_by_nhood[,1])){
    if (constructions_by_nhood[i,2] >= m){
      high_construction_nhood2[j] <- constructions_by_nhood[i,1]
      j =j+1
    }
    else{
      low_construction_nhood2[k] <- constructions_by_nhood[i,1]
      k = k+1
    }
    
  }
  write.csv(high_construction_nhood2,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/high_construction_nhood2_nh.csv")
  write.csv(low_construction_nhood2,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/low_construction_nhood2_nh.csv")
  
  
  
  
  #Calcolo tre clusters
  q1 <- quantile(constructions_by_nhood[,2],probs = 0.33)
  q2 <- quantile(constructions_by_nhood[,2],probs = 0.66)
  high_construction_nhood3 <- NA
  medium_construction_nhood3 <- NA
  low_construction_nhood3 <- NA
  j <- 1
  k <- 1
  t <- 1
  for(i in 1:length(constructions_by_nhood[,1])){
    if (constructions_by_nhood[i,2] >= q2){
      high_construction_nhood3[j] <- constructions_by_nhood[i,1]
      j =j+1
    }
    else if (constructions_by_nhood[i,2] <= q1){
      low_construction_nhood3[k] <- constructions_by_nhood[i,1]
      k = k+1
    }
    else {
      medium_construction_nhood3[t] <- constructions_by_nhood[i,1]
      t <- t+1
    }
    
  }
  
  write.csv(high_construction_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/high_construction_nhood3_nh.csv")
  write.csv(low_construction_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/low_construction_nhood3_nh.csv")
  write.csv(medium_construction_nhood3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/medium_construction_nhood3_nh.csv")
  
} #Calcolo le new_construction/square feet per ogni nhood e poi faccio clusters con k = 2 e k = 3

constr_coord <- New_construction_clean_geocoded_nh[,c(14,15)]
write.csv(constr_coord,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/constr_coord.csv", row.names = FALSE)
constr_coord <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/constr_coord.csv", header=TRUE)

{ 
  New_construction_clean_geocoded_nh <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/New_construction_clean_geocoded_nh.csv", header=TRUE)
  constructions_by_nhood = aggregate(New_construction_clean_geocoded_nh$new_units_built, by = list(New_construction_clean_geocoded_nh$neighborhoods), FUN = sum)

  vect_year = paste(New_construction_clean_geocoded_nh$year)
  vect_nhood = paste(New_construction_clean_geocoded_nh$neighborhoods)
  vect_aus2 = paste(vect_year,vect_nhood)
  New_construction_clean_geocoded_nh$year_nhood = vect_aus2
  rm(vect_aus2,vect_year,vect_nhood)
  
  length(unique(New_construction_clean_geocoded_nh$year_nhood))
  New_construction_nhood_yearly = aggregate(New_construction_clean_geocoded_nh$new_units_built, by = list(New_construction_clean_geocoded_nh$year_nhood), FUN = sum)
  
  for (i in 1:length(New_construction_nhood_yearly[,1])) {   
    temp <- strsplit(New_construction_nhood_yearly[i,1], " ")
    for (j in 1:length(temp[[1]])){
      New_construction_nhood_yearly[i,2+j] <- temp[[1]][j]
    }
  }
  
  for( i in 1:length(New_construction_nhood_yearly[,1])){  #preparo l'address
    j = 5
    while (j < 8) {
      if(!(is.na(New_construction_nhood_yearly[i,j]))) {
        New_construction_nhood_yearly[i,4] = paste(New_construction_nhood_yearly[i,4], New_construction_nhood_yearly[i,j], sep=" ")
        j = j+1
      }
      else
        j = 14
      
    }
  }
  New_construction_nhood_yearly <- New_construction_nhood_yearly[,-c(5:7)]
  colnames(New_construction_nhood_yearly) <- c('group', 'new_units_built', 'year','nhood')
  
  write.csv(New_construction_nhood_yearly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/New_construction_nhood_year_GAM_nh.csv", row.names = FALSE)
  
  } #Calcolo le new_construction per ogni nhood e anno per il GAM


# ------------------------------------------------------- Parcels  ------------------------------------------------------------------------------------


{
Parcels <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Original Datasets/Parcels.csv", header=TRUE)
Parcels <- Parcels[,c(3,7:10)]
write.csv(Parcels,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Parcels_poly.csv",row.names=FALSE)

} # Continua su "Parcel centroid"



# ------------------------------------------------------- SFNeighborhoods_new  ------------------------------------------------------------------------------------

{
#   SFNeighborhoods_new <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/SFNeighborhoods_new.csv", header=TRUE)
# 
#   for( i in 1:length(SFNeighborhoods_new[,1])) { 
#     SFNeighborhoods_new[i,2] <- chartr('(', ' ', SFNeighborhoods_new[i,2])
#     SFNeighborhoods_new[i,2] <- chartr(')', ' ', SFNeighborhoods_new[i,2])
#     #SFNeighborhoods_new[i,2] <- chartr(',', ' ', SFNeighborhoods_new[i,2])
#     SFNeighborhoods_new[i,2] <- gsub("," , "",  SFNeighborhoods_new[i,2])
#     #SFNeighborhoods_new[i,2] <- gsub("  " , " ",  SFNeighborhoods_new[i,2])
#   }
# 
#  
#  for( i in 1:length(SFNeighborhoods_new[,1])) {       #tolgo la scritta "MULTIPOLYGON
#     temp <- strsplit(SFNeighborhoods_new[i,2], "N   ")
#     SFNeighborhoods_new[i,2] <- temp[[1]][2]
#  }
# 
# 
# options(digits=20)
# nhood_poly1 = matrix(as.numeric(strsplit(SFNeighborhoods_new[1,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly2 = matrix(as.numeric(strsplit(SFNeighborhoods_new[2,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly3 = matrix(as.numeric(strsplit(SFNeighborhoods_new[3,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly4 = matrix(as.numeric(strsplit(SFNeighborhoods_new[4,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly5 = matrix(as.numeric(strsplit(SFNeighborhoods_new[5,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly6 = matrix(as.numeric(strsplit(SFNeighborhoods_new[6,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly7 = matrix(as.numeric(strsplit(SFNeighborhoods_new[7,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly8 = matrix(as.numeric(strsplit(SFNeighborhoods_new[8,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly9 = matrix(as.numeric(strsplit(SFNeighborhoods_new[9,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly10 = matrix(as.numeric(strsplit(SFNeighborhoods_new[10,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly11 = matrix(as.numeric(strsplit(SFNeighborhoods_new[11,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly12 = matrix(as.numeric(strsplit(SFNeighborhoods_new[12,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly13 = matrix(as.numeric(strsplit(SFNeighborhoods_new[13,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly14 = matrix(as.numeric(strsplit(SFNeighborhoods_new[14,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly15 = matrix(as.numeric(strsplit(SFNeighborhoods_new[15,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly16 = matrix(as.numeric(strsplit(SFNeighborhoods_new[16,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly16 <- nhood_poly16[!(is.na(nhood_poly16[,1])), ] 
# nhood_poly16 <- nhood_poly16[-838, ] 
# nhood_poly17 = matrix(as.numeric(strsplit(SFNeighborhoods_new[17,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly18 = matrix(as.numeric(strsplit(SFNeighborhoods_new[18,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly19 = matrix(as.numeric(strsplit(SFNeighborhoods_new[19,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly20 = matrix(as.numeric(strsplit(SFNeighborhoods_new[20,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly21 = matrix(as.numeric(strsplit(SFNeighborhoods_new[21,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly22 = matrix(as.numeric(strsplit(SFNeighborhoods_new[22,2]," ")[[1]][-c(1,1677)]),ncol=2,byrow=TRUE)
# nhood_poly22 <-nhood_poly22[-838,] #dava problemi strani, l ho tolta, tanto è un poligono di 838 vertici
# nhood_poly23 = matrix(as.numeric(strsplit(SFNeighborhoods_new[23,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly24 = matrix(as.numeric(strsplit(SFNeighborhoods_new[24,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly24 <- nhood_poly24[-866, ] 
# nhood_poly24 <- nhood_poly24[!(is.na(nhood_poly24[,1])), ] 
# nhood_poly25 = matrix(as.numeric(strsplit(SFNeighborhoods_new[25,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly25 <-nhood_poly25[-839,] #dava problemi strani, l ho tolta, tanto è un poligono di 838 vertici
# nhood_poly26 = matrix(as.numeric(strsplit(SFNeighborhoods_new[26,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly27 = matrix(as.numeric(strsplit(SFNeighborhoods_new[27,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly28 = matrix(as.numeric(strsplit(SFNeighborhoods_new[28,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly29 = matrix(as.numeric(strsplit(SFNeighborhoods_new[29,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly30 = matrix(as.numeric(strsplit(SFNeighborhoods_new[30,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly31 = matrix(as.numeric(strsplit(SFNeighborhoods_new[31,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly32 = matrix(as.numeric(strsplit(SFNeighborhoods_new[32,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly33 = matrix(as.numeric(strsplit(SFNeighborhoods_new[33,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly34 = matrix(as.numeric(strsplit(SFNeighborhoods_new[34,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly35 = matrix(as.numeric(strsplit(SFNeighborhoods_new[35,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly36 = matrix(as.numeric(strsplit(SFNeighborhoods_new[36,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly36 <- nhood_poly36[-847, ] 
# nhood_poly36 <- nhood_poly36[!(is.na(nhood_poly36[,1])), ] 
# nhood_poly37 = matrix(as.numeric(strsplit(SFNeighborhoods_new[37,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly38 = matrix(as.numeric(strsplit(SFNeighborhoods_new[38,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly39 = matrix(as.numeric(strsplit(SFNeighborhoods_new[39,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly40 = matrix(as.numeric(strsplit(SFNeighborhoods_new[40,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# nhood_poly41 = matrix(as.numeric(strsplit(SFNeighborhoods_new[41,2]," ")[[1]][-1]),ncol=2,byrow=TRUE)
# 
# 
# write.csv(nhood_poly1,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly1.csv",row.names=FALSE)
# write.csv(nhood_poly2,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly2.csv",row.names=FALSE)
# write.csv(nhood_poly3,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly3.csv",row.names=FALSE)
# write.csv(nhood_poly4,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly4.csv",row.names=FALSE)
# write.csv(nhood_poly5,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly5.csv",row.names=FALSE)
# write.csv(nhood_poly6,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly6.csv",row.names=FALSE)
# write.csv(nhood_poly7,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly7.csv",row.names=FALSE)
# write.csv(nhood_poly8,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly8.csv",row.names=FALSE)
# write.csv(nhood_poly9,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly9.csv",row.names=FALSE)
# write.csv(nhood_poly10,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly10.csv",row.names=FALSE)
# write.csv(nhood_poly11,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly11.csv",row.names=FALSE)
# write.csv(nhood_poly12,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly12.csv",row.names=FALSE)
# write.csv(nhood_poly13,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly13.csv",row.names=FALSE)
# write.csv(nhood_poly14,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly14.csv",row.names=FALSE)
# write.csv(nhood_poly15,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly15.csv",row.names=FALSE)
# write.csv(nhood_poly16,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly16.csv",row.names=FALSE)
# write.csv(nhood_poly17,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly17.csv",row.names=FALSE)
# write.csv(nhood_poly18,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly18.csv",row.names=FALSE)
# write.csv(nhood_poly19,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly19.csv",row.names=FALSE)
# write.csv(nhood_poly20,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly20.csv",row.names=FALSE)
# write.csv(nhood_poly21,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly21.csv",row.names=FALSE)
# write.csv(nhood_poly22,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly22.csv",row.names=FALSE)
# write.csv(nhood_poly23,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly23.csv",row.names=FALSE)
# write.csv(nhood_poly24,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly24.csv",row.names=FALSE)
# write.csv(nhood_poly25,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly25.csv",row.names=FALSE)
# write.csv(nhood_poly26,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly26.csv",row.names=FALSE)
# write.csv(nhood_poly27,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly27.csv",row.names=FALSE)
# write.csv(nhood_poly28,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly28.csv",row.names=FALSE)
# write.csv(nhood_poly29,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly29.csv",row.names=FALSE)
# write.csv(nhood_poly30,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly30.csv",row.names=FALSE)
# write.csv(nhood_poly31,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly31.csv",row.names=FALSE)
# write.csv(nhood_poly32,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly32.csv",row.names=FALSE)
# write.csv(nhood_poly33,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly33.csv",row.names=FALSE)
# write.csv(nhood_poly34,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly34.csv",row.names=FALSE)
# write.csv(nhood_poly35,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly35.csv",row.names=FALSE)
# write.csv(nhood_poly36,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly36.csv",row.names=FALSE)
# write.csv(nhood_poly37,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly37.csv",row.names=FALSE)
# write.csv(nhood_poly38,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly38.csv",row.names=FALSE)
# write.csv(nhood_poly39,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly39.csv",row.names=FALSE)
# write.csv(nhood_poly40,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly40.csv",row.names=FALSE)
# write.csv(nhood_poly41,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Nhoods Polygon/nhood_poly41.csv",row.names=FALSE)
} #Preparo il dataset in modo tale da poter plottare i poligoni (INUTILE)

{
  SFNeighborhoods_new <- read_sf("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/SFNeighborhoods_new.geojson")
  original <- SFNeighborhoods_new
 SFNeighborhoods_new$centroid_lat <- NA
 SFNeighborhoods_new$centroid_long <- NA
  for (i in 1:length(SFNeighborhoods_new$centroid)){
   SFNeighborhoods_new$centroid[i] <- chartr('c(', '  ', SFNeighborhoods_new$centroid[i])
   SFNeighborhoods_new$centroid[i] <- chartr(')', '  ', SFNeighborhoods_new$centroid[i])
   SFNeighborhoods_new$centroid_long[i] <- as.double(strsplit(SFNeighborhoods_new$centroid[i],", ")[[1]][1])
   SFNeighborhoods_new$centroid_lat[i] <- as.double(strsplit(SFNeighborhoods_new$centroid[i],", ")[[1]][2])
 }
  
  
  
  write.csv(SFNeighborhoods_new,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/SFNeighborhoods_new.csv", row.names = FALSE)
  
  
} # Divido "centroide" in due colonne, pronto all'uso

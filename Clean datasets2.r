library(readxl)
library("writexl")


Buyout_Agreements <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Buyout_Agreements.csv", header=TRUE)
rent <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent.csv", header=TRUE)
Eviction_Notices <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices.csv", header=TRUE)
New_construction <-  read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/New_construction.csv", header=TRUE, sep = ";")
Parcels <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Parcels.csv", header=TRUE)


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
write.csv(eviction_nhood_monthly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_nhood_monthly.csv")



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
write.csv(eviction_nhood_yearly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/eviction_nhood_yearly.csv")
} # Preparazione di eviction_nhood_yearly & eviction_nhood_monthly





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





# ------------------------------------------------------- Rent ------------------------------------------------------------------------------------




{
rent$d = as.Date(as.character(rent$date),format = "%Y%m%d") 
rent <- rent[,-c(1:3,15:17)]
rent <- rent[(rent[,2] == 'san francisco'),] #solo s francisco città
rent <- rent[!(is.na(rent[,7])), ] #rimuovo quelli che non hanno i metri quadri, secondo me sono inutili
rent <- rent[!(is.na(rent[,4])), ] #rimuovo quelli che non hanno il prezzo, secondo me sono inutili
rent <- rent[!duplicated(rent),] #elimino rent duplici (fatti nello stesso giorno, alla stessa cifra, nello stesso indirizzo)
write.csv(rent,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean.csv")
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
rent_nhood_yearly = aggregate(rent_clean$`rent.sq`, by = list(rent_clean$year_nhood), FUN = mean)
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'x'] <- 'avg_rent.sq'

length(unique(rent_clean$month_year_nhood))
rent_nhood_monthly = aggregate(rent_clean$`rent.sq`, by = list(rent_clean$month_year_nhood), FUN = mean)
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
  while (j < 13) {
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
write.csv(rent_nhood_monthly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_nhood_monthly.csv")



for (i in 1:length(rent_nhood_yearly[,1])) {   #preparo bene le coordinate
  temp <- strsplit(rent_nhood_yearly[i,1], " ")
  for (j in 1:length(temp[[1]])){
    rent_nhood_yearly[i,2+j] <- temp[[1]][j]
  }
}


for( i in 1:length(rent_nhood_yearly[,1])){  #preparo l'address
  j = 5
  while (j < 12) {
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
write.csv(rent_nhood_yearly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_nhood_yearly.csv")

rent_clean <- rent_clean[,-c(10,11)]
colnames(rent_clean) <- c('X','nhood', 'lat','lon', 'd','day','month','year', 'price_sqft')
rent_clean$price_mq <- NA
rent_clean$price_mq <- (rent_clean$price_sqft)/0.0929
write.csv(rent_clean,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean.csv")

} # Preparazione di rent_nhood_yearly & rent_nhood_monthly



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

}




# ------------------------------------------------------- Parcels  ------------------------------------------------------------------------------------


{
Parcels <- Parcels[which(Parcels$RESUNITS > 0), ] #elimino quelle dove non abita nessuno
Parcels <- Parcels[which(Parcels$LANDUSE == 'RESIDENT'), ] #elemino quelle che non sono a fini abitativi
Parcels <- Parcels[,-c(14,16:21) ]
} # Continua su "Parcel centroid"
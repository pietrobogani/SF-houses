library(readxl)
library("writexl")


Buyout_Agreements <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Buyout_Agreements.csv", header=TRUE)
rent <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent.csv", header=TRUE)
Eviction_Notices <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices.csv", header=TRUE)
October2015Permits <- read_excel("October2015Permits.xlsx") # per provare a pulirne uno
Parcels <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Parcels.csv", header=TRUE)



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

write.csv(Eviction_Notices,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Eviction_Notices_Clean.csv")







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
  rent[i,13] <- temp[[1]][1]
  rent[i,14] <- temp[[1]][2]
}
length(rent[rent$V13 == '2001',13]) #5
length(rent[rent$V13 == '2002',13]) #6
length(rent[rent$V13 == '2003',13]) #7   
length(rent[rent$V13 == '2004',13]) #250
length(rent[rent$V13 == '2005',13]) #28
length(rent[rent$V13 == '2006',13]) #67
length(rent[rent$V13 == '2007',13]) #24
length(rent[rent$V13 == '2008',13]) #8
length(rent[rent$V13 == '2009',13]) #5
length(rent[rent$V13 == '2010',13]) #6
length(rent[rent$V13 == '2011',13]) #1241    #Basandoci su 'rent.csv', gli unici anni in cui studiare sono dal 2011 al 2018
length(rent[rent$V13 == '2012',13]) #2707
length(rent[rent$V13 == '2013',13]) #884
length(rent[rent$V13 == '2014',13]) #623
length(rent[rent$V13 == '2015',13]) #1587
length(rent[rent$V13 == '2016',13]) #2121
length(rent[rent$V13 == '2017',13]) #262
length(rent[rent$V13 == '2018',13]) #354

#I remove them if year < 2011    => Mi servono Construction dal 2007 al 2014
rent <- rent[as.double(rent$V13) > 2010 ,]

#Aggiungo rent/mq e rimuovo sq feet e price, beds, rooms, address, data completa, city&county( always == san francisco)
rent[,15]= rent[,4]/rent[,7]
rent <- rent[,-c(2:5,6:9,12)]
colnames(rent) <- c('nhood', 'lat', 'lon', 'year', 'month', 'rent/mq')
write.csv(rent,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/rent_clean.csv")
rent_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/rent_clean.csv", header=TRUE)


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
rent_nhood_yearly = aggregate(rent_clean$`rent.mq`, by = list(rent_clean$year_nhood), FUN = mean)
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'x'] <- 'avg_rent.mq'

length(unique(rent_clean$month_year_nhood))
rent_nhood_monthly = aggregate(rent_clean$`rent.mq`, by = list(rent_clean$month_year_nhood), FUN = mean)
names(rent_nhood_monthly)[names(rent_nhood_monthly) == 'Group.1'] <- 'nhood_month_year'
names(rent_nhood_monthly)[names(rent_nhood_monthly) == 'x'] <- 'avg_rent.mq'
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











October2015Permits <- October2015Permits[,-c(1:3,5:9,14:20,29:40,42)] #Script iniziale eventualmente da estendere su tutti i permits, qui fatto solo su Ottobre
October2015Permits <- October2015Permits[((October2015Permits[,4]=='APARTMENTS') | (October2015Permits[,4]=='1 FAMILY DWELLING') | 
                                            (October2015Permits[,4]=='2 FAMILY DWELLING')), ] #Rimuovo i lavori fatti su costruzioni che non mi interessano
October2015Permits <- October2015Permits[((October2015Permits[,5]-October2015Permits[,3]>0)), ] #Rimuovo i lavori che non aumentano le unità abitative
October2015Permits <- October2015Permits[!duplicated(October2015Permits),]
October2015Permits <- October2015Permits[!(is.na(October2015Permits[,4])), ] #elimino quelli di cui non so la destinazione d'uso
October2015Permits[,15] <- October2015Permits[,5]-October2015Permits[,3]
October2015Permits <- October2015Permits[,-c(2:5,9,12:14)] #colonne non utili

for (i in 1:length(October2015Permits[,1])) {    #preparo le date
  October2015Permits[i,9] <- October2015Permits[i,1][[1]]
}



for (i in 1:106) {    #preparo le date
  temp <- strsplit(as.character(October2015Permits[i,1][[1]]), "-")
  October2015Permits[i,8] <- temp[[1]][1]
  October2015Permits[i,9] <- temp[[1]][2]
}
October2015Permits <- October2015Permits[,-c(1)]
colnames(October2015Permits) <- c('block', 'lot', 'street_number', 'street_name','street_suffix','new_units_built', 'year', 'month')
write.csv(October2015Permits,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/October2015Permits_clean.csv")






Parcels <- Parcels[which(Parcels$RESUNITS > 0), ] #elimino quelle dove non abita nessuno
Parcels <- Parcels[which(Parcels$LANDUSE == 'RESIDENT'), ] #elemino quelle che non sono a fini abitativi
Parcels <- Parcels[,-c(14,16:21) ]
#Continua su "Parcel centroid"
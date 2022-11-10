library(readxl)
library("writexl")


Buyout_Agreements <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Buyout_Agreements.csv", header=TRUE)
rent <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/rent.csv", header=TRUE)
Eviction_Notices <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Eviction_Notices.csv", header=TRUE)
October2015Permits <- read_excel("October2015Permits.xlsx") # per provare a pulirne uno
Parcels <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Parcels.csv", header=TRUE)


Eviction_Notices <- Eviction_Notices[,-c(1,3:5,7:27,30:45)] #selezione covariate di interesse
Eviction_Notices <- Eviction_Notices[!(Eviction_Notices[,3]==""), ] #elimino gli sfratti senza quartiere
Eviction_Notices <- Eviction_Notices[!duplicated(Eviction_Notices),] #elimino gli sfratti duplici (fatti lo stesso giorno nello stesso indirizzo)
write.csv(Eviction_Notices,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Eviction_Notices_Clean.csv")

Buyout_Agreements <- Buyout_Agreements[,-c(1,2,5:7,27,9,10,13:27)] #selezione covariate di interesse
Buyout_Agreements <- Buyout_Agreements[!(Buyout_Agreements[,4]==""), ] #elimino buyout senza quartiere
Buyout_Agreements <- Buyout_Agreements[!duplicated(Buyout_Agreements),] #elimino buyout duplici (fatti nello stesso giorno, alla stessa cifra, nello stesso indirizzo)
write.csv(Buyout_Agreements,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Buyout_Agreements_Clean.csv")


rent$d = as.Date(as.character(rent$date),format = "%Y%m%d") 
rent <- rent[,-c(1:3,15:17)]
rent <- rent[(rent[,2] == 'san francisco'),] #solo s francisco città
rent <- rent[!(is.na(rent[,7])), ] #rimuovo quelli che non hanno i metri quadri, secondo me sono inutili
rent <- rent[!(is.na(rent[,4])), ] #rimuovo quelli che non hanno il prezzo, secondo me sono inutili
rent <- rent[!duplicated(rent),] #elimino rent duplici (fatti nello stesso giorno, alla stessa cifra, nello stesso indirizzo)
write.csv(rent,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/rent_clean.csv")
rent_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/rent_clean.csv", header=TRUE)
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






October2015Permits <- October2015Permits[,-c(1:3,5:9,14:20,29:40,42)] #Script iniziale eventualmente da estendere su tutti i permits, qui fatto solo su Ottobre
October2015Permits <- October2015Permits[((October2015Permits[,4]=='APARTMENTS') | (October2015Permits[,4]=='1 FAMILY DWELLING') | 
                                            (October2015Permits[,4]=='2 FAMILY DWELLING')), ] #Rimuovo i lavori fatti su costruzioni che non mi interessano
October2015Permits <- October2015Permits[((October2015Permits[,5]-October2015Permits[,3]>0)), ] #Rimuovo i lavori che non aumentano le unità abitative
October2015Permits <- October2015Permits[!duplicated(October2015Permits),]
October2015Permits <- October2015Permits[!(is.na(October2015Permits[,4])), ] #elimino quelli di cui non so la destinazione d'uso
write.csv(October2015Permits,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/October2015Permits_clean.csv")


Parcels <- Parcels[which(Parcels$RESUNITS > 0), ] #elimino quelle dove non abita nessuno
Parcels <- Parcels[which(Parcels$LANDUSE == 'RESIDENT'), ] #elemino quelle che non sono a fini abitativi
Parcels <- Parcels[,-c(14,16:21) ]

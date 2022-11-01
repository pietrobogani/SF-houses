library(readxl)

Buyout_Agreements <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Buyout_Agreements.csv", header=TRUE)
#new_construction <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/new_construction.csv", header=TRUE)
rent <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/rent.csv", header=TRUE)
#sf_permits <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/sf_permits.csv", header=TRUE)
Eviction_Notices <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Eviction_Notices.csv", header=TRUE)

Buyout_Agreements <- read.csv("Buyout_Agreements.csv", header=TRUE)
#new_construction <- read.csv("new_construction.csv", header=TRUE)
rent <- read.csv("rent.csv", header=TRUE)
#sf_permits <- read.csv("sf_permits.csv", header=TRUE)
Eviction_Notices <- read.csv("Eviction_Notices.csv", header=TRUE)
October2015Permits <- read_excel("October2015Permits.xlsx") # per provare a pulirne uno



Eviction_Notices <- Eviction_Notices[,-c(1,3:5,7:27,30:45)] #selezione covariate di interesse
Eviction_Notices <- Eviction_Notices[!(Eviction_Notices[,3]==""), ] #elimino gli sfratti senza quartiere
Eviction_Notices <- Eviction_Notices[!duplicated(Eviction_Notices),] #elimino gli sfratti duplici (fatti lo stesso giorno nello stesso indirizzo)

Buyout_Agreements <- Buyout_Agreements[,-c(1,2,5:7,27,9,10,13:27)] #selezione covariate di interesse
Buyout_Agreements <- Buyout_Agreements[!(Buyout_Agreements[,4]==""), ] #elimino buyout senza quartiere
Buyout_Agreements <- Buyout_Agreements[!duplicated(Buyout_Agreements),] #elimino buyout duplici (fatti nello stesso giorno, alla stessa cifra, nello stesso indirizzo)

#new_construction <- new_construction[,-c(1:3,10)]
#new_construction <- new_construction[!duplicated(new_construction),] #son sparsi per varie county e non solo SF =(

#sf_permits per me non sappiamo che farcene

rent$d = as.Date(as.character(rent$date),format = "%Y%m%d") 
rent <- rent[,-c(1:3,15:17)]
rent <- rent[!(is.na(rent[,7])), ] #rimuovo quelli che non hanno i metri quadri, secondo me sono inutili
rent <- rent[!(is.na(rent[,4])), ] #rimuovo quelli che non hanno il prezzo, secondo me sono inutili
rent <- rent[!duplicated(rent),] #elimino rent duplici (fatti nello stesso giorno, alla stessa cifra, nello stesso indirizzo)


October2015Permits <- October2015Permits[,-c(1:3,5:9,14:20,29:40,42)] #Script iniziale eventualmente da estendere su tutti i permits, qui fatto solo su Ottobre
October2015Permits <- October2015Permits[((October2015Permits[,4]=='APARTMENTS') | (October2015Permits[,4]=='1 FAMILY DWELLING') | 
                                            (October2015Permits[,4]=='2 FAMILY DWELLING')), ] #Rimuovo i lavori fatti su costruzioni che non mi interessano
October2015Permits <- October2015Permits[((October2015Permits[,5]-October2015Permits[,3]>0)), ] #Rimuovo i lavori che non aumentano le unità abitative
October2015Permits <- October2015Permits[!duplicated(October2015Permits),]
October2015Permits <- October2015Permits[!(is.na(October2015Permits[,4])), ] #elimino quelli di cui non so la destinazione d'uso

unique(rent$nhood)
ind_alameda = which(rent$nhood == 'alameda')
plot(rent[ind_alameda,]$d, rent[ind_alameda,]$price)

unique(Buyout_Agreements$Analysis.Neighborhood)
unique(Eviction_Notices$Neighborhoods...Analysis.Boundaries)

Buyout_Agreements <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Buyout_Agreements.csv", header=TRUE)
new_construction <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/new_construction.csv", header=TRUE)
rent <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/rent.csv", header=TRUE)
sf_permits <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/sf_permits.csv", header=TRUE)
Eviction_Notices <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Eviction_Notices.csv", header=TRUE)

Buyout_Agreements <- read.csv("Buyout_Agreements.csv", header=TRUE)
new_construction <- read.csv("new_construction.csv", header=TRUE)
rent <- read.csv("rent.csv", header=TRUE)
sf_permits <- read.csv("sf_permits.csv", header=TRUE)
Eviction_Notices <- read.csv("Eviction_Notices.csv", header=TRUE)


Eviction_Notices <- Eviction_Notices[,-c(1,3:5,7:27,30:45)] #selezione covariate di interesse
Eviction_Notices <- Eviction_Notices[!duplicated(Eviction_Notices),] #nota: elimino notifiche di sfratto ripetute?? su cosa si basa la ripetizione ??
Eviction_Notices <- Eviction_Notices[!(Eviction_Notices[,3]==""), ] #elimino gli sfratti senza quartiere

Buyout_Agreements <- Buyout_Agreements[,-c(1,2,5:7,27,9,10,13:27)] #selezione covariate di interesse
Buyout_Agreements <- Buyout_Agreements[!duplicated(Buyout_Agreements),] #nota: elimino ripetizioni rispetto cosa? se si ripete nel tempo ha senso tenerla...
Buyout_Agreements <- Buyout_Agreements[!(Buyout_Agreements[,4]==""), ] #elimino buyout senza quartiere

new_construction <- new_construction[,-c(1:3,10)]
new_construction <- new_construction[!duplicated(new_construction),] #son sparsi per varie county e non solo SF =(

#sf_permits per me non sappiamo che farcene

rent <- rent[,-c(1,15:17)]
rent <- rent[!duplicated(rent),] #nota: rispetto cosa tolgli i duplicati??
rent <- rent[!(is.na(rent[,9])), ] #rimuovo quelli che non hanno i metri quadri, secondo me sono inutili
rent$d = as.Date(as.character(rent$date),format = "%Y%m%d") 

unique(rent$nhood)
ind_alameda = which(rent$nhood == 'alameda')
plot(rent[ind_alameda,]$date, rent[ind_alameda,]$price)

unique(Buyout_Agreements$Analysis.Neighborhood)
unique(Eviction_Notices$Neighborhoods...Analysis.Boundaries)

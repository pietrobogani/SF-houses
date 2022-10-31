Buyout_Agreements <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Buyout_Agreements.csv", header=TRUE)
new_construction <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/new_construction.csv", header=TRUE)
rent <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/rent.csv", header=TRUE)
sf_permits <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/sf_permits.csv", header=TRUE)
Eviction_Notices <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Eviction_Notices.csv", header=TRUE)

Eviction_Notices <- Eviction_Notices[,-c(1,3:5,7:27,30:45)]
Eviction_Notices <- Eviction_Notices[!duplicated(Eviction_Notices),]
Eviction_Notices <- Eviction_Notices[!(Eviction_Notices[,3]==""), ]

Buyout_Agreements <- Buyout_Agreements[,-c(1,2,5:7,27,9,10,13:27)]
Buyout_Agreements <- Buyout_Agreements[!duplicated(Buyout_Agreements),]
Buyout_Agreements <- Buyout_Agreements[!(Buyout_Agreements[,4]==""), ]

new_construction <- new_construction[,-c(1:3,10)]
new_construction <- new_construction[!duplicated(new_construction),]

#sf_permits per me non sappiamo che farcene

rent <- rent[,-c(1,15:17)]
rent <- rent[!duplicated(rent),]
rent <- rent[!(is.na(rent[,9])), ] #rimuovo quelli che non hanno i metri quadri, secondo me sono inutili

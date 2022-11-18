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

#Creo i dataset clean:
#write.csv(Buyout_Agreements, "C://Users/tomas/Desktop/san francisco/repo_github_SF_houses/SF-houses/buyout_clean.csv")
#write.csv(rent, "C://Users/tomas/Desktop/san francisco/repo_github_SF_houses/SF-houses/rent_clean.csv")
#write.csv(Eviction_Notices, "C://Users/tomas/Desktop/san francisco/repo_github_SF_houses/SF-houses/evictions_clean.csv")



October2015Permits <- October2015Permits[,-c(1:3,5:9,14:20,29:40,42)] #Script iniziale eventualmente da estendere su tutti i permits, qui fatto solo su Ottobre
October2015Permits <- October2015Permits[((October2015Permits[,4]=='APARTMENTS') | (October2015Permits[,4]=='1 FAMILY DWELLING') | 
                                            (October2015Permits[,4]=='2 FAMILY DWELLING')), ] #Rimuovo i lavori fatti su costruzioni che non mi interessano
October2015Permits <- October2015Permits[((October2015Permits[,5]-October2015Permits[,3]>0)), ] #Rimuovo i lavori che non aumentano le unitÃ  abitative, nota: perchè?
October2015Permits <- October2015Permits[!duplicated(October2015Permits),]
October2015Permits <- October2015Permits[!(is.na(October2015Permits[,4])), ] #elimino quelli di cui non so la destinazione d'uso

unique(rent$nhood)
ind_alameda = which(rent$nhood == 'alameda')
plot(rent[ind_alameda,]$d, rent[ind_alameda,]$price)

unique(Buyout_Agreements$Analysis.Neighborhood)
unique(Eviction_Notices$Neighborhoods...Analysis.Boundaries)

A=tolower(sf_permits$neighborhoods_analysis_boundaries[order(sf_permits$neighborhoods_analysis_boundaries)])
B=tolower(rent$nhood[order(rent$nhood)])
C=tolower(Eviction_Notices$Neighborhoods...Analysis.Boundaries)
D=tolower(Buyout_Agreements$Analysis.Neighborhood)

# length(unique(A)) #42
# length(unique(B)) #158
# length(unique(C)) #41
# length(unique(D)) #39

intersect(intersect(intersect(A,B),C),D)
intersect(intersect(A,C),D)
length(intersect(intersect(A,C),D)) #tutti i quartieri di Buyout_Agreements sono contenuti in sf_permits e Eviction_Notices
# setdiff(A,D) #sf_permits e Eviction_Notices hanno due quartieri in piÃ¹ rispetto a Buyout_Agreements: "golden gate park", "treasure island"

setdiff(intersect(intersect(A,C),D),B)
unique(B)


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
rent_nhood_yearly = aggregate(rent_clean$`rent/mq`, by = list(rent_clean$year_nhood), FUN = mean)
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(rent_nhood_yearly)[names(rent_nhood_yearly) == 'x'] <- 'avg_rent/mq'

length(unique(rent_clean$month_year_nhood))
rent_nhood_monthly = aggregate(rent_clean$`rent/mq`, by = list(rent_clean$month_year_nhood), FUN = mean)
names(rent_nhood_monthly)[names(rent_nhood_monthly) == 'Group.1'] <- 'nhood_month_year'
names(rent_nhood_monthly)[names(rent_nhood_monthly) == 'x'] <- 'avg_rent/mq'
#Adesso son da splittare di nuovo mese-anno e nhood e si può plottare tutto (analisi esplorativa)!



#Calcolo i df con numero di evictions per nhood per anno e per mese
vect_year = paste(Eviction_Notices_Clean$year)
vect_month = paste(Eviction_Notices_Clean$month)
vect_nhood = paste(Eviction_Notices_Clean$nhood)
vect_aus = paste(vect_month,vect_year,vect_nhood)
vect_aus2 = paste(vect_year,vect_nhood)
Eviction_Notices_Clean$year_nhood = vect_aus2
Eviction_Notices_Clean$month_year_nhood = vect_aus
rm(vect_aus,vect_aus2,vect_year,vect_nhood,vect_month)

length(unique(Eviction_Notices_Clean$year_nhood)) #Dalle 973 osservazioni non sono state tolte quelle dal 1997 al 2007!
Eviction_Notices_Clean$dummy = 1
eviction_nhood_yearly = aggregate(Eviction_Notices_Clean$dummy, by = list(Eviction_Notices_Clean$year_nhood), FUN = sum)
names(eviction_nhood_yearly)[names(eviction_nhood_yearly) == 'Group.1'] <- 'year_nhood'
names(eviction_nhood_yearly)[names(eviction_nhood_yearly) == 'x'] <- 'Count'

length(unique(Eviction_Notices_Clean$month_year_nhood)) #Dalle 8943 osservazioni non sono state tolte quelle dal 97 al 07
eviction_nhood_monthly = aggregate(Eviction_Notices_Clean$dummy, by = list(Eviction_Notices_Clean$month_year_nhood), FUN = sum)
names(eviction_nhood_monthly)[names(eviction_nhood_monthly) == 'Group.1'] <- 'nhood_month_year'
names(eviction_nhood_monthly)[names(eviction_nhood_monthly) == 'x'] <- 'Count'
#Adesso son da splittare di nuovo mese-anno e nhood e si può plottare tutto (analisi esplorativa)!




#Calcolo i df con numero di buyout per nhood per anno e per mese
ind = which(Buyout_Agreements_Clean$year >= 2007)
Buyout_Agreements_Clean = Buyout_Agreements_Clean[ind,]
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
write.csv(buyout_nhood_yearly,'buyout_nhood_yearly.csv')

length(unique(Buyout_Agreements_Clean$month_year_nhood)) #1383 osservazioni 
buyout_nhood_monthly = aggregate(Buyout_Agreements_Clean[,c(2,11)], by = list(Buyout_Agreements_Clean$month_year_nhood), FUN = sum)
names(buyout_nhood_monthly)[names(buyout_nhood_monthly) == 'Group.1'] <- 'nhood_month_year'
names(buyout_nhood_monthly)[names(buyout_nhood_monthly) == 'dummy'] <- 'Count'
buyout_nhood_monthly$avg_buyout = buyout_nhood_monthly$buyout_amount / buyout_nhood_monthly$Count
write.csv(buyout_nhood_monthly,'buyout_nhood_monthly.csv')
#Adesso son da splittare di nuovo mese-anno e nhood e si può plottare tutto (analisi esplorativa)!
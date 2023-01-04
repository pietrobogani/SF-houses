Parcels <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Parcels.csv", header=TRUE)


Parcels <- Parcels[which(Parcels$RESUNITS > 0), ] #elimino quelle dove non abita nessuno
Parcels <- Parcels[which(Parcels$LANDUSE == 'RESIDENT'), ] #elemino quelle che non sono a fini abitativi
Parcels <- Parcels[,-c(14,16:21) ]


#                          CALCULATION OF CENTROID OF THE PARCELS


for( i in 1:length(Parcels[,1])) {       #estraggo coordinate, codice lento da eseguire, non fatelo
  temp <- strsplit(Parcels[i,3], ", ")
  for (j in 1:length(temp[[1]])){
    Parcels[i,16+j] <- temp[[1]][j]
  }
}


#HP CONSIDERO UN MASSIMO DI 50 PUNTI => TENGO SOLO 67 COLONNE DI "PARCELS"

Parcels <- Parcels[,-c(50:2941)]
Complete_Parcels_not_to_modify <- Parcels



for( i in 1:length(Parcels[,1])) {        # sistemo il primo punto che ha anche "MULTIPOLYGON..." da togliere
  temp <- strsplit(Parcels[i,17], "\\(" )
  Parcels[i,17] <- temp[[1]][4]
}


j = 18
for( i in 1:length(Parcels[,1])) {   #sistemo l'ultimo punto che contiene ")))" da togliere length(Parcels[,1])
  while( j < length(Parcels[1,])) { 
    if(!(is.na(Parcels[i,j])))  {
      j = j + 1
    }
    
    else {
      temp = strsplit(Parcels[i,j-1], "\\)" )
      Parcels[i,j-1] = temp[[1]][1]
      j = length(Parcels[1,]) + 1
    } 
    
    
  }
  j = 18
}


Parcels <- Parcels_prova


Parcels_prova[90948,32] = "-122.41039847930617 37.726766468046975" #Fix di errori già presenti nella creazione del dataset
Parcels_prova[90948,33] = "-122.41056570544711 37.72627503015284"
Parcels_prova[105516,23] = "-122.43679266986896 37.73394341984941"
Parcels_prova[105516,24] = "-122.43757072924244 37.7340980056725"



sum_long = 0
sum_lat = 0
counter = 0
j = 17

for( i in 1:length(Parcels[,1])) {              #poichè le coordinate sono molto vicine tra loro le considero cartesiane e faccio una media campionaria
  while (j < length(Parcels[1,])-1){
    if(!(is.na(Parcels_prova[i,j]))) {
      temp <- strsplit(Parcels_prova[i,j], " " )
      sum_long = sum_long + as.numeric(temp[[1]][1])
      sum_lat = sum_lat + as.numeric(temp[[1]][2])
      counter = counter + 1
      j = j+1
    }
    else {
      j = 2942   #se trovo anche solo un NA, ho finito di guardare quella riga, quindi esco dal ciclo sulle colonne
    }
  }
  
  Parcels_prova$Centroid_Lat[i] = sum_lat/counter
  Parcels_prova$Centroid_Long[i] = sum_long/counter
  sum_lat = 0
  sum_long = 0
  counter = 0
  j = 17
}



CENTROID_LAT =  Parcels_prova$Centroid_Lat
CENTROID_LONG =  Parcels_prova$Centroid_Long

flag = 0
j=1
index=0
for (i in 1:length(CENTROID_LONG)){
  if((is.na(CENTROID_LAT[i]))) {
    index[j]=i
    j=j+1
    flag = flag+1           #controllo se ci sono degli NA e dove. Se flag=0 allora è tutto giusto
  }
}

options(digits=9)

Parcels_final = Parcels[,-c(17:49)]   #tolgo colonne che non servono
save(Parcels_final, file = 'Parcels_finall.RData')
write.csv(Parcels_final,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Parcels_final.csv")
Parcels_final <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Parcels_final.csv", header=TRUE)
Parcels_final <- Parcels_final[,-c(1:6,11:16)]
write.csv(Parcels,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/Parcels_poly.csv",row.names=FALSE)


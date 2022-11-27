library(tibble)
library(dplyr)
Parcels_final <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/Parcels_final.csv", header=TRUE)
New_construction_clean_geocoded <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/New_construction_clean_geocoded.csv", header=TRUE)
r_earth <- 6378000




#I will compute the number of new_constructions within 100m from each parcel

#I create a square box with l = 50m and s.t. the center is exactly the parcel: this is the "quadrato circoscritto"

dy = 100
dx = 100

outside_square_box_100 <- Parcels_final[,c(6,7)]
outside_square_box_100 <- add_column(outside_square_box_100, upper_limit = NA,lower_limit = NA,right_limit = NA,left_limit = NA)
for(i in 1:length(outside_square_box_100[,1])){ #computazionalmente lungo
  outside_square_box_100[i,3] <- outside_square_box_100[i,1]  + (dy / r_earth) * (180 / pi) #upper limit of the box
  outside_square_box_100[i,4] <- outside_square_box_100[i,1]  - (dy / r_earth) * (180 / pi) #lower limit of the box
  outside_square_box_100[i,5] <- outside_square_box_100[i,2]  + (dx / r_earth) * (180 / pi) / cos(outside_square_box_100[i,1] * pi/180) #right limit of the box
  outside_square_box_100[i,6] <- outside_square_box_100[i,2]  - (dx / r_earth) * (180 / pi) / cos(outside_square_box_100[i,1] * pi/180) #left limit of the box
}

#I create a square box with l = 100/sqrt(2)m and s.t. the center is exactly the parcel: this is the "quadrato inscritto"
dy = 100/sqrt(2)
dx = 100/sqrt(2)

inside_square_box_100 <- Parcels_final[,c(6,7)]
inside_square_box_100 <- add_column(inside_square_box_100, upper_limit = NA,lower_limit = NA,right_limit = NA,left_limit = NA)
for(i in 1:length(inside_square_box_100[,1])){ #computazionalmente lungo
  inside_square_box_100[i,3] <- inside_square_box_100[i,1]  + (dy / r_earth) * (180 / pi) #upper limit of the box
  inside_square_box_100[i,4] <- inside_square_box_100[i,1]  - (dy / r_earth) * (180 / pi) #lower limit of the box
  inside_square_box_100[i,5] <- inside_square_box_100[i,2]  + (dx / r_earth) * (180 / pi) / cos(inside_square_box_100[i,1] * pi/180) #right limit of the box
  inside_square_box_100[i,6] <- inside_square_box_100[i,2]  - (dx / r_earth) * (180 / pi) / cos(inside_square_box_100[i,1] * pi/180) #left limit of the box
}











#I will first compute the number of new_contructions within 500m and at least 100m from each parcel
dy = 500
dx = 500
#To do so, I create a square box with l = 250m and s.t. the center is exactly the parcel

outside_square_box_500 <- Parcels_final[,c(6,7)]
outside_square_box_500 <- add_column(outside_square_box_500, upper_limit = NA,lower_limit = NA,right_limit = NA,left_limit = NA)
for(i in 1:length(outside_square_box_100[,1])){ #computazionalmente lungo
  outside_square_box_500[i,3] <- outside_square_box_500[i,1]  + (dy / r_earth) * (180 / pi) #upper limit of the box
  outside_square_box_500[i,4] <- outside_square_box_500[i,1]  - (dy / r_earth) * (180 / pi) #lower limit of the box
  outside_square_box_500[i,5] <- outside_square_box_500[i,2]  + (dx / r_earth) * (180 / pi) / cos(outside_square_box_500[i,1] * pi/180) #right limit of the box
  outside_square_box_500[i,6] <- outside_square_box_500[i,2]  - (dx / r_earth) * (180 / pi) / cos(outside_square_box_500[i,1] * pi/180) #left limit of the box
}



#I create a square box with l = 500/sqrt(2)m and s.t. the center is exactly the parcel: this is the "quadrato inscritto"
dy = 500/sqrt(2)
dx = 500/sqrt(2)

inside_square_box_500 <- Parcels_final[,c(6,7)]
inside_square_box_500 <- add_column(inside_square_box_500, upper_limit = NA,lower_limit = NA,right_limit = NA,left_limit = NA)
for(i in 1:length(inside_square_box_500[,1])){ #computazionalmente lungo
  inside_square_box_500[i,3] <- inside_square_box_500[i,1]  + (dy / r_earth) * (180 / pi) #upper limit of the box
  inside_square_box_500[i,4] <- inside_square_box_500[i,1]  - (dy / r_earth) * (180 / pi) #lower limit of the box
  inside_square_box_500[i,5] <- inside_square_box_500[i,2]  + (dx / r_earth) * (180 / pi) / cos(inside_square_box_500[i,1] * pi/180) #right limit of the box
  inside_square_box_500[i,6] <- inside_square_box_500[i,2]  - (dx / r_earth) * (180 / pi) / cos(inside_square_box_500[i,1] * pi/180) #left limit of the box
}





#I will first compute the number of new_contructions  within 1000m and at least 500m from each parcel
dy = 1000
dx = 1000
#To do so, I create a square box with l = 250m and s.t. the center is exactly the parcel

outside_square_box_1000 <- Parcels_final[,c(6,7)]
outside_square_box_1000 <- add_column(outside_square_box_500, upper_limit = NA,lower_limit = NA,right_limit = NA,left_limit = NA)
for(i in 1:length(outside_square_box_100[,1])){ #computazionalmente lungo
  outside_square_box_1000[i,3] <- outside_square_box_1000[i,1]  + (dy / r_earth) * (180 / pi) #upper limit of the box
  outside_square_box_1000[i,4] <- outside_square_box_1000[i,1]  - (dy / r_earth) * (180 / pi) #lower limit of the box
  outside_square_box_1000[i,5] <- outside_square_box_1000[i,2]  + (dx / r_earth) * (180 / pi) / cos(outside_square_box_1000[i,1] * pi/180) #right limit of the box
  outside_square_box_1000[i,6] <- outside_square_box_1000[i,2]  - (dx / r_earth) * (180 / pi) / cos(outside_square_box_1000[i,1] * pi/180) #left limit of the box
}

#I create a square box with l = 1000/sqrt(2)m and s.t. the center is exactly the parcel: this is the "quadrato inscritto"
dy = 1000/sqrt(2)
dx = 1000/sqrt(2)

inside_square_box_1000 <- Parcels_final[,c(6,7)]
inside_square_box_1000 <- add_column(inside_square_box_1000, upper_limit = NA,lower_limit = NA,right_limit = NA,left_limit = NA)
for(i in 1:length(inside_square_box_1000[,1])){ #computazionalmente lungo
  inside_square_box_1000[i,3] <- inside_square_box_1000[i,1]  + (dy / r_earth) * (180 / pi) #upper limit of the box
  inside_square_box_1000[i,4] <- inside_square_box_1000[i,1]  - (dy / r_earth) * (180 / pi) #lower limit of the box
  inside_square_box_1000[i,5] <- inside_square_box_1000[i,2]  + (dx / r_earth) * (180 / pi) / cos(inside_square_box_1000[i,1] * pi/180) #right limit of the box
  inside_square_box_1000[i,6] <- inside_square_box_1000[i,2]  - (dx / r_earth) * (180 / pi) / cos(inside_square_box_1000[i,1] * pi/180) #left limit of the box
}










#I will first compute the number of new_contructions  within 2000m and at least 1000m from each parcel
dy = 2000
dx = 2000
#To do so, I create a square box with l = 250m and s.t. the center is exactly the parcel

outside_square_box_2000 <- Parcels_final[,c(6,7)]
outside_square_box_2000 <- add_column(outside_square_box_500, upper_limit = NA,lower_limit = NA,right_limit = NA,left_limit = NA)
for(i in 1:length(outside_square_box_100[,1])){ #computazionalmente lungo
  outside_square_box_2000[i,3] <- outside_square_box_2000[i,1]  + (dy / r_earth) * (180 / pi) #upper limit of the box
  outside_square_box_2000[i,4] <- outside_square_box_2000[i,1]  - (dy / r_earth) * (180 / pi) #lower limit of the box
  outside_square_box_2000[i,5] <- outside_square_box_2000[i,2]  + (dx / r_earth) * (180 / pi) / cos(outside_square_box_2000[i,1] * pi/180) #right limit of the box
  outside_square_box_2000[i,6] <- outside_square_box_2000[i,2]  - (dx / r_earth) * (180 / pi) / cos(outside_square_box_2000[i,1] * pi/180) #left limit of the box
}



#I create a square box with l = 2000/sqrt(2)m and s.t. the center is exactly the parcel: this is the "quadrato inscritto"
dy = 2000/sqrt(2)
dx = 2000/sqrt(2)

inside_square_box_2000 <- Parcels_final[,c(6,7)]
inside_square_box_2000 <- add_column(inside_square_box_2000, upper_limit = NA,lower_limit = NA,right_limit = NA,left_limit = NA)
for(i in 1:length(inside_square_box_2000[,1])){ #computazionalmente lungo
  inside_square_box_2000[i,3] <- inside_square_box_2000[i,1]  + (dy / r_earth) * (180 / pi) #upper limit of the box
  inside_square_box_2000[i,4] <- inside_square_box_2000[i,1]  - (dy / r_earth) * (180 / pi) #lower limit of the box
  inside_square_box_2000[i,5] <- inside_square_box_2000[i,2]  + (dx / r_earth) * (180 / pi) / cos(inside_square_box_2000[i,1] * pi/180) #right limit of the box
  inside_square_box_2000[i,6] <- inside_square_box_2000[i,2]  - (dx / r_earth) * (180 / pi) / cos(inside_square_box_2000[i,1] * pi/180) #left limit of the box
}


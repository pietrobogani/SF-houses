rent_nhood_monthly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_nhood_monthly.csv", header=TRUE)
rent_nhood_yearly <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_nhood_yearly.csv", header=TRUE)



ind = which(rent_nhood_monthly$avg_rent.mq < 7)
plot(rent_nhood_monthly$d[ind],rent_nhood_monthly$avg_rent.mq[ind])

min(rent_nhood_monthly$year)

#Analisi esplorativa su rent_nhood_monthly
{
#Aggiungo la data al rent monthly
library(readr)
rent_nhood_monthly <- read_csv("rent_nhood_monthly.csv")
vect_month = rent_nhood_monthly$month
vect_yr = rent_nhood_monthly$year
vect_day = rep(1,length(vect_yr))
vect_aus = paste(vect_yr,vect_month,vect_day,sep = '-')
rent_nhood_monthly$d = vect_aus
rm(vect_yr,vect_day, vect_aus,vect_month)
rent_nhood_monthly$d = as.Date(rent_nhood_monthly$d, tryFormats = '%Y-%m-%d')
list_nhood = unique(rent_nhood_monthly$nhood)
for(nh in list_nhood){
  ind = which(rent_nhood_monthly$nhood == nh)
  print(nh)
  print(length(ind))
}

#write.csv(rent_nhood_monthly,"C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_nhood_monthly.csv")


#Costruisco il dataset func_data
library(lubridate)
first_d = min(rent_nhood_monthly$d)
final_d = max(rent_nhood_monthly$d)
7*12+7 #91 mesi in totale
vect_dates = rep(first_d,91)
for(i in 2:91){
  vect_dates[i] = vect_dates[i-1]%m+% months(1)
}
vect_dates
funct_data = as.data.frame( matrix(nrow = 91, ncol = length(unique(rent_nhood_monthly$nhood))) )
list_nhood = unique(rent_nhood_monthly$nhood)
colnames(funct_data) = list_nhood
funct_data = cbind(vect_dates,funct_data)
row.names(funct_data) = vect_dates
#Riempio il dataset funct_data
for(nh in list_nhood){
  ind_nh = which(rent_nhood_monthly$nhood == nh)
  list_dates_nh = rent_nhood_monthly[ind_nh,]$d
  for(ind_dt in 1:length(list_dates_nh)){
    dt = list_dates_nh[ind_dt]
    selected_ind = which(rent_nhood_monthly$d == dt & rent_nhood_monthly$nhood == nh)
    selected_funct_ind = which(funct_data$vect_dates == dt)
    val = rent_nhood_monthly[selected_ind,]$avg_rent.mq
    funct_data[selected_funct_ind,nh] = val
  }
}
funct_data = funct_data[,2:dim(funct_data)[2]]
#Provo a togliere le date con più buchi (ma non è fattibile...comunque troppi buchi)
funct_data_t = t(funct_data)
funct_data_t = funct_data_t[,-c(1:10)]
funct_data_t = funct_data_t[,-c(11:14,16:21)] 
funct_data_t = funct_data_t[,-c(12:15)]
funct_data_t = funct_data_t[,-c(10,15:28)] 
funct_data_t = funct_data_t[,-c(16,18)]
funct_data_t = funct_data_t[,-c(45,47,49:50)] 
funct_data_t = funct_data_t[,-c(30:38)] 
funct_data_t = funct_data_t[,-c(31:36)] 
dim(funct_data_t)
#Plot dei dati funzionali
matplot(vect_dates,funct_data[,1:dim(funct_data)[2]], type = 'l')
library(roahd)
f_data <- fData(vect_dates,t(funct_data))
plot(f_data)
#Troppi buchi ...
}
#Conclusione: troppi buchi ... bisogna aggregare su unità temporale più ampia del mese


#Analisi esplorativa di rent_nhood_year 
{
rent_nhood_yearly <- read_csv("rent_nhood_yearly.csv")
rent_nhood_yearly$avg_rent.mq = rent_nhood_yearly$avg_rent.mq/0.092903
first_yr = min(rent_nhood_yearly$year)
final_yr = max(rent_nhood_yearly$year)
funct_data = as.data.frame(matrix(nrow = 8,ncol = length(unique(rent_nhood_yearly$nhood))))
vect_year = 2011:2018
list_nh = unique(rent_nhood_yearly$nhood)
colnames(funct_data) = list_nh
funct_data = cbind(vect_year,funct_data)
for(nh in list_nh){
  ind_nh = which(rent_nhood_yearly$nhood == nh)
  list_yrs_nh = rent_nhood_yearly[ind_nh,]$year
  for(yr in list_yrs_nh){
    ind_selected = which(rent_nhood_yearly$year == yr & rent_nhood_yearly$nhood == nh)
    val = rent_nhood_yearly[ind_selected,]$avg_rent.mq
    ind_yr = which(funct_data$vect_year == yr)
    funct_data[ind_yr,nh] = val
  }
}
rm(list_nh,ind_nh,list_yrs_nh,ind_selected,ind_yr,val,yr,nh)
funct_data = funct_data[,2:dim(funct_data)[2]]
#Tolgo le funzioni dei nh con degli na
funct_data_t = t(funct_data)
funct_data_t = funct_data_t[complete.cases(funct_data_t), ] 
funct_data = t(funct_data_t)
rm(funct_data_t)
#Plotto per anno i avg rent
matplot(vect_year, funct_data, type = 'l')
#f_data <- fData(vect_year,t(funct_data))
#plot(f_data)

#Parte di smoothing presa da applied
abscissa = vect_year
Xobs0 = as.matrix(funct_data)
Xobs0
m = 4
basis <- create.bspline.basis(range(abscissa), nbasis = 4, norder = m) 
Xss <- smooth.basis(abscissa, Xobs0, basis)
plot(Xss)# Analogue to: plot.fd(Xss$fd)
grid_time = seq(2011,2018,0.1)
Xss0 <- eval.fd(grid_time, Xss$fd)
matplot(grid_time,Xss0,type = 'l')
rm(abscissa,Xobs0,m,basis,Xss)

#Da qui in poi ho funct_data_smooth e grid_time per fare analisi esplorativa!!!
funct_data_smooth = fData(grid_time,t(Xss0))
rm(Xss0)
plot(funct_data_smooth)
band_depth <- BD(Data = funct_data_smooth)
modified_band_depth <- MBD(Data = funct_data_smooth)
median_curve <- median_fData(fData = funct_data_smooth, type = "MBD")
plot(funct_data_smooth)
plot(median_curve, col = 'black',lwd = 3 , add = T)
}
#Conclusione: un po troppo approssimata... descrivo una funzione con 8 punti

for(nh in unique(rent_nhood_sem$nhood)){
  print(nh)
  ind = which(rent_nhood_sem$nhood == nh)
  print(dim(rent_nhood_sem[ind,])[1])
}


#Analisi esplorativa su rent_nhood_sem
{
library(readr)
rent_nhood_sem <- read_csv("rent_nhood_sem.csv")
#Costruisco il dataset func_data
library(lubridate)
first_date = min(rent_nhood_sem$date)
final_date = max(rent_nhood_sem$date)
#2011 -> 2018 = 8 anni = 16 semestri
#OSS: si parte da gennaio/2011 e si arriva a luglio/2018, quindi 2 semestri anche per
#     gli anni ai bordi
num_sem = 16
vect_dates = rep(first_date,num_sem)
for(i in 2:num_sem){
  vect_dates[i] = vect_dates[i-1]%m+% months(6)
}
vect_dates
funct_data = as.data.frame( matrix(nrow = num_sem, ncol = length(unique(rent_nhood_sem$nhood))) )
list_nhood = unique(rent_nhood_sem$nhood)
colnames(funct_data) = list_nhood
funct_data = cbind(vect_dates,funct_data)
#Riempio il dataset funct_data
for(nh in list_nhood){
  ind_nh = which(rent_nhood_sem$nhood == nh)
  list_dates_nh = rent_nhood_sem[ind_nh,]$date
  for(ind_dt in 1:length(list_dates_nh)){
    dt = list_dates_nh[ind_dt]
    selected_ind = which(rent_nhood_sem$date == dt & rent_nhood_sem$nhood == nh)
    selected_funct_ind = which(funct_data$vect_dates == dt)
    val = rent_nhood_sem[selected_ind,]$avg_rent_mq
    funct_data[selected_funct_ind,nh] = val
  }
}
row.names(funct_data) = vect_dates
funct_data = funct_data[,2:dim(funct_data)[2]]
#Plot dei dati funzionali
matplot(vect_dates,funct_data, type = 'l')
library(roahd)
f_data <- fData(vect_dates,t(funct_data))
plot(f_data)
#Provo a rimuovere date con poche osservazioni
funct_data_t = t(funct_data)
funct_data_t = funct_data_t[,-c(1,7,13,16)]
funct_data_t = funct_data_t[complete.cases(funct_data_t), ] 
funct_data = t(funct_data_t)
rm(funct_data_t)

#Parte di smoothing presa da applied
abscissa = vect_dates[-c(1,7,13,16)]
Xobs0 = as.matrix(funct_data)
Xobs0
m = 4 #degree = m - 1
library(fda)
basis <- create.bspline.basis(range(abscissa), nbasis = 4, norder = m) # io userei 4 o 5 basi
Xss <- smooth.basis(abscissa, Xobs0, basis)
plot(Xss)# Analogue to: plot.fd(Xss$fd)
grid_time = seq(min(abscissa),max(abscissa),1)
Xss0 <- eval.fd(grid_time, Xss$fd)
matplot(grid_time,Xss0,type = 'l')
rm(abscissa,Xobs0,m,basis,Xss)

#Da qui in poi ho funct_data_smooth e grid_time per fare analisi esplorativa!!!
funct_data_smooth = fData(grid_time,t(Xss0))
rm(Xss0)
plot(funct_data_smooth)
band_depth <- BD(Data = funct_data_smooth)
modified_band_depth <- MBD(Data = funct_data_smooth)
median_curve <- median_fData(fData = funct_data_smooth, type = "MBD")
plot(funct_data_smooth)
plot(median_curve, col = 'black',lwd = 3 , add = T)

}
#Conclusione: un po meglio dell'year ma comunque molto approssimata (12 punti) e 
#             comunque alcuni nhood sono da rimuovere... 
#             Sarebbe da trovare la giusta unità che sia sufficentemente ampia da non
#              creare troppi buchi ma abbastanza piccola da creare tanti punti per stimare
#              le funzioni... Trimestre?Quadrimestre?
# FALLIMENTI INIZIALI =( #######################################################

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
#Provo a togliere le date con pi? buchi (ma non ? fattibile...comunque troppi buchi)
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
matplot(vect_dates,funct_data[,2:dim(funct_data)[2]], type = 'l')
library(roahd)
f_data <- fData(vect_dates,t(funct_data))
plot(f_data)
#Troppi buchi ...
}
#Conclusione: troppi buchi ... bisogna aggregare su unit? temporale pi? ampia del mese


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
#             Sarebbe da trovare la giusta unit? che sia sufficentemente ampia da non
#              creare troppi buchi ma abbastanza piccola da creare tanti punti per stimare
#              le funzioni... Trimestre?Quadrimestre?



# RENT #########################################################################
  

## Smoothing ###################################################################

#Provo a fare smoothing con kernel regr sui rent 
{
library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)
library(magrittr)
library(KernSmooth)
library(readr)
library(stringr)
  
  
rent_clean <- read_csv("rent_clean_nh.csv")
aus_df <- data.frame(year = as.numeric(format(rent_clean$d, format = "%Y")),
                     month = as.numeric(format(rent_clean$d, format = "%m")),
                     day = as.numeric(format(rent_clean$d, format = "%d")))
rent_clean = cbind(rent_clean,aus_df)
rm(aus_df)
rent_clean = rent_clean[which(rent_clean$year >= 2011 & rent_clean$year <= 2018),]
ind_typo = which(rent_clean$price_mq > 175) #la media in quel nhood e anno ? 10 volte meno...typo?
rent_clean = rent_clean[-c(ind_typo),]
#vect_date_num = as.numeric(paste(rent_clean$year,rent_clean$month,rent_clean$day, sep = ''))
#rent_clean = cbind(rent_clean,vect_date_num)
rent_clean$date_num = as.numeric(rent_clean$d)
x11()
plot(rent_clean$d,rent_clean$price_mq,xlab = 'Year',ylab = 'Price/mq',ylim = c(0,150), main = 'Raw rents',cex = 0.5)
list_nhood = unique(rent_clean$nhood)
first_date = min(rent_clean$d)
final_date = max(rent_clean$d)
grid_time = seq(first_date,final_date,by = 1)
grid_time_num = data.frame(date_num = as.numeric(grid_time))
funct_data = data.frame(row.names = grid_time)

for(nh in list_nhood){
  ind_nh = which(rent_clean$nhood == nh)
  data = rent_clean[ind_nh,]
  #bw = npregbw(formula = price_mq ~ date_num, bws = 365*6/12, data = data)
  m_loc = npreg( price_mq ~ date_num,
                 ckertype = 'gaussian', 
                 bws = 365*6/12, # bandwidth di 6 mesi oppure bw$bw
                 data = data)
  preds=predict(m_loc,newdata=grid_time_num,se=T)
  se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
  funct_data = cbind(funct_data,preds$fit)
}
rm(nh, data,m_loc,preds,se.bands,ind_nh)
colnames(funct_data) = list_nhood
dim(funct_data)
length(grid_time)
library(roahd)
funz_rent = funct_data
funct_data = fData(grid_time,t(funct_data))
x11()
plot(funct_data, xlab = 'Year', ylab = 'Price/mq', main = 'Smoothed rent functions')
#Calcolo derivate prime 
diff_rent = funz_rent[2:dim(funz_rent)[1],] - funz_rent[1:dim(funz_rent)[1]-1,]
dim(funz_rent)
dim(diff_rent)
funct_data_diff = fData(grid_time[2:length(grid_time)], t(diff_rent))
x11()
plot(funct_data_diff, xlab = 'Year', ylab = 'd/dt(Price/mq)', main = 'Approximation of first derivative of rent functions')
}
#Conclusione: usando la bandwidth ottenuta con CV con npregbw si ottengono funzioni molto
#             disomogenee nelle variazioni... Potrebbe influire (negativamente) sullo di 
#             functional depth measures/outliers! Non sarebbe meglio tenere una finestra che
#             fornisce variazioni simili per tutti (cos? si evidenzia trend)?

# funz_rent       : table con colonne nhood e righe tempo
# diff_rent       : table con colonne nhood e righe tempo (valori sono le differenze all'indietro!)
# funct_data      : rent funzionali
# funct_data_diff : derivate funzionali dei rent


## Test  (DOPO DEPTH&OUTLIERS) ################################################# 

#Test su partizioni delle funzioni dei prezzi nei nhood 
{

library(fdatest)
#NB: data si assume con tempo sulle righe e nomi sulle colonne
data = diff_rent #cambiare con funz_rent per fare i test sulle funzioni dei rent e non derivate!
low_evictions_nhood2 <- read_csv("2 Clusters of nhood based on Evictions/low_evictions_nhood_nh2.csv")
high_evictions_nhood2 <- read_csv("2 Clusters of nhood based on Evictions/high_evictions_nhood_nh2.csv")
low_construction_nhood2_nh <- read_csv("2 Clusters of nhood based on Constructions/low_construction_nhood2_nh.csv")
high_construction_nhood2_nh <- read_csv("2 Clusters of nhood based on Constructions/high_construction_nhood2_nh.csv")
list_low_constr = as.vector(low_construction_nhood2_nh$x)
list_low_constr = list_low_constr[-c(5,7)] #tolgo i nhood non osservati in rent
list_high_constr = as.vector(high_construction_nhood2_nh$x)
list_high_constr = list_high_constr[-c(3,10)]
list_low_evict = as.vector(low_evictions_nhood2$x)
list_high_evict = as.vector(high_evictions_nhood2$x)
list_low_evict = list_low_evict[-c(4,6,7,8,12,16)]#non ci sono osservazioni negli annunci di questi quindi rimuovo
list_high_evict = list_high_evict[-c(3,7)] #non ci sono osservazioni negli annunci quindi tolgo

data1 = data[,list_low_constr]
data2 = data[,list_high_constr]
#data1 = data[,1:23] 
#data2 = data[,24:46]
data_bind = rbind(t(data1),t(data2)) #data_bind ha nomi sulle righe!
n = nrow(data_bind)
n1 = nrow(t(data1)) #numero nomi in gruppo1
n2 = nrow(t(data2)) #numero nomi in gruppo2
data1 = fData(grid_time[2:length(grid_time)],t(data1)) # NB: mettere grid_time se si testano funzioni dei rent.Mettere grid_time[2:length(grid_time)] per derivate
data2 = fData(grid_time[2:length(grid_time)],t(data2)) # NB: mettere grid_time se si testano funzioni dei rent,
seed=2781991
set.seed(seed)
B=1000
mean_diff = median_fData(data1, type = 'MBD') - median_fData(data2, type = 'MBD')
plot(mean_diff)
T0 = sum(abs(mean_diff$values))
T0
data_fd = append_fData(data1,data2)
T0_perm = numeric(B)
x11()
plot(data1, col = 'red', xlab = 'Year', ylab = 'd/dt(Price/mq)', main = 'Derivative of Rent functions', ylim = c(-0.1,0.13)  ) #ylim = c(15,70) per funz e c(-0.1,0.13) per deriv
plot(data2, col = 'black', add = T)
legend('topleft', legend=c("Low construction density", "High construction density"),
       col=c("red", "black"),lty = 1, cex=0.8)

library(progress)
pb <- progress_bar$new(format = "  processing [:bar] :percent eta: :eta",total = B, clear = FALSE)
for(perm in 1:B){
  permutazione <- sample(n)
  data_perm=data_fd[permutazione,]
  perm1 = data_perm[1:n1,] 
  perm2 = data_perm[(n2+1):n,] 
  mean_diff=median_fData(perm1,type='MBD')-median_fData(perm2,type='MBD')
  T0_perm[perm]=sum(abs(mean_diff$values))
  pb$tick()
}
sum(T0_perm >= T0)/B
hist(T0_perm)
abline(v=T0,col='green')


#Provo a fare il test "locale"
#data1 = data[,1:23] 
#data2 = data[,24:46]

seed=2781991
set.seed(seed)
data1 = data[,list_low_evict]
data2 = data[,list_high_evict]
tst = IWT2(t(data1),t(data2))
plot(tst)

}

## Depth measures & outliers ###################################################

#Depth measures & outliers per funzioni rent
{
  #Analisi di funct_data e funct_data_diff
  
  
  #for(i in 1:dim(funz_rent)[2]){
  #  x11()
  #  plot(grid_time,funz_rent[,i], main = colnames(funz_rent)[i])
  #}
  #which(colnames(funz_rent) == 'Treasure Island') #32 Treasure Island ? lo scalino!
  
  #Provo a fare un plot "simil matplot" dei raw rent:
  list_nhood = unique(rent_clean$nhood)
  x11()
  plot(0,0,cex = 0 ,xlim = range(rent_clean$d),ylim = range(rent_clean$price_mq) )
  for(i in list_nhood){
    ind_nh = which(rent_clean$nhood == i)
    aus = data.frame(d = rent_clean[ind_nh,]$d, price_mq = rent_clean[ind_nh,]$price_mq)
    aus = aus[order(aus$d),]
    lines(aus$d,aus$price_mq,xlim = range(rent_clean$d),ylim = range(rent_clean$price_mq), add = T, type = 'l')
  }
  
  #Plot delle funzioni dei rent e delle derivate
  x11()
  par(mfrow=c(1,2))
  plot(funct_data, main = 'Smoothed Rent functions')
  plot(funct_data_diff, main = 'Approximated first derivative')
  
  
  #MBD,mediana e boxplot per rent funzionali
  modified_band_depth_rent = MBD(funct_data)
  print(paste('The nhood with max depth (ie median) is:', colnames(funz_rent)[which.max(modified_band_depth_rent)] ))
  median_curve_rent = median_fData(fData = funct_data, type = 'MBD' )
  x11()
  plot(funct_data, xlab = 'Year', ylab = 'Price/mq', main = 'Smoothed rent functions with median (wrt MBD)')
  plot(median_curve_rent, col = 'red', lwd = 3, add = T)
  #roahd::fbplot(funct_data, main = 'Functional box.plot for rent functions') #da errori...perch??
  funct_data_num = fData(grid_time_num$date_num,t(funz_rent))
  x11()
  fbpl = roahd::fbplot(funct_data_num, main = 'Functional box-plot for rent functions' )
  fbpl$ID_outliers
  x11()
  out_funct <- outliergram(funct_data) #Mi viene spottata Treasure Island-32 !!
  out_funct$ID_outliers
  
  #MBD,mediana e boxplot per derivate di rent
  modified_band_depth_rent_diff = MBD(funct_data_diff)
  print(paste('The nhood with max depth is:', colnames(funz_rent)[which.max(modified_band_depth_rent_diff)] ))
  median_curve_rent_diff = median_fData(fData = funct_data_diff, type = 'MBD' )
  x11()
  plot(funct_data_diff,xlab = 'Year', ylab = 'Price/mq', main = 'First derivative of rent functions with median (wrt MBD)')
  plot(median_curve_rent_diff, col = 'red', lwd = 3, add = T)
  #fbplot(funct_data_diff) da errori... perch??
  funct_data_diff_num = fData(grid_time_num$date_num[2:dim(grid_time_num)[1]],t(diff_rent))
  x11()
  fbplt_der = roahd::fbplot(funct_data_diff_num, main = 'Functional box-plot for derivatives of rent functions')
  fbplt_der$ID_outliers
  x11()
  out_funct_diff <- outliergram(funct_data_diff) 
  out_funct_diff$ID_outliers
  
  
  #Rimuovo treasure island da funz_rent e diff rent e aggiorno funct_data e funct_data_diff
  funz_rent = funz_rent[,-28]
  diff_rent = diff_rent[,-28]
  funct_data = fData(grid_time,t(funz_rent))
  funct_data_diff = fData(grid_time[2:length(grid_time)], t(diff_rent))

  #Plot delle funzioni cleaned 
  #Plot delle funzioni dei rent e delle derivate
  x11()
  par(mfrow=c(1,2))
  plot(funct_data, main = 'Smoothed Rent functions', xlab = 'Year', ylab = 'Price/mq')
  plot(funct_data_diff, main = 'Approximated first derivative', xlab = 'Year', ylab = 'd/dt(Price/mq)')
  
}




# EVICTIONS ####################################################################

## Smoothing ###################################################################

#Provo a fare smoothing con kernel reg sulle evictions mensili
{
#Preparo i dati per il modello
library(readr)
library(sf)
geo = read_sf('SFNeighborhoods_new.geojson')
eviction_nhood_monthly <- read_csv("eviction_monthly_nh.csv")
eviction_nhood_monthly = eviction_nhood_monthly[-2251,] #tolgo la riga con il count totale
vect_year = eviction_nhood_monthly$year
vect_month = eviction_nhood_monthly$month
vect_day = rep(1,length(vect_year))
date = paste(vect_year,vect_month,vect_day, sep = '-')
eviction_nhood_monthly$date = date
rm(vect_day,vect_month,vect_year,date)
eviction_nhood_monthly$date = as.Date(eviction_nhood_monthly$date, tryFormats = '%Y-%m-%d')
#typeof(eviction_nhood_monthly$date)
eviction_nhood_monthly$date_num = as.numeric(eviction_nhood_monthly$date)
list_nhood = unique(eviction_nhood_monthly$nhood)
for(nh in list_nhood){
  print(nh)
  print(dim(eviction_nhood_monthly[which(eviction_nhood_monthly$nhood == nh),]))
}
# Rimuovo McLaren Park , Treasure Island e Lincoln Park dato che ho 1 osservazione ...
# Rimuovo anche Golden Gate Park perchè ha una parcel!
nh_multiple_osservations = c('McLaren Park', 'Treasure Island', 'Lincoln Park', 'Golden Gate Park')
for(nh in nh_multiple_osservations){
  ind = which(eviction_nhood_monthly$nhood == nh)
  eviction_nhood_monthly = eviction_nhood_monthly[-ind,]
}
rm(nh,nh_multiple_osservations, ind)
#Aggiungo l'area di ciascun nhood
eviction_nhood_monthly$area = rep(0,dim(eviction_nhood_monthly)[1])
list_nhood = unique(geo$nhood)
for(i in list_nhood){
  ind_eviction_nhood = which(eviction_nhood_monthly$nhood == i) 
  if(length(ind_eviction_nhood) > 0){
    eviction_nhood_monthly[ind_eviction_nhood,]$area = geo[which(geo$nhood == i),]$area
  }
}
#Aggiungo il numero di parcels di ciascun nhood
parcels <- read_csv("Parcels_augmented.csv")
parcels$count = 1
parcels_for_nhood = aggregate(parcels$count , by = list(parcels$neighborhoods), FUN = sum)
list_nhood = unique(parcels_for_nhood$Group.1)
eviction_nhood_monthly$parcels = rep(0,dim(eviction_nhood_monthly)[1])
for(i in list_nhood){
  ind_eviction_nhood = which(eviction_nhood_monthly$nhood == i) 
  if(length(ind_eviction_nhood) > 0){
    eviction_nhood_monthly[ind_eviction_nhood,]$parcels = parcels_for_nhood[which(parcels_for_nhood$Group.1 == i),]$x
  }
}
#Aggiungo il numero di residents units di ciasun nhood
resunits_for_nhood = aggregate(parcels$RESUNITS , by = list(parcels$neighborhoods), FUN = sum)
list_nhood = unique(resunits_for_nhood$Group.1)
eviction_nhood_monthly$resunits = rep(0,dim(eviction_nhood_monthly)[1])
for(i in list_nhood){
  ind_eviction_nhood = which(eviction_nhood_monthly$nhood == i) 
  if(length(ind_eviction_nhood) > 0){
    eviction_nhood_monthly[ind_eviction_nhood,]$resunits = resunits_for_nhood[which(resunits_for_nhood$Group.1 == i),]$x
  }
}

#NB: da qui in poi (per non dover riscrivere tutto il codice!) si avrà:
#    - count = #evictions/area
#    - count_not_norm = #evictions
#    - count_parcels = #evictions/#parcels
#    - count_resunits = #evictions/#resunits


eviction_nhood_monthly$count_not_norm = eviction_nhood_monthly$count
eviction_nhood_monthly$count = eviction_nhood_monthly$count / eviction_nhood_monthly$area
eviction_nhood_monthly$count_parcels = eviction_nhood_monthly$count / eviction_nhood_monthly$parcels
eviction_nhood_monthly$count_resunits = eviction_nhood_monthly$count / eviction_nhood_monthly$resunits

#Plot delle raw evictions (ie non smoothed)
x11()
plot(eviction_nhood_monthly$date,eviction_nhood_monthly$count,
     xlab = 'Year', ylab = 'Density of evictions', main = 'Density of evictions')

#Creo modello di kern smoothing e preparo i dataset funzionali
first_date = min(eviction_nhood_monthly$date)
final_date = max(eviction_nhood_monthly$date)
grid_time = seq(first_date,final_date,by = 1)
grid_time_num = data.frame(date_num = as.numeric(grid_time))
funz_evictions = data.frame(row.names = grid_time)
list_nhood = unique(eviction_nhood_monthly$nhood)
for(nh in list_nhood){
  ind_nh = which(eviction_nhood_monthly$nhood == nh)
  data = eviction_nhood_monthly[ind_nh,]
  #bw = npregbw(formula = count ~ date_num, bws = 365*6/12, data = data)
  m_loc = npreg( count ~ date_num,
                 ckertype = 'gaussian', 
                 bws = 356*6/12,  #bw$bw, # bandwidth di 6 mesi oppure bw$bw
                 data = data)
  preds=predict(m_loc,newdata=grid_time_num,se=T)
  se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
  funz_evictions = cbind(funz_evictions,preds$fit)
}
rm(data,nh,ind_nh,m_loc,preds,se.bands)
colnames(funz_evictions) = list_nhood
library(roahd)
funct_data = funz_evictions
funct_data = fData(grid_time,t(funct_data))
#Calcolo derivate prime e creo i dataset
diff_evictions = funz_evictions[2:dim(funz_evictions)[1],] - funz_evictions[1:dim(funz_evictions)[1]-1,]
dim(funz_evictions)
dim(diff_evictions)
funct_data_diff = fData(grid_time[2:length(grid_time)], t(diff_evictions))
#Plot di funzioni e derivate delle evictions
x11()
plot(funct_data, xlab = 'Year', ylab = 'Density of evictions', main = 'Smoothed functions of evictions')
x11()
plot(funct_data_diff, xlab = 'Year', ylab = 'd/dt(Density of evictions)', main = 'Approximation of first derivative')
}
# funz_evictions  : table evictions
# diff_evictions  : table derivate differenze all'indietro evictions
# funct_data      : evictions funzioanli (f_data type)
# funct_data_diff : derivate evictions funzionali (f_data type)


## Test ########################################################################

#Test su partizioni delle funzioni delle evictions nei nhood
{
  
library(fdatest)
#NB: data si assume con tempo sulle righe e nomi sulle colonne
data = funz_evictions
dim(data)
data1 = data[,1:18] 
data2 = data[,19:37]
data_bind = rbind(t(data1),t(data2)) #data_bind ha nomi sulle righe!
n = nrow(data_bind)
n1 = nrow(t(data1)) #numero nomi in gruppo1
n2 = nrow(t(data2)) #numero nomi in gruppo2
data1 = fData(grid_time,t(data1)) #partizione di quartieri 1
data2 = fData(grid_time,t(data2)) #partizione di quartieri 2
seed=2781991
B=1000
mean_diff = median_fData(data1, type = 'MBD') - median_fData(data2, type = 'MBD')
plot(mean_diff)
T0 = sum(abs(mean_diff$values))
T0
data_fd = append_fData(data1,data2)
T0_perm = numeric(B)
library(progress)
pb <- progress_bar$new(format = "  processing [:bar] :percent eta: :eta",total = B, clear = FALSE)
for(perm in 1:B){
  permutazione <- sample(n)
  data_perm=data_fd[permutazione,]
  perm1 = data_perm[1:n1,] 
  perm2 = data_perm[(n2+1):n,] 
  mean_diff=median_fData(perm1,type='MBD')-median_fData(perm2,type='MBD')
  T0_perm[perm]=sum(abs(mean_diff$values))
  pb$tick()
}
sum(T0_perm >= T0)/B
hist(T0_perm)
abline(v=T0,col='green')


#Provo a fare il test "locale"
data1 = fData(grid_time,t(data[,1:18])) #da definire le partizioni!
data2 = fData(grid_time,t(data[,19:37])) #da definire le partizioni!
seed=2781991
tst = IWT2(t(data1), t(data2))
plot(tst)
}


## Depth measures & outliers ###################################################

#Depth measures & outliers per funzioni evictions
{
  
  #Analisi di funct_data e funct_data_diff 
  x11()
  par(mfrow=c(1,2))
  plot(funct_data, xlab = 'Year',ylab = 'Density of evictions', main = 'Smoothed functions of evictions')
  plot(funct_data_diff,xlab = 'Year', ylab = 'd/dt(Density of evictions)', main = 'Approximation of first derivatives')
  
  #MBD,mediana e boxplot per evictions funzionali
  modified_band_depth_rent = MBD(funct_data)
  print(paste('The nhood with max depth is:', colnames(funz_evictions)[which.max(modified_band_depth_rent)] ))
  median_curve_rent = median_fData(fData = funct_data, type = 'MBD' )
  x11()
  plot(funct_data, xlab = 'Year', ylab = 'Density of evictions', main = 'Smoothed functions of evictions with median (wrt MBD)')
  plot(median_curve_rent, col = 'red', lwd = 3, add = T)
  # fbplot(funct_data) da errori...bisogna usare la griglia di numeri e non di date!
  funct_data_num = fData(grid_time_num$date_num,t(funz_evictions))
  fbplot = fbplot(funct_data_num, main = 'Functional box-plot for functions of evictions')
  fbplot$ID_outliers
  x11()
  out_funct <- outliergram(funct_data) 
  out_funct$ID_outliers
  
  #MBD,mediana e boxplot per derivate di evictions
  modified_band_depth_rent_diff = MBD(funct_data_diff)
  print(paste('The nhood with max depth is:', colnames(funz_evictions)[which.max(modified_band_depth_rent_diff)] ))
  median_curve_rent_diff = median_fData(fData = funct_data_diff, type = 'MBD' )
  x11()
  plot(funct_data_diff,xlab = 'Year', ylab = 'd/dt(Density of evictions)', main = 'Approximation of first derivative with median (wrt MBD)')
  plot(median_curve_rent_diff, col = 'red', lwd = 3, add = T)
  x11()
  #fbplot(funct_data_diff) da errori... perch??
  funct_data_diff_num = fData(grid_time_num$date_num[2:dim(grid_time_num)[1]],t(diff_evictions))
  fbplot(funct_data_diff_num, main = 'Functional box-plot for first derivative of evictions')
  out_funct_diff <- outliergram(funct_data_diff) #Non ha alcun senso...
}


#Codice per gam a granularità parcel

library(readr)


parcels_smooth_price <- read_csv("parcels_smooth_price.csv")
parcels_augmented_complete <- read_csv("Parcels_augmented_complete.csv")
parcels = merge(parcels_smooth_price, parcels_augmented_complete, by = 'OBJECTID')
rm(parcels_smooth_price,parcels_augmented_complete)

#Aggiungo delle colonne per le variazioni dei prezzi
parcels$incr_2012 = parcels$price_2012 - parcels$price_2011
parcels$incr_2013 = parcels$price_2013 - parcels$price_2012
parcels$incr_2014 = parcels$price_2014 - parcels$price_2013
parcels$incr_2015 = parcels$price_2015 - parcels$price_2014
parcels$incr_2016 = parcels$price_2016 - parcels$price_2015
parcels$incr_2017 = parcels$price_2017 - parcels$price_2016
parcels$incr_2018 = parcels$price_2018 - parcels$price_2017


#Devo costruire un dataset per girare: 
# price ~ anno + num_constr anno-dist + num_shuttlestop + nhood + dist caltr + dist fin
#Devo riuscire a mettere tutto nella stessa riga! 

#Duplico le osservazioni per gli anni 2011-2018 (ie 8 repliche)
parcels_2011 = data.frame(OBJECTID = parcels$OBJECTID,lon = parcels$lon , lat = parcels$lat,
                          nhood = parcels$neighborhoods, resunits = parcels$RESUNITS,
                          FinDistr_distance = parcels$FinDistr_distance,
                          CaltStat_distance = parcels$CaltStat_distance,
                          minDistGB = parcels$MinDistGB,
                          year = 2011,
                          price = parcels$price_2011,
                          delta_price = NA,
                          newc_100m_4  = parcels$newc_100m_2007,
                          newc_500m_4  = parcels$newc_500m_2007,
                          newc_1000m_4 = parcels$newc_1000m_2007,
                          newc_2000m_4 = parcels$newc_2000m_2007,
                          newc_100m_3  = parcels$newc_100m_2008,
                          newc_500m_3  = parcels$newc_500m_2008,
                          newc_1000m_3 = parcels$newc_1000m_2008,
                          newc_2000m_3 = parcels$newc_2000m_2008,
                          newc_100m_2  = parcels$newc_100m_2009,
                          newc_500m_2  = parcels$newc_500m_2009,
                          newc_1000m_2 = parcels$newc_1000m_2009,
                          newc_2000m_2 = parcels$newc_2000m_2009,
                          newc_100m_1  = parcels$newc_100m_2010,
                          newc_500m_1  = parcels$newc_500m_2010,
                          newc_1000m_1 = parcels$newc_1000m_2010,
                          newc_2000m_1 = parcels$newc_2000m_2010,
                          newc_100m_0  = parcels$newc_100m_2011,
                          newc_500m_0  = parcels$newc_500m_2011,
                          newc_1000m_0 = parcels$newc_1000m_2011,
                          newc_2000m_0 = parcels$newc_2000m_2011)

parcels_2012 = data.frame(OBJECTID = parcels$OBJECTID,lon = parcels$lon , lat = parcels$lat,
                          nhood = parcels$neighborhoods, resunits = parcels$RESUNITS,
                          FinDistr_distance = parcels$FinDistr_distance,
                          CaltStat_distance = parcels$CaltStat_distance,
                          minDistGB = parcels$MinDistGB,
                          year = 2012,
                          price = parcels$price_2012,
                          delta_price = parcels$incr_2012,
                          newc_100m_4  = parcels$newc_100m_2008,
                          newc_500m_4  = parcels$newc_500m_2008,
                          newc_1000m_4 = parcels$newc_1000m_2008,
                          newc_2000m_4 = parcels$newc_2000m_2008,
                          newc_100m_3  = parcels$newc_100m_2009,
                          newc_500m_3  = parcels$newc_500m_2009,
                          newc_1000m_3 = parcels$newc_1000m_2009,
                          newc_2000m_3 = parcels$newc_2000m_2009,
                          newc_100m_2  = parcels$newc_100m_2010,
                          newc_500m_2  = parcels$newc_500m_2010,
                          newc_1000m_2 = parcels$newc_1000m_2010,
                          newc_2000m_2 = parcels$newc_2000m_2010,
                          newc_100m_1  = parcels$newc_100m_2011,
                          newc_500m_1  = parcels$newc_500m_2011,
                          newc_1000m_1 = parcels$newc_1000m_2011,
                          newc_2000m_1 = parcels$newc_2000m_2011,
                          newc_100m_0  = parcels$newc_100m_2012,
                          newc_500m_0  = parcels$newc_500m_2012,
                          newc_1000m_0 = parcels$newc_1000m_2012,
                          newc_2000m_0 = parcels$newc_2000m_2012
                          )


parcels_2013 = data.frame(OBJECTID = parcels$OBJECTID,lon = parcels$lon , lat = parcels$lat,
                          nhood = parcels$neighborhoods, resunits = parcels$RESUNITS,
                          FinDistr_distance = parcels$FinDistr_distance,
                          CaltStat_distance = parcels$CaltStat_distance,
                          minDistGB = parcels$MinDistGB,
                          year = 2013,
                          price = parcels$price_2013,
                          delta_price = parcels$incr_2013,
                          newc_100m_4  = parcels$newc_100m_2009,
                          newc_500m_4  = parcels$newc_500m_2009,
                          newc_1000m_4 = parcels$newc_1000m_2009,
                          newc_2000m_4 = parcels$newc_2000m_2009,
                          newc_100m_3  = parcels$newc_100m_2010,
                          newc_500m_3  = parcels$newc_500m_2010,
                          newc_1000m_3 = parcels$newc_1000m_2010,
                          newc_2000m_3 = parcels$newc_2000m_2010,
                          newc_100m_2  = parcels$newc_100m_2011,
                          newc_500m_2  = parcels$newc_500m_2011,
                          newc_1000m_2 = parcels$newc_1000m_2011,
                          newc_2000m_2 = parcels$newc_2000m_2011,
                          newc_100m_1  = parcels$newc_100m_2012,
                          newc_500m_1  = parcels$newc_500m_2012,
                          newc_1000m_1 = parcels$newc_1000m_2012,
                          newc_2000m_1 = parcels$newc_2000m_2012,
                          newc_100m_0  = parcels$newc_100m_2013,
                          newc_500m_0  = parcels$newc_500m_2013,
                          newc_1000m_0 = parcels$newc_1000m_2013,
                          newc_2000m_0 = parcels$newc_2000m_2013
)


parcels_2014 = data.frame(OBJECTID = parcels$OBJECTID,lon = parcels$lon , lat = parcels$lat,
                          nhood = parcels$neighborhoods, resunits = parcels$RESUNITS,
                          FinDistr_distance = parcels$FinDistr_distance,
                          CaltStat_distance = parcels$CaltStat_distance,
                          minDistGB = parcels$MinDistGB,
                          year = 2014,
                          price = parcels$price_2014,
                          delta_price = parcels$incr_2014,
                          newc_100m_4  = parcels$newc_100m_2010,
                          newc_500m_4  = parcels$newc_500m_2010,
                          newc_1000m_4 = parcels$newc_1000m_2010,
                          newc_2000m_4 = parcels$newc_2000m_2010,
                          newc_100m_3  = parcels$newc_100m_2011,
                          newc_500m_3  = parcels$newc_500m_2011,
                          newc_1000m_3 = parcels$newc_1000m_2011,
                          newc_2000m_3 = parcels$newc_2000m_2011,
                          newc_100m_2  = parcels$newc_100m_2012,
                          newc_500m_2  = parcels$newc_500m_2012,
                          newc_1000m_2 = parcels$newc_1000m_2012,
                          newc_2000m_2 = parcels$newc_2000m_2012,
                          newc_100m_1  = parcels$newc_100m_2013,
                          newc_500m_1  = parcels$newc_500m_2013,
                          newc_1000m_1 = parcels$newc_1000m_2013,
                          newc_2000m_1 = parcels$newc_2000m_2013,
                          newc_100m_0  = parcels$newc_100m_2014,
                          newc_500m_0  = parcels$newc_500m_2014,
                          newc_1000m_0 = parcels$newc_1000m_2014,
                          newc_2000m_0 = parcels$newc_2000m_2014
)

parcels_2015 = data.frame(OBJECTID = parcels$OBJECTID,lon = parcels$lon , lat = parcels$lat,
                          nhood = parcels$neighborhoods, resunits = parcels$RESUNITS,
                          FinDistr_distance = parcels$FinDistr_distance,
                          CaltStat_distance = parcels$CaltStat_distance,
                          minDistGB = parcels$MinDistGB,
                          year = 2015,
                          price = parcels$price_2015,
                          delta_price = parcels$incr_2015,
                          newc_100m_4  = parcels$newc_100m_2011,
                          newc_500m_4  = parcels$newc_500m_2011,
                          newc_1000m_4 = parcels$newc_1000m_2011,
                          newc_2000m_4 = parcels$newc_2000m_2011,
                          newc_100m_3  = parcels$newc_100m_2012,
                          newc_500m_3  = parcels$newc_500m_2012,
                          newc_1000m_3 = parcels$newc_1000m_2012,
                          newc_2000m_3 = parcels$newc_2000m_2012,
                          newc_100m_2  = parcels$newc_100m_2013,
                          newc_500m_2  = parcels$newc_500m_2013,
                          newc_1000m_2 = parcels$newc_1000m_2013,
                          newc_2000m_2 = parcels$newc_2000m_2013,
                          newc_100m_1  = parcels$newc_100m_2014,
                          newc_500m_1  = parcels$newc_500m_2014,
                          newc_1000m_1 = parcels$newc_1000m_2014,
                          newc_2000m_1 = parcels$newc_2000m_2014,
                          newc_100m_0  = parcels$newc_100m_2015,
                          newc_500m_0  = parcels$newc_500m_2015,
                          newc_1000m_0 = parcels$newc_1000m_2015,
                          newc_2000m_0 = parcels$newc_2000m_2015
)


parcels_2016 = data.frame(OBJECTID = parcels$OBJECTID,lon = parcels$lon , lat = parcels$lat,
                          nhood = parcels$neighborhoods, resunits = parcels$RESUNITS,
                          FinDistr_distance = parcels$FinDistr_distance,
                          CaltStat_distance = parcels$CaltStat_distance,
                          minDistGB = parcels$MinDistGB,
                          year = 2016,
                          price = parcels$price_2016,
                          delta_price = parcels$incr_2016,
                          newc_100m_4  = parcels$newc_100m_2012,
                          newc_500m_4  = parcels$newc_500m_2012,
                          newc_1000m_4 = parcels$newc_1000m_2012,
                          newc_2000m_4 = parcels$newc_2000m_2012,
                          newc_100m_3  = parcels$newc_100m_2013,
                          newc_500m_3  = parcels$newc_500m_2013,
                          newc_1000m_3 = parcels$newc_1000m_2013,
                          newc_2000m_3 = parcels$newc_2000m_2013,
                          newc_100m_2  = parcels$newc_100m_2014,
                          newc_500m_2  = parcels$newc_500m_2014,
                          newc_1000m_2 = parcels$newc_1000m_2014,
                          newc_2000m_2 = parcels$newc_2000m_2014,
                          newc_100m_1  = parcels$newc_100m_2015,
                          newc_500m_1  = parcels$newc_500m_2015,
                          newc_1000m_1 = parcels$newc_1000m_2015,
                          newc_2000m_1 = parcels$newc_2000m_2015,
                          newc_100m_0  = parcels$newc_100m_2016,
                          newc_500m_0  = parcels$newc_500m_2016,
                          newc_1000m_0 = parcels$newc_1000m_2016,
                          newc_2000m_0 = parcels$newc_2000m_2016
)


parcels_2017 = data.frame(OBJECTID = parcels$OBJECTID,lon = parcels$lon , lat = parcels$lat,
                          nhood = parcels$neighborhoods, resunits = parcels$RESUNITS,
                          FinDistr_distance = parcels$FinDistr_distance,
                          CaltStat_distance = parcels$CaltStat_distance,
                          minDistGB = parcels$MinDistGB,
                          year = 2017,
                          price = parcels$price_2017,
                          delta_price = parcels$incr_2017,
                          newc_100m_4  = parcels$newc_100m_2013,
                          newc_500m_4  = parcels$newc_500m_2013,
                          newc_1000m_4 = parcels$newc_1000m_2013,
                          newc_2000m_4 = parcels$newc_2000m_2013,
                          newc_100m_3  = parcels$newc_100m_2014,
                          newc_500m_3  = parcels$newc_500m_2014,
                          newc_1000m_3 = parcels$newc_1000m_2014,
                          newc_2000m_3 = parcels$newc_2000m_2014,
                          newc_100m_2  = parcels$newc_100m_2015,
                          newc_500m_2  = parcels$newc_500m_2015,
                          newc_1000m_2 = parcels$newc_1000m_2015,
                          newc_2000m_2 = parcels$newc_2000m_2015,
                          newc_100m_1  = parcels$newc_100m_2016,
                          newc_500m_1  = parcels$newc_500m_2016,
                          newc_1000m_1 = parcels$newc_1000m_2016,
                          newc_2000m_1 = parcels$newc_2000m_2016,
                          newc_100m_0  = parcels$newc_100m_2017,
                          newc_500m_0  = parcels$newc_500m_2017,
                          newc_1000m_0 = parcels$newc_1000m_2017,
                          newc_2000m_0 = parcels$newc_2000m_2017
)


parcels_2018 = data.frame(OBJECTID = parcels$OBJECTID,lon = parcels$lon , lat = parcels$lat,
                          nhood = parcels$neighborhoods, resunits = parcels$RESUNITS,
                          FinDistr_distance = parcels$FinDistr_distance,
                          CaltStat_distance = parcels$CaltStat_distance,
                          minDistGB = parcels$MinDistGB,
                          year = 2018,
                          price = parcels$price_2018,
                          delta_price = parcels$incr_2018,
                          newc_100m_4  = parcels$newc_100m_2014,
                          newc_500m_4  = parcels$newc_500m_2014,
                          newc_1000m_4 = parcels$newc_1000m_2014,
                          newc_2000m_4 = parcels$newc_2000m_2014,
                          newc_100m_3  = parcels$newc_100m_2015,
                          newc_500m_3  = parcels$newc_500m_2015,
                          newc_1000m_3 = parcels$newc_1000m_2015,
                          newc_2000m_3 = parcels$newc_2000m_2015,
                          newc_100m_2  = parcels$newc_100m_2016,
                          newc_500m_2  = parcels$newc_500m_2016,
                          newc_1000m_2 = parcels$newc_1000m_2016,
                          newc_2000m_2 = parcels$newc_2000m_2016,
                          newc_100m_1  = parcels$newc_100m_2017,
                          newc_500m_1  = parcels$newc_500m_2017,
                          newc_1000m_1 = parcels$newc_1000m_2017,
                          newc_2000m_1 = parcels$newc_2000m_2017,
                          newc_100m_0  = parcels$newc_100m_2018,
                          newc_500m_0  = parcels$newc_500m_2018,
                          newc_1000m_0 = parcels$newc_1000m_2018,
                          newc_2000m_0 = parcels$newc_2000m_2018
)


data_gam = rbind(parcels_2011, parcels_2012, parcels_2013, parcels_2014,
                 parcels_2015, parcels_2016 ,parcels_2017, parcels_2018)
#rm(parcels_2011, parcels_2012, parcels_2013, parcels_2014,
#   parcels_2015, parcels_2016 ,parcels_2017, parcels_2018, parcels)

#Riscalo le distanze in km (così sono circa dello stesso ordine di grandezza delle altre variabili) 
# per vedere se poi cambia qualcosa (e comunque ha più senso in km...) :
data_gam$year_fct = as.factor(data_gam$year)
data_gam$minDistGB_km = data_gam$minDistGB / 1e3
data_gam$FinDistr_distance_km = data_gam$FinDistr_distance / 1e3
data_gam$CaltStat_distance_km = data_gam$CaltStat_distance / 1e3
#Aggrego le costruzioni
data_gam$newc_100m = data_gam$newc_100m_4 + data_gam$newc_100m_3 + data_gam$newc_100m_2 +data_gam$newc_100m_1 + data_gam$newc_100m_0
data_gam$newc_500m = data_gam$newc_500m_4 + data_gam$newc_500m_3 + data_gam$newc_500m_2 +data_gam$newc_500m_1 + data_gam$newc_500m_0
data_gam$newc_1000m = data_gam$newc_1000m_4 + data_gam$newc_1000m_3 + data_gam$newc_1000m_2 +data_gam$newc_1000m_1 + data_gam$newc_1000m_0
data_gam$newc_2000m = data_gam$newc_2000m_4 + data_gam$newc_2000m_3 + data_gam$newc_2000m_2 +data_gam$newc_2000m_1 + data_gam$newc_2000m_0

#boxplot(data_gam[,c(2,3,10:30,32:34)])


#Gam base con factor dell'anno e costruzioni separate
model_gam=gam(price ~ s(lon,lat,bs='tp') + nhood + year_fct +
                s(newc_100m_4,bs='cr') + s(newc_500m_4,bs='cr') + s(newc_1000m_4,bs='cr') + s(newc_2000m_4,bs='cr')+
                s(newc_100m_3,bs='cr') + s(newc_500m_3,bs='cr') + s(newc_1000m_3,bs='cr') + s(newc_2000m_3,bs='cr')+
                s(newc_100m_2,bs='cr') + s(newc_500m_2,bs='cr') + s(newc_1000m_2,bs='cr') + s(newc_2000m_2,bs='cr')+
                s(newc_100m_1,bs='cr') + s(newc_500m_1,bs='cr') + s(newc_1000m_1,bs='cr') + s(newc_2000m_1,bs='cr')+
                s(newc_100m_0,bs='cr') + s(newc_500m_0,bs='cr') + s(newc_1000m_0,bs='cr') + s(newc_2000m_0,bs='cr')+
                s(minDistGB_km,bs='cr'), #+ s(FinDistr_distance,bs ='cr') + s(CaltStat_distance,bs='cr'),
              data = data_gam)
summary(model_gam) #Effective degrees of freedom (edf) is a summary statistic of GAM and it reflects the degree of non-linearity of a curve
hist(model_gam$residuals)
qqnorm(model_gam$residuals)
#shapiro.test(model_gam$residuals)
#par(mfrow = c(2,2))
plot(model_gam)
#R2 0.865, tutto significativo a parte qualche nhood.
#Non metterei le distanze da fin disr e caltr station perchè interferiscono con lat-long
#Per i plot, l'interpretazione direi solo che si può guardare dove prevale un effetto rispetto l'altro
#Non mi pare si riesca a concludere nulla sul fatto che "costruzioni vicine fanno aumentare" e/o
# "lontane fanno diminuire"


#Modello per solo un anno (2018)
parcels_2018$year_fct = as.factor(parcels_2018$year)
parcels_2018$minDistGB_km = parcels_2018$minDistGB / 1e3
parcels_2018$FinDistr_distance_km = parcels_2018$FinDistr_distance / 1e3
parcels_2018$CaltStat_distance_km = parcels_2018$CaltStat_distance / 1e3
model_gam2018=gam(price ~ s(lon,lat,bs='tp') + nhood +
                s(newc_100m_4,bs='cr') + s(newc_500m_4,bs='cr') + s(newc_1000m_4,bs='cr') + s(newc_2000m_4,bs='cr')+
                s(newc_100m_3,bs='cr') + s(newc_500m_3,bs='cr') + s(newc_1000m_3,bs='cr') + s(newc_2000m_3,bs='cr')+
                s(newc_100m_2,bs='cr') + s(newc_500m_2,bs='cr') + s(newc_1000m_2,bs='cr') + s(newc_2000m_2,bs='cr')+
                s(newc_100m_1,bs='cr') + s(newc_500m_1,bs='cr') + s(newc_1000m_1,bs='cr') + s(newc_2000m_1,bs='cr')+
                s(newc_100m_0,bs='cr') + s(newc_500m_0,bs='cr') + s(newc_1000m_0,bs='cr') + s(newc_2000m_0,bs='cr')+
                s(minDistGB_km,bs='cr'), #+ s(FinDistr_distance,bs ='cr') + s(CaltStat_distance,bs='cr'),
              data = parcels_2018)
summary(model_gam2018)
plot(model_gam2018)
#R2 a 0.974 
#Tutto significativo ... I plot possono essere interpretati come:
# - bassa dipendenza, i due effetti si bilanciano
# - dipendenza in aumento positivo, effetto "bel vicinato" prevale
# - dipendenza in diminuzione, inizia a prevalere l'effetto dell'aumento dell offerta
# - Ha senso interpretare anche il segno delle ordinate? Cioè aumento (+) o diminuzione del prezzo (-)?
#Non mi pare si riesca a concludere nulla sul fatto che "costruzioni vicine fanno aumentare" e/o
# "lontane fanno diminuire"


#Modello in cui raggruppo gli anni a due a due
model_gam_2a2=gam(price ~ s(lon,lat,bs='tp') + nhood + year_fct +
                s(I(newc_100m_4+newc_100m_3),bs='cr') + s(I(newc_500m_4+newc_500m_3),bs='cr') +
                s(I(newc_1000m_4+newc_1000m_3),bs='cr') + s(I(newc_2000m_4+newc_2000m_3),bs='cr')+
                s(I(newc_100m_2+newc_100m_1),bs='cr') + s(I(newc_500m_2+newc_500m_1),bs='cr') +
                s(I(newc_1000m_2+newc_1000m_1),bs='cr') + s(I(newc_2000m_2+newc_2000m_1),bs='cr')+
                s(newc_100m_0,bs='cr') + s(newc_500m_0,bs='cr') + s(newc_1000m_0,bs='cr') + s(newc_2000m_0,bs='cr')+
                s(minDistGB_km,bs='cr'), #+ s(FinDistr_distance,bs ='cr') + s(CaltStat_distance,bs='cr'),
              data = data_gam)
summary(model_gam_2a2)
plot(model_gam_2a2)
#R2 a 0.862
#Tutto significativo...solita interpretazione e conclusione



#E se si provasse a guardare il numero di new_constr vicine indipendentemente dagli anni?
# Provo ad aggregare le costruzioni dei 4 anni precedenti considerando solo un anno
#Modello per solo un anno (2018) con costruzioni aggregate
model_gam2018_aggr=gam(price ~ s(lon,lat,bs='tp') + nhood +
                    s(I(newc_100m_4+newc_100m_3+newc_100m_2+newc_100m_1+newc_100m_0),bs='cr') +
                    s(I(newc_500m_4+newc_500m_3+newc_500m_2+newc_500m_1+newc_500m_0),bs='cr') + 
                    s(I(newc_1000m_4+newc_1000m_3+newc_1000m_2+newc_1000m_1+newc_1000m_0),bs='cr') +
                    s(I(newc_2000m_4+newc_2000m_3+newc_2000m_2+newc_2000m_1+newc_2000m_0),bs='cr')+
                    s(minDistGB_km,bs='cr'), #+ s(FinDistr_distance,bs ='cr') + s(CaltStat_distance,bs='cr'),
                  data = parcels_2018)
summary(model_gam2018_aggr)
plot(model_gam2018_aggr)
#R2 0.968
#Tutto significativo
#Se ha senso interpretare ordinata positiva/negativa, l'interpretazione viene!
# Costr vicine implicano sempre effetto pos e in base al numero entra in gioco anche l'effetto 
# "offerta aumentata" (quindi cambia la pendenza) ma comunque rimane sempre positivo!
#Le altre costruzioni hanno effetti minori e 1000-2000 sono negativi in alcune zone!




#Provo a rifare il modello con anni aggregati ma su tutti gli anni...
# Ha senso? Le osservazioni sono fortemente dipendenti!!!

# model_gam_aggr=gam(price ~ s(lon,lat,bs='tp') + nhood + year_fct +
#                          s(I(newc_100m_4+newc_100m_3+newc_100m_2+newc_100m_1+newc_100m_0),bs='cr') +
#                          s(I(newc_500m_4+newc_500m_3+newc_500m_2+newc_500m_1+newc_500m_0),bs='cr') + 
#                          s(I(newc_1000m_4+newc_1000m_3+newc_1000m_2+newc_1000m_1+newc_1000m_0),bs='cr') +
#                          s(I(newc_2000m_4+newc_2000m_3+newc_2000m_2+newc_2000m_1+newc_2000m_0),bs='cr')+
#                          s(minDistGB_km,bs='cr'), #+ s(FinDistr_distance,bs ='cr') + s(CaltStat_distance,bs='cr'),
#                        data = data_gam)
model_gam_aggr = gam(price ~ nhood + s(year,bs = 'cr',k = 8) + #s(lon,lat,bs='tp') + 
                     s(newc_100m,bs='cr') +
                     s(newc_500m,bs='cr') + 
                     s(newc_1000m,bs='cr') +
                     s(newc_2000m,bs='cr')+
                     s(minDistGB_km,bs='cr'),
                   data = data_gam)
summary(model_gam_aggr)
plot(model_gam_aggr)
#R2 a 0.857
#Tutto significativo a parte qualche nhood
#Considerando un factor sull'anno, le dipendenze dalle costruzioni diventano un po strane...
# Quasi sempre negative e con pendenza negativa (ie effetto offerta prevale!) 



#Rifaccio il modello con le costruzioni aggregate ma senza il factor sull'year
model_gam_aggr=gam(price ~ s(lon,lat,bs='tp') + nhood + #year_fct +
                     s(I(newc_100m_4+newc_100m_3+newc_100m_2+newc_100m_1+newc_100m_0),bs='cr') +
                     s(I(newc_500m_4+newc_500m_3+newc_500m_2+newc_500m_1+newc_500m_0),bs='cr') + 
                     s(I(newc_1000m_4+newc_1000m_3+newc_1000m_2+newc_1000m_1+newc_1000m_0),bs='cr') +
                     s(I(newc_2000m_4+newc_2000m_3+newc_2000m_2+newc_2000m_1+newc_2000m_0),bs='cr')+
                     s(minDistGB_km,bs='cr'), #+ s(FinDistr_distance,bs ='cr') + s(CaltStat_distance,bs='cr'),
                   data = data_gam)
summary(model_gam_aggr)
plot(model_gam_aggr)
#R2 a 0.7
# Effetto delle costruzioni vicine positivo, quello delle altre è strano (molto 
# negativo per poche costruzioni e poi sale...)





#Tolgo i nhood
model_gam_aggr=gam(price ~ s(lon,lat,bs='tp') + s(year,bs = 'cr', k = 8) + nhood + 
                     s(newc_100m,bs='cr') +
                     s(newc_500m,bs='cr') + 
                     s(newc_1000m,bs='cr')+
                     s(newc_2000m,bs='cr')+
                     s(minDistGB_km,bs='cr'),
                   data = data_gam)
summary(model_gam_aggr)
plot(model_gam_aggr)










#Provo a considerare le variazioni di prezzo per gli anni superiori al 2011

ind_with_delta = which(is.na(data_gam$delta_price) == FALSE)
data_gam_from2012 = data_gam[ind_with_delta,]

model_gam_aggr_delta_price=gam(delta_price ~ nhood + year_fct + s(lon,lat,bs='tp') + 
                     s(newc_100m,bs='cr') +
                     s(newc_500m,bs='cr') + 
                     s(newc_1000m,bs='cr') +
                     s(newc_2000m,bs='cr')+
                     s(minDistGB_km,bs='cr'),
                   data = data_gam_from2012)
summary(model_gam_aggr_delta_price)
plot(model_gam_aggr_delta_price)
#R2 a 0.18 ... non ha molto senso come modello...?
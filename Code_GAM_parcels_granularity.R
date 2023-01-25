#Codice per gam a granularità parcel

library(readr)


parcels_smooth_price <- read_csv("parcels_smooth_price.csv")
parcels_augmented_complete <- read_csv("Parcels_augmented_complete.csv")
parcels = merge(parcels_smooth_price, parcels_augmented_complete, by = 'OBJECTID')
rm(parcels_smooth_price,parcels_augmented_complete)

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
rm(parcels_2011, parcels_2012, parcels_2013, parcels_2014,
   parcels_2015, parcels_2016 ,parcels_2017, parcels_2018, parcels)



model_gam=gam(price ~ s(lon,lat,bs='tp') + nhood + year +
                s(newc_100m_4,bs='cr') + s(newc_500m_4,bs='cr') + s(newc_1000m_4,bs='cr') + s(newc_2000m_4,bs='cr')+
                s(newc_100m_3,bs='cr') + s(newc_500m_3,bs='cr') + s(newc_1000m_3,bs='cr') + s(newc_2000m_3,bs='cr')+
                s(newc_100m_2,bs='cr') + s(newc_500m_2,bs='cr') + s(newc_1000m_2,bs='cr') + s(newc_2000m_2,bs='cr')+
                s(newc_100m_1,bs='cr') + s(newc_500m_1,bs='cr') + s(newc_1000m_1,bs='cr') + s(newc_2000m_1,bs='cr')+
                s(newc_100m_0,bs='cr') + s(newc_500m_0,bs='cr') + s(newc_1000m_0,bs='cr') + s(newc_2000m_0,bs='cr')+
                s(minDistGB,bs='cr'), #+ s(FinDistr_distance,bs ='cr') + s(CaltStat_distance,bs='cr'),
              data = data_gam)
summary(model_gam) #Effective degrees of freedom (edf) is a summary statistic of GAM and it reflects the degree of non-linearity of a curve
hist(model_gam$residuals)
qqnorm(model_gam$residuals)
shapiro.test(model_gam$residuals)
#par(mfrow = c(2,2))
plot(model_gam)









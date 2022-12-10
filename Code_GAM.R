#Code for GAM model (yearly)

#Preparo dataset con numero di costruzioni per year-nhood ######################

new_constr <- read_csv("New_construction_clean_geocoded_nh.csv")
unique(new_constr$neighborhoods)
unique(new_constr$year) #ho degli NA nelle date da rimuovere dopo!

new_constr$date = as.Date(new_constr$date, tryFormats = '%d/%m/%Y')
aus_df <- data.frame(year = as.numeric(format(new_constr$date, format = "%Y")),
                     month = as.numeric(format(new_constr$date, format = "%m")),
                     day = as.numeric(format(new_constr$date, format = "%d")))
new_constr = cbind(new_constr,aus_df)
rm(aus_df)

vect_nhood = new_constr$neighborhoods
vect_year = new_constr$year
vect_aus = paste(vect_year,vect_nhood,sep = '-')
new_constr$year_nhood = vect_aus
rm(vect_nhood,vect_year,vect_aus)

new_constr = cbind(new_constr, count = rep(1,dim(new_constr)[1]))
num_constr =aggregate(new_constr[,c(11,21)], by = list(new_constr$year_nhood), FUN = sum)
colnames(num_constr)[1] = 'year_nhood'
num_constr
rm(new_constr)
num_constr$year = rep(NA,dim(num_constr)[1])
num_constr$nhood = rep(NA,dim(num_constr)[1])
for(i in 1:dim(num_constr)[1]){
  splitted = strsplit(num_constr[i,]$year_nhood,split = '-')
  num_constr[i,]$year = splitted[[1]][1]
  num_constr[i,]$nhood = splitted[[1]][2]
}
rm(splitted,i)
num_constr = num_constr[-which(num_constr$year == 'NA'),]

rent_clean <- read_csv("rent_clean.csv")
unique(rent_clean$nhood)
unique(num_constr$nhood)

rent_clean$num_constr = rep(0,dim(rent_clean)[1])
rent_clean$num_units = rep(0,dim(rent_clean)[1])
for(i in 1:dim(num_constr)[1]){
  ind_nhood_year = which(rent_clean$nhood == num_constr[i,]$nhood & rent_clean$year == num_constr[i,]$year)
  rent_clean[ind_nhood_year,]$num_constr = num_constr[i,]$count
  rent_clean[ind_nhood_year,]$num_units = num_constr[i,]$new_units_built
}
rent_clean = rent_clean[,-c(1,2,4,5)]
rm(num_constr,i,ind_nhood_year)


# GAM model ####################################################################
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)


lin_mod = lm(price_mq ~ nhood:year + num_units , data = rent_clean)
summary(lin_mod)

{#GAM with natural splines
  model_gam_ns <-lm(price_mq ~ ns(num_units, df = 3) + ns(year, df = 3), data = rent_clean)
  summary(model_gam_ns)
  gam::plot.Gam(model_gam_ns, se=TRUE)
  #plot(model_gam_ns$residuals,model_gam$residuals)
  #cor(model_gam_ns$residuals,model_gam$residuals) to compare the residuals with "GAM with smoothing cubic splines"
}










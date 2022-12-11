rent_clean <- read.csv("C:/Users/Pietro/Desktop/Pietro/Politecnico/Magistrale/Nonparametric_Statistics/Progetto/ricerca di progetti/Progetto Case SF/SF-houses/rent_clean_nh.csv", header=TRUE)





# ---------- 7) Permutational ANOVA on rent_clean grouping by NHOOD

#I need to choose the years to be considered, I need the ones with the most observations
{
length(rent_clean[rent_clean$year == '2001',9]) #5
length(rent_clean[rent_clean$year == '2002',9]) #6
length(rent_clean[rent_clean$year == '2003',9]) #7   
length(rent_clean[rent_clean$year == '2004',9]) #250
length(rent_clean[rent_clean$year == '2005',9]) #28
length(rent_clean[rent_clean$year == '2006',9]) #67
length(rent_clean[rent_clean$year == '2007',9]) #24
length(rent_clean[rent_clean$year == '2008',9]) #8
length(rent_clean[rent_clean$year == '2009',9]) #5
length(rent_clean[rent_clean$year == '2010',9]) #6
length(rent_clean[rent_clean$year == '2011',9]) #1241    #Basandoci su 'rent_clean.csv', gli unici anni in cui studiare sono dal 2011 al 2018
length(rent_clean[rent_clean$year == '2012',9]) #2707
length(rent_clean[rent_clean$year == '2013',9]) #884
length(rent_clean[rent_clean$year == '2014',9]) #623
length(rent_clean[rent_clean$year == '2015',9]) #1587
length(rent_clean[rent_clean$year == '2016',9]) #2121
length(rent_clean[rent_clean$year == '2017',9]) #262
length(rent_clean[rent_clean$year == '2018',9]) #354
}

#I will keep only years s.t. 2010 < year < 2017

rent_temp <- rent_clean
rent_temp <- rent_temp[rent_temp$year > 2010 & rent_temp$year < 2017,]


Levels.name1 <- factor(rent_temp$nhood)
Levels.name <- factor(rent_temp$nhood, labels=c(1:length(levels(Levels.name1))))
treat   <- levels(Levels.name)      # levels of the treatment
g       <- length(treat)    

plot(Levels.name, rent_temp$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood 


# I will start by running 6 separate ANOVA, one for each year from 2011 to 2016
rent_2011 <- rent_temp[rent_temp$year == 2011,]
rent_2012 <- rent_temp[rent_temp$year == 2012,]
rent_2013 <- rent_temp[rent_temp$year == 2013,]
rent_2014 <- rent_temp[rent_temp$year == 2014,]
rent_2015 <- rent_temp[rent_temp$year == 2015,]
rent_2016 <- rent_temp[rent_temp$year == 2016,]



#-- ANOVA on year = 2011
{
Levels.name1 <- factor(rent_2011$nhood)
treat1   <- levels(Levels.name1)      # levels of the treatment
Levels.name <- factor(rent_2011$nhood, labels=c(1:34))
treat   <- levels(Levels.name)      # levels of the treatment
g       <- length(treat)    # 34 nhoods  
B = 1000
seed = 26111992

#Let's count the number of observations in each nhood
i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
 
}

#I will remove nhood with less than 10 observations.(I tried with 5, not enough to remove differences)
remove_index <- which(nn<10)
remove_nhood <- treat1[remove_index]
rent_2011 <- rent_2011[which(rent_2011[,3]!=remove_nhood[1] & rent_2011[,3]!=remove_nhood[2] & rent_2011[,3]!=remove_nhood[3] & rent_2011[,3]!=remove_nhood[4] & rent_2011[,3]!=remove_nhood[5] & rent_2011[,3]!=remove_nhood[6] & rent_2011[,3]!=remove_nhood[7]),]
rent_2011g = rent_2011[,c(3,11)]

#Now let's proceed doing an ANOVA
Levels.name1 <- factor(rent_2011g$nhood)
treat1   <- levels(Levels.name1)      # levels of the treatment
Levels.name <- factor(rent_2011g$nhood, labels=c(1:length(treat1)))
treat   <- levels(Levels.name)      # levels of the treatment

g       <- length(treat)    # 31 nhoods  
i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
  
}
n  <- sum(nn) #1175

plot(Levels.name, rent_2011g$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood nell anno 2011
fit <- aov(rent_2011g$price_mq ~ Levels.name)
summary(fit)


T0 <- summary(fit)[[1]][1,4] #F-value of Levels
T0

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  covariate_perm <- rent_2011g$price_mq[permutation]
  fit_perm <- aov(covariate_perm ~ Levels.name)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,30))
abline(v=T0,col=3,lwd=4)


# p-value
p_val <- sum(T_stat>=T0)/B
p_val

#-- p-value 0. Ci sono nhood diversi. Troviamoli


means <- numeric(g)
count = numeric(length(levels(Levels.name1)))
means= numeric(length(levels(Levels.name1)))
i = 1
for (ch in levels(Levels.name1)){
  
  for(j in 1:length(rent_2011g[,1])){
    if(rent_2011g[j,1]==ch){
      means[i] = means[i] + rent_2011g[j,2]
      count[i] = count[i]+1
  
    }
  }
  i = i+1
}
measn = means/count

plot(c(1:length(treat1)),measn) #sembra uno solo sia fuori scala!
first <- which.max(measn) #10imo è quello fuori scala
second <- which.max(measn[-first]) + 1                  # 30esimo è il secondo più fuori scala verso l'alto
levels(Levels.name1)[first]                # Financial District/South Beach/SOMA è quello che sembra fuori scala verso l'alto.
levels(Levels.name1)[second]               # SOMA / south beach è il secondo
rent_2011g <- rent_2011g[which(rent_2011g[,1]!="Financial District/South Beach/SOMA"  & rent_2011g[,1]!="Tenderloin") ,]
#rimuovo anche "candlestick" perchè ha pochissima varianza

#-- Rifaccio ANOVA uguale a prima, ma ora ho due nhood in meno
Levels.name1 <- factor(rent_2011g$nhood)
treat1   <- levels(Levels.name1)  
Levels.name <- factor(rent_2011g$nhood, labels=c(1:length(treat1)))
treat   <- levels(Levels.name)      # levels of the treatment
  
g       <- length(treat)    # 29 nhoods  
B = 1000
seed = 26111992


i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
  
}
n  <- sum(nn) #1127

plot(Levels.name, rent_2011g$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood nell anno 2011
fit <- aov(rent_2011g$price_mq ~ Levels.name)
summary(fit)


T0 <- summary(fit)[[1]][1,4] #F-value of Levels
T0

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  covariate_perm <- rent_2011g$price_mq[permutation]
  fit_perm <- aov(covariate_perm ~ Levels.name)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,30))
abline(v=T0,col=3,lwd=4)


# p-value
p_val <- sum(T_stat>=T0)/B
p_val
}

#---- sempre 0. non riesco a risolvere nulla. Passo al 2012


#-- ANOVA on year = 2012
{
Levels.name1 <- factor(rent_2012$nhood)
treat1   <- levels(Levels.name1)      # levels of the treatment
Levels.name <- factor(rent_2012$nhood, labels=c(1:length(unique(treat1))))
treat   <- levels(Levels.name)      # levels of the treatment
g       <- length(treat)    # 45 nhoods  
B = 1000
seed = 26111992

#Let's count the number of observations in each nhood
i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
  
}

#I will remove nhood with less than 10 observations.(I tried with 5, not enough to remove differences)
remove_index <- which(nn<10)
remove_nhood <- treat1[remove_index]
rent_2012 <- rent_2012[which(rent_2012[,3]!=remove_nhood[1] & rent_2012[,3]!=remove_nhood[2] & rent_2012[,3]!=remove_nhood[3]),]
rent_2012g = rent_2012[,c(3,11)]

#Now let's proceed doing an ANOVA
Levels.name1 <- factor(rent_2012g$nhood)
treat1   <- levels(Levels.name1)      # levels of the treatment
Levels.name <- factor(rent_2012g$nhood, labels=c(1:length(treat1)))
treat   <- levels(Levels.name)      # levels of the treatment
treat1   <- levels(Levels.name1)      # levels of the treatment
g       <- length(treat)    # 41 nhoods  
i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
  
}
n  <- sum(nn) #2690

plot(Levels.name, rent_2012g$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood nell anno 2011
fit <- aov(rent_2012g$price_mq ~ Levels.name)
summary(fit)


T0 <- summary(fit)[[1]][1,4] #F-value of Levels
T0

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  covariate_perm <- rent_2012g$price_mq[permutation]
  fit_perm <- aov(covariate_perm ~ Levels.name)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,30))
abline(v=T0,col=3,lwd=4)


# p-value
p_val <- sum(T_stat>=T0)/B
p_val

#-- p-value 0. Ci sono nhood diversi. Troviamoli


means <- numeric(g)
count = numeric(length(levels(Levels.name1)))
means= numeric(length(levels(Levels.name1)))
i = 1
for (ch in levels(Levels.name1)){
  
  for(j in 1:length(rent_2012g[,1])){
    if(rent_2012g[j,1]==ch){
      means[i] = means[i] + rent_2012g[j,2]
      count[i] = count[i]+1
      
    }
  }
  i = i+1
}
measn = means/count

plot(c(1:length(treat1)),measn) #sembra uno solo sia fuori scala!
first <- which.max(measn) #12esimo è quello fuori scala
levels(Levels.name1)[first]                # Financial District/South Beach/SOMA è quello che sembra fuori scala verso l'alto.
rent_2012g <- rent_2012g[which(rent_2012g[,1]!="Financial District/South Beach/SOMA"),]
#rimuovo anche "candlestick" perchè ha pochissima varianza

#-- Rifaccio ANOVA uguale a prima, ma ora ho due nhood in meno
Levels.name1 <- factor(rent_2012g$nhood)
treat1   <- levels(Levels.name1)  
Levels.name <- factor(rent_2012g$nhood, labels=c(1:length(treat1)))
treat   <- levels(Levels.name)      # levels of the treatment

g       <- length(treat)    # 29 nhoods  
B = 1000
seed = 26111992


i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
  
}
n  <- sum(nn) #1127

plot(Levels.name, rent_2012g$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood nell anno 2011
fit <- aov(rent_2012g$price_mq ~ Levels.name)
summary(fit)


T0 <- summary(fit)[[1]][1,4] #F-value of Levels
T0

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  covariate_perm <- rent_2012g$price_mq[permutation]
  fit_perm <- aov(covariate_perm ~ Levels.name)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,30))
abline(v=T0,col=3,lwd=4)


# p-value
p_val <- sum(T_stat>=T0)/B
p_val
}

#---- sempre 0. non riesco a risolvere nulla. Passo al 2013


#-- ANOVA on year = 2013
{
Levels.name1 <- factor(rent_2013$nhood)
treat1   <- levels(Levels.name1)      # levels of the treatment
Levels.name <- factor(rent_2013$nhood, labels=c(1:length(unique(treat1))))
treat   <- levels(Levels.name)      # levels of the treatment
g       <- length(treat)    # 42 nhoods  
B = 1000
seed = 26111992

#Let's count the number of observations in each nhood
i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
  
}

#I will remove nhood with less than 20 observations.(I tried with 5 and 10, not enough to remove differences)
remove_index <- which(nn<20)
remove_nhood <- treat1[remove_index]
rent_2013 <- rent_2013[which(rent_2013[,3]!=remove_nhood[1] & rent_2013[,3]!=remove_nhood[2] & rent_2013[,3]!=remove_nhood[3] & rent_2013[,3]!=remove_nhood[4] & rent_2013[,3]!=remove_nhood[5] & rent_2013[,3]!=remove_nhood[6] & rent_2013[,3]!=remove_nhood[7]  & rent_2013[,3]!=remove_nhood[8]  & rent_2013[,3]!=remove_nhood[9] &  rent_2013[,3]!=remove_nhood[10] & rent_2013[,3]!=remove_nhood[11] &  rent_2013[,3]!=remove_nhood[12] &  rent_2013[,3]!=remove_nhood[13] & rent_2013[,3]!=remove_nhood[14] & rent_2013[,3]!=remove_nhood[15] & rent_2013[,3]!=remove_nhood[16]),]
rent_2013g = rent_2013[,c(3,11)]

#Now let's proceed doing an ANOVA
Levels.name1 <- factor(rent_2013g$nhood)
treat1   <- levels(Levels.name1)      # levels of the treatment
Levels.name <- factor(rent_2013g$nhood, labels=c(1:length(treat1)))
treat   <- levels(Levels.name)      # levels of the treatment
treat1   <- levels(Levels.name1)      # levels of the treatment
g       <- length(treat)    # 41 nhoods  
i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
  
}
n  <- sum(nn) #2690

plot(Levels.name, rent_2013g$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood nell anno 2013
fit <- aov(rent_2013g$price_mq ~ Levels.name)
summary(fit)


T0 <- summary(fit)[[1]][1,4] #F-value of Levels
T0

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  covariate_perm <- rent_2013g$price_mq[permutation]
  fit_perm <- aov(covariate_perm ~ Levels.name)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,30))
abline(v=T0,col=3,lwd=4)


# p-value
p_val <- sum(T_stat>=T0)/B
p_val

#-- p-value 0. Ci sono nhood diversi. Troviamoli


means <- numeric(g)
count = numeric(length(levels(Levels.name1)))
means= numeric(length(levels(Levels.name1)))
i = 1
for (ch in levels(Levels.name1)){
  
  for(j in 1:length(rent_2013g[,1])){
    if(rent_2013g[j,1]==ch){
      means[i] = means[i] + rent_2013g[j,2]
      count[i] = count[i]+1
      
    }
  }
  i = i+1
}
measn = means/count

plot(c(1:length(treat1)),measn) #sembra uno solo sia fuori scala!
first <- which.max(measn) #12esimo è quello fuori scala
levels(Levels.name1)[first]                # Financial District/South Beach/SOMA è quello che sembra fuori scala verso l'alto.
rent_2013g <- rent_2013g[which(rent_2013g[,1]!="Financial District/South Beach/SOMA"),]


#-- Rifaccio ANOVA uguale a prima, ma ora ho due nhood in meno
Levels.name1 <- factor(rent_2013g$nhood)
treat1   <- levels(Levels.name1)  
Levels.name <- factor(rent_2013g$nhood, labels=c(1:length(treat1)))
treat   <- levels(Levels.name)      # levels of the treatment

g       <- length(treat)    # 29 nhoods  
B = 1000
seed = 26111992


i <- numeric(g)
nn <- numeric(g)
for (j in 1:g){
  i <- which(Levels.name==j)
  nn[j] = length(i)
  
}
n  <- sum(nn) #1127

plot(Levels.name, rent_2013g$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood nell anno 2011
fit <- aov(rent_2013g$price_mq ~ Levels.name)
summary(fit)


T0 <- summary(fit)[[1]][1,4] #F-value of Levels
T0

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  covariate_perm <- rent_2013g$price_mq[permutation]
  fit_perm <- aov(covariate_perm ~ Levels.name)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,30))
abline(v=T0,col=3,lwd=4)


# p-value
p_val <- sum(T_stat>=T0)/B
p_val
}

#---- sempre 0. non riesco a risolvere nulla. Passo al 2014


#-- ANOVA on year = 2014
{
  Levels.name1 <- factor(rent_2014$nhood)
  treat1   <- levels(Levels.name1)      # levels of the treatment
  Levels.name <- factor(rent_2014$nhood, labels=c(1:length(unique(treat1))))
  treat   <- levels(Levels.name)      # levels of the treatment
  g       <- length(treat)    # 42 nhoods  
  B = 1000
  seed = 26111992
  
  #Let's count the number of observations in each nhood
  i <- numeric(g)
  nn <- numeric(g)
  for (j in 1:g){
    i <- which(Levels.name==j)
    nn[j] = length(i)
    
  }
  
  #I will remove nhood with less than 20 observations.(I tried with 5 and 10, not enough to remove differences)
  remove_index <- which(nn<20)
  remove_nhood <- treat1[remove_index]
  rent_2014 <- rent_2014[which(rent_2014[,3]!=remove_nhood[1] & rent_2014[,3]!=remove_nhood[2] & rent_2014[,3]!=remove_nhood[3] & rent_2014[,3]!=remove_nhood[4] & rent_2014[,3]!=remove_nhood[5] & rent_2014[,3]!=remove_nhood[6] & rent_2014[,3]!=remove_nhood[7]  & rent_2014[,3]!=remove_nhood[8]  & rent_2014[,3]!=remove_nhood[9] &  rent_2014[,3]!=remove_nhood[10] & rent_2014[,3]!=remove_nhood[11] &  rent_2014[,3]!=remove_nhood[12] &  rent_2014[,3]!=remove_nhood[13] & rent_2014[,3]!=remove_nhood[14] & rent_2014[,3]!=remove_nhood[15] & rent_2014[,3]!=remove_nhood[16] & rent_2014[,3]!=remove_nhood[17] & rent_2014[,3]!=remove_nhood[18] & rent_2014[,3]!=remove_nhood[19] & rent_2014[,3]!=remove_nhood[20]  & rent_2014[,3]!=remove_nhood[21]  & rent_2014[,3]!=remove_nhood[22] &  rent_2014[,3]!=remove_nhood[23] & rent_2014[,3]!=remove_nhood[24] & rent_2014[,3]!=remove_nhood[25] &  rent_2014[,3]!=remove_nhood[26] & rent_2014[,3]!=remove_nhood[27] & rent_2014[,3]!=remove_nhood[28] &  rent_2014[,3]!=remove_nhood[29] & rent_2014[,3]!=remove_nhood[30]),]
  rent_2014g = rent_2014[,c(3,11)]
  
  #Now let's proceed doing an ANOVA
  Levels.name1 <- factor(rent_2014g$nhood)
  treat1   <- levels(Levels.name1)      # levels of the treatment
  Levels.name <- factor(rent_2014g$nhood, labels=c(1:length(treat1)))
  treat   <- levels(Levels.name)      # levels of the treatment
  treat1   <- levels(Levels.name1)      # levels of the treatment
  g       <- length(treat)    # 41 nhoods  
  i <- numeric(g)
  nn <- numeric(g)
  for (j in 1:g){
    i <- which(Levels.name==j)
    nn[j] = length(i)
    
  }
  n  <- sum(nn) #2690
  
  plot(Levels.name, rent_2014g$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood nell anno 2013
  fit <- aov(rent_2014g$price_mq ~ Levels.name)
  summary(fit)
  
  
  T0 <- summary(fit)[[1]][1,4] #F-value of Levels
  T0
  
  T_stat <- numeric(B) 
  
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:n)
    covariate_perm <- rent_2014g$price_mq[permutation]
    fit_perm <- aov(covariate_perm ~ Levels.name)
    
    # Test statistic:
    T_stat[perm] <- summary(fit_perm)[[1]][1,4]
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat),xlim=c(-1,30))
  abline(v=T0,col=3,lwd=4)
  
  
  # p-value
  p_val <- sum(T_stat>=T0)/B
  p_val
  
  #-- p-value 0. Visualizziamo le medie
  
  
  means <- numeric(g)
  count = numeric(length(levels(Levels.name1)))
  means= numeric(length(levels(Levels.name1)))
  i = 1
  for (ch in levels(Levels.name1)){
    
    for(j in 1:length(rent_2014g[,1])){
      if(rent_2014g[j,1]==ch){
        means[i] = means[i] + rent_2014g[j,2]
        count[i] = count[i]+1
        
      }
    }
    i = i+1
  }
  measn = means/count
  
  plot(c(1:length(treat1)),measn)
}

#---- non riesco a risolvere nulla. Passo al 2015


#-- ANOVA on year = 2015

{
  Levels.name1 <- factor(rent_2015$nhood)
  treat1   <- levels(Levels.name1)      # levels of the treatment
  Levels.name <- factor(rent_2015$nhood, labels=c(1:length(unique(treat1))))
  treat   <- levels(Levels.name)      # levels of the treatment
  g       <- length(treat)    # 42 nhoods  
  B = 1000
  seed = 26111992
  
  #Let's count the number of observations in each nhood
  i <- numeric(g)
  nn <- numeric(g)
  for (j in 1:g){
    i <- which(Levels.name==j)
    nn[j] = length(i)
    
  }
  
  #I will remove nhood with less than 20 observations.(I tried with 5 and 10, not enough to remove differences)
  remove_index <- which(nn<10)
  remove_nhood <- treat1[remove_index]
  rent_2015 <- rent_2015[which(rent_2015[,3]!=remove_nhood[1] & rent_2015[,3]!=remove_nhood[2] & rent_2015[,3]!=remove_nhood[3] & rent_2015[,3]!=remove_nhood[4] & rent_2015[,3]!=remove_nhood[5] & rent_2015[,3]!=remove_nhood[6] & rent_2015[,3]!=remove_nhood[7]  & rent_2015[,3]!=remove_nhood[8]  & rent_2015[,3]!=remove_nhood[9] &  rent_2015[,3]!=remove_nhood[10] & rent_2015[,3]!=remove_nhood[11]),]
  rent_2015g = rent_2015[,c(3,11)]
  
  #Now let's proceed doing an ANOVA
  Levels.name1 <- factor(rent_2015g$nhood)
  treat1   <- levels(Levels.name1)      # levels of the treatment
  Levels.name <- factor(rent_2015g$nhood, labels=c(1:length(treat1)))
  treat   <- levels(Levels.name)      # levels of the treatment
  treat1   <- levels(Levels.name1)      # levels of the treatment
  g       <- length(treat)    # 31 nhoods  
  i <- numeric(g)
  nn <- numeric(g)
  for (j in 1:g){
    i <- which(Levels.name==j)
    nn[j] = length(i)
    
  }
  n  <- sum(nn) #1518
  
  plot(Levels.name, rent_2015g$price_mq, xlab='treat',col=rainbow(g),main='Original Data') # boxplot dei vari nhood nell anno 2013
  fit <- aov(rent_2015g$price_mq ~ Levels.name)
  summary(fit)
  
  
  T0 <- summary(fit)[[1]][1,4] #F-value of Levels
  T0
  
  T_stat <- numeric(B) 
  
  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:n)
    covariate_perm <- rent_2015g$price_mq[permutation]
    fit_perm <- aov(covariate_perm ~ Levels.name)
    
    # Test statistic:
    T_stat[perm] <- summary(fit_perm)[[1]][1,4]
  }
  
  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
  abline(v=T0,col=3,lwd=2)
  
  plot(ecdf(T_stat),xlim=c(-1,30))
  abline(v=T0,col=3,lwd=4)
  
  
  # p-value
  p_val <- sum(T_stat>=T0)/B
  p_val
  
  #-- p-value 0. Visualizziamo le medie
  
  
  means <- numeric(g)
  count = numeric(length(levels(Levels.name1)))
  means= numeric(length(levels(Levels.name1)))
  i = 1
  for (ch in levels(Levels.name1)){
    
    for(j in 1:length(rent_2015g[,1])){
      if(rent_2015g[j,1]==ch){
        means[i] = means[i] + rent_2015g[j,2]
        count[i] = count[i]+1
        
      }
    }
    i = i+1
  }
  measn = means/count
  
  plot(c(1:length(treat1)),measn)
}

#---- non riesco a risolvere nulla.

#---- Le ANOVA anno-per-anno concludono solamente che i vari nhood siano diversi








#-- TESTIAMO ORA CON UNA ANOVA TWO-WAY. I GRUPPI ORA SONO DEFINITI DA nhood & year
rent <- rent_clean[,c(3,9,11)]
rent$year.nhood <- NA
for (i in 1: length(rent[,1])){
  rent$year.nhood[i] <-paste(rent$year[i], rent$nhood[i],  sep=" ") 
}
rent <- rent[order(rent$year.nhood),]
g <- length(levels(factor(rent$nhood)))
b <- length(levels(factor(rent$year)))
year_nhood <- factor(rent$year.nhood)
new <-factor(rent$year.nhood )
nnew <- rent$price_mq

means <-with(rent, tapply(rent$price_mq, year_nhood, mean))
means

plot(new, nnew,  ylim=c(10,100))
points(unique(year_nhood), means, col="green", pch=19)
B = 1000
seed = 26111992
set.seed(seed)

M           <- mean(rent$price_mq)
Mnhood    <- tapply(rent$price_mq,rent$nhood, mean)
Myear     <- tapply(rent$price_mq,rent$year, mean) 

### X.ijk = mu + alpha.i + beta.j + gamma.ij + eps.ijk;     eps.ijk~N(0,sigma^2)
#I need to test alpha.i=0 ,beta.j=0 ,gamma.ij =0 one at a time    |  i = 1,...,g , j = 1,...,b


# H0:gamma.ij = 0   vs   H1: gamma.ij != 0 (i.e. can I remove the interaction?)


summary.aov(aov(rent$price_mq ~ rent$nhood + rent$year + rent$nhood:rent$year))
T0_nhood.year <- summary.aov(aov(rent$price_mq ~rent$nhood + rent$year + rent$nhood:rent$year))[[1]][3,4] #I need to extract the F-value associated with rent$nhood:rent$year
T0_nhood.year

aov.H0nhood.year <- aov(rent$price_mq ~ rent$nhood + rent$year)
aov.H0nhood.year
residuals.H0nhood.year <- aov.H0nhood.year$residuals
n = length(rent[,1]) 


T_nhood.year <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0nhood.year <- residuals.H0nhood.year[permutation]
  price_mq.perm.H0nhood.year <- aov.H0nhood.year$fitted + residuals.H0nhood.year
  T_nhood.year[perm] <- summary.aov(aov(price_mq.perm.H0nhood.year ~  rent$nhood + rent$year + rent$nhood:rent$year))[[1]][3,4]
}

pval_nhood.year = sum(T_nhood.year >= T0_nhood.year)/B

#Low p-value, I keep the interaction and I go on testing the other 2 one at a time.




# H0:alpha.i = 0   vs   H1: alpha.i != 0 (i.e. can I remove the nhood effect?)

T0_nhood <- summary.aov(aov(rent$price_mq ~rent$nhood + rent$year + rent$nhood:rent$year))[[1]][1,4] #I need to extract the F-value associated with rent$nhood. I put here the complete model
aov.H0nhood <- aov(rent$price_mq ~ rent$year + rent$nhood:rent$year)
residuals.H0nhood <- aov.H0nhood$residuals

T_nhood <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  price_mq.perm.H0nhood <- aov.H0nhood$fitted + residuals.H0nhood[permutation]
  T_nhood[perm] <- summary.aov(aov( price_mq.perm.H0nhood ~ rent$nhood + rent$year+ rent$nhood:rent$year))[[1]][1,4]  #I put here the complete model
  
}

pval_nhood = sum(T_nhood >= T0_nhood)/B

#high p-value. I remove rent$nhood. It means that the average rent is affected by changing nhood.



# H0:beta.j = 0   vs   H1: beta.j != 0

T0_year <- summary.aov(aov(rent$price_mq ~  rent$nhood:rent$year + rent$year))[[1]][2,4] #I need to extract the F-value associated with rent$year. #I put here the complete model
aov.H0year <- aov(rent$price_mq ~ rent$nhood:rent$year)
residuals.H0year <- aov.H0year$residuals

T_year <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  price_mq.perm.H0year <- aov.H0year$fitted + residuals.H0year[permutation]
  T_year[perm] <- summary.aov(aov(price_mq.perm.H0year ~  rent$year + rent$nhood:rent$year))[[1]][2,4] #I put here the complete model
}

pval_year = sum(T_year >= T0_year)/B


#conclusioni: - l'effetto che ha "year" sui "nhood" è diverso in base a qual è il "nhood"
#              -l'effetto che ha "nhood" sui "year" è diverso in base a qual è lo "year"



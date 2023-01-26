# R project
# Setting up our data base #

library(readr)
BTC<- read_csv("C:/Users/flore/OneDrive/Bureau/MASTER 1/Semestre 1/Logiciel R/Projet/Script R/BDD/BTC_USDBitfinexHistorical Data (1).csv")
XRP<- read_csv("C:/Users/flore/OneDrive/Bureau/MASTER 1/Semestre 1/Logiciel R/Projet/BDD/Créationet traitementBDD/XRP_USD OKEx Historical Data (2).csv")
LTC<- read_csv("C:/Users/flore/OneDrive/Bureau/MASTER 1/Semestre 1/Logiciel R/Projet/BDD/Créationet traitementBDD/LTC_USD ZB.COM Historical Data (3).csv")
EUR<- read_csv("C:/Users/flore/OneDrive/Bureau/MASTER 1/Semestre 1/Logiciel R/Projet/BDD/Créationet traitementBDD/EUR_USD Historical Data (3).csv")
GBP<- read_csv("C:/Users/flore/OneDrive/Bureau/MASTER 1/Semestre 1/Logiciel R/Projet/BDD/Créationet traitementBDD/GBP_USD Historical Data (3).csv")
JPY<- read_csv("C:/Users/flore/OneDrive/Bureau/MASTER 1/Semestre 1/Logiciel R/Projet/BDD/Créationet traitementBDD/JPY_USD Historical Data (2).csv")

dim(EUR)
dim(GBP)
dim(JPY)
dim(LTC)
dim(XRP)

# selecting date and information

## BTC
BTC2<-BTC[-(158:264),]
View(BTC2)
BTC3<-BTC2[,-(3:7)]
View(BTC3)
BTC4<-BTC3[,(-1)]
Bitcoin<-BTC4
View(Bitcoin)

## GBP
GBP2<-GBP[-(158:264),]
GBP3<-GBP2[,-(1)]
GBP4<-GBP3[,-(2:5)]
Livre<-GBP4
View(Livre)

## EUR
EUR2<-EUR[-(158:316),]
EUR3<-EUR2[,-(1)]
EUR4<-EUR3[,-(2:5)]
Euro<-EUR4
View(Euro)

## Yen
JPY2<-JPY[-(158:316),]
JPY3<-JPY2[,-(1)]
JPY4<-JPY3[,-(2:5)]
Yen<-JPY4
View(Yen)

## Ripple
XRP2<-XRP[,-(1)]
XRP3<-XRP2[,-(2:6)]
XRP4<-XRP3[1:157,]
Ripple<-XRP4
View(Ripple)
## Litecoin
LTC2<-LTC[,-(1:1)]
LTC3<-LTC2[,-(2:6)]
Litecoin<-LTC3
View(Litecoin)

str(Bitcoin)
str(Litecoin)
str(Ripple)
str(Euro)
str(Livre)
str(Yen)
# We've got the same size

## Creation of the data base ##
BDD1=cbind(Bitcoin,Litecoin,Ripple,Euro,Livre,Yen)
colnames(BDD1) <- c("Bitcoin","Litecoin","Ripple","Euro","Livre","Yen")
plot(BDD1$Bitcoin, main = "prix du Bitcoin en $ entre 2018-2021", xlab = "temps", ylab = "prix du Bitcoin")
BDD<-BDD1[dim(BDD1)[1]:1,]
plot(BDD$Bitcoin, col="black", main = "prix du Bitcoin en $ entre 2018-2021", xlab = "temps", ylab = "prix du Bitcoin" )
getwd()
write.table(BDD,file="création et traitement BDD")

library("psych")
library("ggplot2")
library("questionr")
library("car")

## Univariate ##
summary(BDD$Bitcoin)
summary(BDD$Litecoin)
summary(BDD$Ripple)
summary(BDD$Euro)
summary(BDD$Livre)
summary(BDD$Yen)

plot(BDD$Bitcoin,type="o")
plot(BDD$Litecoin, type="o")
plot(BDD$Ripple, type="o")
plot(BDD$Euro, type="o")
plot(BDD$Livre, type="o")
plot(BDD$Yen, type="o")

plot(density(BDD$Bitcoin), main = "Densité cours du bitcoin")
plot(density(BDD$Litecoin), main = "Cours du litecoin")
plot(density(BDD$Ripple), main = "Cours du ripple")
plot(density(BDD$Euro), main = "Cours de l'euro")
plot(density(BDD$Livre), main = "Cours de la livre")
plot(density(BDD$Yen), main = "Cours du Yen")


# Let's make a synthetical histogram (plot with density )
# PS : loading time
install.packages('ggplot2')
ggplot(BDD, aes(x=BDD$Bitcoin)) + geom_histogram()
ggplot(BDD, aes(x=BDD$Bitcoin)) +
  geom_histogram(binwidth=1)
p<-ggplot(df, aes(x=weight)) +
  geom_histogram(color="black", fill="white")
p
p+ geom_vline(aes(xintercept=mean(BDD$Bitcoin)), color="blue", linetype="dashed", size=1)
ggplot(BDD, aes(x=BDD$Bitcoin)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
## For Yen
ggplot(BDD, aes(x=BDD$Litecoin)) + geom_histogram()
ggplot(BDD, aes(x=BDD$Litecoin)) +
  geom_histogram(binwidth=1)
p<-ggplot(df, aes(x=weight)) +
  geom_histogram(color="black", fill="white")
p
p+ geom_vline(aes(xintercept=mean(BDD$Litecoin)), color="blue", linetype="dashed", size=1)
ggplot(BDD, aes(x=BDD$Litecoin)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

# Let's confirmed our graph diagnostic (asymmetric, leptokurtic) with test
install.packages("moments")
skewness(BDD$Bitcoin) # Sk = 3,13 > 1,96 => distribution is asymetric
kurtosis(BDD$Bitcoin) # Kr = 15,33 > 1,96 => distribution is leptokurtic (Kr>3)
jarque.test(BDD$Bitcoin)# residuals are not normal distributed
# IS there some extreme values ?
boxplot(BDD$Bitcoin, col = c("yellow"),main = "Bitcoin", ylab = "Quantiles")
boxplot(BDD$Ripple, col = c("yellow"),main = "Ripple", ylab = "Quantiles")
boxplot(BDD$Litecoin, col = c("yellow"),main = "Litecoin", ylab = "Quantiles")
boxplot(BDD$Euro, col = c("orange"),main = "Euro", ylab = "Quantiles")
boxplot(BDD$Livre, col = c("orange"),main = "Livre", ylab = "Quantiles")
boxplot(BDD$Yen, col = c("orange"),main = "Yen", ylab = "Quantiles")

## Bivariate ##
# Scatterplot in order to detect extreme values
scatterplot(BDD$Bitcoin~BDD$Ripple)
scatterplot(BDD$Bitcoin~BDD$Euro)
scatterplot(BDD$Bitcoin~BDD$Livre)
scatterplot(BDD$Bitcoin~BDD$Yen)
# Extreme values don't skew our regression
# >Extreme value may coming from a speculative bubble
scatterplot(BDD$Bitcoin~BDD$Litecoin,xlab="Bitcoin", ylab="Litecoin",main=" Enhanced Scatter Plot")
abline(lm(BDD$Bitcoin~BDD$Litecoin), col="red") # regression line (y~x)
lines(lowess(BDD$Bitcoin,BDD$Litecoin), col="blue") # lowess line (x,y)
# Correlation between variables
install.packages("corrplot")
library(corrplot)
corrplot(cor(BDD))

install.packages("tseries")
install.packages("MultipleBubbles")
# see details about the package here : https://cran.r-project.org/web/packages/MultipleBubbles/MultipleBubbles.pdf
install.packages("moments")
install.packages("exuber")
# see details about the package here : https://cran.r-project.org/web/packages/exuber/exuber.pdf
skewness(BDD$Bitcoin) # Sk = 3,13 > 1,96 => asymetric distribution
kurtosis(BDD$Bitcoin) # Kr = 15,33 > 1,96 => leptokurtic distribution (Kr>3)
jarque.test(BDD$Bitcoin) # p-value near 0%, reject H0 => residuals are non-normal
# Asymetric, leptokurtic and non-normal residuals = indicator of a explosive process

# We're gonna test the presence of such a process :
library(MultipleBubbles)
# We must have a "time-series" data
ADFBtc = ts(data = BDD[,1])
plot(ADFBtc)
# We use an Supremum Augmented Dickey Fueller test :
sadf_gsadf(ADFBtc, adflag = 1,mflag = 1, IC=1, parallel = FALSE)
# Results :
# SADF = 5.567739 > 1,39 (theorical) => reject H0
# SADF = 7.840847 > 1,39 (theorical) => reject H0
# There is two episodes of speculative bubbles. # But when ?
# We see that on the following graph :
graphBulle <- radf(ADFBtc)
autoplot(graphBulle)
autoplot(graphBulle,xaxp=c(0,154,1))
# We note that there is two speculative bubbles :
# First around 70th and 80th weeks
# Second around 150th and 157th weeks


# Naive regressions
RegCrypto<-lm(BDD$Bitcoin~BDD$Litecoin+BDD$Ripple)
summary(RegCrypto)
RegMoClassique<-lm(BDD$Bitcoin~BDD$Euro+BDD$Livre+BDD$Yen)
summary(RegMoClassique)
RegTot<-lm(BDD$Bitcoin~BDD$Litecoin+BDD$Ripple+BDD$Euro+BDD$Livre+BDD$Yen)
summary(RegTot)
raintest(RegCrypto)# nonlinear
raintest(RegMoClassique)# nonlinear
raintest(RegTot) # nonlinear
vif(RegTot)
# no value > 10 => no multicollinearity
# SBS stands for "sans bulle spéculative" View(BDD)
BDD_SBS<-BDD[-(150:157),]
View(BDD_SBS)
plot(BDD_SBS$Bitcoin, type = "l") # we cut the last speculative bubble part according to SADF test
RegTotSBS<-lm(BDD_SBS$Bitcoin~BDD_SBS$Litecoin+BDD_SBS$Ripple+BDD_SBS$Euro+BDD_SBS$Livre+BDD_SBS$Yen)
summary(RegTotSBS)

# But ... # Is this model satisfactory ?
dwtest(RegTotSBS) # reject H0 : residuals are autocorrelated
bptest(RegTotSBS) # reject H0 : residuals are heteroscedastic
raintest(RegTotSBS) # reject H0 : model non-linear
shapiro.test(RegTotSBS$residuals)# reject H0: residuals are not normal distributed
vif(RegTotSBS) # no value > 10 => no multicollinearity
# => This model is not satisfacotory
# We log-linearise the regression
LBitcoin<-log(Bitcoin)
LLitecoin<-log(Litecoin)
LRipple<-log(Ripple)
LEuro<-log(Euro)
LLivre<-log(Livre)
LYen<-log(Yen)
LBDD1=cbind(LBitcoin,LLitecoin,LRipple,LEuro,LLivre,LYen)
colnames(LBDD1) <- c("LBitcoin","LLitecoin","LRipple","LEuro","LLivre","LYen")
LBDD2<-LBDD1[dim(LBDD1)[1]:1,]
LBDD3<-LBDD2[-(150:157),]
LBDD<-LBDD3
View(LBDD)

wilcox.test(LBDD$LBitcoin, LBDDS$LBitcoin, alternative = "two.sided") # H0 : same distribution
# accept H0
# Still, let's go back to the regression :
RegLCrypto<-lm(LBDD$LBitcoin~LBDD$LLitecoin+LBDD$LRipple)
summary(RegLCrypto)
RegLMoClassique<-lm(LBDD$LBitcoin~LBDD$LEuro+LBDD$LLivre+LBDD$LYen)
summary(RegLMoClassique)

# MODELE 1 :
RegLTot<-lm(LBDD$LBitcoin~LBDD$LLitecoin+LBDD$LRipple+LBDD$LEuro+LBDD$LLivre+LBDD$LYen)
summary(RegLTot)
# testing our last model :
raintest(RegLTot) # p-value = 0.1124 => linear model
# test on residuals
dwtest(RegLTot) # reject H0 : residuals are correlated
bptest(RegTotSBS) # reject H0 : residuals are heteroscedastic
shapiro.test(RegLTot$residuals) # BUT residuals are normal at 1%
vif(RegLTot) # no value > 10 => no multicollinearity
install.packages("forecast")
checkresiduals(RegLTot)
checkresiduals(RegLMoClassique)
checkresiduals(RegLCrypto)
# More detail about residuals :
install.packages("qqplotr")
qqline(RegLTot$residuals)
hist(residuals(RegLTot))
qqnorm(residuals(RegLTot))
qqline(residuals(RegLTot))
plot(RegLTot, which =1)

# What if we cut sooner the bubble :
LBDD31<-LBDD2[-(140:157),]
LBDDS<-LBDD31
View(LBDDS) # S for sooner
RegLTotS<-lm(LBDDS$LBitcoin~LBDDS$LLitecoin+LBDDS$LRipple+LBDDS$LEuro+LBDDS$LLivre+LBDDS$LYen)
summary(RegLTotS)
# Is this model good ?
checkresiduals(RegLTotS)
raintest(RegLTotS) # p-value = 0.8079 => linear model
dwtest(RegLTotS) # residuals are correlated
bptest(RegLTotS)# residuals are heteroscedastic
shapiro.test(RegLTotS$residuals)# residuals are not normal distributed
vif(RegLTotS) # no value > 10 => no multicollinearity

# Comparaison
AIC(RegLTot) # -22.61434
AIC(RegLTotS) # -34.13266

#Forecasting :
prev_MCO<-predict(RegLTotS)
plot(prev_MCO, type = "l")
plot(LBDDS$LBitcoin, type = "l")
# The model doesn't really understand the price

# Let's check the causality sens.
install.packages("MLmetrics")
MAPE(prev_MCO, LBDDS$LBitcoin)
MAE(LBDDS$LBitcoin, prev_MCO)

## Causality test ##
## Test unit-root
kpss.test(BDD$Bitcoin) # reject h0 (process is not stationary)
pp.test(BDD$Bitcoin)# accept H0 (process is not stationary)
DBitcoin<-diff(BDD_SBS$Bitcoin)
plot(DBitcoin)
abline(h=0)
kpss.test(BDD_SBS$Litecoin) # p-value = 0.01056
kpss.test(BDD_SBS$Ripple) # p-value = 0.01
kpss.test(BDD_SBS$Euro) # p-value = 0.01
kpss.test(BDD_SBS$Livre) # p-value = 0.01
kpss.test(BDD_SBS$Yen) # p-value = 0.01
# => Not stationary
# We differentiate
Dlitecoin<-diff(BDD_SBS$Litecoin)
DRipple<-diff(BDD_SBS$Ripple)
DEuro<-diff(BDD_SBS)
DLivre<-diff(BDD_SBS$Livre)
DYen<-diff(BDD_SBS$Yen)

# Our series is now differentiate, stationary without any trend or intercept. BDD_D1=cbind(DBitcoin,Dlitecoin, DRipple,DEuro,DLivre, DYen)
colnames(BDD_D1) <- c("DBitcoin","DLitecoin", "DRipple", "DEuro", "DLivre", "DYen")
View(BDD_D1)
BDD_D<-BDD_D1[dim(BDD_D1)[1]:1,]
View(BDD_D)
BDDDiff_SBS<-as.data.frame(BDD_D)
View(BDDDiff_SBS)

# Granger causality test :
install.packages("vars")
VAR_DL<- VAR(BDDDiff_SBS, p = 1, type = c("const", "trend", "both", "none"), season = NULL, exogen = NULL, lag.max = NULL,
             ic = c("AIC", "HQ", "SC", "FPE"))
causality(VAR_DL, cause = "DBitcoin")

install.packages("marima")
install.packages("tseries")
acf2y(DBitcoin,lag.max=36)
# This is the behavioral of a stationary series
# No autocorrelation (be careful with lag 20)
armaselect(DBitcoin,max.p=20,max.q=20,nbmod=10)
# We're going to test :
# ARIMA(0,1,1)
# ARIMA(1,1,1)
# ARIMA(0,1,2)
# ARIMA(2,1,1)
# ARIMA(1,1,2)
ARIMA011 <- arma(DBitcoin , order = c(0, 1)) # AIC = AIC = 2347.44
summary(ARIMA011) # No sign. ARIMA111 <- arma(DBitcoin , order = c(1, 0)) # AIC = 2347.73
summary(ARIMA111) # No sign. ARIMA012 <- arma(DBitcoin , order = c(0, 2)) # AIC = 2349.34
summary(ARIMA012) # No sign. ARIMA211<- arma(DBitcoin , order = c(2, 1)) # AIC = 2342.19
summary(ARIMA211) # Yes, model significant
ARIMA112<- arma(DBitcoin , order = c(1, 2)) # AIC = 2345.12
summary(ARIMA112) # Yes, model significant
# => We choose ARIMA (2,1,1)
# Validity
install.packages("forecast")
install.packages("FitAR")
checkresiduals(ARIMA211)
acf(fitARIMA$residuals)
library(FitAR)
boxresult<-LjungBoxTest(fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
abline(h=0.5)
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)
# Forecast
fitARIMA<-arima(DBitcoin, order=c(2,1,1), method="ML")
fitARIMA
predict(fitARIMA,n.ahead = 10)
futurVal <-forecast(fitARIMA,h=10, level=c(99.5))
plot(futurVal)

AIC(RegLTotS) #-34.13266
summary(ARIMA211) # AIC = 2342.19
# Our model is (RegLTotS)
# May be our model could be better
# Indeed, our data base still has extreme value
install.packages("qqplotr")
plot(RegLTotS, which =1)
# We're gonna cut the first speculative bubble. BDD_2<-LBDD[-(69:77),]
View(BDD_2)
RegFinale<-lm(BDD_2$LBitcoin~BDD_2$LLitecoin+BDD_2$LRipple+BDD_2$LEuro+BDD_2$LLivre+BDD_2$LYen)
summary(RegFinale)
plot(RegFinale, which =1)
AIC(RegFinale) # -15.11716
# Residuals checking
install.packages("qqplotr")
qqline(RegFinale$residuals)
hist(residuals(RegFinale))
qqnorm(residuals(RegFinale))
qqline(residuals(RegFinale))
# validity
raintest(RegFinale)# p-value = 0.2382 => linear model (H0)
# But
checkresiduals(RegFinale) # residuals are correlated
dwtest(RegFinale) # autocorrelation
## We choose the model : summary(RegLTotS)

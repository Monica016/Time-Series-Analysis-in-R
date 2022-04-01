setwd("C:/Users/yhwen/Documents/Fall 2020")
data = read.delim('mortgage.txt',sep = " ",header = TRUE)
data1=data[,c('morg')]
data2 = data[,c('ffr')]
datats = ts(data1,start = c(1971,4),end = c(2011,12),frequency = 12)
datats2 = ts(data2,start = c(1971,4),end = c(2011,12),frequency = 12)
tworates = cbind(Morg = datats,
                 Funds = datats2)
plot(tworates,main = 'Mortgage Rate & Federal Funds Rate')

hist(datats,main = 'Histogram of Mortgage rate')
hist(datats2,main = 'Histogram of Federal Funds rate')
datats_p1 = datats[1:(length(datats)/2)]
datats_p2 = datats[(length(datats)/2+1):length(datats)]
var_p1 = var(datats_p1)
var_p2 = var(datats_p2)
datats2_p1 = datats2[1:(length(datats2)/2)]
datats2_p2 = datats2[(length(datats2)/2+1):length(datats2)]
var2_p1 = var(datats2_p1)
var2_p2 = var(datats2_p2)

acf(datats,main = 'ACF of mortgage rate')
morgr = cbind(
              logx = log(datats),
              diff = diff(log(datats)))
plot(morgr,main = 'Log transformation & Differencing')
datad = diff(log(datats))
hist(datad,main = "Histogram of differenced series")
qqnorm(datad, main = 'Normal Q-Q plot of differenced series')
qqline(datad)
acf(datad,main='ACF')
pacf(datad,main = 'PACF')

M1 = astsa::sarima(datad, 0,1,1)
M2 = astsa::sarima(datad,1,1,1)

astsa::lag2.plot(datad2,datad,8)
datad2 = diff(log(datats2))
fish = ts.intersect(datad,ft1 = lag(datad2,-1),dframe = TRUE)
summary(fit1<-lm(datad~ft1,data = fish, na.action=NULL))
ft1 = lag(datad2,-1)
astsa::sarima(datad,1,1,1,xreg=cbind(ft1))



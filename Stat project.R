#Import the data and check a type of data
vehicle <- read.csv("D:/Stat Modeling/vehicle.csv",header = T)
class(vehicle)
veh.ts <- ts(vehicle[,2],start = 1970,frequency = 12)
plot(veh.ts)
adf.test(veh.ts)
#Check ACF and PACF of this data
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(veh.ts, lag.max=50, type = "correlation", main="ACF for the \n vehicle miles traveled")
acf(veh.ts, lag.max=50, type = "partial", main="PACF for the \n vehile miles traveled")

#1st diff for non-stationary series
wj.veh <- diff(diff(veh.ts,lag=1),lag=12)
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf (wj.veh,lag.max=100,type = "correlation", main="ACF for w(t)")
acf (wj.veh,lag.max=100,type = "partial", main="PACF for w(t)")
plot(wj.veh)

#Fit a seasonal ARIMA(2,1,0)x(2,1,2)12 model to the data
veh.fit.sar1 <-arima(veh.ts,order=c(2,1,0),
                     seasonal=list(order=c(2,1,2),period=12),)
veh.fit.sar1

#Fit a seasonal ARIMA(1,1,1)x(2,1,2)12 model to the data
veh.fit.sar2 <-arima(veh.ts,order=c(1,1,1),
                    seasonal=list(order=c(2,1,2),period=12),)
veh.fit.sar2

#Fit a seasonal ARIMA(2,1,1)x(2,1,2)12 model to the data
veh.fit.sar3 <-arima(veh.ts,order=c(2,1,1),
                     seasonal=list(order=c(2,1,2),period=12),)
veh.fit.sar3

#Fit a seasonal ARIMA(2,1,0)x(1,1,1)12 model to the data
veh.fit.sar4 <-arima(veh.ts,order=c(2,1,0),
                     seasonal=list(order=c(1,1,1),period=12),)
veh.fit.sar4

#Fit a seasonal ARIMA(1,1,1)x(1,1,1)12 model to the data
veh.fit.sar5 <-arima(veh.ts,order=c(1,1,1),
                     seasonal=list(order=c(1,1,1),period=12),)
veh.fit.sar5

#Fit a seasonal ARIMA(2,1,1)x(1,1,1)12 model to the data
veh.fit.sar6 <-arima(veh.ts,order=c(2,1,1),
                     seasonal=list(order=c(1,1,1),period=12),)
veh.fit.sar6

#Check the residual of this series
res.veh.sar<-as.vector(residuals(veh.fit.sar2))

#To obtain the fitted values we use the function fitted the forecast function
#install.packages('forecast')
library(forecast)
fit.veh.sar<-as.vector(fitted(veh.fit.sar2))
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf (res.veh.sar,lag.max=50,type = "correlation", main="ACF for the Residuals")
acf (res.veh.sar,lag.max=50,type = "partial", main="PACF for the Residuals")
tsdiag(veh.fit.sar2)

#Create the data visualization of the residual
par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(res.veh.sar,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(res.veh.sar,datax=TRUE)
plot(fit.veh.sar,res.veh.sar,pch=16,xlab='Fitted Value',ylab='Residual')
abline(h=0)
hist(res.veh.sar,col="gray",xlab='Residual',main='')
plot(res.veh.sar,type="l",xlab='Observation Order',ylab='Residual')
points(res.veh.sar,pch=16,cex=0.5)
abline(h=0)

#model = arimax(veh.ts,order=c(1,1,1),seasonal=list(order=c(2,1,2),period=24),xreg=regParams,xtransf=transferParams,transfer=list(c(1,1))
#pred = predict(model, newxreg=regParams)

library(forecast)
y.auto=auto.arima(veh.ts,max.p=2,max.d=1,max.q=2,max.P=2,max.D=1,max.Q=3,ic='aic',
                  stationary=FALSE,stepwise=FALSE,trace=TRUE,approximation=FALSE)

#Plot fitted values ??????????????????
plot(wj.veh,type="p",pch=16,cex=0.5,xlab='Date',ylab='Millions of Miles',xaxt='n')
axis(1,seq(1,600,50), wj.veh[seq(1,600,12),1])
lines(1:600,fit.veh.sar)
legend(2,17500,c("Vehicle Miles Traveled",
                 "Fitted"),pch=c(16,NA),lwd=c(NA,.5),cex=.55,col=c("black","black"))
#auto.arima ??????????????????
get.best.arima <- function(veh.ts, maxord = c(2,1,1,2,1,2))
{
  best.aic <- 1e8
  n <- length(veh.ts)
  for (p in 0:maxord[1]) for (d in 0:maxord[2]) for (q in 0:maxord[3])
    for (P in 0:maxord[4]) for (D in 0:maxord[5]) for (Q in 0:maxord[6])
    {
      fit <- arima(veh.ts,order = c(p,d,q),
                   seas = list(order=c(P,D,Q),
                               frequency(veh.ts)), method = "CSS")
      fit.aic <- -2*fit$loglik + 2*length(fit$coef)
      if(fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic,best.fit,best.model)
}
best.arima.veh <- get.best.arima(veh.ts,maxord=c(2,1,1,2,1,2))
best.arima.veh[[1]]
best.arima.veh[[2]]
best.arima.veh[[3]]

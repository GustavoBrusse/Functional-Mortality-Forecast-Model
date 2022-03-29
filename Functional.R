install.packages("demography")
library(demography)
library(forecast)

#Reading the data
fra<-hmd.mx("ESP", username = , password = , label = "Spain")

#Smoothing the mortality data
smus <- smooth.demogdata(fra)
plot(fra, years=2003, series="male", type="p", pch=1, col="black") 
lines(smus, years=2003, series="male", col="red")

# creating a FDM model with smoothing
model<-fdm(smus,series="male", years=1985:2016, order=3, interpolate=T)
plot(model$fitted)

# creating a FDM model without smoothing
model2<-fdm(fra,series="male", years=1985:2016, order=3, interpolate=T)
plot(model2$fitted)

#Forecasting the FDM model
forecast.model<-forecast(model, h=30)

#Forecasting the not smoothing FDM model
forecast.model2<-forecast(model2, h=30)

#Ploting the components of the model with smoothing
plot(forecast.model, plot.type="component")
plot(forecast.model)

#Ploting the forecasted life-expectancy at birth smoothing X not smoothing
e0.model<-e0(forecast.model, PI=T)
e0.model2<-e0(forecast.model2, PI=T)

par(mfrow=c(1,2), cex=1.2)
plot(e0.model2, main="Spain: Forecasted e0 without smoothed data", col="blue")
plot(e0.model, main="Spain: Forecasted e0 with smoothed data", col="red")



e0 <-life.expectancy(irl, series = 'male', years = irl$year,
                     type = c("period", "cohort"), age = min(irl$age),
                     max.age = min(100, max(irl$age)))
plot(e0, xlim=c(1950,2045), ylim=c(64,88)) 
lines(e0.model$mean)

# effect
diff<-e0.model$mean-e0.model2$mean
plot(diff)

########################## COMPARING ORDER 1 and 2+

### creating a FDM model with 1 order
model3<-fdm(smus,series="male", years=1985:2016, order=1, interpolate=T)
plot(model3$fitted)

#Forecasting the not smoothing FDM model
forecast.model3<-forecast(model3, h=30)
plot(forecast.model3, plot.type="component")
plot(forecast.model3)

#Ploting the forecasted life-expectancy at birth smoothing X not smoothing
e0.model<-e0(forecast.model2, PI=T)
e0.model3<-e0(forecast.model3, PI=T)

par(mfrow=c(1,2), cex=1.2)
plot(e0.model3, main="Spain: Forecasted e0 (Order=1)", col="blue")
plot(e0.model, main="Spain: Forecasted e0 (Order=3)", col="red")

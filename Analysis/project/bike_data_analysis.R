library(tswge)
library(tidyverse)
library(plotly)
library(xts)

#### check for stationarity 

bike_data = read_csv('/Users/pankaj/Downloads/bike data.csv')
head(bike_data)

bike_data$`Total Users`

plotts.sample.wge(bike_data$`Total Users`)

plotts.sample.wge(bike_data$`Total Users`[1:(length(bike_data$`Total Users`)/2)])

plotts.sample.wge(bike_data$`Total Users`[(length(bike_data$`Total Users`)/2+1):(length(bike_data$`Total Users`))])


########

# ARMA Model:
  

aic_ar= aic5.wge(bike_data$`Total Users`)
est_bike_arma = est.arma.wge(bike_data$`Total Users`, p=5, q=2)

plotts.sample.wge(est_bike_arma$res, arlimits = TRUE)

for_bike_arma= fore.arma.wge(bike_data$`Total Users`, phi=est_bike_arma$phi, theta = est_bike_arma$theta,  
                             n.ahead= 120 , limits = F , lastn=T )

plot(x= seq((length(bike_data$`Total Users`) -120 ), length(bike_data$`Total Users`)) , y = 
       bike_data$`Total Users`[(length(bike_data$`Total Users`) -120 ): length(bike_data$`Total Users`)])
lines(x= seq((length(bike_data$`Total Users`) -120 ), length(bike_data$`Total Users`)) , y = 
       bike_data$`Total Users`[(length(bike_data$`Total Users`) -120 ): length(bike_data$`Total Users`)])

lines(x= seq((length(bike_data$`Total Users`) -120+1 ), length(bike_data$`Total Users`)) , y = for_bike_arma$f,
      type= "b")

ASE1 = mean((for_bike_arma$f - bike_data$`Total Users`[(length(bike_data$`Total Users`) -120+1 ): length(bike_data$`Total Users`)])^2)



### ARIMA model 


bike_tr1 = artrans.wge(bike_data$`Total Users`, phi = 1)

aic5.wge(bike_tr1)


est_bike_arima = est.arma.wge(bike_tr1, p=5, q=2)

plotts.sample.wge(est_bike_arima$res, arlimits = TRUE)

for_bike_arima= fore.aruma.wge(bike_data$`Total Users`, phi=est_bike_arima$phi, theta = est_bike_arima$theta, d=1,  
                             n.ahead= 120 , limits = F , lastn=T )

plot(x= seq((length(bike_data$`Total Users`) -120 ), length(bike_data$`Total Users`)) , y = 
       bike_data$`Total Users`[(length(bike_data$`Total Users`) -120 ): length(bike_data$`Total Users`)])
lines(x= seq((length(bike_data$`Total Users`) -120 ), length(bike_data$`Total Users`)) , y = 
        bike_data$`Total Users`[(length(bike_data$`Total Users`) -120 ): length(bike_data$`Total Users`)])

lines(x= seq((length(bike_data$`Total Users`) -120+1 ), length(bike_data$`Total Users`)) , y = for_bike_arima$f,
      type= "b")

ASE2 = mean((for_bike_arima$f - bike_data$`Total Users`[(length(bike_data$`Total Users`) -120+1 ): length(bike_data$`Total Users`)])^2)
ASE2


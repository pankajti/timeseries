
library(tswge)
library(tidyverse)
sales_df = read_csv( '/Users/pankaj/dev/git/smu/timeseries/data/businesssales.csv')

plot.ts(sales_df$sales)
ksfit = lm(sales~ad_tv+ad_online+discount, sales_df)

ksfit_aic = aic.wge(ksfit$residuals, p = 0:8, q=0:0)
ksfit_aic
fit = arima(sales_df$sales, order = c(7,0,0), xreg = sales_df[,3:5])
fit

###### include time
t= 1:100

kstfit = lm(sales~t+ad_tv+ad_online+discount, sales_df)
kstfit

kstfit_aic = aic.wge(kstfit$residuals, p = 0:8, q=0:0)
kstfit_aic
fit = arima(sales_df$sales, order = c(6,0,0), xreg = sales_df[,3:5])
fit


####### lagged var

ad_tv1 = dplyr::lag(sales_df$ad_tv,1)
ad_online1 = dplyr::lag(sales_df$ad_online,1)

sales_df$ad_tv1=ad_tv1
sales_df$ad_online1=ad_online1

ksfit_l = lm(sales~ad_tv1+ad_online1+discount, sales_df)
ksfit_l_aic = aic.wge(ksfit_l$residuals, p = 0:8, q=0:0)
ksfit_l_aic
fit = arima(sales_df$sales, order = c(7,0,0), xreg = cbind(ad_tv1, ad_online1, sales_df$discount))
fit 


####### lagged var with time

 

ksfit_lt = lm(sales~t+ad_tv1+ad_online1+discount, sales_df)
ksfit_lt_aic = aic.wge(ksfit_lt$residuals, p = 0:8, q=0:0)
ksfit_lt_aic
fit = arima(sales_df$sales, order = c(7,0,0), xreg = cbind(t,ad_tv1, ad_online1, sales_df$discount))
fit 

ljung.wge(fit$resid)

###### what is the lag
lags_df = read_csv( '/Users/pankaj/dev/git/smu/timeseries/data/whatisthelag.csv')
 
head(lags_df)

ccf(lags_df$X1_1, lags_df$Y)



########

data("sunspot.classic")


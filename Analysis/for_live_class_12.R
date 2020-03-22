library(tswge)
library(tidyverse)

la_cmort = read_csv('/Users/pankaj/Downloads/la_cmort_study.csv')

### check model stationarity 

plotts.sample.wge(la_cmort$cmort)
head(la_cmort)
## check first and second half of the data

plotts.sample.wge(la_cmort$cmort[1:(length(la_cmort$cmort)/2)])

plotts.sample.wge(la_cmort$cmort[(length(la_cmort$cmort)/2 +1):(length(la_cmort$cmort))])


##### fit model based on multivariate analysis

cmot_fit = lm(cmort~temp+part, la_cmort)

cf = aic.wge(cmot_fit$residuals, p=0:8, q=0:0)


fit = arima(la_cmort$cmort, order = c(4,0,0), xreg = la_cmort[,2:3])
fit
ljung.wge(fit$residuals)

ljung.wge(fit$residuals, K=48)


##### fit model based on multivariate analysis with time component 


cmot_fit_t = lm(cmort~Week+temp+part, la_cmort)

cf1 = aic.wge(cmot_fit_t$residuals, p=0:8, q=0:0)
cf1

fit1 = arima(la_cmort$cmort, order = c(2,0,0), xreg = la_cmort[1:3])
fit1
ljung.wge(fit1$residuals)

ljung.wge(fit1$residuals, K=48)





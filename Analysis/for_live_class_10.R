library(tswge)
library(tidyverse)
library(orcutt)

x4 = gen.sigplusnoise.wge(100, b0=0, b1=0, phi = 0.95 , sn = 21)
t4= seq(1, 100,1)
df = data.frame(x= x4, t = t4 )

fit = lm(x~t , data = df )
summary(fit)

cfit = cochrane.orcutt(fit)
summary(cfit)

 
df = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/onion_price.csv')
df_delhi = filter(df, Centre_Name =='DELHI')
 delhi_onion = select(df_delhi, Date, Price) %>% mutate(data_date = as.Date(Date, format='%d-%m-%y')) %>% select(data_date, Price)
delhi_onion = delhi_onion %>% complete(data_date = seq.Date(min(data_date), max(data_date), by="day")) %>% 
  fill('Price')


plotts.sample.wge(delhi_onion$Price)

delhi_onion_diff1= artrans.wge(delhi_onion$Price , phi.tr = 1)
plotts.sample.wge(delhi_onion_diff1)

mean(delhi_onion$Price)
aic5.wge(delhi_onion_diff1, p=0:5, q=0:2)

arima_est_df1= est.arma.wge(delhi_onion_diff1, p=3,q=2)

arima_est_df1$phi

est.ar.wge(delhi_onion$Price, p=15 , type="burg")

delhi_onion_diff360= artrans.wge(delhi_onion$Price , phi.tr = c(rep(0,360),1))


delhi_onion_diff360.1 = artrans.wge(delhi_onion_diff360 , phi.tr = 1)


delhi_onion_diff120= artrans.wge(delhi_onion$Price , phi.tr = c(rep(0,120),1))

aa= est.ar.wge(delhi_onion_diff120, p=3 , type="burg")

aa= est.ar.wge(delhi_onion$Price, p=360 , type="burg")


f= facaa$phi

factor.wge(phi = aa$phi)



factor.wge(phi = (c(rep(0,8),1)))

plotts.sample.wge(delhi_onion_diff360)

delhi_onion_diff120.1 = artrans.wge(delhi_onion_diff120 , phi.tr = 1)




# check for seasonality 




###### check for trend

library(orcutt)
ts = seq(1, length(delhi_onion$Price ),1)
df = data.frame(x= delhi_onion$Price , t = ts )
fit = lm(x~t , data = df )
summary(fit)
cfit = cochrane.orcutt(fit)
summary(cfit)





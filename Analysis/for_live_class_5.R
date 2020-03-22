library(tidyverse)
library(xts)
library(tswge)
df = read_csv('../timeseries/data/onion_price.csv')
df_delhi = filter(df, Centre_Name =='DELHI')

head(df_delhi)
delhi_onion = select(df_delhi, Date, Price) %>% mutate(data_date = as.Date(Date, format='%d-%m-%y')) %>% select(data_date, Price)
delhi_onion = delhi_onion %>% complete(data_date = seq.Date(min(data_date), max(data_date), by="day")) %>% 
  fill('Price')

plotts.true.wge(delhi_onion$Price)
parzen.wge(delhi_onion$Price)
aic5.wge(delhi_onion$Price)


wm_data= read.csv('/Users/pankaj/dev/git/smu/timeseries/data/Walmart.csv')
wmst9it50 = wm_data  %>% filter(item==50, store==9)   
plotts.sample.wge( wmst9it50$sales)
aic5.wge( wmst9it50$sales)

ts_1 = gen.arma.wge(1000, theta=c(0.967))

sample_model = gen.arma.wge(500, phi = c(.4,-.3,.2) , theta = c(.2))
plotts.sample.wge(sample_model)
aic5.wge(sample_model)
plotts.sample.wge(ts_1)


swadelay_data= read.csv('/Users/pankaj/dev/git/smu/timeseries/data/swadelay.csv')
plotts.sample.wge(swadelay_data$arr_cancelled)
aic5.wge(swadelay_data$arr_cancelled)




plotts.sample.wge( wmst9it50$sales)
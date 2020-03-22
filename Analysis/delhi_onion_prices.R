library(tidyverse)
library(xts)
library(tswge)
df = read_csv('data/onion_price.csv')
df_delhi = filter(df, Centre_Name =='DELHI')

head(df_delhi)
delhi_onion = select(df_delhi, Date, Price) %>% mutate(data_date = as.Date(Date, format='%d-%m-%y')) %>% select(data_date, Price)
delhi_onion = delhi_onion %>% complete(data_date = seq.Date(min(data_date), max(data_date), by="day")) %>% 
  fill('Price')
plotts.true.wge(delhi_onion$Price)

parzen.wge(delhi_onion$Price)

bond_df = read_csv('data/10_year_bond_rate_2010-2015.csv')

head(bond_df)

plotts.sample.wge(bond_df$Close)


acf(delhi_onion$Price[1:length(delhi_onion$Price)/2])

acf(delhi_onion$Price[length(delhi_onion$Price)/2]:length(delhi_onion$Price))

l = length(delhi_onion$Price)

 
acf(delhi_onion$Price)

acf(delhi_onion$Price[1:(l/2)])

acf(delhi_onion$Price[(l/2):(l)])

plot(delhi_onion$data_date[(l/2):l], delhi_onion$Price[(l/2):l])

plotts.sample.wge(delhi_onion$Price, lag.max = 1500)

length(delhi_onion)

ggplot(data = delhi_onion, aes(x = data_date, y = Price))+
  geom_line(color = "#00AFBB", size = 1)

acf(delhi_onion$Price, lag.max = 1500)



library(tswge)
library(tidyverse)
realization = gen.sigplusnoise.wge(200, coef = c(5,0), freq = c(.1,0),vara= 10 , sn = 1)
 
ma = filter(realization, rep(1,5))/5

ma_5 = stats::filter(realization, rep(1,5))/5

plotts.sample.wge(ma_5[10:20])


ma_7 = filter(realization, rep(1,7))/7

plotts.sample.wge(ma[3:195])


wm_data= read.csv('/Users/pankaj/dev/git/smu/timeseries/data/Walmart.csv')

length(wm_data)
length(wm_data$store)


###### for generating realizations :

gen.arma.wge(n = 100, phi=c(-0.9))


data_1 = read.csv('/Users/pankaj/Downloads/Unit3BOut1Part2.csv')

data_1








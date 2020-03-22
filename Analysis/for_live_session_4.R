library(tswge)
library(tidyverse)
factor.wge(phi = c(-.5,-.6 ))

ts = gen.arma.wge(n = 100, phi = c(-.5, -.6))

plotts.parzen.wge(ts)

plotts.true.wge(phi = c(-.5, -.6))

wm_data= read.csv('/Users/pankaj/dev/git/smu/timeseries/data/Walmart.csv')
wmst9it50 = wm_data  %>% filter(item==50, store==9)   
plotts.sample.wge( wmst9it50$sales)

ts_1 = gen.arma.wge(1000, theta=c(0.967))
plotts.sample.wge(ts_1)


ts_2 = gen.arma.wge(1000, theta=c(1.452, -.453, -.294, .175, -.237, -.154))
plotts.sample.wge(ts_2)

ts_3 = gen.arma.wge(1000, theta=c(1.445, -.411, -.038, .170, .362, -.245, -.177, .213))
plotts.sample.wge(ts_3)

ts_4 = gen.arma.wge(1000, theta=c(1.384, -.359, -.309, .063, .317, -.140, -.0587, -.199, .2877))
plotts.sample.wge(ts_4)

ts_spec = gen.arma.wge(1000, theta=c(-1.452, -.453, -.294, .175, -.237, -.154))
plotts.sample.wge(ts_spec)

factor.wge(phi= c(1.384, -.359, -.309, .063, .317, -.140, -.0587, -.199, .2877))
 

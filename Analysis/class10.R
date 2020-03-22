library(tswge)
library(tidyverse)
data("dowjones2014")
aic.wge(dowjones2014, p=0:5, q= 0:2)
mean(dowjones2014)

diff_x_1 = artrans.wge(dowjones2014, phi.tr = 1)

xd1= gen.arima.wge(n = 200, phi=c(1.27,-.8), d=1 , sn =56)
xd1.diff = artrans.wge(xd1, phi.tr = 1)

aic5_prd= aic5.wge(xd1.diff)

est.ar.wge(xd1.diff, p=2)

bond_data = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/10_year_bond_rate_2010-2015.csv')

plotts.sample.wge(bond_data$Close)

plotts.sample.wge(bond_data$Close[1:(length(bond_data$Close)/2)])
plotts.sample.wge(bond_data$Close[(length(bond_data$Close)/2+1):(length(bond_data$Close))])

bond_data.diff1 = artrans.wge(bond_data$Close, phi.tr = 1)

bond_data.diff11 = artrans.wge(bond_data.diff1, phi.tr = 1)


plotts.sample.wge(bond_data.diff1)


aic5.wge(bond_data.diff1)

aic5.wge(bond_data$Close)

x2 = gen.arima.wge(n=200, d= 2 , phi = c(1.2, -.6), sn =132, vara =1 )

x2.d1 = artrans.wge(x2, phi.tr = 1)

x2.d11 = artrans.wge(x2.d1, phi.tr = 1)

aic5.wge(x2.d11, p= 0:5, q = 0:2)

est.ar.wge(x2.d11, p =2 )

  
fore.aruma.wge(xd1, d=1, phi = c(1.27, -.8), n.ahead = 50 )

fore.aruma.wge(x2, d=2, phi = c(1.27, -.68), n.ahead = 50 )

################

dow = dowjones2014

dow.1 = artrans.wge(dow, phi.tr = 1)
aic5.wge(dow.1, p = 0:5, q = 0:2)

est.dow1 = est.arma.wge(dow.1 , p=4, q =1 )

est.dow2 = est.arma.wge(dow.1 , p=4, q =1 )


############

xd1 = gen.arima.wge(n = 200 , phi = c(1.2, -.8), d=1 , sn =56)

est.ar.wge(xd1, p=6, type = "burg")


est.ar.wge(xd1, p=8, type = "burg")

#####

est.ar.wge(bond_data$Close,p=6, type ="burg")




zero_or_one = read_csv('/Users/pankaj/Downloads/Zero_One_Or_TwoRootsOfOne.csv')

plotts.sample.wge(zero_or_one$x)

est.ar.wge(zero_or_one$x, p=7, type="burg")


install.packages(tseries)

library(tseries)


x = gen.arma.wge(200,phi = c(.9), sn = 5)

adf.test(x)

recs =c()
for (i in c(seq(0:9)))
  {
x = gen.arma.wge(200,phi = c(.9))
#print(b$p.value>0.025)
b = adf.test(x)
recs = c(recs, (b$p.value<0.025))
}
print(sum(recs))


####

x3 = gen.aruma.wge(n =200 , s=12 , phi = c(1.5,-.8), sn =87 )
plotts.sample.wge(x3)

d15 = est.ar.wge(x3 , p=15 , type = 'burg')
y3 = artrans.wge(x3 , phi.tr = c(rep(0,11),1), )
plotts.sample.wge(y3)

aic5.wge(y3 , p = 0:13 , q = 0:3)

factor.wge()

swa = read_csv('/Users/pankaj/Downloads/SWADelay.csv')
 
swa.est.ar15 = est.ar.wge(swa$arr_delay, p =15, type = "burg")


#############
library(orcutt)
x4 = gen.sigplusnoise.wge(100, b0=0, b1=0, phi = 0.95 , sn = 21)
t4= seq(1, 100,1)
df = data.frame(x= x4, t = t4 )

fit = lm(x~t , data = df )
summary(fit)

cfit = cochrane.orcutt(fit)
summary(cfit)
  
  








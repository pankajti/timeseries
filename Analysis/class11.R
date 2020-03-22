library(tswge)
library(tidyverse)
library(orcutt)

x1 = gen.arma.wge(n =100, phi = c(1.6, -.9), theta= 0.8, sn =67)
x1 = x1+10
plotts.sample.wge(x1)
aic.wge(x1, p= 0:8, q= 0:4)
x21 = est.arma.wge(x1, p=2,q=1)

plotts.sample.wge(x21$res)

ljung.wge(x21$res,p=2, q=1)

ljung.wge(x21$res,p=2, q=1,K=48)


####### 2 seasonal 

x2 = gen.aruma.wge(n=200, phi= c(1.5, -0.8), s=12, sn =87)

x2 =x2+50

plotts.sample.wge(x2)

y2 = artrans.wge(x2 , phi = c(rep(0,11),1))
aic.wge(y2, type ='bic')

est.y = est.ar.wge(y2, p=2)

plotts.sample.wge(est.y$res)
ljung.wge(est.y$res, p=2)
ljung.wge(est.y$res, p=2, K=48)

############# 3
data("airlog")
plotts.sample.wge(airlog)
airlog.d = artrans.wge(airlog, phi.tr = 1)

airlog.d.s = artrans.wge(airlog.d, phi.tr = c(rep(0,11),1))
aic.wge(airlog.d.s, p=0:15, q = 0:3)
est.arma.s.d = est.arma.wge(airlog.d.s, p=12 , q = 1)
plotts.sample.wge(est.arma.s.d$res)

ljung.wge(est.arma.s.d$res , p=12, q=1)

ljung.wge(est.arma.s.d$res , p=12, q=1, K=24)

#############

data(airlog) # load from tswge package
airlog1 = artrans.wge(airlog,phi.tr=1)
airlog1.12 = artrans.wge(airlog1,phi.tr = c(rep(0,11),1))
ww = est.ar.wge(airlog1.12,p = 12)

plotts.sample.wge(ww$res)

ljung.wge(ww$res , p=12 , K=24)


fore.aruma.wge(airlog, d=1, s=12 , phi = est.arma.s.d$phi, theta = est.arma.s.d$theta, n.ahead = 2,
               limits =T)


########################
data('global.temp')

mean(global.temp)

plotts.sample.wge(global.temp)

aic5.wge(global.temp ,p =0:6, q=0:1)

temp.est = est.arma.wge(global.temp, p=3, q=1)
plotts.sample.wge(temp.est$res, arlimits = TRUE)
ljung.wge(temp.est$res, p=3,q=1)

ljung.wge(temp.est$res, p=3,q=1, K=48)


#################################
d1.temp = artrans.wge(global.temp, phi.tr = 1)
plotts.sample.wge(d1.temp, arlimits = TRUE)

aic5.wge(d1.temp, p=0:6, q=0:1)

aic5.wge(d1.temp, p=0:6, q=0:2, type= 'bic')

d1.temp.est = est.arma.wge(d1.temp, p=2, q=1)

d1.temp.est$theta

plotts.sample.wge(d1.temp.est$res, arlimits = TRUE)

ljung.wge(d1.temp.est$res, p=2, q=1 )
ljung.wge(d1.temp.est$res, p=2, q=1, K=48 )

#####################################3

x=global.temp
n=length(x)
t= 1:n
d=lm(x~t)
x.z = x-d$coefficients[1]-d$coefficients[2]*t
plotts.sample.wge(x.z, arlimits = T)

ar.z = aic.wge(x.z, p=0:6)

y.trans = artrans.wge(global.temp, phi.tr = ar.z$phi)
t.trans = artrans.wge(t, phi.tr = ar.z$phi)
fitco = lm(y.trans~t.trans)
summary(fitco)

plotts.sample.wge(fitco$residuals, arlimits = TRUE)
acf(fitco$residuals)
ljung.wge(fitco$residuals)
  
#########################
data('sunspot.classc')
data("sunspot.classic")
  
plotts.sample.wge(sunspot.classic)
acf(sunspot.classic)
  
  pacf(sunspot.classic)

s2 = est.ar.wge(sunspot.classic, p=2)
plotts.sample.wge(s2$res, arlimits = T)

aic5.wge(sunspot.classic, p=0:10, q=0:0)









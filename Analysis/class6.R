library(tswge)
library(tidyverse)

gen.sigplusnoise.wge(231, b0=2, b1=4, vara = 10)


gen.sigplusnoise.wge(231, b0=2, b1=4, vara = 100)

model = mult.wge(fac1=c(1.5, -.8), fac2=c(1))

model$model.coef

factor.wge(model$model.coef)

x = gen.arima.wge(200, phi = c(1.5, -.8), var =1 , d =1 , sn =31)

FirstDiff = artrans.wge(x,1 )
parzen.wge(FirstDiff)


model2 = mult.wge(fac1=c(.6, -.8), fac2=c(1), fac3 = c(1))

model2$model.coef

factor.wge(model2$model.coef)


x2 = gen.arima.wge(500, phi = c(.6, -.8), var =1 , d =2, theta = c(-.3) , sn =35)
parzen.wge(x2)


FirstDiff = artrans.wge(x2,1 )
SecondDiff = artrans.wge(FirstDiff,1 )

aic5.wge(SecondDiff)

parzen.wge(FirstDiff)

x3 = gen.aruma.wge(n = 200, phi = c(.6, -.94), s=6 , theta = c(-.3), sn=19)

x4 = gen.aruma.wge(n = 500, phi = c(.6, -.8), s=12 , theta = c(.3, -.7), sn=37)

firstDiff = artrans.wge(x4, c(rep(0,11),1))

aic5.wge(firstDiff)

factor.wge(c(.5, -.2,0,1,-.5,.2))

factor.wge(c(.3, -1.2,-.4, 0, -.5,0,0,0,0,0,0,1,-.3,1.2,.4))



factor.wge(c(0.6996, 0.2599, 0.0079, -0.0646, 0.1381, -0.0953, 0.0235, -0.0969, 0.1770, 
             -0.1191, 0.1030, 0.7754, -0.4590, -0.4099, 0.0501))

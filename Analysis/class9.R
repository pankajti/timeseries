# (1-1.6B+0.8B^2)(Xt-50) = (1-0.8B)at  : sigma(a) ^2 = 0.5

x21 = gen.arma.wge(100, phi= c(1.6, -0.8), theta = c(0.8) , vara = 0.5 , sn =55)
x21 = x21 + 50
est.arma.wge(x21 , p =2, q =1 )

#########

# (1-.3B +0.7B^2)(Xt-37) = (1+0.4B)at   s^2 = 4 

x1 = gen.arma.wge(200 , phi = c(.3, -0.7), theta = c(-0.4) , vara = 4 , sn = 27)
x1 = x1+37

  est.arma.wge(x1, p =2 , q =1 )

mean(x1)

#####

x2 = gen.arma.wge(200, sn = 33 , phi = c(1.6, -0.9), vara= 2)
x2.yw = est.ar.wge(x2, p=2 , type = "yw")

x2.burg = est.ar.wge(x2, p=2 , type = "burg")

##############

x3 = gen.arma.wge(200 , phi = c(.3, -0.7), vara = 4 , sn = 27)
x3 = x3+37

x3.est= est.ar.wge(x3,  p =2 , type = "burg" )


#### white noise variance 

mean(x3.est$res^2)

x3.est$avar


######

maybe_wn = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/maybewhitenoise1.csv')

plotts.sample.wge(maybe_wn$x)

maybe_wn2 = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/maybewhitenoise2.csv')

log(3)


######

data("fig3.16a")

plotts.sample.wge(fig3.16a)

aic.wge(fig3.16a, p =0:5, q=0:2, type ='aic')

u = mean(fig3.16a)


x4   = gen.arma.wge(n=100, phi = c(1.6,-.9), theta= .8, sn =67)
x4= x4+10

plotts.sample.wge(x4)

aic.wge(x4, p= 0:8, q=0:2)

mean(x4)
est.arma.wge(x4, p=2, q=1)

# 1-2.3B+1.92B^2

x31 = gen.arma.wge(n=75, phi = c(2.3,-1.92,+.56), theta = -.8, sn =61)
x31+x31+30

plotts.sample.wge(x31)

aic5.wge(x31, p=0:8, q=0:2)

inf_data  = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/inflation.csv')

inf_data$Inflation

aic5.wge(inf_data$Inflation)

aic5.wge(inf_data$Inflation, type= 'bic')




x5   = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/armawhatpq1.csv')

pacf(x5$x)

acf(x5$x)

pacf(inf_data$Inflation)


aic5.wge(x5$x, type ='bic')

x6 = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/putittogether.csv')
plotts.sample.wge(x6$x)

est.ar.wge(x6$x)

aic5.wge(x6$x)


tg.data = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/texasgasprice.csv')

plotts.sample.wge(tg.data$Price)
x7model= aic.wge(tg.data$Price, type= 'aic')
fcst7=fore.arma.wge(tg.data$Price, phi=x7model$phi, theta = x7model$theta, n.ahead = 10)
fcst7$f[8]            
              
              
tg.est = est.ar.wge(tg.data$Price, p=2)    
mean(tg.data$Price)


tg.est$phi

tg.est$avar

#(1-1.381158B+0.4077B^2)(Xt-2.19) = at


###### 
tg.est.burg = est.ar.wge(tg.data$Price, p=2, type= "burg")  
mean(tg.data$Price)


######## ASE MLE
tg.est.fore = fore.aruma.wge(tg.data$Price, phi = tg.est$phi,   n.ahead= 24 , limits = F , lastn=T )
ASE1 = mean((tg.est.fore$f - tg.data$Price[182:205])^2)
ASE1

######## ASE MLE burg
tg.est.fore.burg = fore.aruma.wge(tg.data$Price, phi = tg.est.burg$phi,   n.ahead= 24 , 
                             limits = F , lastn=T )
ASE2 = mean((tg.est.fore.burg$f - tg.data$Price[182:205])^2)


ASE2


plotts.sample.wge(tg.data$Price)

#####




bo2 = read_csv('/Users/pankaj/Downloads/Unit9_2.csv')

pf = pacf(bo2$x, lag.max = 5)

plotts.sample.wge(bo2$x)

aic5.wge(bo2$x, p =0:5, q=0:4)

est.arma.wge(bo2$x, p=3, q=3)

pf
#0.848  0.106 -0.063 -0.080 -0.166 


-0.0405 

bo2_ar1= est.ar.wge(bo2$x, p=1, type = "yw")  

bo2_ar2= est.ar.wge(bo2$x, p=2)  

bo2_ar3= est.ar.wge(bo2$x, p=3)  

bo2_ar4= est.ar.wge(bo2$x, p=4)  

bo2_ar3= est.ar.wge(bo2$x, p=3)  



bo2_ar1$phi

pf$acf

pf$acf

#################

bo4 = read_csv('/Users/pankaj/Downloads/Unit9_1.csv')

plotts.sample.wge(bo4$x)



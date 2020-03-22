library(tswge)
library(tidyverse)

data("fig6.1nf")

plotts.wge(fig6.1nf)

fore.arma.wge(fig6.1nf, phi= 0.8, n.ahead= 20, limits = FALSE)

x1= gen.arma.wge(100, phi =-0.8)


fore.arma.wge(x1, phi= -0.8, n.ahead= 20, limits = FALSE)

x2 = gen.arma.wge(n = 75 , phi =c(1.6, -.8), sn =24)
x2 = x2+25

plotts.wge(x2)

fore.arma.wge(x2 , phi = c(1.6 , -.8) , n.ahead = 20, limits = TRUE)

# ARMA(2,1)

x3 = gen.arma.wge(n = 75, phi = c(1.6, -.8), theta = -.9, sn =24)

fore.arma.wge(x3 , phi = c(1.6, -.8), theta = -.9, n.ahead = 20, limits = FALSE)

  fore.arma.wge(x3 , phi = c(.8), theta = -.9, n.ahead = 20, limits = FALSE)
  
  
  data("llynx")
  
 x4 = plotts.wge(llynx)
 
 fore.arma.wge(llynx, )
 
 psi.weights.wge(phi = c(1.2, -.6), theta =.5, lag.max = 5)
 
LynxF_AR4 = fore.arma.wge(llynx, phi = c(1.3 ,-0.7, 0.1,-0.2), n.ahead= 30 , limits = F , lastn=T )
ASE = mean((LynxF_AR4$f - llynx[85:114])^2)
ASE

x5 = gen.aruma.wge(n= 50 ,phi =.8, d=1 , sn = 15)
fore.aruma.wge(x5, d=1 , n.ahead= 20, limits = FALSE)


fore.aruma.wge(x5,phi=.8, d=1 , n.ahead= 20, limits = FALSE)
fore.aruma.wge(x5,phi=.8, d=2 , n.ahead= 20, limits = FALSE)
x6 = gen.aruma.wge(n = 20, s=4 , sn =6)

fore.aruma.wge(x6, s=4, n.ahead = 24, lastn = FALSE, limits = FALSE)
fore.aruma.wge(x6, s=4, n.ahead = 8, lastn = FALSE, limits = FALSE)

fore.aruma.wge(x6, s=4, n.ahead = 8, lastn = TRUE, limits = FALSE)

x7 = gen.aruma.wge(n = 20, phi=0.8,s=4 , sn =6)

fore.aruma.wge(x7, phi=0.8,s=4 , n.ahead = 8, limits=FALSE)


####
  airline_data = read_csv('../timeseries/data/swadelay.csv')
  AD_AR12 = fore.arma.wge(airline_data$arr_delay, phi = c(.44,.02,-.12, .08, 0 , .02,.06, -.09
                                                ,.06,.07,.02,.37), n.ahead = 30, limits = F, lastn = T)
  ASE = mean((AD_AR12$f - airline_data$arr_delay[148:177])^2)
  ASE
 

AD_AR12_MA1 = fore.arma.wge(airline_data$arr_delay, phi = c(.34,.07,-.11, .07, .01 , .02,.06, -.09
                                                          ,.05,.07,.02,.39), theta= -0.12, n.ahead = 30, limits = F, 
                            lastn = T)
ASE2 = mean((AD_AR12_MA1$f - airline_data$arr_delay[148:177])^2)
ASE2 

AD_AR12_WD= fore.aruma.wge(airline_data$arr_delay, phi = c(-.34,-.05,-.14, -.11, .04 , .09,.02, .02
                                              ,.17,.03,-.10, -.38), d=1 , s=12, n.ahead = 30, limits = F, 
              lastn = T)


ASE3 = mean((AD_AR12_WD$f - airline_data$arr_delay[148:177])^2)
ASE3






 
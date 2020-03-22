library(tswge)
library(tidyverse)

 data("fig1.21a")
 data("fig1.21a")
 
 plotts.parzen.wge(fig1.21a)
 
 
 
 #plotts.parzen.wge(fuig1.21a)
 

 x= gen.sigplusnoise.wge(100, coef = c(3, 1.5) , freq = c(0.05, 0.35), psi = c(0,2 ))
 
plotts.sample.wge(x)
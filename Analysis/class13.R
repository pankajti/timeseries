library(tswge)
library(tidyverse)
library(nnfor)
 airline = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/swadelay.csv')
 airline$arr_delay
 
 SWATrain = ts(airline$arr_delay[1:141], start =c(2004,1), frequency= 12)
 SWATest = ts(airline$arr_delay[142:177], start =c(2015,10), frequency= 12)
 set.seed(2)
 fit_mlp = mlp(SWATrain, reps = 50 , comb = 'mean')
 fit_mlp
 plot(fit_mlp)
 for.mlp = forecast(fit_mlp, h=36)
 plot(for.mlp)
 
 ASE= mean((SWATest- for.mlp$mean)^2)
 ASE
 #[1] 317604252
 
 
 
 ######
 fit_mlp2 = mlp(SWATrain, lags = c(seq(1,12)), allow.det.season = FALSE)
 set.seed(2)
 fit_mlp2
 for.mlp2 = forecast(fit_mlp2, h=36)
 plot(fit_mlp2)
 
 plot(for.mlp2)
 ASE2= mean((SWATest- for.mlp2$mean)^2)
 ASE2
 
 ########
 
 
 fit_mlp3 = mlp(SWATrain, lags = c(seq(1,6, 12)), allow.det.season = FALSE, reps = 100)
 for.mlp3 = forecast(fit_mlp3, h=36)
 ASE3= mean((SWATest- for.mlp3$mean)^2)
 ASE3
 
 #####
 
 air_train = ts(airlog[1:108], start = c(1949,1), frequency = 12)
 air_test = ts(airlog[109:144], start = c(1958,1), frequency = 12)
 set.seed(2)
 fit_mlp_air = mlp(air_train)
  
 plot(fit_mlp_air)
 for_mlp_air = forecast(fit_mlp_air, h=36)

 plot(for_mlp_air)
 ASE_air = mean((air_test- for_mlp_air$mean)^2)
 ASE_air
 
 
 #####

 set.seed(2)
 fit_mlp_air = mlp(air_train, difforder = c(12))
 
 plot(fit_mlp_air)
 for_mlp_air = forecast(fit_mlp_air, h=36)
 
 plot(for_mlp_air)
 ASE_air = mean((air_test- for_mlp_air$mean)^2)
 ASE_air
 
 ######
 
 
 set.seed(2)
 fit_mlp_air = mlp(air_train,   hd = NULL, hd.auto.type = "cv")
 set.seed(2)
 
 plot(fit_mlp_air)
 for_mlp_air = forecast(fit_mlp_air, h=36)
 
 plot(for_mlp_air)
 ASE_air = mean((air_test- for_mlp_air$mean)^2)
 ASE_air
 
 ###########
 
 set.seed(2)
  
 bs = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/businesssales.csv')
 tbs80 = ts(bs$sales[1:80])
 tbx= data.frame(ad_tv = ts(bs$ad_tv), ad_online = ts(bs$ad_online, frequency=7), 
                 discount = ts(bs$discount) )
 fit3 = mlp(tbs80, xreg = tbx, hd = NULL, hd.auto.type = "cv")
   
  f= forecast(fit3, h=20, xreg = tbx) 
  
  ASE = mean((bs$sales[81:100]-f$mean)^2)
  ASE
 
 
 
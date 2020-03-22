library(tswge)
library(tidyverse)
library(orcutt)

suns_data = read.delim('/Users/pankaj/Downloads/SN_y_tot_V2.0.csv', header=FALSE, sep= ';',
                       col.names = c('year', 'mean', 'std', 'count', 'marker'), strip.white = TRUE)

plotts.wge(suns_data$mean)


plotts.sample.wge(suns_data$mean[1:(length(suns_data$mean)/2)])
plotts.sample.wge(suns_data$mean[(length(suns_data$mean)/2+1):(length(suns_data$mean))])

acf(suns_data$mean[1:(length(suns_data$mean)/2)])


aic5.wge(suns_data$mean, type ='aic')

aic5.wge(suns_data$mean, type ='bic')

suns_est = est.ar.wge(suns_data$mean, p=3 , type = 'burg')

suns_est.fore = fore.arma.wge(suns_data$mean, phi = suns_est$phi,   n.ahead= 15 ,
                              limits = F , lastn=T )
ASE1 = mean((suns_est.fore$f - suns_data$mean[(length(suns_data$mean-15+1)):(suns_data$mean)])^2)
ASE1

suns_data.d1= artrans.wge(suns_data$mean, phi.tr = c(1))


suns_est.aruma = est.arma.wge(suns_data$mean, p=3 )


suns_est_aruma.fore = fore.aruma.wge(suns_data$mean, phi = suns_est.aruma$phi, s=1, n.ahead= 15 ,
                              limits = F , lastn=T )

plotts.sample.wge(suns_est.aruma$res, arlimits = TRUE)

ASE2 = mean((suns_est_aruma.fore$f - suns_data$mean[(length(suns_data$mean-15+1)):(suns_data$mean)])^2)
ASE2



aa = est.ar.wge(suns_data$mean, p=12 , type = 'burg')
factor.wge(aa$phi)

suns_est.fore = fore.arma.wge(suns_data$mean, phi = suns_est$phi,   n.ahead= 10 ,
                              limits = T , lastn=F)


suns_est_aruma.fore = fore.aruma.wge(suns_data$mean, phi = suns_est.aruma$phi, s=1, n.ahead= 25 ,
                                     limits = F , lastn=F )



library(tidyverse)
library(tswge)

mt_data = read_csv('/Users/pankaj/dev/git/smu/timeseries/data/midterm2020.csv') 
 
plotts.wge(mt_data$x)

acf(mt_data$x)


acf(mt_data$x[1:72])

acf(mt_data$x[72:144])



plotts.sample.wge(mt_data$x[1:72])

plotts.sample.wge(mt_data$x[72:144])


length(mt_data$x)
plotts.parzen.wge(mt_data$x)

plotts.true.wge(mt_data$x)


parzen.wge(mt_data$x)

md1= gen.aruma.wge(200, phi = c(0.5380, -0.0606,   -0.1923),   s=c(12), sn =42)
plotts.parzen.wge(md1)

md2= gen.arma.wge(200, phi = c(1.0507,-0.0756), theta = c(0.5927 , 0.2751), sn = 42)
plotts.parzen.wge(md2)

fore.aruma.wge(mt_data$x, c(0.5380, -0.0606,   -0.1923),   s=c(12) )

psi.weights.wge(phi = c(0.5380, 0.0606,   0.1923) , lag.max = 4 )

parzen.wge()
 

factor.wge(phi = c(1.0507,-0.0756))

psi.weights.wge(phi = c(0.5380, 0.0606,   0.1923) , lag.max = 4 )

# model 1
factor.wge(phi = c(0.5380, 0.0606,   0.1923)  )

# calculate 10 ase by moving 1 data point back each time
mt_fore_AR2MA2 = fore.arma.wge(mt_data$x, phi = c(1.0507,-0.0756), theta = c(0.5927 , 0.2751), n.ahead= 12 , limits = F , lastn=T )
ASE2 = mean((mt_fore_AR2MA2$f - mt_data$x[133:144])^2)
sum_ase_model_2=0
for (i in seq(0,10)){
  mt_fore_AR2MA2 = fore.arma.wge(mt_data$x[1:(144-i)], phi = c(1.0507,-0.0756), theta = c(0.5927 , 0.2751), n.ahead= 12 , limits = F , lastn=T )
  sum_ase_model_2 = sum_ase_model_2 +mean((mt_fore_AR2MA2$f - mt_data$x[(133-i):(144-i)])^2)
}
print(paste0("ASE2 ::" , ASE2))
print(paste0("Avg ASE2 " , sum_ase_model_2/ 10))
print(paste0("Diff ASE2 ", (ASE2- sum_ase_model_2/ 10)) )


mt_fore_ARuma3S12 = fore.aruma.wge(mt_data$x, phi = c(0.5380, 0.0606,   0.1923), s=12, n.ahead= 12 , limits = F , lastn=T )
ASE1 = mean((mt_fore_ARuma3S12$f - mt_data$x[133:144])^2)

sum_ase_model_1=0
for (i in seq(0,10)){
  mt_fore_ARuma3S12 = fore.aruma.wge(mt_data$x[1:(144-i)], phi = c(0.5380, 0.0606,   0.1923), s=12, n.ahead= 12 , limits = F , lastn=T )
  sum_ase_model_1 = sum_ase_model_1 + mean((mt_fore_ARuma3S12$f - mt_data$x[(133-i):(144-i)])^2)
}
print(paste0("ASE1 ::" , ASE1))
print(paste0("Avg ASE1 " , sum_ase_model_1/ 10))
print(paste0("Diff ASE1 ", (ASE1- sum_ase_model_1/ 10)) )


library(ggplot2)
library(reshape2)
forecasts =c()
for (i in seq(0,36)){
  mt_fore_ARuma3S12 = fore.aruma.wge(mt_data$x[1:(144-i)], phi = c(0.5380, 0.0606,   0.1923),
                                     s=12, n.ahead= 12 , limits = F , lastn=T )
  sum_ase_model_1 = sum_ase_model_1 + 
    mean((mt_fore_ARuma3S12$f - mt_data$x[(133-i):(144-i)])^2)
  forecasts= rbind(forecasts, mt_fore_ARuma3S12$f)
}

ggplot(melt(forecasts), aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Month", y="Forecast No", title="Rolling Window Forecasts") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))


forecasts







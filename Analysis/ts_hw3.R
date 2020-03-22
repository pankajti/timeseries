# 2.1  

x= gen.sigplusnoise.wge(100, coef = c(3, 1.5) , freq = c(0.05, 0.35), psi = c(0,2 ))


lp_x=butterworth.wge(x, order = 3 , type = "low", cutoff = 0.2)

hp_x= butterworth.wge(x, order = 3 , type = "high", cutoff = 0.2)

lp_hp_x = butterworth.wge(hp_x$x.filt, order = 3 , type = "low", cutoff = 0.2)
 
plotts.sample.wge(x)

plotts.sample.wge(lp_x$x.filt)

plotts.sample.wge(hp_x$x.filt)

plotts.sample.wge(lp_hp_x$x.filt)

xdif = artrans.wge(x, phi.tr = 1)
plotts.sample.wge(xdif)

ma = stats::filter(x,rep(1/5,5))
plot(ma)
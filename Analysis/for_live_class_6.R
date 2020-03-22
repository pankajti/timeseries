library(tswge)
library(tidyverse)

order_7_model = gen.aruma.wge(250, s=7)

factor.wge(order_7_model)

x = gen.aruma.wge(200, phi = c(.5, -.8, .2, .05, .07, .09, .06), var =1 , d =4 )

factor.wge(c(.5, -.8, .2, .05, .07, .09, .06))

factor.wge(phi = c(rep(0,6),1))

alphavantage_key  = '9UMGP0FOZZ74E18Y'
library(alphavantager)
av_api_key(alphavantage_key)
msft_data = av_get(symbol = "MSFT", av_fun = "TIME_SERIES_DAILY",   outputsize = "full")


one_year_data = filter(msft_data, timestamp >'2019-02-012')

plotts.sample.wge(one_year_data$close)

FirstDiff = artrans.wge(one_year_data$close,1 )

plotts.sample.wge(FirstDiff)

aic5.wge(one_year_data$close)

aic5.wge(FirstDiff)
library(tswge)
library(tidyverse)
wm_data= read.csv('/Users/pankaj/dev/git/smu/timeseries/data/Walmart.csv')
# filtering based on the condition in breakout 3 class 2
wm_st_8_it_1 = wm_data  %>% filter(item==1, store==8)   
plotts.parzen.wge( wm_st_8_it_1$sales)

smooth_51 = stats::filter( wm_st_8_it_1$sales, rep(1,51))/51
plotts.sample.wge(smooth_51[26:(length(smooth_51)-25)])

smooth_5 = stats::filter( wm_st_8_it_1$sales, rep(1,5))/5
plotts.sample.wge(smooth_5[3:(length(smooth_5)-2)])    
 

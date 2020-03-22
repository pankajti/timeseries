library(tidyverse)

sales_csv = read_csv('/Users/pankaj/Downloads/m5-forecasting-accuracy/sales_train_validation.csv')
head(sales_csv)

num_columns = length(names(sales_csv))

sales_data = sales_csv  %>%
  gather(key = "days" , value = sales ,  names(sales_csv)[7:num_columns])

sales_data = sales_data %>% filter(grepl("HOBBIES_1_001",item_id) )

 
cal  = read_csv('/Users/pankaj/Downloads/m5-forecasting-accuracy/calendar.csv')%>%
  select(d, date)


 
merged_data = dplyr::full_join(sales_data, cal , by =c("days"="d"))

write_csv(merged_data, "/Users/pankaj/dev/git/smu/timeseries/data/mdf_merged_data1.csv")

acoin_data = read_csv('/Users/pankaj/Downloads/cc_histories/acoin.csv')

plot(merged_data$sales)


library(tidyverse)
library(reticulate)
library(rvest)

url = 'https://en.wikipedia.org/wiki/Republic_Day_(India)'

rep_data= read_html(url)
region_node = html_node(rep_data, xpath = '//*[@id="mw-content-text"]/div/table[2]')

region_table = html_table(region_node)


ggplot(region_table, mapping = aes(Region, Invitations))+
  geom_bar(stat= 'Identity')+
  theme(axis.text.x = element_text(angle = 90, hjust =1))


nation_node = html_node(rep_data, xpath = 
                          '//*[@id="mw-content-text"]/div/table[3]')

nation_table = html_table(nation_node, fill = TRUE)

nation_df =as.data.frame(nation_table)

aa = c()

length(table(aa))
library(rworldmap)
ggplot(as.data.frame(aa, stringsAsFactors = FALSE), mapping = aes(Country))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 90, hjust =1))+
  #theme(axis.text.y = element_text(angle = 90, hjust =1))+
  coord_flip()+
  title(' REpublic day parade chief guest country count')

country_count = as_tibble(aa, stringsAsFactors = FALSE) 

country_count = mutate(country_count , Country = replace(Country , Country =='United States' , 'USA'))%>%
  mutate(  Country = replace(Country , Country =='United Kingdom' , 'UK'))%>%
  mutate(  Country = replace(Country , Country =='Soviet Union' , 'Russia'))%>%
  as.data.frame()

country_count= country_count %>% group_by(Country) %>% count() %>%
  ungroup(Country)

 
country_count$region = country_count$Country

life.exp.map <- full_join(country_count, world_map, by = c(  "region"))


ggplot(life.exp.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = n ), color = "white")+
  scale_fill_viridis_c(option = "C")

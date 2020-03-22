library(tidyverse)
library(reticulate)
library(rvest)
require(maps)

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

mutate(country_count , Country = replace(Country , Country =='United States' , 'USA'),
       stringsAsFactors = FALSE)


country_count = as.data.frame(aa) %>% group_by(Country) %>% count()

country_count$region = country_count$Country

life.exp.map <- left_join(country_count, world_map, by = c(  "region"))


ggplot(life.exp.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = n ), color = "white")+
  scale_fill_viridis_c(option = "C")

country_count$
  
  sapply(life.exp.map$Country, levels)
 
distinct(life.exp.map, Country)
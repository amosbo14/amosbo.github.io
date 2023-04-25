library(tidyverse)
library(ggthemes)
library(Hmisc)
library(viridisLite)
dat <- read.csv('home_sales_nyc.csv')

ggplot(dat) +
  geom_histogram(aes(sale.price))

ggplot(dat) +
  geom_histogram(aes(log(sale.price))) 


shapiro.test(log(sample_n(dat, 10000)[,12]))
ks.test(log(dat[,12]), 'pnorm')





us_states <- map_data('county')

cali_dat <- read.csv('california_housing.csv')

ggplot(cali_dat) +
  geom_histogram(aes(medianHouseValue))

ggplot(cali_dat) +
  geom_histogram(aes(log(medianHouseValue))) 

la <- c('LA', -118.15, 34.03)
sf <- c('San Francisco', -122.24, 37.46)

locs <- as.data.frame(matrix(c(la, sf), nrow = 2, byrow = T))
colnames(locs) <- c('place','long','lat')

ggplot(cali_dat) +
  geom_polygon(data = filter(us_states, region == 'california'),
               mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(aes(x = longitude, y = latitude, color = medianHouseValue, size = population), alpha = .5) +
  coord_fixed() +
  theme_map() +
  theme(legend.position = "right") +
  scale_color_continuous(type = 'viridis') +
  labs(color = 'Median House Value', size = 'Population', title = 'Median House Prices and Population in California') +
  ggrepel::geom_text_repel(data = locs, aes(x = as.numeric(long), y = as.numeric(lat), label = place), hjust = 3, 
                           vjust = 3)



#+
#  scale_color_gradient2(breaks = c(100000,200000,300000,400000,500000),
#                        labels = c('$100,000','$200,000','$300,000','$400,000','$500,000'))






hist.data.frame(cali_dat)


## Coast map stuff ----------


coasts <- read.csv('Coastal Borders_Migrated Data.csv')

ggplot(coasts) +
  geom_polygon(aes(x = Longitude, y = Latitude, group = Sub.Polygon.Id), fill = 'white', color = 'black' ) +
  coord_fixed() +
  theme_map()

ggplot(coasts) +
  geom_point(aes(x = Longitude, y = Latitude))




## Function for distance to coast ------
distance <- function(lat, long){
  dist <- sqrt( ( (lat - coasts$Latitude)*69 )^2 + ( (long - coasts$Longitude)*54.6 )^2)
  return(min(dist, na.rm = T))
}


lon <- -120

lat <- 40

distance(lat, lon) # should get roughly 156




cali_dat <- read.csv('california_housing.csv')

## Creates distance entry
cali_dat$Distance <- 0

for(i in 1:nrow(cali_dat)){
  cali_dat$Distance[i] <- distance(cali_dat$latitude[i], cali_dat$longitude[i])
}




library(tidyverse)
library(socviz)
library(ggthemes)
library(hrbrthemes)
theme_set(theme_minimal())
organdata <- organdata %>%
  mutate(country = fct_reorder(country, donors, na.rm = T) ) 





p <- ggplot(data = organdata,
            mapping = aes(x = country, y = donors, fill = world))
p + geom_boxplot() +
  labs(x = NULL) +
  coord_flip()
 + theme(legend.position = "top", legend.direction = "horizontal")


by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize(donors_mean= mean(donors, na.rm = TRUE),
            donors_sd = sd(donors, na.rm = TRUE),
            gdp_mean = mean(gdp, na.rm = TRUE),
            health_mean = mean(health, na.rm = TRUE),
            roads_mean = mean(roads, na.rm = TRUE),
            cerebvas_mean = mean(cerebvas, na.rm = TRUE))
by_country


by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, list(mean = mean, 
                                median = median), 
               na.rm = TRUE) %>%
  ungroup()


by_country <- by_country %>% 
  mutate(country = fct_reorder(country, donors_mean))

p <- ggplot(data = by_country,
            mapping = aes(x = country, y = donors_mean, color = consent_law))
p + geom_point(size = 5) +
  theme(legend.position = "top", legend.direction = "horizontal") +
  coord_flip()


p + geom_point(size = 5) +
  facet_wrap(~consent_law, scales = 'free_y', ncol = 1)+
  theme(legend.position = "top", legend.direction = "horizontal") +
  coord_flip()

p + geom_pointrange(aes(ymin = donors_mean - donors_sd,
                        ymax = donors_mean + donors_sd)) +
  facet_wrap(~consent_law, scales = 'free_y', ncol = 1)+
  theme(legend.position = "top", legend.direction = "horizontal") +
  coord_flip()






p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = world))
p + geom_point()



p + geom_point() +
  scale_y_continuous(breaks = c(5,15,25),
                     labels = c('Five','Fiften','Twenty five'))

p + geom_point()+
  scale_color_discrete(labels = c('Corporatist','Liberal','Social Democratic','Unclassified'))

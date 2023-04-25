#2/21/23

library(tidyverse)
library(socviz)
by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, list(mean = mean, 
                                median = median), 
               na.rm = TRUE) %>%
  ungroup()


p <- ggplot(data = by_country,
            mapping = aes(x = roads_mean, y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country), hjust = -.1, vjust = 1)


install.packages('ggrepel')
library(ggrepel)

p + geom_point() + geom_text_repel(mapping = aes(label = country))

library(ggthemes)
library(tidyverse)
library(socviz)

rel_by_region <- gss_sm %>%
  group_by( bigregion, religion ) %>%
  summarize( N = n() ) %>%
  mutate( freq = N / sum(N),
          pct = round( (freq*100), 0) )
rel_by_region

p <- ggplot( rel_by_region, 
             aes( x = bigregion, 
                  y = pct, 
                  fill = religion))
p + geom_col( position = "dodge2" ) +
  labs(x = "Region", 
       y = "Percent", 
       fill = "Religion") +
  theme(legend.position 
        = "top")


p <- ggplot( rel_by_region, 
             aes( x = religion, 
                  y = pct, 
                  fill = religion))
p + geom_col( position = "dodge2" ) +
  labs(x = "Religion", 
       y = "Percent", 
       fill = "Religion") +
  facet_grid(.~bigregion) +
  coord_flip() +
  theme(legend.position 
        = "none")

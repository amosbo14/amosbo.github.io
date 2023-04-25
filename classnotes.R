#install.packages("socviz")
library(tidyverse)
library(socviz)
?gss_sm
glimpse(gss_sm)
skimr::skim(gss_sm)
gss_sm <- gss_sm

p <- ggplot(data = gss_sm,
            mapping = aes( x = age, y = childs ))
p + geom_point(alpha = 0.2) +
  geom_smooth(method = lm)

p <- ggplot(data = gss_sm,
            mapping = aes( x = age, y = childs ))
p + geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_grid(sex~race)


p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion))
p + geom_bar(mapping = aes(y = ..prop..))


table(gss_sm$bigregion)

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion))
p + geom_bar(mapping = aes(y = ..prop.., group = 1))



p <- ggplot(data = gss_sm,
            mapping = aes(x = religion, fill = religion))
p + geom_bar() +
  guides(fill = 'none')

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, fill = religion))
p + geom_bar(position = 'stack')

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, fill = religion))
p + geom_bar(position = 'fill')

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, fill = religion))
p + geom_bar(position = 'identity') #all bars start from 0


p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, fill = religion))
p + geom_bar(position = "dodge",
             mapping = aes(y = ..prop..))

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, fill = religion))
p + geom_bar(position = "dodge2",
             mapping = aes(y = ..prop..))

p + geom_bar(position = "dodge2",
             mapping = aes(y = ..prop.., group = religion))







?midwest
glimpse(midwest)
skim(midwest)
view(midwest)

p <- ggplot(data = midwest,
            mapping = aes(x = area))
p + geom_histogram()



p <- ggplot(data = midwest,
            mapping = aes(x = area, fill = state, color = state))
p + geom_density(alpha = 0.3)



socviz::titanic

p <- ggplot(data = titanic,
            mapping = aes(x = fate, y = percent, fill = sex))
p + geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top")

oecd_sum

p <- ggplot(data = oecd_sum,
            mapping = aes(x = year, y = diff, fill = hi_lo))
p + geom_histogram(stat = 'identity') + theme(plot.subtitle = element_text(size = 13),
    plot.caption = element_text(size = 11),
    plot.title = element_text(size = 18),
    panel.background = element_rect(fill = "gray86"),
    legend.position = "none") + 
  labs(title = "The US Life Expectancy Gap",
      x = NULL, y = "Difference in Years",
      subtitle = "Difference between US and OECD",
      caption = "Data: OECD \n Washington Post")

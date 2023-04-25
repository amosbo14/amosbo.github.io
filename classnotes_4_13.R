#install.packages("plotly")
library(plotly)
library(tidyverse)
library(socviz)
dat <- data.frame(cond = rep(c("A", "B"), each = 10),
                  xvar = 1:20 + rnorm(20, sd=3),
                  yvar = 1:20 + rnorm(20, sd=3))
p <- ggplot(dat, aes(x = xvar, y = yvar)) +
  geom_point(shape=1)      # Use hollow circles
fig <- ggplotly(p)
fig


cces <- read_csv(url("https://bcdanl.github.io/data/cces.csv"))

cces <- cces %>% 
  mutate(party = recode(dem, `1` = "Democrat", `0` = "Republican"))

p <- ggplot(cces, aes(x = seniority, y = les,
                      color = party))+
  geom_point()+
  scale_color_manual(values=c("blue","red")) +
  labs(x = "Seniority", y = "Leg. Effectiveness")
p1 <- ggplotly(p)


install.packages("htmlwidgets")
library(htmlwidgets)
saveWidget(p1, "fig.html")

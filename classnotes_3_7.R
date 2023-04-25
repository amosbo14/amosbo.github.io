library(gapminder)
library(tidyverse)

fit_ols <- function(df) {
  lm(lifeExp ~ log(gdpPercap), data = df)
}
out_le <- gapminder %>%
  group_by(continent, year) %>%
  nest() %>% 
  mutate(model = map(data, fit_ols)) 
out_le



fit_ols <- function(df) {
  lm(lifeExp ~ log(gdpPercap), data = df)
}
out_tidy <- gapminder %>%
  group_by(continent, year) %>%
  nest() %>% 
  mutate(model = map(data, fit_ols),
         tidied = map(model, tidy)) %>%
  unnest(tidied, .drop = TRUE) %>%
  filter(term != "(Intercept)" &
           continent != "Oceania")

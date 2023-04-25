library(tidyverse)
library(gapminder)
library(broom)
library(socviz)

out <- lm(formula = lifeExp ~ gdpPercap + pop + continent,
          data = gapminder)
summary(out)

min_gdp <- min(gapminder$gdpPercap)
max_gdp <- max(gapminder$gdpPercap)
med_pop <- median(gapminder$pop)
pred_df <- expand.grid(gdpPercap = (seq(from = min_gdp,
                                        to = max_gdp,
                                        length.out = 100)),
                       pop = med_pop,
                       continent = c("Africa", "Americas",
                                     "Asia", "Europe", "Oceania"))
dim(pred_df); head(pred_df)


pred_out <- predict(object = out,
                    newdata = pred_df,
                    interval = "predict")
head(pred_out)

pred_df <- cbind(pred_df, pred_out)
head(pred_df)

p <- ggplot(data = filter(pred_df, continent %in% c("Europe", "Africa")),
            aes(x = gdpPercap, 
                y = fit, ymin = lwr, ymax = upr,
                color = continent, fill = continent, group = continent))
p + geom_point(data = filter(gapminder,
                             continent %in% c("Europe", "Africa")),
               aes(x = gdpPercap, y = lifeExp,
                   color = continent),
               alpha = 0.5,
               inherit.aes = FALSE) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) + # the area covered by the prediction intervals
  scale_x_log10(labels = scales::dollar) # not a straight line because of the log scale


out_aug <- augment(out)
head(out_aug)
p <- ggplot(data = out_aug,
            mapping = aes(x = .fitted, y = .resid))
p + geom_point(aes(color = continent))


glance(out) %>% round_df()

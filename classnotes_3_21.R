library(tidyverse)
library(socviz)
library(ggthemes)
election %>% select(state, total_vote,
                    r_points, pct_trump, party, census) %>%
  sample_n(5)



party_colors <- c("#2E74C0", "#CB454A")  # Hex color codes for Dem Blue and Rep Red
p0 <- ggplot(data = filter(election, st != "DC"),
             mapping = aes(x = r_points,
                           y = reorder(state, r_points),
                           color = party))


p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") +
  geom_point(size = 2)
p1 # Colors are wrong, so we need to correct it


p2 <- p1 + scale_color_manual(values = party_colors)
p2


p3 <- p2 + scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30, 40),
                              labels = c("30\n (Clinton)", "20", "10", "0",
                                         "10", "20", "30", "40\n(Trump)"))
p3


p3 + facet_wrap(~ census, ncol=1, scales="free_y") + # this scales argument keeps the names from appearing for everything
  guides(color = "none") + labs(x = "Point Margin", y = "") +
  theme(axis.text=element_text(size=8))



us_states <- map_data("state") # from the 'maps' package
us_states
view(us_states)


p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))
p + geom_polygon(fill = "white", color = "black")


p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)


p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)


election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election)

p0 <- ggplot(data = us_states_elec,
             aes(x = long, y = lat,
                 group = group, fill = party))
p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = party_colors) +
  theme_map()



p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2016", fill = NULL)
p2 + theme_map()



p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat, group = group, fill = pct_trump))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p1 + labs(title = "Trump vote") + theme_map() + labs(fill = "Percent")


p2 <- p1 + scale_fill_gradient(low = "white", high = "#CB454A") +
  labs(title = "Trump vote") 
p2 + theme_map() + labs(fill = "Percent")


p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat, group = group, fill = d_points))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_gradient2() + labs(title = "Winning margins") 
p2 + theme_map() + labs(fill = "Percent")



p3 <- p1 + scale_fill_gradient2(low = "red", 
                                mid = scales::muted("purple"),
                                high = "blue", 
                                breaks = c(-25, 0, 25, 50, 75)) 
p3 + theme_map() + labs(fill = "Percent", title = "Winning margins")




p0 <- ggplot(data = filter(us_states_elec,
                           region != "district of columbia"),
             aes(x = long, y = lat, group = group, fill = d_points))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p3 <- p1 + scale_fill_gradient2(low = "red", 
                                mid = scales::muted("purple"),
                                high = "blue", 
                                breaks = c(-25, 0, 25, 50, 75)) 
p3 + theme_map() + labs(fill = "Percent", title = "Winning margins", caption = "DC is omitted.")



county_map
county_data %>%
  select(id, name, state, pop_dens, pct_black) %>%
  sample_n(5)
county_full <- 
  left_join(county_map, county_data, by = "id")


p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat,
                          fill = pop_dens, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05)
p1

p1 + coord_equal()



p2 <- p1 + scale_fill_brewer(
  palette = "Blues",
  labels = c("0-10", "10-50", "50-100", "100-500",
             "500-1,000", "1,000-5,000", ">5,000"))
p2



p2 + labs(fill = "Population per\nsquare mile") +
  theme_map() +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")



p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat, fill = pct_black, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2 <- p1 + scale_fill_brewer(palette="Greens")
p2 + labs(fill = "US Population, Percent Black") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() + theme(legend.position = "bottom")



orange_pal <- RColorBrewer::brewer.pal(n = 6, name = "Oranges")
orange_pal
orange_rev <- rev(orange_pal)
orange_rev


pop_p <- ggplot(data = county_full,
                mapping = aes(x = long, y = lat,
                              fill = pop_dens6, 
                              group = group))
pop_p1 <- pop_p + geom_polygon(color = "gray90", size = 0.05) +
  coord_equal()
pop_p2 <- pop_p1 + scale_fill_manual(values = orange_pal)
pop_p2 + labs(title = "Population Density",
              fill = "People per square mile") +
  theme_map() + theme(legend.position = "bottom")


pop_p2_rev <- pop_p1 + scale_fill_manual(values = orange_rev)
pop_p2_rev + labs(title = "Reverse-coded Population Density",
                  fill = "People per square mile") +
  theme_map() + theme(legend.position = "bottom")







library(viridis)

NY_socioecon_geo_poverty <- read_csv(
  'https://bcdanl.github.io/data/NY_socioecon_geo_poverty.csv'
)

p <- ggplot(data = NY_socioecon_geo_poverty,
            mapping = aes(x = long, y = lat, group = group, 
                          fill = c04_058 ))
p1 <- p + geom_polygon(color = "grey", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_viridis_c(option = "plasma") + theme_map() 
p2

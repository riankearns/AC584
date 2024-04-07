install.packages("tidyverse")
install.packages("plotly")
library(tidyverse)
library(plotly)
Metadata <- read_csv("Metadata.csv")
Indicator_Unicef <- read_csv("Indicator Unicef.csv")

data_join <- Metadata %>%
full_join(Indicator_Unicef, by = c("country"= "country", "year" = "time_period"))

install.packages("maps")

map_world <- map_data("world")

data_join_2019 <- data_join %>% 
  filter(year == 2019)

map_data_join_2019 <- full_join(data_join_2019, map_world, by = c("country" = "region"))
ggplot(map_data_join_2019) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  labs(
    title = "Primary School Students Achieving Min. Proficiency Level in Maths 2019",
    x= "",
    y="",
    fill = "Proportion of Students %")

#Scatter Plot
ggplot(data_join) +
aes(obs_value, life_expectancy_at_birth_total) + 
  geom_point(alpha=0.2, color = "red") +
  labs(x = "Proportion of students at the end of primary achieving at least a minimum proficiency level in mathematics", 
       y = "Life Expectancy at Birth", 
       title = "Life Expectancy and Math Proficiency Relationship") +
theme_classic() +
geom_smooth(color = "blue")  + 
 theme(text = element_text(family = "serif"))

#bar chart
data_join_2019 <- data_join %>%
  filter(year == 2019, country %in% c("Russia", "Chad"))


data_join_2019 <- data_join_2019 %>%
  group_by(country, year)

ggplot(data_join_2019, aes(x = reorder(country, population_total), y = population_total, fill = country)) +
  geom_col() +
  labs(x = "",
       y = "Population",
       fill = "Country",
       title = "Population Comparison of Russia and Chad in 2019") +
  

  theme_classic() +
  theme(
    text = element_text(family = "serif")
  )
options(scipen = 999)

 #time series
filtered_data <- data_join %>%
  filter(country %in% c("Russia", "Chad"))

ggplot(filtered_data, aes(x = year, y = life_expectancy_at_birth_total, group = country, color = country)) +
  geom_line() +
  labs(title = "Life Expectancy Trends for Chad and Russia",
       x = "Year",
       y = "Population") +
  theme_minimal()



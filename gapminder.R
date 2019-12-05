library(gapminder)
library(ggplot2)

# Explore the gapminder dataset
data <- gapminder %>%
  filter(year == 2007)

# Visualize with facet wrap by continent
ggplot(data = data, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + 
  geom_point() + scale_x_log10() + facet_wrap(~ continent)

# Visualize with facet wrap by year
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() + scale_x_log10() + facet_wrap(~year)

# Explore population distribution in 1952
lifeExp_1952 <- gapminder %>%
  filter(year == 1952) 

ggplot(lifeExp_1952, aes(x = pop)) + geom_histogram() + scale_x_log10()

# Explore boxplot of life expectancy by continent in 2007
lifeExp_2007 <- gapminder %>%
  filter(year == 2007) 

ggplot(lifeExp_2007, aes(x = continent, y = lifeExp)) + geom_boxplot()

# Plot life expectancy as a function of GDP per cap by continent
ggplot(lifeExp_2007, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(aes(size = pop)) +
  scale_x_continuous(trans = "log10") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", title = "Life Expectancy as a Function of GDP, by Continent", 
       color = "Continent", size = "Population")

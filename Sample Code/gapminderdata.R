library(dslabs)
library(dplyr)
library(ggplot2)
library(stringi)
data("gapminder")
head(gapminder)

# Simple scatterplot

gapminder %>% filter(year==1962) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent))+
  geom_point() +
  ds_theme_set()

# Let's now FACET the data

gapminder %>% filter(year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent))+
  geom_point() +
  ds_theme_set()+
  facet_grid(continent~year)

gapminder %>% filter(year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent))+
  geom_point() +
  ds_theme_set()+
  facet_grid(.~year)

# Let's use the facet wrap function (VERY NICE!)

gapminder %>% filter(year %in% c(1962,1980,1990,2000,2012) & continent %in% c("Asia", "Europe")) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent))+
  geom_point() +
  ds_theme_set()+
  facet_wrap(year~.)

# Time series plot

countries <- c("Germany", "South Korea")

gapminder %>% filter(country %in% countries) %>% 
  ggplot(aes(year, fertility, color=country))+
  geom_line() +
  ds_theme_set()

# Time series plot with data labels

labels <- data.frame(country = countries, x=c(1965,1978), y=c(72,60)) # pick these by eyes typically

gapminder %>%  filter(country %in% countries) %>% 
  ggplot(aes(year, life_expectancy, col=country))+
  geom_line() +
  geom_text(data = labels, aes(x,y,label=country),size=5)+
  ds_theme_set()+
  theme(legend.position = "none")

# Transformations

gapminder <- gapminder %>% 
  mutate(dollars_per_day=gdp/population/365)

past_year <- 1970

gapminder %>% 
  filter(year==past_year& !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth=1, color="black")
  
# Using log 2 transformation

gapminder %>% 
  filter(year==past_year& !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) + 
  geom_histogram(binwidth=1, color="black")

  # on this one we see two "local modes"

# Only transforming the axis (better because we see the actual values instead!)

gapminder %>% 
  filter(year==past_year& !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth=1, color="black")+
  scale_x_continuous(trans = "log2")

# Stacking boxplots next to each other

p <- gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN=median)) %>% 
  ggplot(aes(region, dollars_per_day, fill=continent))+
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=90, hjust = 1)) +
  xlab("")+
  scale_y_continuous(trans="log2")+
  ylab("Dollars per day (log2 scale)")
  
p

# Comparing Distributions

west <-  c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

past_year <- 1970
present_year <- 2010

gapminder %>% 
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>% 
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color="black")+
  scale_x_continuous(trans = "log2")+
  facet_grid(year ~group)

# Making sure we use only countries with data for both years (note that there suposedly is a better way than this)

country_list_1 <-  gapminder %>% 
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country

country_list_2 <-  gapminder %>% 
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country

country_list <- intersect(country_list_1, country_list_2)

gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color="black")+
  scale_x_continuous(trans = "log2")+
  facet_grid(year ~group)

# Boxplots for 2010 now

p <- gapminder %>% 
  filter(year %in% c(past_year,present_year), country %in% country_list) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN=median)) %>% 
  ggplot()+
  theme(axis.text.x=element_text(angle=90, hjust = 1)) +
  xlab("")+
  scale_y_continuous(trans="log2")+
  ylab("Dollars per day (log2 scale)")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year~.)

# This is great, but it's hard to compare still... so instead of faceting we will plot together (make sure P is de)

p <- gapminder %>% 
  filter(year %in% c(past_year,present_year), country %in% country_list) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN=median)) %>% 
  ggplot()+
  theme(axis.text.x=element_text(angle=90, hjust = 1)) +
  xlab("")+
  scale_y_continuous(trans="log2")+
  ylab("Dollars per day (log2 scale)")

p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year))) 

# Let's use smooth density plots to check if the gap between rich and poor is closing

p <- gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%   
  ggplot(aes(dollars_per_day, y= ..count.., fill =group)) +  #The ..count.. is to make it clear developing has more datapoints
  scale_x_continuous(trans = "log2")

p+geom_density(alpha=0.2, bw=0.75) + facet_grid(year ~.) # BW argument controls smoothness

# We can show regions separately

gapminder <-  gapminder %>% 
  mutate(group = case_when(
    .$region %in% west ~"West",
    .$region %in% c("Eastern Asia","South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

gapminder <- gapminder %>% 
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

p <- gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>% 
  mutate(weight=population/sum(population)*2) %>% 
  ungroup() %>% 
  ggplot(aes(dollars_per_day, fill =group, weight = weight)) +  
  scale_x_continuous(trans = "log2")

p+geom_density(alpha=0.2, bw=0.75, position = "stack") + facet_grid(year ~.) 

# Let us now define more regions

gapminder <-  gapminder %>% 
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia","South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~"Pacific Islands"))

surv_income <- gapminder %>% 
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality), !is.na(group)) %>% 
  group_by(group) %>% 
  summarise(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

# we use the logit transformation to highlight the differences in numbers that are very close to 1.
surv_income %>%  ggplot(aes(income, infant_survival_rate, label=group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150))+
  scale_y_continuous(trans = "logit", limit = c(0.875,0.9981), breaks = c(.85,.90,.95,.99,.995,.998))+
  geom_label(size=3, show.legend = FALSE)

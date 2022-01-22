# Chapter 7 Data Visualization

# ====================
# Exercise 7.1
# ====================
library(ggplot2)
library(tidyverse)

d <- mpg  %>%
  group_by(class) %>%
  summarise(n = n())


 ggplot(data = d) +
   geom_bar(mapping = aes(x = reorder(class, -n), y = n), 
      stat = "identity")
# ====================
# END
# ====================



# ====================
# Exercise 7.2
# ====================
# ALTERNATIVE ANSWER: using geom_bin_2d()
# ggplot(data = mpg, aes(x = manufacturer, y = class)) +
#   geom_bin_2d() +
#   scale_fill_gradient(low = "#FFFFFF",
#                       high = "#414396") +
#   theme(axis.text.x = element_text(angle=-90)) 
  

#convert to count of manufacturer and class
mydata_new <- mpg %>% group_by(manufacturer, class) %>% 
  summarise(n = n())

#plot
ggplot(data = mydata_new, aes(x = manufacturer, y = class)) +
  geom_tile(aes(fill = n), color = "white") +
  scale_fill_gradient(low = "#FFFFFF",
                      high = "#414396") +
  geom_text(aes(label = n), 
            color = "black") +
  theme(axis.text.x = element_text(angle=-90)) 
# ====================
# END
# ====================



# ====================
# Exercise 7.3
# ====================
## Method 1
ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point()
# This method will automatically color the plot according to 
# the variable "drv", which includes 3 types:
# f = front-wheel drive, r = rear wheel drive, 4 = 4wd.
# The color will also be different and we have a description box 
# of the color's according type


## Method 2
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "steelblue")
# This method colors the plot based on the color we choose. 
# Unlike method 1, we do not have separate colors for different factors.
# All the data points are colored in steelblue.
# ====================
# END
# ====================



# ====================
# Exercise 7.4
# ====================
ggplot(data = mpg, aes(x = manufacturer, y = class)) +
  geom_count(aes(color = ..n.., size = ..n..)) + 
  theme(axis.text.x = element_text(angle=-90)) +
  guides(color = 'legend') +
  scale_color_gradient(low = "yellow", high = "red")
# ====================
# END
# ====================



# ====================
# Exercise 7.5
# ====================
iris_d <- iris

ggplot(data = iris_d, 
       aes(x = Sepal.Length, y = Petal.Length, color = Species, shape = Species)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Relationship between Sepal Length and Petal Length")
  
# ====================
# END
# ====================



# ====================
# Exercise 7.6
# ====================
ggplot(data = iris_d, 
       aes(x = Species, y = Petal.Width, color = Species, fill = Species)) + 
  geom_boxplot(outlier.size = 2, notch = TRUE, color = "black") 
# ====================
# END
# ====================



# ====================
# Exercise 7.7
# ====================
iris_pivot <-
  iris_d %>%
  pivot_longer(
               cols = ends_with("Width"),
               names_to = "Width.Types",
               values_to = "Values") 

iris_pivot = subset(iris_pivot, select = -c(1:2))

iris_pivot

ggplot(data = iris_pivot, 
       aes(x = Species, y = Values, color = Width.Types, fill = Width.Types)) + 
  geom_boxplot(outlier.size = 1, notch = FALSE, color = "black") 

# ====================
# END
# ====================



# ====================
# Exercise 7.8
# ====================
setwd("~/Working_Directory")
covid19 = read.csv(
  file = 'demo_data/data-covid19.csv', 
  stringsAsFactors = T)
covid19
# ====================
# END
# ====================



# ====================
# Exercise 7.9
# ====================
library(lubridate)
library(scales)
library(stringr)
require(zoo)
#load in data
confirmed_country = 
  subset(covid19, select = c(2,4,6,7))

#rename column
rename(confirmed_country, country = `Country.Region`) -> confirmed_country

#transform datatype to date
confirmed_country$ObservationDate = mdy(confirmed_country$ObservationDate)

#filter countries
confirmed_country7 <- 
  confirmed_country %>%
  filter(country == "Taiwan"| 
           country == "Japan"|
           country == "US"|
           country == "UK"|
           country == "Germany"|
           country == "Netherlands"|
           country == "Mainland China")


#sum up each day's confirmed cases
confirmed_country7 = confirmed_country7 %>%
  group_by(ObservationDate, country) %>%
  summarize(Confirmed_day = sum(Confirmed))

#line plot
ggplot(data = confirmed_country7, aes(x= ObservationDate, y = Confirmed_day, color = country)) +
  geom_line() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Number of COVID-19 Confirmed Cases By Month") +
  xlab("Month-Year") + 
  ylab("Confirmed Case Number") +
  scale_x_date(date_breaks = "month", labels = date_format("%b-%y")) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))

 
# ====================
# END
# ====================



# ====================
# Exercise 7.10
# ====================
confirmed_Total <- confirmed_country %>%
  filter(ObservationDate == max(ObservationDate)) %>%
  group_by(country) %>%
  summarise(Confirmed = sum(Confirmed))

confirmed_Total <- confirmed_Total[with(confirmed_Total, order(-Confirmed)),]
confirmed_Total <- confirmed_Total[1:10,]


ggplot(confirmed_Total, aes(x = reorder(country, -Confirmed), y = Confirmed)) +
  geom_col(aes(fill = country)) +
  xlab("Country") + 
  ylab("Number of Confirmed Cases") +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme(axis.text.x = element_text(angle=90))
# ====================
# END
# ====================



# ====================
# Exercise 7.11
# ====================
death_rate <- confirmed_country %>%
  filter(ObservationDate == max(ObservationDate)) %>%
  group_by(country) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths)) %>%
  mutate(death_r = Deaths/Confirmed) 
  

death_rate <- death_rate[with(death_rate, order(-death_r)),]
death_rate <- death_rate[1:10,]


ggplot(death_rate, aes(x = reorder(country, -death_r), y = death_r)) +
  geom_col(aes(fill = country)) +
  xlab("Country") + 
  ylab("Death Rate") +
  theme(axis.text.x = element_text(angle=90))
# ====================
# END
# ====================



# ====================
# Exercise 7.12
# ====================
library(maps)

confirmed_T <- confirmed_country %>%
  filter(ObservationDate == max(ObservationDate)) %>%
  group_by(country) %>%
  summarise(Confirmed = sum(Confirmed))

country <- confirmed_T$country
as.character(country) -> country
country <- replace(country, country == "Mainland China", "China")
country <- replace(country, country == "US", "USA")
confirmed_T$region <- country

confirmed_T <- confirmed_T %>%
  mutate(C_log = log10(Confirmed))



world_map <- map_data("world")
confirmed_map <- left_join(world_map, confirmed_T, by = "region")

m_labels = as.character(c(1, 10, 100, "1,000", "10,000", "1,000,000", "10,000,000", "100,000,000"))

ggplot(confirmed_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = C_log))+
  scale_fill_gradient2(name = "Confirmed Number\n(Log10-scaled)",
                       breaks = c(1,2,3,4,5,6,7,8),
                       labels = m_labels)+ 
  labs(title = "Outbreak of COVID 19")+
  xlab("")+
  ylab("")
# ====================
# END
# ====================

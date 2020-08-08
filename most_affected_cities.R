library(jsonlite)
library(dplyr)
library(ggplot2)
library(forcats)
library(gganimate)

raw_data <- readLines("https://api.covid19india.org/raw_data.json")
data <- as.data.frame(fromJSON(raw_data))

city_wise_data <- data %>% group_by(raw_data.detectedstate, raw_data.detectedcity, raw_data.dateannounced) %>% summarise(count=n())
top_10_cities_list <- as.list(city_wise_data[city_wise_data$raw_data.detectedcity != "" ,] %>% group_by(raw_data.detectedcity) %>% summarise(Cumulative.Sum=sum(count))   %>% arrange(desc(Cumulative.Sum)) %>% top_n(10) %>% select(raw_data.detectedcity))

city_wise_data <- city_wise_data %>% arrange(as.Date(raw_data.dateannounced, "%d/%m/%Y"))
View(city_wise_data)
print(top_10_cities_list)
city_wise_data <- city_wise_data %>% group_by(raw_data.detectedcity) %>% mutate(Cumulative.Sum=cumsum(count))

names(city_wise_data) <- c("State", "city", "Date", "Count", "Cumulative.Sum")

#Cumulative Plot
g <- ggplot(city_wise_data[city_wise_data$city %in% top_10_cities_list$raw_data.detectedcity,], aes(x=fct_inorder(Date), y=as.integer(Cumulative.Sum), group=city )) + 
  geom_smooth(aes(color=city), size=1.5) + 
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("Most Affected Cities due to C0VID-19") + 
  theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5))
  #+ transition_time(as.Date(Date))
#ggsave("Most_affected_cities.gif", width = 10, height = 8, dpi = 150, units = "in",  device="gif", path="~/Desktop")
print(g)

#g + transition_time(Date)
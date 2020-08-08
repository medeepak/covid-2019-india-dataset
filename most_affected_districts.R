library(jsonlite)
library(dplyr)
library(ggplot2)
library(forcats)
library(gganimate)
library(ggrepel)

raw_data <- readLines("https://api.covid19india.org/raw_data.json")
data <- as.data.frame(fromJSON(raw_data))

district_wise_data <- data %>% group_by(raw_data.detectedstate, raw_data.detecteddistrict, raw_data.dateannounced) %>% summarise(count=n())
top_10_districts_list <- as.list(district_wise_data[district_wise_data$raw_data.detecteddistrict != "" ,] %>% group_by(raw_data.detecteddistrict) %>% summarise(Cumulative.Sum=sum(count))   %>% arrange(desc(Cumulative.Sum)) %>% top_n(10) %>% select(raw_data.detecteddistrict))

district_wise_data <- district_wise_data %>% arrange(as.Date(raw_data.dateannounced, "%d/%m/%Y"))
district_wise_data <- district_wise_data %>% group_by(raw_data.detecteddistrict) %>% mutate(Cumulative.Sum=cumsum(count))

names(district_wise_data) <- c("State", "District", "Date", "Count", "Cumulative.Sum")

#Cumulative Plot
g <- ggplot(district_wise_data[district_wise_data$District %in% top_10_districts_list$raw_data.detecteddistrict,], aes(x=fct_inorder(Date), y=as.integer(Cumulative.Sum), group=District )) + 
  geom_smooth(aes(color=District), size=1.5) + 
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("Most Affected Cities due to C0VID-19") + 
  theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5))
#ggsave("Most_affected_districts.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")
#print(g)

delhi_data <- district_wise_data %>% subset(State=="Delhi")
delhi_data$row_count <- seq.int(nrow(delhi_data))
View(delhi_data)
g <- ggplot(delhi_data, aes(x=row_count, y=as.integer(Cumulative.Sum), group=State )) +
  geom_line() +
  #geom_segment(aes(xend = 46, yend = as.integer(Cumulative.Sum)), linetype = 2, colour = 'grey') +
  geom_point(size=1.5) +
  geom_label(aes(label="Delhi")) +
  scale_x_discrete()
a <- g+transition_reveal(delhi_data$row_count)
print(a)
  
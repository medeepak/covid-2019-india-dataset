library(jsonlite)
library(dplyr)
library(ggplot2)
library(forcats)

raw_data <- readLines("https://api.covid19india.org/raw_data.json")
data <- as.data.frame(fromJSON(raw_data))

district_wise_data <- data %>% group_by(raw_data.detectedstate, raw_data.detecteddistrict, raw_data.dateannounced) %>% summarise(count=n())
top_10_districts_list <- as.list(district_wise_data[district_wise_data$raw_data.detecteddistrict != "" & district_wise_data$raw_data.detectedstate== "Tamil Nadu",] %>% group_by(raw_data.detecteddistrict) %>% summarise(Cumulative.Sum=sum(count))   %>% arrange(desc(Cumulative.Sum)) %>% top_n(10) %>% select(raw_data.detecteddistrict))

district_wise_data <- district_wise_data %>% arrange(as.Date(raw_data.dateannounced, "%d/%m/%Y"))
district_wise_data <- district_wise_data %>% group_by(raw_data.detecteddistrict) %>% mutate(Cumulative.Sum=cumsum(count))

names(district_wise_data) <- c("State", "District", "Date", "Count", "Cumulative.Sum")

#Cumulative Plot
ggplot(district_wise_data[district_wise_data$District %in% top_10_districts_list$raw_data.detecteddistrict,], aes(x=fct_inorder(Date), y=as.integer(Cumulative.Sum), group=District )) + 
  geom_line(aes(color=District), size=1.5) + 
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("Most Affected Districts in Tamil Nadu due to C0VID-19") + 
  theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5))
ggsave("Most_affected_tamilnadu_districts.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")
#print(g)
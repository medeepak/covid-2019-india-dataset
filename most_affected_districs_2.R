library(jsonlite)
library(dplyr)
library(ggplot2)
library(forcats)
library(gganimate)
library(ggrepel)
library(zoo)
library(ggimage)
library(lubridate)

raw_data <- readLines("https://api.covid19india.org/raw_data1.json")
data <- as.data.frame(fromJSON(raw_data))
raw_data <- readLines("https://api.covid19india.org/raw_data2.json")
data <- rbind(data,as.data.frame(fromJSON(raw_data)))

raw_data <- readLines("https://api.covid19india.org/raw_data3.json")
data2 <- as.data.frame(fromJSON(raw_data))

break_from_loop <<- FALSE 
for (iter in seq(4,100)) {
  
  raw_data_url <- paste0("https://api.covid19india.org/raw_data",iter,".json")
  print(raw_data_url)
  tryCatch(raw_data <- readLines(raw_data_url),
           error = function(e) {break_from_loop <<- TRUE})
  
  if(break_from_loop){
    break
  }
  data2 <- rbind(data2,as.data.frame(fromJSON(raw_data)))
  
}

district_wise_data <- as.data.frame(data %>% group_by(raw_data.detecteddistrict, raw_data.dateannounced) %>% subset(raw_data.detecteddistrict !="") %>% summarise(count=n()))
temp_df <- as.data.frame(data %>% group_by(raw_data.detectedstate, raw_data.dateannounced) %>% subset(raw_data.detectedstate =="Delhi") %>% summarise(count=n()))
names(temp_df) <- c("raw_data.detecteddistrict", "raw_data.dateannounced", "count")
district_wise_data <- rbind(district_wise_data, temp_df)

district_wise_data_2 <- as.data.frame(data2 %>% group_by(raw_data.detecteddistrict, raw_data.dateannounced) %>% subset(raw_data.detecteddistrict !="") %>% subset(raw_data.currentstatus == "Hospitalized") %>% summarise(count=sum(as.integer(raw_data.numcases))))

temp_df <- as.data.frame(data2 %>% group_by(raw_data.detectedstate, raw_data.dateannounced) %>% subset(raw_data.currentstatus == "Hospitalized") %>% subset(raw_data.detectedstate =="Delhi") %>% summarise(count=sum(as.integer(raw_data.numcases))))
names(temp_df) <- c("raw_data.detecteddistrict", "raw_data.dateannounced", "count")
district_wise_data_2 <- rbind(district_wise_data_2, temp_df)
district_wise_data <- rbind(district_wise_data, district_wise_data_2)

top_3_hotspots <- as.list(c("Chennai", "Delhi", "Mumbai", "Hyderabad", "Bengaluru Urban"))

next_12_districts_list <- as.list(district_wise_data[district_wise_data$raw_data.detecteddistrict != "" & district_wise_data$raw_data.dateannounced == format(Sys.Date()-2, "%d/%m/%Y") ,] %>% group_by(raw_data.detecteddistrict) %>% summarise(Cumulative.Sum=sum(count))   %>% arrange(desc(Cumulative.Sum)) %>% top_n(15) %>% top_n(-12) %>% select(raw_data.detecteddistrict))

district_wise_data <- district_wise_data %>% arrange(as.Date(raw_data.dateannounced, "%d/%m/%Y"))
district_wise_data <- district_wise_data %>% group_by(raw_data.detecteddistrict) %>% mutate(Cumulative.Sum=cumsum(count))
names(district_wise_data) <- c("district", "Date", "Count", "Cumulative.Sum")
district_wise_data <- district_wise_data %>% group_by(district) %>% mutate(Rolling.Average=rollapply(Count,7,mean,align='right',fill=NA))
district_wise_data <- district_wise_data %>% group_by(district) %>% mutate(Rolling.Average.7.Days=rollapply(Count,7,mean,align='right',fill=NA))
district_wise_data <- district_wise_data %>% group_by(district) %>% mutate(Growth.Rate=(Cumulative.Sum-lag(Cumulative.Sum))*100/lag(Cumulative.Sum))
district_wise_data <- district_wise_data %>% group_by(district) %>%  mutate(Seven.day.growth.rate=rollapply(Growth.Rate,7,mean,align='right',fill=NA))

district_wise_data$Date <- as.Date(district_wise_data$Date, "%d/%m/%Y") 

top_3_hotspots_data <- district_wise_data[district_wise_data$district %in% top_3_hotspots,]
next_12_districts_data <- district_wise_data[district_wise_data$district %in% next_12_districts_list$raw_data.detecteddistrict,]

#Cumulative Plot

g <- ggplot(top_3_hotspots_data, aes(x=Date, y=as.integer(Cumulative.Sum), group=district )) + 
  geom_smooth(aes(color=district), size=1.5) + 
  #scale_x_discrete() +
  scale_x_date() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("Other highly districts due to COVID-19") + 
  theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())
ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Top_3_hotspots.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

g <- ggplot(next_12_districts_data, aes(x=Date, y=as.integer(Cumulative.Sum), group=district )) + 
  geom_smooth(aes(color=district), size=1.5) + 
  #scale_x_discrete() +
  scale_x_date() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("Other highly districts due to COVID-19") + 
  theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())
ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Other_most_affected_districts.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")
#print(g)

next_12_districts_data <- next_12_districts_data %>% subset(Cumulative.Sum>100)
next_12_districts_data <- next_12_districts_data %>% group_by(district) %>% mutate(Row.Count=as.integer(as.Date(Date,"%d/%m/%Y") - as.Date("2020-03-21")))
#View(next_12_districts_data)
g <- ggplot(next_12_districts_data, aes(x=as.Date(Date,"%d/%m/%Y"), y=as.integer(Cumulative.Sum), group=district, color=district )) +
  geom_line() +
  scale_x_discrete() +
  ylab("Confirmed cases") +
  xlab("") +
  ggtitle("Trend of Confirmed Cases since 21st March in Most Affected districts") +
  theme(text=element_text(family="Helvetica", size=12) , plot.title = element_text(hjust = 0.5),   legend.position = "off") + 
  labs(title = 'Coronavirus confirmed cases since reaching 100') +
  #ylim(0,14000) + 
  geom_label_repel(aes(label=district), nudge_x = 3, direction = "y", hjust = 1, segment.size = 0.2, text=element_text(family="Roboto", size=8), data=head(tail(next_12_districts_data,20),10))

ggsave("ConfirmedCasesdistrictWise.png", width = 14, height = 10, dpi = 300, units = "in",  device="png", path="~/Desktop")
#a <- g+transition_reveal(as.Date(Date,"%d/%m/%Y")) 
#anim <- animate(a, fps=5)
#anim <- ggbackground(anim, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
#anim_save("~/Desktop/Most_affected_districts_anim.gif",anim, width = 14, height = 10)

g <- ggplot(next_12_districts_data, aes(x=Date, y=Rolling.Average, group=district)) + 
  scale_x_date() +
  geom_line(aes(color=district), size=1.5) + 
  ylab("Count") +
  ggtitle("New cases in hotspots (7 day rolling average)") +
  facet_wrap(~district,nrow=4, scales = "free_y") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.25, size=18), panel.grid.major = element_blank(), legend.position = "off", plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.25), text=element_text(family="Roboto", size=12, face="bold")) + 
  theme(axis.text.y = element_text(vjust = -0.25), text=element_text(family="Roboto", size=12, face="bold")) +
  xlab("Date") 

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("districtwise_confirmed_cases_rolling_average.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

g <- ggplot(top_3_hotspots_data, aes(x=Date, y=Rolling.Average.7.Days, group=district)) + 
  scale_x_date() +
  geom_line(aes(color=district), size=1.5) + 
  ylab("Count") +
  ggtitle("New cases in hotspots (7 day rolling average)") +
  #facet_wrap(~district,ncol=4, scales = "free_y") + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  vjust = -0.25, 
                                  size=18), 
        panel.grid.major = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = -0.25), 
        text=element_text(family="Roboto", 
                          size=12, 
                          face="bold")) + 
  theme(axis.text.y = element_text(vjust = -0.25), 
        text=element_text(family="Roboto", 
                          size=12, 
                          face="bold"),
        legend.position = "off") +
  facet_wrap(~district,nrow=2, scales = "free_y") + 
  xlab("Date") 

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Top_5_hotspots_7_day_rolling_average.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

g <- ggplot(top_3_hotspots_data, aes(x=Date, y=Seven.day.growth.rate, group=district)) + 
  scale_x_date() +
  geom_line(aes(color=district), size=1.5) + 
  ylab("Growth rate") +
  lims(x= c(Sys.Date() - 20, NA), y = c(0, 15)) +
  ggtitle("Growth rate in hotspots (7 day rolling average)") +
  #facet_wrap(~district,ncol=4, scales = "free_y") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.25, size=18), panel.grid.major = element_blank(), legend.position = "bottom", legend.title = element_blank(), plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.25), text=element_text(family="Roboto", size=12, face="bold")) + 
  theme(axis.text.y = element_text(vjust = -0.25), text=element_text(family="Roboto", size=12, face="bold")) +
  xlab("Date") 

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Top_3_hotspots_7_day_growth-rate.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

g <- ggplot(next_12_districts_data, aes(x=Date, y=Rolling.Average.7.Days, group=district)) + 
  scale_x_date() +
  geom_line(aes(color=district), size=1.5) + 
  ylab("Count") +
  ggtitle("New cases in other hotspots (7 day rolling average)") +
  facet_wrap(~district,nrow=4, scales = "free_y") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.25, size=18), panel.grid.major = element_blank(), legend.position = "off", plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.25), text=element_text(family="Roboto", size=12, face="bold")) + 
  theme(axis.text.y = element_text(vjust = -0.25), text=element_text(family="Roboto", size=12, face="bold")) +
  xlab("Date") 

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Other_districts_7_day_rolling_average.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")


top_3_hotspots_data <- top_3_hotspots_data %>%
                       mutate(Day=wday(Date))

top_3_hotspots_weekly_data <- top_3_hotspots_data %>%
                              subset(Day==wday(Sys.Date()-1))
top_3_hotspots_weekly_data <- top_3_hotspots_weekly_data %>%
                              group_by(district) %>%
                              mutate(Weekly.Increase=rollapply(Cumulative.Sum,2,diff,align='right',fill=NA))
top_3_hotspots_weekly_data <- top_3_hotspots_weekly_data %>%
                              group_by(district) %>%
                              mutate(Weekly.Average.Growth.Rate=(Cumulative.Sum-lag(Cumulative.Sum))*100/(7*lag(Cumulative.Sum)))
top_3_hotspots_weekly_data <- top_3_hotspots_weekly_data %>%
                              group_by(district) %>%
                              mutate(Case.Doubling.Time=(0.30103/log10(1+(Weekly.Average.Growth.Rate/100))))

top_3_hotspots_weekly_data <- top_3_hotspots_weekly_data %>%
                              subset(Date >= Sys.Date()-30)

g <- ggplot(top_3_hotspots_weekly_data, aes(x=Date,y=Weekly.Increase, fill=district)) +
     geom_bar(stat="identity", width=2,position = "dodge") +
     scale_x_date() +
     ylab("Newly Confirmed Cases") + 
     xlab("Week Ending In") + 
     ggtitle("Confirmed Cases in Hotspots") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.25, size=18), panel.grid.major = element_blank(), legend.position = "bottom", plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.5), text=element_text(family="Roboto", size=14, face="bold")) + 
  theme(axis.text.y = element_text(vjust = -0.5), text=element_text(family="Roboto", size=14, face="bold"))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Hotspots-Confirmed_Cases_Comparison.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

g <- ggplot(top_3_hotspots_weekly_data, aes(x=Date,y=Case.Doubling.Time, fill=district)) +
  geom_bar(stat="identity", width=2,position = "dodge") +
  scale_x_date() +
  ylab("Doubling Time (in Days)") + 
  xlab("Week Ending In") + 
  ggtitle("Doubling Time in Hotspots") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -0.25, size=18), panel.grid.major = element_blank(), legend.position = "bottom", plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.5), text=element_text(family="Roboto", size=14, face="bold")) + 
  theme(axis.text.y = element_text(vjust = -0.5), text=element_text(family="Roboto", size=14, face="bold"))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Hotspots_Case_Doubling_Time.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")


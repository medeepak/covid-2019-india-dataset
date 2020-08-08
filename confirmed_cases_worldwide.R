library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
require(ggrepel)
library(directlabels)
library(jpeg)
library(grid)
library(ggpubr)
library(ggimage)
library(zoo)

setwd("/Users/deepaksr/Documents/datasciencecourse/github_root/COVID-19/")
system("git pull")
confirmed_cases <- fread("/Users/deepaksr/Documents/datasciencecourse/github_root/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
img <- jpeg::readJPEG("~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

cumulative_confirmed_cases_by_country <- confirmed_cases[,lapply(.SD, sum, na.rm=TRUE), by="Country/Region", .SDcols=-sd.cols]
cumulative_confirmed_cases_by_country_df <- as.data.frame(cumulative_confirmed_cases_by_country)
last_col_id <- ncol(cumulative_confirmed_cases_by_country_df)
last_col_name <- colnames(cumulative_confirmed_cases_by_country_df)[ncol(cumulative_confirmed_cases_by_country_df)]
most_affected_countries <- cumulative_confirmed_cases_by_country_df[,c(1,last_col_id)]
most_affected_countries$latest <-  most_affected_countries[,2]
most_affected_countries <- most_affected_countries %>% arrange(desc(as.integer((latest)))) %>% top_n(20)

cumulative_confirmed_cases_by_country_df <- melt(cumulative_confirmed_cases_by_country_df)
names(cumulative_confirmed_cases_by_country_df) <- c('Country', 'Date','Confirmed.Cases')

countries_to_plot <- cumulative_confirmed_cases_by_country_df %>% subset(cumulative_confirmed_cases_by_country_df$Count %in% most_affected_countries$`Country/Region`)
countries_to_plot <- countries_to_plot %>% subset(Confirmed.Cases >= 1000)
countries_to_plot <- countries_to_plot %>% arrange(Confirmed.Cases) %>% group_by(Country) %>%  mutate(row.number=row_number())
countries_to_plot <- countries_to_plot %>% mutate(color=ifelse(Country=="India",'red', "grey"))
countries_to_plot <- countries_to_plot %>% mutate(size=ifelse(Country=="India",2, 1)) %>% arrange(row.number)

g <- ggplot(countries_to_plot, aes(x=row.number, y=Confirmed.Cases, group=Country)) +
  #geom_path(color=countries_to_plot$color, size=countries_to_plot$size) +
  geom_path(data=countries_to_plot[countries_to_plot$Country!="India",], color="gray", size=1) +
  geom_path(data=countries_to_plot[countries_to_plot$Country=="India",],color="red", size=2) +
  #scale_y_log10(name="Count (Log Scale)", labels = scales::comma) +
  scale_y_sqrt(name="Count (SQRT Scale)", labels = scales::comma, breaks=c(10000,50000,100000,500000,1000000)) +
  xlab("Days since reaching 1000 cases") +
  ggtitle("Confirmed COVID-19 Cases Word Wide vs India") + 
  theme(text=element_text(family="Roboto", size=14, face="bold") , plot.title = element_text(hjust = 0.5), legend.position = "off") +
  #geom_dl(aes(label = Country), method = list(dl.combine("last.points"), cex = 0.8))
  geom_label_repel(data = . %>% mutate(label=ifelse(Date==last_col_name,Country,"")),
                   aes(label = label))

#ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Countrywise_confirmed_cases_vs_India.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

g <- ggplot(countries_to_plot, aes(x=row.number, y=Confirmed.Cases, group=Country)) +
  #geom_path(color=countries_to_plot$color, size=countries_to_plot$size) +
  geom_path(data=countries_to_plot[countries_to_plot$Country!="India",], color="gray", size=1) +
  geom_path(data=countries_to_plot[countries_to_plot$Country=="India",],color="red", size=2) +
  scale_y_log10(name="Count (Log Scale)", labels = scales::comma, breaks=c(10000,50000,100000,500000,1000000)) +
  xlab("Days since reaching 1000 cases") +
  ggtitle("Confirmed COVID-19 Cases Word Wide vs India") + 
  theme(text=element_text(family="Roboto", size=14, face="bold") , plot.title = element_text(hjust = 0.5), legend.position = "off") +
  #geom_dl(aes(label = Country), method = list(dl.combine("last.points"), cex = 0.8))
  geom_label_repel(data = . %>% mutate(label=ifelse(Date==last_col_name,Country,"")),
                   aes(label = label))

#ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Countrywise_confirmed_cases_vs_India_log.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

g <- ggplot(countries_to_plot, aes(x=row.number, y=Confirmed.Cases, group=Country)) +
  #geom_path(aes(color=Country)) +
  geom_path(data=countries_to_plot[countries_to_plot$Country!="India",], color="gray", size=1) +
  geom_path(data=countries_to_plot[countries_to_plot$Country=="India",],color="red", size=2) +
  #scale_y_log10(name="Count (Log Scale)", labels = scales::comma) +
  scale_y_sqrt(name="Count (SQRT Scale)", labels = scales::comma, breaks=c(10000,50000,100000,500000,1000000)) +
  xlab("Days since reaching 1000 cases") +
  ggtitle("Confirmed COVID-19 Cases Word Wide") + 
  theme(text=element_text(family="Roboto", size=14, face="bold") , plot.title = element_text(hjust = 0.5), legend.position = "off") +
  geom_label_repel(data = . %>% mutate(label=ifelse(Date==last_col_name,Country,"")),
                   aes(label = label))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Countrywise_confirmed_cases.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

countries_to_plot <- countries_to_plot %>% mutate(growth=(Confirmed.Cases-lag(Confirmed.Cases))*100/lag(Confirmed.Cases))

g <- ggplot(countries_to_plot, aes(x=row.number, y=growth, group=Country)) +
  #geom_path(color=countries_to_plot$color, size=countries_to_plot$size) +
  geom_path(data=countries_to_plot[countries_to_plot$Country!="India",], color="gray", size=1) +
  geom_path(data=countries_to_plot[countries_to_plot$Country=="India",],color="red", size=2) +
  ggtitle("Growth rate of COVID-19 Cases World Wide") + 
  xlab("Days since reaching 1000 cases") +
  ylab("Growth Rate") +
  xlim(30,100) +
  ylim(0,15) +  
  theme(text=element_text(family="Roboto", size=14, face="bold") , plot.title = element_text(hjust = 0.5), legend.position = "off") +
  geom_label_repel(data = . %>% mutate(label=ifelse(Date==last_col_name,Country,"")),
                   aes(label = label))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Countrywise_growth_rates.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

countries_to_plot <- countries_to_plot %>% mutate(Rolling.average.of.growth.rate=rollapply(growth,7,mean,align='right',fill=NA))

g <- ggplot(countries_to_plot, aes(x=row.number, y=Rolling.average.of.growth.rate, group=Country)) +
  #geom_path(color=countries_to_plot$color, size=countries_to_plot$size) +
  geom_path(data=countries_to_plot[countries_to_plot$Country!="India",], color="gray", size=1) +
  geom_path(data=countries_to_plot[countries_to_plot$Country=="India",],color="red", size=2) +
  ggtitle("7 Day Rolling Average of Growth rate of COVID-19 Cases World Wide") + 
  xlab("Days since reaching 1000 cases") +
  ylab("Growth Rate") +
  xlim(30,200) +
  ylim(0,15) + 
  theme(text=element_text(family="Roboto", size=14, face="bold") , plot.title = element_text(hjust = 0.5), legend.position = "off") +
  geom_label_repel(data = . %>% mutate(label=ifelse(Date==last_col_name,Country,"")),
                   aes(label = label))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Countrywise_growth_rates_7_day_rolling_average.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")


total_cases_world_wide <- cumulative_confirmed_cases_by_country_df %>%
                          group_by(Date) %>%
                          summarize(World.Total.Count=sum(Confirmed.Cases))
total_cases_world_wide$Date <- as.Date(total_cases_world_wide$Date,"%m/%d/%y")

india_data <- countries_to_plot %>%
              subset(Country=="India")
india_data$Date <- as.Date(india_data$Date,"%m/%d/%y")


india_data <- merge(india_data, total_cases_world_wide, by.x="Date", by.y = "Date")
india_data <- india_data %>%
              mutate(Share.Of.India.Cases = round(((Confirmed.Cases*100)/World.Total.Count),2))

g <- ggplot(india_data, aes(x=Date, y=Share.Of.India.Cases)) +
  geom_path(color="red", size=2) +
  ggtitle("Share of India in Confirmed Covid-19 Cases ") + 
  xlab("Date") +
  ylab("Percentage") +
  theme(text=element_text(family="Roboto", size=14, face="bold") , plot.title = element_text(hjust = 0.5), legend.position = "off") +
  geom_vline(xintercept = as.Date("2020-04-14"), linetype = "dashed", color="gray") + 
  geom_text(aes(x = as.Date("2020-04-14"), y = max(Share.Of.India.Cases), label="End of\n Lockdown 1.0"),color="gray2", size=3) +
  geom_vline(xintercept = as.Date("2020-05-03"), linetype = "dashed", color="gray") + 
  geom_text(aes(x = as.Date("2020-05-03"), y = max(Share.Of.India.Cases), label="End of\n Lockdown 2.0"),color="gray2", size=3) + 
  geom_vline(xintercept = as.Date("2020-05-17"), linetype = "dashed", color="gray") + 
  geom_text(aes(x = as.Date("2020-05-17"), y = max(Share.Of.India.Cases), label="End of\n Lockdown 3.0"),color="gray2", size=3) + 
  geom_vline(xintercept = as.Date("2020-05-31"), linetype = "dashed", color="gray") + 
  geom_text(aes(x = as.Date("2020-05-31"), y = max(Share.Of.India.Cases), label="End of\n Lockdown 4.0"),color="gray2", size=3) 

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Share_of_cases_from_India.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

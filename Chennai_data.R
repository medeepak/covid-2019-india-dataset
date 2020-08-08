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
library(pdftools)
library(stringr)
library(jsonlite)
library(tidyr)
library(ggthemes)

Caps <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

chennai_data <- as.data.frame(fromJSON("https://v2-api.sheety.co/be53bea9995480777df56e14adcfd93b/covid19Chennai/cases"))
chennai_data$cases.date <- as.Date(chennai_data$cases.date)
chennai_data$cases.zoneName <- tolower(chennai_data$cases.zoneName)
chennai_data$cases.zoneName <- sapply(chennai_data$cases.zoneName, Caps)

chennai_data <- chennai_data %>%
                          group_by(cases.zoneName) %>%
                          mutate(new_cases=cases.confirmedCases-lag(cases.confirmedCases))

chennai_data <- chennai_data %>%
  group_by(cases.zoneName) %>%
  mutate(daily.growth.rate=round(new_cases*100/lag(cases.confirmedCases),1))

chennai_data <- chennai_data %>%
  group_by(cases.zoneName) %>%
  mutate(seven.day.avg.growth.rate=rollapply(daily.growth.rate,7, mean, align = "right", fill = NA))

chennai_data <- chennai_data %>%
  group_by(cases.zoneName) %>%
  mutate(seven.day.avg.new.cases=rollapply(new_cases,7, mean, align = "right", fill = NA))

chennai_data$new_cases <- replace_na(chennai_data$new_cases, 0)
chennai_data$seven.day.avg.new.cases <- replace_na(chennai_data$seven.day.avg.new.cases, 0)

g <- ggplot(chennai_data, aes(x=cases.date, y=cases.confirmedCases, group=cases.zoneName)) + 
  scale_x_date() +
  geom_line(aes(color=cases.zoneName), size=1.5) +
  scale_y_log10() +
  ylab("Count") +
  ggtitle("Chennai - Zone wise Confirmed Cases") +
  facet_wrap(~cases.zoneName, nrow = 5, scales = "free_y") +
  theme(legend.position = "off")

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Chennai_zonewise_data.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

g <- ggplot(chennai_data[chennai_data$cases.zoneName!= "Other District",], 
            aes(x=cases.date, 
                y=seven.day.avg.new.cases, 
                group=cases.zoneName)) +
     theme(legend.position = "off") + 
     scale_color_brewer(palette="Set1") + 
     scale_fill_brewer(palette="Set1") + 
     #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
     geom_col(aes(y=new_cases,
                  fill='blue', 
                  color='blue')) +
     geom_line(aes(color='black'), 
               size=1.5) +
     scale_x_date() +
     ylab("Count") +
     xlab("") +
    ggtitle("Chennai - Zone wise daily new cases") +
    facet_wrap(~cases.zoneName, nrow = 5, scales = "free_y") + 
    theme_economist() + 
    theme(legend.position = "off", 
          text = element_text(size = 12), 
          plot.title = element_text(hjust = 0.5,
                                    size = 14))

#ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Chennai_zonewise_daily_data.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")
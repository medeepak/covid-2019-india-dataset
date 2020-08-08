library(jsonlite)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(tidyr)
library(directlabels)
library(jpeg)
library(grid)
library(ggpubr)
library(ggimage)
library(zoo)
library(pdftools)
library(stringr)
library(lubridate)

json_data <- readLines("https://raw.githubusercontent.com/datameet/covid19/master/data/icmr_testing_status.json")
data <- as.data.frame(fromJSON(json_data))
data <- data$rows.value

data <- data %>% mutate(daily.tests=rollapply(samples,2,diff,align='right',fill=NA))

data[nrow(data)+1,c(3,9)] <- list('2020-07-28T09:00:00.00+05:30', 528082)
data <- data %>% mutate(row=row_number())

data <- data %>%
        arrange(report_time) %>%
        mutate(Cumulative.Tests=cumsum(replace_na(daily.tests, 0)))

data$report_time <- as.Date(data$report_time)
data$day_of_week <- wday(data$report_time)

data$seven_day_avg <- rollapply(data$daily.tests, 
                                7,
                                mean,
                                align='right',
                                fill=NA)

data$month <- month(data$report_time)
total_tests <- last(data$Cumulative.Tests)

month_wise_tests <- data %>%
                    group_by(month) %>%
                    summarise(monthly_tests=sum(daily.tests)) 

month_wise_tests <- month_wise_tests %>%
                    replace_na(monthly_tests, 0)

month_wise_tests$percentage <- round(month_wise_tests$monthly_tests*100/total_tests,1)
        
g <- ggplot(data, aes(x=`report_time`, y=seven_day_avg)) +
     geom_col(aes(y=daily.tests , fill="blue")) +
     geom_path(aes(colour="black"),
               size=1.5) +
     scale_fill_manual(values = "#56B4E9") +
     ggtitle("Daily Tests performed in India - 7 Day Average") +
     ylab("Daily Tests Performed") +
     scale_x_date() +         
     theme_light() + 
     scale_y_continuous(labels = scales::comma, breaks=seq(0,600000,100000) ) + 
     theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -1, size=16), 
        panel.background = element_blank(),
        legend.position = "none")
ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Tests_performed_in_India.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

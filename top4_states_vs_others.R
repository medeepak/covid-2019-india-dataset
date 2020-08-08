library(jsonlite)
library(dplyr)
library(ggplot2)
library(forcats)
library(gganimate)
library(ggrepel)
library(zoo)
library(ggimage)
library(ggthemes)

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

state_wise_data <- data %>% 
                  group_by(raw_data.detectedstate, raw_data.dateannounced) %>% 
                  subset(raw_data.detectedstate !="") %>% 
                  summarise(Count=n())

state_wise_data_2 <- data2 %>% 
                     group_by(raw_data.detectedstate, raw_data.dateannounced) %>% 
                    subset(raw_data.detectedstate !="" & raw_data.currentstatus=="Hospitalized") %>%                      summarise(Count=sum(as.integer(raw_data.numcases)))
state_wise_data <- rbind(state_wise_data, state_wise_data_2)
state_wise_data <- na.omit(state_wise_data)

state_district_wise_data <- data %>% 
    group_by(raw_data.detectedstate, raw_data.detecteddistrict, raw_data.dateannounced) %>% 
    subset(raw_data.detectedstate !="") %>% 
    summarise(Count=n())

state_district_wise_data_2 <- data2 %>% 
    group_by(raw_data.detectedstate, raw_data.detecteddistrict, raw_data.dateannounced) %>% 
    subset(raw_data.detectedstate !="" & raw_data.currentstatus=="Hospitalized") %>%                      summarise(Count=sum(as.integer(raw_data.numcases)))
state_district_wise_data <- rbind(state_district_wise_data, state_district_wise_data_2)
state_district_wise_data <- na.omit(state_district_wise_data)

states_of_interest <- c("Maharashtra","Tamil Nadu", "Karnataka", "Andhra Pradesh")
state_wise_data <- state_wise_data %>% 
                   mutate(Region=ifelse(raw_data.detectedstate %in% states_of_interest, "MH,TN,KA","Others"))


state_wise_data$Date <- as.Date(state_wise_data$raw_data.dateannounced, "%d/%m/%Y")

daily_count_by_region <- state_wise_data[,-1:-2] %>%
                   select(c(Region,Date,Count)) %>%
                   arrange(Date) %>%
                   group_by(Region,Date) %>%
                   summarise(Daily.Count=sum(Count))

daily_count_by_region <- daily_count_by_region %>%
    subset(Date > '2020-03-01')

daily_count_by_region <- daily_count_by_region %>%
                   arrange(Date) %>%
                   group_by(Region) %>%   
                   mutate(Cumulative.Cases=cumsum(Daily.Count))

daily_count <- daily_count_by_region[,-1] %>%
                    select(c("Date","Daily.Count")) %>%
                    group_by(Date) %>%
                    mutate(Total.Daily.Count=sum(Daily.Count)) %>%
                    select(-c("Daily.Count"))
daily_count <- unique(daily_count)
    
daily_count_by_region <- merge(daily_count_by_region, daily_count, by.x = "Date", by.y = "Date")
daily_count_by_region <- unique(daily_count_by_region)

daily_count_by_region <- daily_count_by_region %>%
                            mutate(Share.Of.Cases=round(Daily.Count*100/Total.Daily.Count,1))

daily_count_by_region <- daily_count_by_region %>%
                         group_by(Region) %>%
                         mutate(Seven.Day.Average.Share.Of.Cases = rollapply(Share.Of.Cases, 7, mean, align = "right", fill = NA))


g <- ggplot(daily_count_by_region, aes(x=Date, 
                                 y=Daily.Count, 
                                 group=Region, 
                                 color=Region )) + 
    geom_path(aes(color=Region), 
              size=1.5) + 
    ylab("Count") +
    scale_x_date() +
    ggtitle("Daily case counts") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=18), 
          panel.grid.major = element_blank(), 
          legend.position = "bottom", 
          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          axis.text.x = element_text(angle = 90, vjust = -0.25), 
          text=element_text(family="Roboto", size=12, face="bold"), 
          axis.text.y = element_text(vjust = -0.25)) +
    xlab("Date")

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Hotspots_vs_Others_daily.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")


g <- ggplot(daily_count_by_region, aes(x=Date, 
                                       y=Cumulative.Cases, 
                                       group=Region, 
                                       color=Region )) + 
    geom_path(aes(color=Region), 
              size=1.5) + 
    ylab("Count") +
    scale_x_date() +
    ggtitle("Cumulative case counts") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=18), 
          panel.grid.major = element_blank(), 
          legend.position = "bottom", 
          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          axis.text.x = element_text(angle = 90, vjust = -0.25), 
          text=element_text(family="Roboto", size=12, face="bold"), 
          axis.text.y = element_text(vjust = -0.25)) +
    xlab("Date")

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Hotspots_vs_Others_cumulative.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

g <- ggplot(daily_count_by_region, aes(x=Date, 
                                       y=Cumulative.Cases, 
                                       group=Region, 
                                       color=Region )) + 
    geom_path(aes(color=Region), 
              size=1.5) + 
    ylab("Count") +
    scale_x_date() +
    ggtitle("Cumulative case counts") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=18), 
          panel.grid.major = element_blank(), 
          legend.position = "bottom", 
          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          axis.text.x = element_text(angle = 90, vjust = -0.25), 
          text=element_text(family="Roboto", size=12, face="bold"), 
          axis.text.y = element_text(vjust = -0.25)) +
    xlab("Date") + 
    scale_y_log10()

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Hotspots_vs_Others_cumulative_log.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

g <- ggplot(daily_count_by_region, aes(x=Date, 
                                       y=Seven.Day.Average.Share.Of.Cases, 
                                       group=Region, 
                                       color=Region )) + 
    geom_path(aes(color=Region), 
              size=1.5) + 
    ylab("Percentage of Cases") +
    scale_x_date() +
    ggtitle("Share of Cases in Hotspots and Other State - 7 Day Rolling Average ") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=18), 
          panel.grid.major = element_blank(), 
          legend.position = "bottom", 
          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          axis.text.x = element_text(angle = 90, vjust = -0.25), 
          text=element_text(family="Roboto", size=12, face="bold"), 
          axis.text.y = element_text(vjust = -0.25)) +
    xlab("Date") 

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Hotspots_vs_Others_share_of_cases.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

daily_count <- daily_count %>%
               arrange(Date) %>%
               mutate(Region="India")

daily_count <- daily_count %>%
                arrange(Date) %>%
                group_by(Region) %>%
                mutate(Cumulative.Count=cumsum(Total.Daily.Count))

daily_count <- daily_count %>%
                arrange(Date) %>%
                group_by(Region) %>%
                mutate(Growth.Rate=(Cumulative.Count-lag(Cumulative.Count))*100/Cumulative.Count)

daily_count <- daily_count %>%
    arrange(Date) %>%
    group_by(Region) %>%
    mutate(Avg.Growth.Rate=rollapply(Growth.Rate, 7, mean, align = "right", fill = NA))

daily_count <- daily_count %>%
    arrange(Date) %>%
    group_by(Region) %>%
    mutate(Avg.New.Cases=rollapply(Total.Daily.Count, 7, mean, align = "right", fill = NA))

daily_count_last_50_days <- daily_count %>%
               subset((Date < Sys.Date()) & (Date> Sys.Date()-50))

g <- ggplot(daily_count_last_50_days, aes(x=Date, 
                                       y=Avg.Growth.Rate, 
                                       group=Region, 
                                       color=Region )) + 
    geom_path(aes(color=Region), 
              size=1.5) + 
    ylab("Growth Rate of Confirmed Cases") +
    scale_x_date() +
    ylim(0,10) +
    ggtitle("India Confirmed Cases Growth Rate - 7 Day Rolling Average ") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=18), 
          panel.grid.major = element_blank(), 
          legend.position = "bottom", 
          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          axis.text.x = element_text(angle = 90, vjust = -0.25), 
          text=element_text(family="Roboto", size=12, face="bold"), 
          axis.text.y = element_text(vjust = -0.25)) +
    xlab("Date") 

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("India_Confirmed_Cases_Growth_Rate.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

daily_count <- daily_count %>%
    subset((Date < Sys.Date()))

g <- ggplot(daily_count, aes(x=Date, y=Avg.New.Cases)) + 
    geom_col(aes(y=Total.Daily.Count,fill="blue", color="blue", alpha=0.2)) +
    geom_path(aes(colour="black"), 
              size=1.5) + 
    ylab("New Cases") +
    scale_x_date() +
    scale_color_manual(values = c("black", "red")) +
    ggtitle("India - Daily New Cases  - 7 Day Rolling Average ") +
    theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=18), 
          panel.grid.major = element_blank(), 
          legend.position = "off", 
          plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          axis.text.x = element_text(angle = 90, vjust = -0.25), 
          text=element_text(family="Roboto", size=12, face="bold"), 
          axis.text.y = element_text(vjust = -0.25)) +
    xlab("Date")

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("India_Confirmed_Cases_New_Cases.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

names(state_district_wise_data) <- c('State', 'District', 'Date', 'Count')
state_district_wise_data$Date <- as.Date(state_district_wise_data$Date, "%d/%m/%Y" )

for(state in states_of_interest) {
    state_df <- state_district_wise_data %>%
                subset(State == state) %>%
                group_by(District) %>%
                filter(n()>10) %>%
                arrange(Date) %>%
                mutate(Average.Count = rollapply(Count, 7, mean, align = "right", fill = NA))
    
    state_df <- state_df %>%
                subset(District != '' )
    
    graph_title <- paste("Daily New Cases in", state,"- 7 day rolling average")
    graph_name <-  paste0("Daily New Cases in ", state, ".png")
    
    g <- ggplot(state_df, aes(x = Date, y = Average.Count, group = District)) +
        scale_color_brewer(palette="RdYlBu") + 
        scale_fill_brewer(palette="Paired") + 
        geom_col(aes(y=Count,
                     fill='blue')) +
        geom_line(aes(color='red',
                      fill='red'), 
                  size=2) +
        ylab("Count") +
        ggtitle(graph_title) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 18), panel.grid.major = element_blank(), legend.position = "off") +
        theme(axis.text.x = element_text(angle = 90, vjust = 1), text = element_text(family = "Roboto", size = 12, face = "bold")) +
        scale_x_date() +
        xlab(" ") +
        facet_wrap(~District, nrow = 5, scales = "free_y") +
        theme(plot.background = element_rect(fill = "#FEF0E3", colour = "#FEF0E3")) +
        theme(panel.background = element_rect(fill = "#FEF0E3", colour = "#FEF0E3"))

    #ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
    ggsave(graph_name, width = 20, height = 12, dpi = 150, units = "in", device = "png", path = "~/Desktop")
    
}
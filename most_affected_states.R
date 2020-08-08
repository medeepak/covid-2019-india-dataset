library(jsonlite)
library(dplyr)
library(ggplot2)
library(forcats)
library(gganimate)
library(ggrepel)
library(zoo)
library(ggimage)

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
                   summarise(count=n())

state_wise_data_2 <- data2 %>% 
                     group_by(raw_data.detectedstate, raw_data.dateannounced) %>% 
                     subset(raw_data.detectedstate !="" & raw_data.currentstatus=="Hospitalized") %>% 
                     summarise(count=sum(as.integer(raw_data.numcases)))

state_wise_data_2 <- na.omit(state_wise_data_2)

state_wise_data <- rbind(state_wise_data, state_wise_data_2)
state_wise_data <- na.omit(state_wise_data)

top_4_state <- c("Maharashtra","Tamil Nadu", "Gujarat", "Delhi")
state_wise_data <- state_wise_data %>% 
                    mutate(Region=ifelse(raw_data.detectedstate %in% top_4_state, "MH,TN,GJ,DL","Others"))
  
  
state_wise_deceased_data <- data %>% 
                            subset(raw_data.currentstatus == "Deceased") %>%
                            group_by(raw_data.detectedstate, raw_data.dateannounced) %>% 
                            summarise(Deceased=n())

state_wise_deceased_data_2 <- data2 %>% 
                              subset(raw_data.currentstatus == "Deceased") %>% 
                              group_by(raw_data.detectedstate, raw_data.dateannounced) %>% 
                              subset(raw_data.detectedstate !="") %>% 
                              summarise(Deceased=sum(as.integer(raw_data.numcases)))


state_wise_deceased_data <- rbind(state_wise_deceased_data, state_wise_deceased_data_2)
state_wise_deceased_data <- state_wise_deceased_data %>% 
                            arrange(as.Date(raw_data.dateannounced, "%d/%m/%Y")) %>% 
                            group_by(raw_data.detectedstate)

state_wise_deceased_data$Deceased <- lapply(state_wise_deceased_data$Deceased, function(x) { ifelse(is.na(x), 0, x) }) 
state_wise_deceased_data <- state_wise_deceased_data  %>% 
                            mutate(Cumulative.Deaths=cumsum(Deceased))

top_10_states_list <- as.list(state_wise_data[state_wise_data$raw_data.detectedstate != "" & state_wise_data$raw_data.dateannounced == format(Sys.Date()-1, "%d/%m/%Y") ,] %>% 
                        group_by(raw_data.detectedstate) %>% 
                        summarise(Cumulative.Sum=sum(count))   %>% 
                        arrange(desc(Cumulative.Sum)) %>% 
                        top_n(10) %>% 
                        select(raw_data.detectedstate))

state_wise_data <- state_wise_data %>% 
                  arrange(as.Date(raw_data.dateannounced, "%d/%m/%Y"))

state_wise_data <- state_wise_data %>% 
                   group_by(raw_data.detectedstate) %>% 
                   mutate(Cumulative.Sum=cumsum(count))

names(state_wise_data) <- c("State", "Date", "Count", "Region", "Cumulative.Sum")
state_wise_data <- na.omit(state_wise_data)

state_wise_data <- state_wise_data %>% 
                   group_by(State) %>% 
                   mutate(Rolling.Average=rollapply(Count,7,mean,align='right',fill=NA))

state_wise_data$Date <- as.Date(state_wise_data$Date, "%d/%m/%Y")
top_10_states_data <- state_wise_data[state_wise_data$State %in% top_10_states_list$raw_data.detectedstate,]

top_10_states_data <- na.omit(top_10_states_data)

state_wise_data <- state_wise_data %>% 
                                     arrange(Date) %>%
                                     group_by(State) %>%
                                     mutate(Daily.Growth.Rate=round(((Cumulative.Sum - lag(Cumulative.Sum))*100/lag(Cumulative.Sum)), 1))

state_wise_data <- state_wise_data %>% 
                    arrange(Date) %>%
                    group_by(State) %>%
                    mutate(Average.Growth.Rate=rollapply(Daily.Growth.Rate,7,mean,align='right',fill=NA))
          
#Cumulative Plot
g <- ggplot(top_10_states_data, aes(x=Date, 
                                    y=Cumulative.Sum, 
                                    group=State )) + 
      geom_path(aes(color=State), size=1.5) + 
      scale_x_date() +
      scale_y_log10() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 0.5),
            legend.position = "off") +
      xlab("Date") +
      ylab("Confirmed cases (Log scale)") + 
      facet_wrap(~State, nrow = 4, scales = "free_y") +
      ggtitle("Covid1- in India") + 
      theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Most_affected_states.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

 
#print(g)

top_10_states_data <- top_10_states_data %>% group_by(State) %>% mutate(Row.Count=as.integer(as.Date(Date,"%d/%m/%Y") - as.Date("2020-03-21")))
#View(top_10_states_data)
g <- ggplot(top_10_states_data, aes(x=as.Date(Date,"%d/%m/%Y"), y=as.integer(Cumulative.Sum), group=State, color=State )) +
  geom_line() +
  #geom_segment(aes(xend = 46, yend = as.integer(Cumulative.Sum)), linetype = 2, colour = 'grey') +
  #geom_point(size=1.5) +
  geom_label(aes(label=State)) +
  scale_x_discrete() +
  ylab("Confirmed cases") +
  xlab("") +
  ggtitle("Trend of Confirmed Cases since 21st March in Most Affected States") +
  theme(text=element_text(family="Helvetica", size=12) , plot.title = element_text(hjust = 0.5),   legend.position = "off") + 
  labs(title = 'Coronavirus confirmed cases since reaching 100 Date:{frame_along}') +
  ylim(0,10000)
ggsave("ConfirmedCasesStateWise.png", width = 14, height = 10, dpi = 300, units = "in",  device="png", path="~/Desktop")
#a <- g+transition_reveal(as.Date(Date,"%d/%m/%Y")) 
#anim <- animate(a, fps=5)
#anim <- ggbackground(anim, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
#anim_save("~/Desktop/Most_affected_states_anim.gif",anim, width = 14, height = 10)

top_10_states_data$Date <- as.Date(top_10_states_data$Date, "%d/%m/%Y")
g <- ggplot(top_10_states_data, aes(x=Date, 
                                    y=Count, 
                                    group=State, 
                                    color=State )) +
  scale_color_brewer(palette="Set1") + 
  scale_fill_brewer(palette="Set3") + 
  geom_col(aes(color="gray",
               fill="gray", 
               alpha=0.8)) +
  geom_path(aes(y=Rolling.Average, 
                color="black"), 
            size=2) +
  ylab("Count") +
  scale_x_date() +
  ggtitle("Daily New Cases - 7 Day Rolling average") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size=18,
                                  vjust = 1), 
        panel.grid.major = element_blank(), 
        legend.position = "off", 
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  facet_wrap(~State,
             nrow=4, 
             scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = -0.25), 
        text=element_text(family="Roboto", 
                          size=12, 
                          face="bold")) + 
  theme(text=element_text(family="Roboto", 
                          size=12, 
                          face="bold")) +
  xlab("Date")

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("statewise_confirmed_cases_rolling_average.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

top_10_states_data_for_deaths <- state_wise_deceased_data[state_wise_deceased_data$raw_data.detectedstate %in% top_10_states_list$raw_data.detectedstate,]

#Cumulative Plot
g <- ggplot(top_10_states_data_for_deaths, aes(x=fct_inorder(raw_data.dateannounced), y=as.integer(Cumulative.Deaths), group=raw_data.detectedstate )) + 
  geom_line(aes(color=raw_data.detectedstate), size=1.5) + 
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("Deaths Affected due to COVID-19") + 
  theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5))
ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

ggsave("Statewise_deaths.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")
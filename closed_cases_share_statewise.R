library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)
library(jsonlite)
library(gganimate)
library(reshape2)
library(ggrepel)

raw_data <- readLines("https://api.covid19india.org/v3/data.json")

state_data <- lapply(fromJSON(raw_data), function(X){
  c(X$total$confirmed, X$total$deceased, X$total$recovered)
  }
)

state_data <- do.call(rbind, state_data)
state_data <- as.data.frame(state_data)

state_data <- tibble::rownames_to_column(state_data, "State")
names(state_data) <- c("State","Confirmed","Deceased","Recovered")


state_data$Recovered <- as.integer(state_data$Recovered)
state_data$Confirmed <- as.integer(state_data$Confirmed)
state_data$Deceased <- as.integer(state_data$Deceased)

state_data <- state_data %>% 
              mutate(Closed.Cases=Deceased+Recovered)

state_data <- state_data %>% 
              mutate(Fatality.Share=Deceased*100/Closed.Cases) 

#Remove unassigned
state_data <- state_data[state_data$State!="UN", ]

#Remove states with less than 3000 closed cases
state_data <- state_data[state_data$Closed.Cases > 3000, ]

state_data$State <- unlist(lapply(state_data$State, function(X){ ifelse(X=="TT", "India", X) }))
state_data$Fill.Color <- ifelse(state_data$State=="India", "gold", "red")

state_data <- state_data %>% 
              arrange(Fatality.Share)

ggplot(state_data, 
       aes(x=fct_inorder(State), 
           y=Fatality.Share, 
           fill=Fill.Color)) + 
       geom_col(position = "dodge") + 
       ggtitle("Share of Deaths in Closed cases (with more than 3000 closed cases)") +
       theme(axis.text.x = element_text(angle = 90, 
                                        vjust = 0.5)) + 
       theme(plot.title = element_text(hjust = 0.5), 
             panel.grid.major = element_blank(), 
             legend.position = "none", 
             panel.background = element_rect(fill = "#BFD5E3"), 
             plot.background = element_rect(fill = "darkgray")) +
      xlab("State") +
      ylab("Percentage")

ggsave("Share_of_fatalities_in_closed_cases.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

max_deaths <- max(as.integer(state_data$Deceased))
ggplot(state_data, aes(x=as.integer(Deceased), y=round(Fatality.Share,2))) +
  geom_point() + 
  geom_label_repel(aes(label=state_data$State)) +
  ylab("Fatality Rates in Closed Cases") +
  xlab("Death count") +
  xlim(0,as.integer(max_deaths)) +
  ggtitle("Fatality Rates in Closed Cases - State wise") + 
  theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5))

ggsave("Share_of_fatalities_in_closed_cases_scatter.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

# 
# 
# state_data <- dcast(data, State+Date~Status, value.var = c('Status'), fun.aggregate = length)
# state_data <- state_data %>% arrange(as.Date(Date, "%d/%m/%Y")) %>% group_by(State) %>% mutate(Cumulative.Deceased=cumsum(Deceased))
# state_data <- state_data %>% arrange(as.Date(Date, "%d/%m/Y")) %>% group_by(State) %>% mutate(Cumulative.Recovered=cumsum(Recovered))
# state_data <- state_data %>% arrange(State) %>% group_by(State) %>% mutate(Average.Deaths=rollapply(Deceased, 7, mean, align='right',fill=NA))
# state_data <- state_data %>% subset(as.Date(Date,"%d/%m/%Y")>=format(Sys.Date()-14)) %>% subset(Cumulative.Deceased > 20)
# #Adding Day count since reaching 20 deaths
# state_data <- state_data %>% mutate(Day.Count=row_number())
# 
# g <- ggplot(state_data, aes(x=Day.Count, y=Average.Deaths), group=State) + 
#   geom_line(stat="identity", position = 'dodge', aes(color=State)) +
#   geom_point(aes(color=State))+
#   ylab("Count") +
#   ggtitle("7 day rolling average of new deaths since reaching 20 deaths") +
#   theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=18), panel.grid.major = element_blank(), legend.position = "bottom") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 1), text=element_text(family="Roboto", size=12, face="bold")) + 
#   xlab("Days since reaching 20 deaths")
#   #geom_label_repel(aes(label=State), position=position_dodge(width=0.9), text=element_text(family="Roboto", size=8), data=head(tail(test_positivity_rate_large_states,20),10))
# 
# #+ geom_text(aes(label=Tests.Performed.pm), position=position_dodge(width=0.9), vjust=-0.25, text=element_text(family="Roboto", size=10, face="bold"))
# 
# ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
# ggsave("rolling_average_of_deaths.png", width = 12, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

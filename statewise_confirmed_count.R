library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)

data <- read.csv("~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/staewise_cases.csv
  ", stringsAsFactors = FALSE)
subset_data <- subset(data, (Status=="Confirmed") )
melted_df <- melt(subset_data, id.vars = c("Date","Status"))
melted_df <- arrange(melted_df, as.Date(Date, "%d-%b-%y"))
melted_df <- melted_df %>% group_by(Status, variable) %>% mutate(cumsum=cumsum(value))
names(melted_df) <- c("Date", "Status", "State", "Count", "Cumulative.Sum")

# Daily Plot
ggplot(melted_df[melted_df$State != 'TT',], aes(x=fct_inorder(Date), y=as.integer(Count), group=State )) + 
  geom_line(aes(color=State)) + 
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("State wise confirmed cases")
ggsave("Statewise_confirmed_cases.png", device="png", path="~/Desktop")

#Cumulative Plot
ggplot(melted_df[melted_df$State != 'TT',], aes(x=fct_inorder(Date), y=as.integer(Cumulative.Sum), group=State )) +
  geom_line(aes(color=State)) + 
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("Identified Cases")
ggsave("Statewise_cumulative_cases.png", device="png", path="~/Desktop")

top_10_states_list <- as.list(melted_df[melted_df$Date==last_day &melted_df$State!= 'TT', ] %>% arrange(desc(Cumulative.Sum)) %>% top_n(10) %>% select(State))

#Cumulative Plot
ggplot(melted_df[melted_df$State %in% top_10_states_list$State,], aes(x=fct_inorder(Date), y=as.integer(Cumulative.Sum), group=State )) + 
  geom_smooth(aes(color=State), size=1.5) + 
  scale_x_discrete() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  ylab("Count") + 
  ggtitle("Cumulative count of most affected states in India") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Most_affected_cumulative_cases.png", device="png", path="~/Desktop")
library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)

data <- read.csv("~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/statewise_tested_numbers.csv", stringsAsFactors = FALSE)
ggplot(data[data$Total.Tested>5000,], aes(x=fct_inorder(Date), y=as.integer(Total.Tested), group=State )) + 
  geom_line(aes(linetype=State, color=State), size=1.5) +
  ylab("Count") +
  ggtitle("Covid - 19 Tests Performed in India - Statewise") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),  panel.background = element_rect(fill = "#BFD5E3"), plot.background = element_rect(fill = "darkgray"),) +
  xlab("Date")
ggsave("Tests_performed_statewise.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop") 
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

fatalities_data <- read.csv("https://raw.githubusercontent.com/medeepak/TamilNaduCovidData/master/TamilNaduFatalitiesDataset.csv", header = TRUE)

fatalities_data$Age <- as.numeric(fatalities_data$Age)
g <- ggplot(fatalities_data, aes(Age,fill="blue")) + 
     geom_histogram( color="black", binwidth = 10) + 
     theme(legend.position = "off", 
        plot.title = element_text(hjust = 0.5, vjust = -1, size=16)) +
     ggtitle("Age wise deaths in Tamil Nadu") +
     ylab("Count")

ggsave("Age_wise_fatalities_in_TamilNadu.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

fatalities_data$Days.Between.Admission.and.Death <- as.Date(fatalities_data$Date.Of.Death, "%d.%m.%Y")- as.Date(fatalities_data$Date.Of.Admission, "%d.%m.%Y")

fatalities_data$Days.Between.Admission.and.Death <- unlist(lapply(fatalities_data$Days.Between.Admission.and.Death,function(X){ifelse(abs(X)>50,NA,X)}))

mean_days_between_admission_and_death <- as.numeric(mean(na.omit(fatalities_data$Days.Between.Admission.and.Death)))

avg_label <- paste("Avg:", round(mean_days_between_admission_and_death,1), "days")

g <- ggplot(fatalities_data, aes(Days.Between.Admission.and.Death,fill="blue")) + 
      geom_histogram(color="black", position="dodge", binwidth = 1) + 
      theme(legend.position = "off", 
            plot.title = element_text(hjust = 0.5, vjust = -1, size=16)) +
      ggtitle("Days between Hospital Admission and Deaths in Tamil Nadu") +
      ylab("Count") + 
      xlab("Days between admission and death") + 
      geom_vline(xintercept=mean_days_between_admission_and_death, color="blue", size=1.5) + 
      geom_label(aes(x=mean_days_between_admission_and_death, y=80, fill="green"),
                 label=avg_label)

ggsave("Days_between_admission_and_death.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

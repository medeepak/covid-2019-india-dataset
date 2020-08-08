library(R0)
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

names(state_wise_data) <- c('State', 'Date', 'Count')
state_wise_data$Date <- as.Date(state_wise_data$Date, "%d/%m/%Y")

#states_of_interest <- c("Tamil Nadu", "Maharashtra", "Delhi", "Karnataka", "Andhra Pradesh")
states_of_interest <- c("Tamil Nadu")
for(state in states_of_interest){
  state_data <- state_wise_data %>%
    subset(State == state) %>%
    group_by(Date) %>%
    summarise(daily_count = sum(Count)) %>%
    arrange(Date)
  
  epid_counts=as.numeric(unlist(state_data[,2]))

  epid_counts <- epid_counts[1:length(epid_counts)-1]

  #GT.flu <- generation.time("gamma", c(2.6,1))
  #mgt= generation.time ("gamma", c(3, 1.5))
  mgt= generation.time ("gamma", c(4.8, 1.5))
  #res.R <- estimate.R(epid=epid_counts, GT=mgt, methods=c("ML","TD"))
  skip_to_next <- FALSE
  begin_3wk <- as.numeric(length(epid_counts)-21)
  begin_1wk <- as.numeric(length(epid_counts)-7)
  begin_3d <- as.numeric(length(epid_counts)-3)
  begin_4d <- as.numeric(length(epid_counts)-4)

  end <- as.numeric(length(epid_counts))
  
  R0_3wk <- est.R0.SB (epid_counts, mgt, begin=begin_3wk, end=end)
  R0_1wk <- est.R0.SB (epid_counts, mgt, begin=begin_1wk, end=end)
  
  #print(paste("Reproduction Number in", state,"is",R0$R))
  tryCatch(R0_3d <- est.R0.SB (epid_counts, mgt, begin=begin_3d, end=end),
           error = function(c){R0_4d <- est.R0.SB (epid_counts, mgt, begin=begin_4d, end=end)})
  
  three_week_pred <-  last(R0_3wk$pred)
  one_week_pred <-  last(R0_1wk$pred)
  three_day_pred <- last(R0_3d$pred)

  pred <- 0.6*three_day_pred + 0.3*one_week_pred + 0.1*three_week_pred
  print(paste("Prediction for next day in", state, "is",pred))
    
}

mgt= generation.time ("gamma", c(5, 0.8))
df <- data.frame(day=integer(), actual=integer(), prediction=integer())
for(iter in seq(130,135)){
  actual <- state_data[iter,2]
  prediction <- last(est.R0.SB(epid_counts, mgt, begin=iter-4, end=iter-1 )$pred)
  print(iter)
  print(actual)
  print(prediction)
  
  df %>% add_row(day = iter, actual = as.integer(actual), prediction = as.integer(prediction))
}


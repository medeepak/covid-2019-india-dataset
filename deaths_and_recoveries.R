library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
require(ggrepel)

india_deaths <- 99
india_recoveries <- 283
india_date <- '4/4/20'

deaths_by_country <- fread("/Users/deepaksr/Documents/datasciencecourse/github_root/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_by_country <- fread("/Users/deepaksr/Documents/datasciencecourse/github_root/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

sd.cols = c("Province/State", "Country/Region", "Lat", "Long")

cumulative_deaths_by_country <- deaths_by_country[,lapply(.SD, sum, na.rm=TRUE), by="Country/Region", .SDcols=-sd.cols]
cumulative_recovered_by_country <- recovered_by_country[,lapply(.SD, sum, na.rm=TRUE), by="Country/Region", .SDcols=-sd.cols]


deaths_by_country_df <- as.data.frame(cumulative_deaths_by_country)
recoveries_by_country_df <- as.data.frame(cumulative_recovered_by_country)
melted_df <- melt(deaths_by_country_df)
res <- mutate(melted_df, status="Deaths")
melted_df <- melt(recoveries_by_country_df)
res <- rbind(res,mutate(melted_df, status="Recoveries"))
names(res) <- c('Country','Date','Count','Status')
deaths_and_recoveries <- recast(res, Country + Date ~ Status, measure.var = c('Count') )
deaths_and_recoveries <- deaths_and_recoveries %>% mutate(Fatality.Rate=round((Deaths*100)/(Deaths+Recoveries),0)) 
deaths_and_recoveries <- deaths_and_recoveries %>% subset(Deaths > 400)
date_of_interest <- gsub("^0", "", format(as.Date(Sys.Date() -1), "%m/%d/%y"))
print(date_of_interest)
deaths_and_recoveries <- deaths_and_recoveries %>% subset(Date == date_of_interest)

set.seed(40)
g <- ggplot(deaths_and_recoveries, aes(x=Deaths, y=Fatality.Rate)) +
  geom_point() + 
  geom_label_repel(aes(label=deaths_and_recoveries$Country)) +
  ylab("Fatality Rates in Closed Cases") + 
  ggtitle("Fatality Rates in Closed Cases - Country wise") + 
  theme(text=element_text(family="Helvetica", size=14) , plot.title = element_text(hjust = 0.5))
ggsave("Countrywise_closed_cases_fatality_rates.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")

print(g)

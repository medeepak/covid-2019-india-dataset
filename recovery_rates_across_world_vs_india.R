library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)

india_deaths <- 99
india_recoveries <- 283
india_date <- '4/4/20'

deaths_by_country <- fread("/Users/deepaksr/Documents/datasciencecourse/github_root/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_by_country <- fread("/Users/deepaksr/Documents/datasciencecourse/github_root/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

#countries_of_interest <- c('US', 'Germany', 'United Kingdom', 'China', 'Italy', 'France', 'Spain', 'Iran', 'India')
countries_of_interest <- c('US', 'Germany', 'United Kingdom', 'China', 'Korea, South', 'India')
sd.cols = c("Province/State", "Country/Region", "Lat", "Long")

cumulative_deaths_by_country <- deaths_by_country[,lapply(.SD, sum, na.rm=TRUE), by="Country/Region", .SDcols=-sd.cols]
cumulative_recovered_by_country <- recovered_by_country[,lapply(.SD, sum, na.rm=TRUE), by="Country/Region", .SDcols=-sd.cols]


deaths_by_country_df <- as.data.frame(cumulative_deaths_by_country)
deaths_by_countries_of_interest <- subset(deaths_by_country_df, `Country/Region` %in% countries_of_interest)
recoveries_by_country_df <- as.data.frame(cumulative_recovered_by_country)
recoveries_by_countries_of_interest <- subset(recoveries_by_country_df, `Country/Region` %in% countries_of_interest)
recoveries_and_deaths <- melt(deaths_by_countries_of_interest)
names(recoveries_and_deaths) <- c("Country", "Date", "Deaths")
recoveries_df <- melt(recoveries_by_countries_of_interest)
names(recoveries_df) <- c("Country", "Date", "Recoveries")

recoveries_and_deaths <- merge(recoveries_and_deaths, recoveries_df, by.x = c("Country","Date"), by.y=c("Country","Date"))

#Removing days where there are zero deaths and recoveries
recoveries_and_deaths <- recoveries_and_deaths %>% mutate(Cases_with_results = Deaths+Recoveries)
recoveries_and_deaths <- recoveries_and_deaths %>% subset(Cases_with_results > 0)
recoveries_and_deaths <-  recoveries_and_deaths %>% mutate(Share_of_deaths_in_closed_cases = Deaths*100/(Deaths+Recoveries))
recoveries_and_deaths <- recoveries_and_deaths %>% arrange(as.Date(Date, "%m/%d/%y"))
recoveries_and_deaths <-  recoveries_and_deaths %>% group_by(Country) %>% mutate(Day=row_number())
ggplot(recoveries_and_deaths, aes(x=Day, y=Share_of_deaths_in_closed_cases)) +
  #geom_line(aes(color=Country), size=1) +
  geom_smooth(aes(color=Country)) +
  ylab("Percentage") +
  ggtitle("Share of deaths in COVID-19 cases that had a resolution") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Share_of_deaths_in_closed_cases.png", width = 10, height = 8, dpi = 150, units = "in",  device="png", path="~/Desktop")
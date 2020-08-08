library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)

india_deaths <- 99
india_recoveries <- 283
india_date <- '4/4/20'

deaths_by_country <- fread("/Users/deepaksr/Documents/datasciencecourse/github_root/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_by_country <- fread("/Users/deepaksr/Documents/datasciencecourse/github_root/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

countries_of_interest <- c('US', 'Germany', 'United Kingdom', 'China', 'Italy', 'France', 'Spain', 'Iran')
sd.cols = c("Province/State", "Country/Region", "Lat", "Long")

cumulative_deaths_by_country <- deaths_by_country[,lapply(.SD, sum, na.rm=TRUE), by="Country/Region", .SDcols=-sd.cols]
cumulative_recovered_by_country <- recovered_by_country[,lapply(.SD, sum, na.rm=TRUE), by="Country/Region", .SDcols=-sd.cols]


deaths_by_country_df <- as.data.frame(cumulative_deaths_by_country)
deaths_by_countries_of_interest <- subset(deaths_by_country_df, `Country/Region` %in% countries_of_interest)
recoveries_by_country_df <- as.data.frame(cumulative_recovered_by_country)
recoveries_by_countries_of_interest <- subset(recoveries_by_country_df, `Country/Region` %in% countries_of_interest)
melted_df <- melt(deaths_by_countries_of_interest)
res <- mutate(melted_df, status="Deaths")
melted_df <- melt(recoveries_by_countries_of_interest)
res <- rbind(res,mutate(melted_df, status="Recoveries"))

recoveries_comparison <- data.frame(country=character(),
                                    date=character(),
                                    deaths=integer(),
                                    recoveries=integer(),
                                    stringsAsFactors = FALSE)

for (country in countries_of_interest) {
  subset_data <- res[res$`Country/Region`==country & res$status == "Deaths",]
  day<- subset_data[which.min(abs(india_deaths - subset_data$value)) ,2]
  #print(paste0("Day ",day," Country ",country, " Deaths ", subset_data[subset_data$variable == day, 3]))
  #print(day)
  recoveries_comparison <- rbind(recoveries_comparison, c(country, as.character(day), subset_data[subset_data$variable == day, 3], res[res$variable == day & res$`Country/Region` == country & res$status == 'Recoveries',3]),stringsAsFactors = FALSE)
}

recoveries_comparison <- rbind(recoveries_comparison, c('India', india_date, india_deaths, india_recoveries))
names(recoveries_comparison) <- c("Country", "Date", "Deaths", "Recoveries")

melted_df <- melt(recoveries_comparison[,-2], id.vars = "Country")
names(melted_df) <- c("Country", "Category", "Value")
ggplot(melted_df, aes(x=Country, y=as.integer(Value), fill=Category )) + geom_col(position = "dodge")+ ylab("Count") + ggtitle("Recoveries in other countries when then death counts were similar to India")
ggsave("Recoveries_comparison_with_India.png", device="png", path="~/Desktop")

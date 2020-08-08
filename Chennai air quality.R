## Having a look at Chennai Air Quality based on PM2.5 measurements made bby US Consulate

library(lubridate)
library(ggplot2)
# Reading data for the past three years
pm2015 <-  read.csv("http://dosairnowdata.org/dos/historical/Chennai/2015/Chennai_PM2.5_2015_YTD.csv", na.strings = "N/A")
pm2016 <-  read.csv("http://dosairnowdata.org/dos/historical/Chennai/2016/Chennai_PM2.5_2016_YTD.csv", na.strings = "N/A")
pm2017 <-  read.csv("http://dosairnowdata.org/dos/historical/Chennai/2017/Chennai_PM2.5_2017_YTD.csv", na.strings = "N/A")
pm2018 <-  read.csv("http://dosairnowdata.org/dos/historical/Chennai/2018/Chennai_PM2.5_2018_YTD.csv", na.strings = "N/A")
pm2019 <-  read.csv("http://dosairnowdata.org/dos/historical/Chennai/2019/Chennai_PM2.5_2019_YTD.csv", na.strings = "N/A")
pm2020 <-  read.csv("http://dosairnowdata.org/dos/historical/Chennai/2020/Chennai_PM2.5_2020_YTD.csv", na.strings = "N/A")

#Take only valid measurements with positive PM2.5 values
pm2015 <- pm2015[pm2015$QC.Name == "Valid" & pm2015$Raw.Conc. > 0 , ]
pm2016 <- pm2016[pm2016$QC.Name == "Valid" & pm2016$Raw.Conc. > 0 , ]
pm2017 <- pm2017[pm2017$QC.Name == "Valid" & pm2017$Raw.Conc. > 0 , ]
pm2018 <- pm2018[pm2018$QC.Name == "Valid" & pm2018$Raw.Conc. > 0, ]
pm2019 <- pm2019[pm2019$QC.Name == "Valid" & pm2019$Raw.Conc. > 0, ]
pm2020 <- pm2020[pm2020$QC.Name == "Valid" & pm2020$Raw.Conc. > 0, ]

#Remove N/A values
pm2015 <- na.omit(pm2015)
pm2016 <- na.omit(pm2016)
pm2017 <- na.omit(pm2017)
pm2018 <- na.omit(pm2018)
pm2019 <- na.omit(pm2019)
pm2020 <- na.omit(pm2020)

#Check variance of PM2.5 values over the years
boxplot(pm2015$Raw.Conc., pm2016$Raw.Conc., pm2017$Raw.Conc., pm2018$Raw.Conc., col=c('green','pink','blue','bisque'),main="PM 2.5 Values in Chennai", ylim=c(0,800))
legend("topright", legend = c("2015","2016","2017","2018"), col=c('green','pink','blue','bisque'), lwd=2, cex=0.5)


library(dplyr)
library(lubridate)
library(ggplot2)

pm2018$DateLT <- as.POSIXct(strptime(pm2018$Date..LT.,"%Y-%m-%d %I:%M %p"))
pm2018$Hour <- hour(pm2018$DateLT)
pm2018$Month <- month(pm2018$DateLT)
pm2018$Year <- year(pm2018$DateLT)
pm2018 <- pm2018[pm2018$Month==4,]

pm2019$DateLT <- as.POSIXct(strptime(pm2019$Date..LT.,"%Y-%m-%d %I:%M %p"))
pm2019$Hour <- hour(pm2019$DateLT)
pm2019$Month <- month(pm2019$DateLT)
pm2019$Year <- year(pm2019$DateLT)
pm2019 <- pm2019[pm2019$Month==4,]

pm2020$DateLT <- as.POSIXct(strptime(pm2020$Date..LT.,"%Y-%m-%d %I:%M %p"))
pm2020$Hour <- hour(pm2020$DateLT)
pm2020$Month <- month(pm2020$DateLT)
pm2020$Year <- year(pm2020$DateLT)
pm2020 <- pm2020[pm2020$Month==4,]


hourly_mean <- summarise( group_by(pm2018, Year, Hour) , Mean=median(AQI))
hourly_mean <- rbind(hourly_mean ,summarise( group_by(pm2019, Year, Hour) , Mean=median(AQI)))
hourly_mean <- rbind(hourly_mean ,summarise( group_by(pm2020, Year, Hour) , Mean=median(AQI)))
names(hourly_mean) <- c("Year","Hour","Mean")
hourly_mean$Year <- as.character(hourly_mean$Year)
g <- ggplot(hourly_mean, aes(Hour, Mean, color=Year)) +
  geom_line(aes(group=Year)) +
  theme_light() +
  ylab("Median PM 2.5 values") +
  ggtitle("Chennai - Median PM 2.5 values for April hourwise")
print(g)
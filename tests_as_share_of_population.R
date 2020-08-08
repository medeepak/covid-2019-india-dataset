library(jsonlite)
library(ggplot2)
library(forcats)
library(jpeg)
library(grid)
library(ggpubr)
library(ggimage)
library(zoo)
library(ggrepel)
library(dplyr)
library(ggalt)
library(ggthemes)
library(reshape2)

tests_df <- read.csv("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv")
population_df <- read.csv("~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/india_population.csv")
img <- jpeg::readJPEG("~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")

merged_df <- merge(tests_df, population_df, by.x = "State", by.y = "State")
merged_df <- merged_df %>% mutate(Positive.Rate = round(Positive * 100 / Total.Tested, 1))
merged_df <- merged_df %>%
  arrange(as.Date(Updated.On, "%d/%m/%Y")) %>%
  group_by(State) %>%
  mutate(Daily.Tests = rollapply(Total.Tested, 2, diff, align = "right", fill = NA))
merged_df$Updated.On <- gsub("2050", "2020", merged_df$Updated.On)

date_of_interest <- format(Sys.Date() - 1, "%d/%m/%Y")
tests_performed_latest <- merged_df %>%
  subset(Updated.On == date_of_interest) %>%
  subset(!is.na(Total.Tested))
tests_performed_per_million <- tests_performed_latest %>%
  mutate(Tests.Performed.pm = round(Total.Tested * 1000000 / Population, 0)) %>%
  dplyr::select("State", "Tests.Performed.pm", "Population", "Test.positivity.rate", "Updated.On") %>%
  arrange(desc(Tests.Performed.pm))

tests_performed_per_million_large_state <- tests_performed_per_million %>%
  subset(Population > 30000000) %>%
  arrange(desc(Tests.Performed.pm))
# View(tests_performed_per_million_large_state)
tests_performed_per_million$State <- factor(tests_performed_per_million$State, levels = tests_performed_per_million$State)
tests_performed_per_million_large_state$State <- factor(tests_performed_per_million_large_state$State, levels = tests_performed_per_million_large_state$State)


g <- ggplot(tests_performed_per_million, aes(x = fct_inorder(State), y = Tests.Performed.pm, fill = Tests.Performed.pm)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Count") +
  ggtitle("Tests performed per million population") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 18), panel.grid.major = element_blank(), legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), text = element_text(family = "Roboto", size = 12, face = "bold")) +
  scale_fill_gradient2(low = "#CC6666", high = "#66CC99", mid = "#CC6666") +
  xlab("State") +
  geom_text(aes(label = Tests.Performed.pm), position = position_dodge(width = 0.9), vjust = -0.25)

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Tests_performed_per_million_all_states.png", width = 10, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

g <- ggplot(tests_performed_per_million_large_state, aes(x = fct_inorder(State), y = Tests.Performed.pm, fill = Tests.Performed.pm)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Count") +
  ggtitle("Tests performed per million population in large states") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 18), panel.grid.major = element_blank(), legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1), text = element_text(family = "Roboto", size = 12, face = "bold")) +
  xlab("State") +
  scale_fill_gradient2(low = "#CC6666", high = "#66CC99", mid = "#CC6666") +
  geom_text(aes(label = Tests.Performed.pm), position = position_dodge(width = 0.9), vjust = -0.25, text = element_text(family = "Roboto", size = 10, face = "bold"))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Tests_performed_per_million_large_states.png", width = 10, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")


start_date <- as.Date(format(Sys.Date() - 16, "%d/%m/%Y"), "%d/%m/%Y")
end_date <- as.Date(format(Sys.Date() - 1, "%d/%m/%Y"), "%d/%m/%Y")

test_positivity_rate_large_states <- merged_df %>%
  subset(as.Date(Updated.On, "%d/%m/%Y") >= start_date & as.Date(Updated.On, "%d/%m/%Y") <= end_date) %>%
  subset((Population > 33000000 | State == "Delhi") & State != "Telangana") %>%
  arrange(Updated.On)

test_positivity_rate_large_states <- test_positivity_rate_large_states %>% mutate(tests_performed_per_million = round(Total.Tested * 1000000 / Population, 0))

g <- ggplot(test_positivity_rate_large_states, aes(x = fct_inorder(as.character(Updated.On)), y = Positive.Rate, group = State)) +
  geom_line(stat = "identity", position = "dodge", aes(color = State)) +
  geom_point(aes(color = State)) +
  ylab("Count") +
  ggtitle("Test positivity rate in large states") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 18), panel.grid.major = element_blank(), legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1), text = element_text(family = "Roboto", size = 12, face = "bold")) +
  xlab("State") +
  geom_label_repel(aes(label = State), position = position_dodge(width = 0.9), text = element_text(family = "Roboto", size = 8), data = head(tail(test_positivity_rate_large_states, 20), 10))

#+ geom_text(aes(label=Tests.Performed.pm), position=position_dodge(width=0.9), vjust=-0.25, text=element_text(family="Roboto", size=10, face="bold"))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Tests_postivity_rate.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

test_positivity_rate_large_states_recent <- test_positivity_rate_large_states %>% subset(Updated.On == format(Sys.Date() - 2, "%d/%m/%Y"))
g <- ggplot(test_positivity_rate_large_states_recent, aes(x = as.integer(tests_performed_per_million), y = round(as.integer(Positive.Rate), 2))) +
  geom_point() +
  geom_label_repel(aes(label = test_positivity_rate_large_states_recent$State)) +
  ylab("Test Positive Rate") +
  xlab("Test Performed per Million") +
  ggtitle("Test Positive Rate vs Test Performed per Million") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 18), panel.grid.major = element_blank()) +
  annotate("rect", xmin = 0, xmax = 2000, ymin = 0, ymax = 2, fill = "orange", alpha = 0.5) +
  annotate("rect", xmin = 2000, xmax = Inf, ymin = 0, ymax = 2, fill = "lightgreen", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = 2000, ymin = 2, ymax = Inf, fill = "darkred", alpha = 0.5) +
  annotate("rect", xmin = 2000, xmax = Inf, ymin = 2, ymax = Inf, fill = "orange", alpha = 0.5)
theme(text = element_text(family = "Helvetica", size = 14), plot.title = element_text(hjust = 0.5))

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Test_positive_rate_vs_Tests_per_million.png", width = 10, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

end_date <- as.Date(format(Sys.Date() - 1, "%d/%m/%Y"), "%d/%m/%Y")
last_7_day_data <- merged_df %>% subset(as.Date(Updated.On, "%d/%m/%Y") >= start_date & as.Date(Updated.On, "%d/%m/%Y") <= end_date)
last_7_day_data_large_state <- last_7_day_data %>%
  subset(Population > 33000000 | State == "Delhi") %>%
  arrange(as.Date(Updated.On, "%d/%m/%Y"))
last_7_day_data_large_state <- last_7_day_data_large_state %>% subset(Daily.Tests > 0)

last_7_day_data_large_state$Updated.On <- as.Date(last_7_day_data_large_state$Updated.On, "%d/%m/%Y")

g <- ggplot(last_7_day_data_large_state, aes(x = Updated.On, y = Daily.Tests, group = State)) +
  geom_line(stat = "identity", position = "dodge", aes(color = State)) +
  geom_point(aes(color = State)) +
  ylab("Count") +
  ggtitle("Daily Tests Performed") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 18), panel.grid.major = element_blank(), legend.position = "off") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1), text = element_text(family = "Roboto", size = 12, face = "bold")) +
  scale_x_date() +
  xlab("Date") +
  facet_wrap(~State, nrow = 4, scales = "free_y")

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Daily_tests_performed.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")



last_7_day_data_large_state <- last_7_day_data_large_state %>%
  arrange(Updated.On) %>%
  mutate(Daily.Positive.Count = rollapply(Positive, 2, diff, align = "right", fill = NA))
last_7_day_data_large_state <- last_7_day_data_large_state %>% mutate(Daily.Test.Positive.Rate = Daily.Positive.Count * 100 / Daily.Tests)
last_7_day_data_large_state <- last_7_day_data_large_state %>% subset(Daily.Test.Positive.Rate > 0 & Daily.Test.Positive.Rate < 70)
last_7_day_data_large_state <- last_7_day_data_large_state %>% mutate(Seven.Day.Average.Daily.Positive.Rate = rollapply(Daily.Test.Positive.Rate, 7, mean, align = "right", fill = NA))

g <- ggplot(last_7_day_data_large_state, aes(x = Updated.On, y = Daily.Test.Positive.Rate, group = State)) +
  geom_line(stat = "identity", position = "dodge", aes(color = State)) +
  geom_point(aes(color = State)) +
  ylab("Count") +
  ggtitle("Daily Tests Positivity Rate") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 18), panel.grid.major = element_blank(), legend.position = "off") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1), text = element_text(family = "Roboto", size = 12, face = "bold")) +
  facet_wrap(~State, nrow = 4, scales = "free_y") +
  xlab("Date") +
  scale_x_date()

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Daily_test_positive_rate.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

g <- ggplot(last_7_day_data_large_state, aes(x = Updated.On, y = Seven.Day.Average.Daily.Positive.Rate, group = State)) +
  geom_line(stat = "identity", position = "dodge", aes(color = State)) +
  geom_point(aes(color = State)) +
  ylab("Count") +
  ggtitle("Daily Tests Positivity Rate - 7 Day Rolling Average") +
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 18), panel.grid.major = element_blank(), legend.position = "off") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1), text = element_text(family = "Roboto", size = 12, face = "bold")) +
  facet_wrap(~State, nrow = 4, scales = "free_y") +
  xlab("Date") +
  scale_x_date()

ggbackground(g, "~/Documents/datasciencecourse/github_root/covid-2019-india-dataset/old_paper.jpg")
ggsave("Daily_test_positive_rate_seven_day_average.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")


change_in_test_positivity_rate <- last_7_day_data_large_state %>%
                                  subset((Updated.On == Sys.Date()-8) |
                                           (Updated.On == Sys.Date()-1)) %>%
                                  select(c('State',
                                           'Updated.On', 
                                           'Seven.Day.Average.Daily.Positive.Rate'))
change_in_test_positivity_rate_casted <- change_in_test_positivity_rate %>%
                                  dcast(State~Updated.On, 
                                        value.var="Seven.Day.Average.Daily.Positive.Rate")
names(change_in_test_positivity_rate_casted) <- c('State','Last.Week','Now')

year_colors = c("2018"="#D7D29E", "2019"="#82C0E9")

g <- ggplot() +
  theme_economist() +
  geom_point(data=change_in_test_positivity_rate, 
             aes(y=State, 
                 x=Seven.Day.Average.Daily.Positive.Rate, 
                 fill=as.factor(Updated.On)),
                 size=6, 
                 shape=21, 
                 color="grey30") +
            geom_segment(data=change_in_test_positivity_rate_casted, 
               aes(y=State, 
                   yend=State, 
                   x=`Last.Week`, 
                   xend=`Now`),
               size=1.8, 
               color="grey30",
               lineend="butt", 
               linejoin="mitre",
               arrow=arrow(length = unit(0.005, "npc"), 
                           type="closed")) +
              ggtitle("Test Positivity Rate Change (7 Day Rolling Average Values)") +
              xlab("Test Positivity Rate") + 
              guides(fill=guide_legend(title="Date")) +
              theme(plot.title = element_text(hjust = 0.5, 
                                              size = 18), 
                    legend.position = "bottom") +
              ylab("") +
              theme(text = element_text(family = "Roboto", 
                                        size = 12)) +
              scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  
ggsave("Change_in_TPR_from_last_week.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

change_in_test_positivity_rate <- change_in_test_positivity_rate %>%
                                  arrange(Updated.On, Seven.Day.Average.Daily.Positive.Rate)

latest_tpr_title <- paste("Average Test Positive Rate as on", Sys.Date()-1)

g <- ggplot(change_in_test_positivity_rate[change_in_test_positivity_rate$Updated.On==Sys.Date()-1,], 
            aes(x = fct_inorder(as.character(State)), 
                y = Seven.Day.Average.Daily.Positive.Rate,
                fill = Seven.Day.Average.Daily.Positive.Rate)) +
              geom_bar(stat = "identity", position = "dodge") +
              ylab("Test Positive Rate") +
              xlab("") +
              ggtitle(latest_tpr_title) +
              geom_hline(yintercept = 5, colour = "red") +
              theme_economist() +
              theme(legend.position = "off",
                    axis.text.x = element_text(angle = 90, vjust = 1)) +
              geom_text(aes(x = as.factor("Rajasthan"), 
                            y = 6, 
                            label="WHO accepted TPR"),
                        color="black", size=3) +
              theme(plot.title = element_text(hjust = 0.5, 
                                              size = 12)) +
              scale_fill_gradient2(high = "#CC6666", 
                                   low = "#00EE00", 
                                   mid = "#224033")
  
ggsave("Latest_TPR.png", width = 12, height = 8, dpi = 150, units = "in", device = "png", path = "~/Desktop")

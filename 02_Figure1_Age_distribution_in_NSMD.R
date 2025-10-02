# Figure 1: Age distribution in the NSMD #

#Retrieve result3 from file 01_DescriptiveTable.R#

result3$innmelding <- as.Date(result3$Start_date)
result3$utmelding <- as.Date(result3$End_date)
result3$kjoenn <- result3$gender

nif_medlemmer <- result3 %>%
  mutate(
    `2015` = ifelse(innmelding < ymd("2015-12-31") & utmelding > ymd("2015-12-30"), 1, 0),
    `2016` = ifelse(innmelding < ymd("2016-12-31") & utmelding > ymd("2016-12-30"), 1, 0),
    `2017` = ifelse(innmelding < ymd("2017-12-31") & utmelding > ymd("2017-12-30"), 1, 0),
    `2018` = ifelse(innmelding < ymd("2018-12-31") & utmelding > ymd("2018-12-30"), 1, 0),
    `2019` = ifelse(innmelding < ymd("2019-12-31") & utmelding > ymd("2019-12-30"), 1, 0),
    `2020` = ifelse(innmelding < ymd("2020-12-31") & utmelding > ymd("2020-12-30"), 1, 0),
    `2021` = ifelse(innmelding < ymd("2021-12-31") & utmelding > ymd("2021-12-30"), 1, 0),
    `2022` = ifelse(innmelding < ymd("2022-12-31") & utmelding > ymd("2022-12-30"), 1, 0),
    `2023` = ifelse(innmelding < ymd("2023-12-31") & utmelding > ymd("2023-12-30"), 1, 0))


master <- nif_medlemmer %>%
  select(c(ID, idrett2, foedselsaar, kjoenn, 17:25))


#Long data#
library(tidyverse)
long_data <- master %>%
  pivot_longer(cols = 5:13, names_to = "year", values_to = "deltar")


long_data$year <- as.numeric(long_data$year)
long_data$alder <- long_data$year - long_data$foedselsaar

figur2 <- long_data %>%
  group_by(alder, kjoenn) %>%
  summarise(members_data = sum(deltar), .groups = "drop")

figur2 <- figur2 %>%
  filter(alder >= 10 & alder <= 70)

figur2$kjoenn <- as.factor(figur2$kjoenn)
#figur2$alder <- as.numeric(figur2$alder)

figur2test <- long_data %>%
  filter(alder >= 10 & alder <= 70 & deltar == 1)

figur2test$kjoenn <- as.factor(figur2test$kjoenn)

library(scales)

ggplot(figur2test, aes(x = alder)) +
  geom_bar(fill = "grey80", color = "black") +
  geom_density(aes(y = ..count.., color = kjoenn), size = 1.5, adjust = 1.5) +
  scale_color_manual(
    values = c("Male" = "steelblue", "Female" = "tomato")
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(10, 70, by = 10)) +
  labs(x = "Age", y = "Members", color = "Gender") +
  theme_minimal() +
  theme(legend.position = "none")
  #annotate("segment", x = 45, xend = 50, y = 380000, yend = 380000, 
        #   color = "steelblue", size = 1) +
  #annotate("text", x = 51, y = 385000, label = "Male", color = "steelblue", hjust = 0) +
  #annotate("segment", x = 45, xend = 50, y = 350000, yend = 350000, 
        #   color = "tomato", size = 1) +
  #annotate("text", x = 51, y = 355000, label = "Female", color = "tomato", hjust = 0)

#Removed the annotate function that creates the legend, since the example dataset contains much fewer rows than the actual data#.
#02_Figure1_Ages#



membership_data <- readRDS("All_members.rds")

#Need to first make all memberships and then append active memberships#
#Then save this for later use in 02_Figure_1#

membership_data$startdate <- as.Date(membership_data$startdate)

membership_data$enddate <- as.Date(membership_data$enddate)

membership_data <- membership_data %>%
  mutate(
    `2015` = ifelse(startdate < ymd("2015-12-31") & enddate > ymd("2015-12-30"), 1, 0),
    `2016` = ifelse(startdate < ymd("2016-12-31") & enddate > ymd("2016-12-30"), 1, 0),
    `2017` = ifelse(startdate < ymd("2017-12-31") & enddate > ymd("2017-12-30"), 1, 0),
    `2018` = ifelse(startdate < ymd("2018-12-31") & enddate > ymd("2018-12-30"), 1, 0),
    `2019` = ifelse(startdate < ymd("2019-12-31") & enddate > ymd("2019-12-30"), 1, 0),
    `2020` = ifelse(startdate < ymd("2020-12-31") & enddate > ymd("2020-12-30"), 1, 0),
    `2021` = ifelse(startdate < ymd("2021-12-31") & enddate > ymd("2021-12-30"), 1, 0),
    `2022` = ifelse(startdate < ymd("2022-12-31") & enddate > ymd("2022-12-30"), 1, 0),
    `2023` = ifelse(startdate < ymd("2023-12-31") & enddate > ymd("2023-12-30"), 1, 0))


master <- membership_data %>%
  select(c(w24_0511_lopenr_person, idrett2, foedselsaar, kjoenn, 14:22))

#Long data#
library(tidyverse)
long_data <- master %>%
  pivot_longer(cols = 5:13, names_to = "year", values_to = "deltar")

long_data$year <- as.numeric(long_data$year)

long_data$age <- long_data$year - long_data$foedselsaar 

long_data <- long_data %>%
  filter(age >= 10 & age <= 70)

figur2 <- long_data %>%
  group_by(age, kjoenn) %>%
  summarise(members_data = sum(deltar), .groups = "drop")


figur2$kjoenn <- as.factor(figur2$kjoenn)

figur2 <- figur2 %>%
  mutate(
    Gender = ifelse(kjoenn == 1, "Male", "Female")
  )

figur2$group = "All memberships"


strict <- readRDS("Active_members.rds")


strict2 <- strict %>%
  select(c(w24_0511_lopenr_person, idrett2, kjoenn, foedselsaar, 11:19))

long_strict2 <- strict2 %>%
  pivot_longer(cols = 5:13, names_to = "year", values_to = "deltar")

long_strict2$year <- as.numeric(long_strict2$year)

long_strict2$age <- long_strict2$year - long_strict2$foedselsaar 

long_strict2 <- long_strict2 %>%
  filter(age >= 10 & age <= 70)

long_strict3 <- long_strict2 %>%
  group_by(age, kjoenn) %>%
  summarise(members_data = sum(deltar), .groups = "drop")


long_strict3$kjoenn <- as.factor(long_strict3$kjoenn)

long_strict3 <- long_strict3 %>%
  mutate(
    Gender = ifelse(kjoenn == 1, "Male", "Female")
  )

long_strict3$group = "Active memberships"



long_strict2$kjoenn <- as.factor(long_strict2$kjoenn)

long_strict2 <- long_strict2 %>%
  mutate(
    Gender = ifelse(kjoenn == 1, "Male", "Female")
  )

long_strict2$group = "Active memberships"



long_data$kjoenn <- as.factor(long_data$kjoenn)

long_data <- long_data %>%
  mutate(
    Gender = ifelse(kjoenn == 1, "Male", "Female")
  )

long_data$group = "All memberships"


figur_tall <- rbind(long_data, long_strict2)

library(scales)

figur_tall <- figur_tall %>%
  filter(deltar == 1)

figur_tall$group <- factor(figur_tall$group, 
                           levels = c("All memberships", "Active memberships"))


ann <- data.frame(
  group = "All memberships",   # facet where the lines should appear
  x1 = 50,
  x2 = 55,
  y_female = 260000,
  y_male = 300000
)

library(ggplot2)
library(showtext)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()

age_figure <- ggplot(figur_tall, aes(x = age)) +
  geom_bar(fill = "grey80", color = "black") +
  geom_density(aes(y = ..count.., color = Gender), size = 1.5, adjust = 1.5) +
  facet_wrap(~ group) +
  scale_color_manual(
    values = c("Male" = "steelblue", "Female" = "tomato")
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(10, 70, by = 10)) +
  labs(x = "Age", y = "Members", color = "Gender") +
  theme_minimal(base_size = 12, base_family = "Outfit") +
  theme(legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid = element_blank(),
        axis.ticks = element_line(),   
        axis.ticks.length = unit(3, "pt")) +
  annotate("segment", x = 45, xend = 50, y = 300000, yend = 300000, 
           color = "steelblue", size = 1) +
  annotate("text", x = 51, y = 300000, label = "Male", color = "steelblue", hjust = 0) +
  annotate("segment", x = 45, xend = 50, y = 270000, yend = 270000, 
           color = "tomato", size = 1) +
  annotate("text", x = 51, y = 270000, label = "Female", color = "tomato", hjust = 0)

age_figure

ggsave("Figure1_Ages.pdf", plot = age_figure, width = 12, height = 6) # Saves as a PDF
ggsave("Figure1_Ages.svg", plot = age_figure, width = 12, height = 6) # Saves as a SVG



age_figure_jpg <- ggplot(figur_tall, aes(x = age)) +
  geom_bar(fill = "grey80", color = "black") +
  geom_density(aes(y = ..count.., color = Gender), size = 1.5, adjust = 1.5) +
  facet_wrap(~ group) +
  scale_color_manual(
    values = c("Male" = "steelblue", "Female" = "tomato")
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(10, 70, by = 10)) +
  labs(x = "Age", y = "Members", color = "Gender") +
  theme_minimal(base_size = 46, base_family = "Outfit") +
  theme(legend.position = "none",
        strip.text = element_text(size = 42, face = "bold"),
        panel.grid = element_blank(),
        axis.ticks = element_line(),   
        axis.ticks.length = unit(1, "pt")) +
  annotate("segment", x = 45, xend = 50, y = 300000, yend = 300000, 
           color = "steelblue", size = 1) +
  annotate("text", x = 51, y = 300000, label = "Male", color = "steelblue", hjust = 0, size = 12) +
  annotate("segment", x = 45, xend = 50, y = 270000, yend = 270000, 
           color = "tomato", size = 1) +
  annotate("text", x = 51, y = 270000, label = "Female", color = "tomato", hjust = 0, size = 12)


ggsave("Figure1_Ages.tiff", plot = age_figure_jpg, width = 12, height = 6, dpi = 300,  compression = "lzw") # Saves as a TIFF
ggsave("Figure1_Ages.jpg", plot = age_figure_jpg, width = 12, height = 6, dpi = 300) # Saves as a jpg



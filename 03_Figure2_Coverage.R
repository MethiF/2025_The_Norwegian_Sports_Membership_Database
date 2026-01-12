#02_Figure2_Coverage#

#This code includes the following figures#:
#Figure2a: Coverage of all memberships in NSMD
#Figure2b: Coverage of active memberships in NSMD
#S-Figure 1: Coverage of active memberships in NSMD (without golf ...)


# Load required libraries
library(dplyr)
library(lubridate)
library(gt)
library(gtsummary)
library(tidyverse)
library(readxl)

# Assuming your dataset is called 'membership_data' with columns:
# ID, Start_date, End_date, gender, foedselsaar



final2 <- readRDS("All_members.rds")

final2 <- final2 %>%
  filter(days > 0)

final2 <- final2 %>%
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

master <- final2 %>%
  select(c(w24_0511_lopenr_person, idrett, idrett2, foedselsaar, kjoenn, 14:22))


#Long data#
library(tidyverse)
long_data <- master %>%
  pivot_longer(cols = 6:14, names_to = "year", values_to = "deltar")

long_data$year <- as.numeric(long_data$year)

long_data$alder <- long_data$year - long_data$foedselsaar

long_data <- long_data %>%
  mutate(
    age = ifelse(alder >= 13 & alder <= 19, "youth",
                 ifelse(alder >= 20 & alder <= 25, "mid",
                        ifelse(alder >= 25 & alder <= 70, "old", "remove")))
  )

#Remove
#Company sports from both nif and aktive and NSMD (aktive and all in NSMD)
#University sports from both aktive and NSMD (aktive and all in NSMD) (not NIF)
#Multisports from aktive and NSMD (aktive in NSMD)

long_data2 <- long_data %>%
  filter(idrett2 != "Bedrift" & idrett2 != "Studentidrett")

collapsed <- long_data2 %>%
  group_by(age, year) %>%
  summarise(
    nsmd = sum(deltar, na.rm = T)
  )

library(readxl)
NIF_medlemmer <- read_excel("N:/durable/codes/Kohortbeskrivelse/NIF_medlemmer.xlsx")

NIF_medlemmer$nif2 <- NIF_medlemmer$nif - NIF_medlemmer$company


combine <- NIF_medlemmer %>%
  left_join(collapsed, by = c("year", "age"))

combine$x <- combine$nsmd/combine$nif2

library(ggplot2)


combine$year <- as.factor(combine$year)


age_labels <- c(
  youth = "13-19 years",
  mid   = "20-25 years",
  old   = "26-70 years"
)

combine$pct <- combine$x*100


combine <- combine %>%
  mutate(
    percentage_label = paste0(round(pct, 1), "%")
  )

library(showtext)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()


combine$age <- factor(combine$age, levels = c("youth", "mid", "old"))

combine <- combine %>%
  filter(year != "2024")

medlemmer_plot <- ggplot(combine, aes(x = year, y = x, group = 1)) +
  geom_area(alpha = 0.3, show.legend = FALSE, fill = "#099eee") +
  geom_line(size = 1, colour = "#122eee", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_point(data = combine, aes(x = year, y = x)) +
  geom_text(data = combine, aes(x = year, y = x, label = percentage_label), 
            vjust = -1.5, hjust = +0.5, size = 4, family = "Outfit") +
  facet_wrap(~ age, labeller = as_labeller(age_labels)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    x = "Year",
    y = "Share (%)",
    title = "All memberships"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold", family = "Outfit"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "Outfit"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0)
  )

medlemmer_plot


ggsave("Figure2A_All_members.pdf", plot = medlemmer_plot, width = 15, height = 6) # Saves as a PDF
ggsave("Figure2A_All_members.svg", plot = medlemmer_plot, width = 15, height = 6) # Saves as a SVG

#Larger text for jpg and tiff:

medlemmer_plot_jpg <- ggplot(combine, aes(x = year, y = x, group = 1)) +
  geom_area(alpha = 0.3, show.legend = FALSE, fill = "#099eee") +
  geom_line(size = 1, colour = "#122eee", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_point(data = combine, aes(x = year, y = x)) +
  geom_text(data = combine, aes(x = year, y = x, label = percentage_label), 
            vjust = -1.5, hjust = +0.5, size = 12, family = "Outfit") +
  facet_wrap(~ age, labeller = as_labeller(age_labels)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    x = "Year",
    y = "Share (%)",
    title = "All memberships"
  ) +
  
  theme_minimal(base_size = 42) +
  theme(
    strip.text = element_text(size = 42, face = "bold", family = "Outfit"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 60, hjust = 0.5, face = "bold", family = "Outfit"),
    plot.subtitle = element_text(size = 42, hjust = 0.5),
    plot.caption = element_text(size = 30, hjust = 0)
  )

ggsave("Figure2A_All_members.tiff", plot = medlemmer_plot_jpg, width = 15, height = 6, dpi = 300,  compression = "lzw") # Saves as a TIFF
ggsave("Figure2A_All_members.jpg", plot = medlemmer_plot_jpg, width = 15, height = 6, dpi = 300) # Saves as a jpg

####################
#ACTIVE MEMBERSHIPS#
####################

new_master <- readRDS("Active_members.rds")


new_master <- new_master %>%
  select(c(w24_0511_lopenr_person, idrett2, foedselsaar, 11:19))

#Long data#
library(tidyverse)
new_master <- new_master %>%
  pivot_longer(cols = 4:12, names_to = "year", values_to = "deltar")

new_master$year <- as.numeric(new_master$year)

new_master$alder <- new_master$year - new_master$foedselsaar



#new_master <- readRDS("strict_definitions.rds")

new_master <- new_master %>%
  mutate(
    age = ifelse(alder >= 13 & alder <= 19, "youth",
                 ifelse(alder >= 20 & alder <= 25, "mid",
                        ifelse(alder >= 25 & alder <= 70, "old", "remove")))
  )


collapsed3 <- new_master %>%
  group_by(age, year) %>%
  summarise(
    nsmd = sum(deltar, na.rm = T)
  )



NIF_medlemmer2 <- read_excel("N:/durable/data/Medlemmer_alle.xlsx")
NIF_medlemmer2$`2015` <- as.numeric(NIF_medlemmer2$`2015`)
NIF_medlemmer2$`2016` <- as.numeric(NIF_medlemmer2$`2016`)
NIF_medlemmer2$`2017` <- as.numeric(NIF_medlemmer2$`2017`)
NIF_medlemmer2$`2018` <- as.numeric(NIF_medlemmer2$`2018`)

NIF_medlemmer2 <- NIF_medlemmer2 %>%
  filter(idrett2 != "Bedrift" & idrett2 != "Studentidrett" & idrett2 != "Fleridretter")

NIF_medlemmer3 <- NIF_medlemmer2 %>%
  select(2:11, "age")

NIF_medlemmer4 <- NIF_medlemmer3 %>%
  group_by(age) %>%
  mutate(
    `2015` = sum(`2015`, na.rm = T),
    `2016` = sum(`2016`, na.rm = T),
    `2017` = sum(`2017`, na.rm = T),
    `2018` = sum(`2018`, na.rm = T),
    `2019` = sum(`2019`, na.rm = T),
    `2020` = sum(`2020`, na.rm = T),
    `2021` = sum(`2021`, na.rm = T),
    `2022` = sum(`2022`, na.rm = T),
    `2023` = sum(`2023`, na.rm = T),
    `2024` = sum(`2024`, na.rm = T)
  )

NIF_medlemmer4 <- NIF_medlemmer4 %>%
  distinct()

NIF_medlemmer5 <- NIF_medlemmer4 %>%
  pivot_longer(
    cols = 1:10,
    names_to = "year",
    values_to = "nif"
  )

NIF_medlemmer5$year <- as.numeric(NIF_medlemmer5$year)

NIF_medlemmer5 <- NIF_medlemmer5 %>%
  mutate(age = ifelse(age == "young", "youth", age))

combine3 <- NIF_medlemmer5 %>%
  left_join(collapsed3, by = c("year", "age"))



#Remove
#Company sports from both nif and aktive and NSMD (aktive and all in NSMD)
#University sports from both aktive and NSMD (aktive and all in NSMD) (not NIF)
#Multisports from aktive and NSMD (aktive in NSMD)


combine3$x <- combine3$nsmd/combine3$nif

library(ggplot2)


combine3$year <- as.factor(combine3$year)


age_labels <- c(
  youth = "13-19 years",
  mid   = "20-25 years",
  old   = "26-70 years"
)


combine3$pct <- combine3$x*100


combine3 <- combine3 %>%
  mutate(
    percentage_label = paste0(round(pct, 1), "%")
  )

library(showtext)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()


combine3$age <- factor(combine3$age, levels = c("youth", "mid", "old"))

combine3 <- combine3 %>%
  filter(year != "2024")

aktive_medlemmer_plot <- ggplot(combine3, aes(x = year, y = x, group = 1)) +
  geom_area(alpha = 0.3, show.legend = FALSE, fill = "#099eee") +
  geom_line(size = 1, colour = "#122eee", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_point(data = combine3, aes(x = year, y = x)) +
  geom_text(data = combine3, aes(x = year, y = x, label = percentage_label), 
            vjust = -1.5, hjust = +0.5, size = 4, family = "Outfit") +
  facet_wrap(~ age, labeller = as_labeller(age_labels)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    x = "Year",
    y = "Share (%)",
    title = "Active memberships"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold", family = "Outfit"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "Outfit"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0)
  )

aktive_medlemmer_plot


ggsave("Figure2B_Active_members.pdf", plot = aktive_medlemmer_plot, width = 15, height = 6) # Saves as a PDF
ggsave("Figure2B_Active_members.svg", plot = medlemmer_plot, width = 15, height = 6) # Saves as a SVG

#Larger text for jpg and tiff:

aktive_medlemmer_jpg <- ggplot(combine3, aes(x = year, y = x, group = 1)) +
  geom_area(alpha = 0.3, show.legend = FALSE, fill = "#099eee") +
  geom_line(size = 1, colour = "#122eee", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_point(data = combine3, aes(x = year, y = x)) +
  geom_text(data = combine3, aes(x = year, y = x, label = percentage_label), 
            vjust = -1.5, hjust = +0.5, size = 12, family = "Outfit") +
  facet_wrap(~ age, labeller = as_labeller(age_labels)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    x = "Year",
    y = "Share (%)",
    title = "Active memberships"
  ) +
  
  theme_minimal(base_size = 42) +
  theme(
    strip.text = element_text(size = 42, face = "bold", family = "Outfit"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 60, hjust = 0.5, face = "bold", family = "Outfit"),
    plot.subtitle = element_text(size = 42, hjust = 0.5),
    plot.caption = element_text(size = 30, hjust = 0)
  )

ggsave("Figure2B_Active_members.tiff", plot = aktive_medlemmer_jpg, width = 15, height = 6, dpi = 300,  compression = "lzw") # Saves as a TIFF
ggsave("Figure2B_Active_members.jpg", plot = aktive_medlemmer_jpg, width = 15, height = 6, dpi = 300) # Saves as a jpg



##############
#Without Golf#
##############


collapsed4 <- new_master %>%
  filter(idrett2 != "Golf")

collapsed4 <- collapsed4 %>%
  group_by(age, year) %>%
  summarise(
    nsmd = sum(deltar, na.rm = T)
  )


NIF_medlemmer2 <- NIF_medlemmer2 %>%
  filter(idrett2 != "Golf")

NIF_medlemmer3 <- NIF_medlemmer2 %>%
  select(2:11, "age")

NIF_medlemmer4 <- NIF_medlemmer3 %>%
  group_by(age) %>%
  mutate(
    `2015` = sum(`2015`, na.rm = T),
    `2016` = sum(`2016`, na.rm = T),
    `2017` = sum(`2017`, na.rm = T),
    `2018` = sum(`2018`, na.rm = T),
    `2019` = sum(`2019`, na.rm = T),
    `2020` = sum(`2020`, na.rm = T),
    `2021` = sum(`2021`, na.rm = T),
    `2022` = sum(`2022`, na.rm = T),
    `2023` = sum(`2023`, na.rm = T),
    `2024` = sum(`2024`, na.rm = T)
  )

NIF_medlemmer4 <- NIF_medlemmer4 %>%
  distinct()

NIF_medlemmer5 <- NIF_medlemmer4 %>%
  pivot_longer(
    cols = 1:10,
    names_to = "year",
    values_to = "nif"
  )

NIF_medlemmer5$year <- as.numeric(NIF_medlemmer5$year)

NIF_medlemmer5 <- NIF_medlemmer5 %>%
  mutate(age = ifelse(age == "young", "youth", age))

combine3 <- NIF_medlemmer5 %>%
  left_join(collapsed3, by = c("year", "age"))


combine3$x <- combine3$nsmd/combine3$nif

library(ggplot2)


combine3$year <- as.factor(combine3$year)


age_labels <- c(
  youth = "13-19 years",
  mid   = "20-25 years",
  old   = "26-70 years"
)


combine3$pct <- combine3$x*100


combine3 <- combine3 %>%
  mutate(
    percentage_label = paste0(round(pct, 1), "%")
  )

library(showtext)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()


combine3$age <- factor(combine3$age, levels = c("youth", "mid", "old"))

combine3 <- combine3 %>%
  filter(year != "2024")

aktive_medlemmer_plot2 <- ggplot(combine3, aes(x = year, y = x, group = 1)) +
  geom_area(alpha = 0.3, show.legend = FALSE, fill = "#099eee") +
  geom_line(size = 1, colour = "#122eee", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_point(data = combine3, aes(x = year, y = x)) +
  geom_text(data = combine3, aes(x = year, y = x, label = percentage_label), 
            vjust = -1.5, hjust = +0.5, size = 4, family = "Outfit") +
  facet_wrap(~ age, labeller = as_labeller(age_labels)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    x = "Year",
    y = "Share (%)",
    title = "Active memberships"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold", family = "Outfit"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = "Outfit"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0)
  )

aktive_medlemmer_plot2


ggsave("SFigure1_No_golf.pdf", plot = aktive_medlemmer_plot2, width = 15, height = 6) # Saves as a PDF



####################
#Coverage by sports#
####################

###########################
#ACTIVE MEMBERS AGES 13-19#
###########################

my_data <- readRDS("Active_members.rds")


my_data <- my_data %>%
  select(c(w24_0511_lopenr_person, idrett2, foedselsaar, 11:19))

#Long data#
library(tidyverse)
my_data <- my_data %>%
  pivot_longer(cols = 4:12, names_to = "year", values_to = "deltar")

my_data$year <- as.numeric(my_data$year)

my_data$alder <- my_data$year - my_data$foedselsaar

##

rm(list = setdiff(ls(), "my_data"))
gc()

my_data <- my_data %>%
  filter(alder >= 13 & alder <= 19)

collapsed_data <- my_data %>%
  group_by(year, idrett2) %>%
  summarise(
    nsmd = sum(deltar, na.rm = T)
  )

library(readxl)
members <- read_excel("N:/durable/data/Medlemmer_13-19.xlsx")
members <- members %>%
  pivot_longer(cols = 2:10, names_to = "year", values_to = "nif")

members$year <- as.numeric(members$year)

master <- left_join(collapsed_data, members, by = c("idrett2", "year"))

master$share <- master$nsmd/master$nif

master <- master %>%
  filter(!is.na(share))

master2 <- master %>%
  mutate(share = ifelse(nif == 0, 0, share))


idrett2_never_high <- master2 %>%
  group_by(idrett2) %>%
  summarise(max_share = max(share, na.rm = TRUE)) %>%
  filter(max_share <= 0.5) %>%
  pull(idrett2)


df_filtered <- master2 %>%
  filter(!(idrett2 %in% idrett2_never_high))


# 1. Find the 10 most common sports
top10_idrett2 <- df_filtered %>%
  group_by(idrett2) %>%
  summarise(total_nsmd = sum(nsmd, na.rm = TRUE)) %>%
  arrange(desc(total_nsmd)) %>%
  slice_head(n = 10) %>%
  pull(idrett2)

# 2. Keep only rows with those sports
df_top <- df_filtered %>%
  filter(idrett2 %in% top10_idrett2)


english <- read_excel("N:/durable/data/Engelske_navn_idretter.xlsx")
df_top_english <- left_join(df_top, english, by = "idrett2")


df_top_english <- df_top_english %>%
  mutate(
    cat = case_when(
      share < 0.40 ~ "<40%",
      share >= 0.40 & share < 0.60 ~ "40-59%",
      share >= 0.60 & share < 0.80 ~ "60-79%",
      share >= 0.80 ~ ">80%"
    ),
    cat = factor(cat, levels = c("<40%", "40-59%", "60-79%", ">80%"))
  )


library(ggplot2)
library(showtext)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()

df_top_english$year <- as.factor(df_top_english$year)

top_10_sports_plot <- ggplot(df_top_english, aes(x = year, y = share, group = english, fill = cat), color = "grey80") +
  #geom_area(alpha = 0.3, show.legend = FALSE) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  facet_wrap(~ english) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #coord_cartesian(ylim = c(0, 1.2)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    x = "Year",
    y = "Share (%)",
  ) +
  scale_fill_manual(
    name = "Coverage",
    values = c(
      "<40%" = "#d73027",    # red
      "40-59%" = "#fc8d59",   # orange
      "60-79%" = "#d9ef8b",   # light green
      ">80%" = "#1a9850"      # dark green
    )) +
  theme_minimal(base_size = 14, base_family = "Outfit") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0)
  )

top_10_sports_plot

ggsave("Figure3_Top_10_sports.pdf", plot = top_10_sports_plot, width = 10, height = 6) # Saves as a PDF
ggsave("Figure3_Top_10_sports.svg", plot = top_10_sports_plot, width = 10, height = 6) # Saves as a PDF



top_10_sports_plot_jpg <- ggplot(df_top_english, aes(x = year, y = share, group = english, fill = cat), color = "grey80") +
  #geom_area(alpha = 0.3, show.legend = FALSE) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  facet_wrap(~ english) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #coord_cartesian(ylim = c(0, 1.2)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    x = "Year",
    y = "Share (%)",
  ) +
  scale_fill_manual(
    name = "Coverage",
    values = c(
      "<40%" = "#d73027",    # red
      "40-59%" = "#fc8d59",   # orange
      "60-79%" = "#d9ef8b",   # light green
      ">80%" = "#1a9850"      # dark green
    )) +
  theme_minimal(base_size = 42, base_family = "Outfit") +
  theme(
    strip.text = element_text(size = 36, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 48, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 42, hjust = 0.5),
    plot.caption = element_text(size = 30, hjust = 0)
  )

ggsave("Figure3_Top_10_sports.tiff", plot = top_10_sports_plot_jpg, width = 10, height = 6, dpi = 400,  compression = "lzw") # Saves as a TIFF
ggsave("Figure3_Top_10_sports.jpg", plot = top_10_sports_plot_jpg, width = 10, height = 6, dpi = 400) # Saves as a jpg



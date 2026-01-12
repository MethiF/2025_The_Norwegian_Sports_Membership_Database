#04_Supplementary_Information#

#This file contains:
#S-Figure 2: Total number of memberships in NSMD and NIF's reports, 13-70 years#
#S-Figure 3: Coverage by type of sport and age groups
#S-Figure 4: Age distribution by sports data coverage.
#S-Figure 5: Geographical distribution by sports data coverage
#S-Figure 6: Parental income distribution by sports data coverage


#The script for "S-Table 2: All and active memberships by sport" is included in 01_Table1_Descriptives.R"

############
#S-Figure 2#
############

#S-Figure 2#
#All memberships in NSMD vs. all memberships in 


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

long_data <- long_data %>%
  filter(age != "remove")


collapsed <- long_data %>%
  group_by(year) %>%
  summarise(
    nsmd = sum(deltar, na.rm = T)
  )

library(readxl)
NIF_medlemmer <- read_excel("N:/durable/codes/Kohortbeskrivelse/NIF_medlemmer.xlsx")

NIF_medlemmer <- NIF_medlemmer %>%
  group_by(year) %>%
  summarise(
    NIF_medlemmer = sum(nif, na.rm = T),
    NIF_aktive = sum(aktive, na.rm = T) 
  )

combine <- NIF_medlemmer %>%
  left_join(collapsed, by = "year")



new_master <- readRDS("Active_members.rds")


new_master <- new_master %>%
  select(c(w24_0511_lopenr_person, idrett2, foedselsaar, 11:19))

#Long data#
library(tidyverse)
new_master <- new_master %>%
  pivot_longer(cols = 4:12, names_to = "year", values_to = "deltar")

new_master$year <- as.numeric(new_master$year)

new_master$alder <- new_master$year - new_master$foedselsaar

new_master <- new_master %>%
  mutate(
    age = ifelse(alder >= 13 & alder <= 19, "youth",
                 ifelse(alder >= 20 & alder <= 25, "mid",
                        ifelse(alder >= 25 & alder <= 70, "old", "remove")))
  )

new_master <- new_master %>%
  filter(age != "remove")

strict <- new_master %>%
  group_by(year) %>%
  summarise(
    nsmd = sum(deltar, na.rm = T)
  )


strict$type <- "Active memberships"
strict$group <- "NSMD"
strict$total <- strict$nsmd

NIF_medlemmer1 <- NIF_medlemmer %>%
  select(year, NIF_medlemmer)

NIF_medlemmer1$type <- "All memberships"
NIF_medlemmer1$group <- "NIF's reports"
NIF_medlemmer1$total <- NIF_medlemmer1$NIF_medlemmer


NIF_medlemmer2 <- NIF_medlemmer %>%
  select(year, NIF_aktive)

NIF_medlemmer2$type <- "Active memberships"
NIF_medlemmer2$group <- "NIF's reports"
NIF_medlemmer2$total <- NIF_medlemmer2$NIF_aktive


all <- collapsed
all$type <- "All memberships"
all$group <- "NSMD"
all$total <- all$nsmd

strict <- strict %>%
  select(year, type, group, total)

all <- all %>%
  select(year, type, group, total)

NIF_medlemmer1 <- NIF_medlemmer1 %>%
  select(year, type, group, total)

NIF_medlemmer2 <- NIF_medlemmer2 %>%
  select(year, type, group, total)

figure1 <- rbind(all, strict, NIF_medlemmer1, NIF_medlemmer2)


library(ggplot2)
library(showtext)
library(scales)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()

figure1$year <- as.factor(figure1$year)

figure2 <- figure1 %>%
  filter(year != "2024")



figure2$type <- factor(figure2$type, 
                       levels = c("All memberships", "Active memberships"))


sfigure1 <- ggplot(figure2, aes(x = year, y = total, colour = group, group = group, linetype = group)) +
  geom_line(size = 1.5) +
  facet_wrap(~ type) +
  scale_color_manual(
    values = c("NSMD" = "#E69F00", "NIF's reports" = "#0072B2")) +
  scale_linetype_manual(
    values = c("NSMD" = "solid","NIF's reports"  = "dashed"), guide = "none") +
  scale_y_continuous(labels = comma,
                     limits = c(0, 1500000),
                     breaks = seq(0, 1500000, by = 250000)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(x = "Year", y = "Members", color = "Source") +
  theme_bw(base_size = 14, base_family = "Outfit") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0)
  )

sfigure1

ggsave("SFigure2_Comparison_NSMD_NIF.pdf", plot = sfigure1, width = 12, height = 6) # Saves as a PDF



############
#S-Figure 3#
############

library(dplyr)
library(readxl)
library(readxl)


my_data <- readRDS("Active_members.rds")


my_data <- my_data %>%
  select(c(w24_0511_lopenr_person, idrett2, foedselsaar, 11:19))

#Long data#
library(tidyverse)
my_data <- my_data %>%
  pivot_longer(cols = 4:12, names_to = "year", values_to = "deltar")

my_data$year <- as.numeric(my_data$year)

my_data$alder <- my_data$year - my_data$foedselsaar


my_data <- my_data %>%
  filter(alder >= 20 & alder <= 25)

collapsed_data <- my_data %>%
  group_by(year, idrett2) %>%
  summarise(
    nsmd = sum(deltar)
  )


members <- read_excel("N:/durable/data/Medlemmer_alle.xlsx")
members <- members %>%
  filter(age == "mid")


members$`2015` <- as.numeric(members$`2015`)
members$`2016` <- as.numeric(members$`2016`)
members$`2017` <- as.numeric(members$`2017`)
members$`2018` <- as.numeric(members$`2018`)


members <- members %>%
  select(2:10, idrett2)

members <- members %>%
  pivot_longer(cols = 1:9, names_to = "year", values_to = "nif")

members$year <- as.numeric(members$year)

master <- left_join(collapsed_data, members, by = c("idrett2", "year"))

master$share <- master$nsmd/master$nif

master <- master %>%
  filter(!is.na(share))

master2 <- master %>%
  mutate(share = ifelse(nif == 0, 0, share))


df_filtered <- master2 



english <- read_excel("N:/durable/data/Engelske_navn_idretter.xlsx")
df_top_english <- left_join(df_filtered, english, by = "idrett2")


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

top_all_sports_2plot <- ggplot(df_top_english, aes(x = year, y = share, group = english, fill = cat), color = "grey80") +
  #geom_area(alpha = 0.3, show.legend = FALSE) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  facet_wrap(~ english) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1.2)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    title = "20-25",
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

top_all_sports_2plot

ggsave("SFigure3B_20-25.pdf", plot = top_all_sports_2plot, width = 20, height = 12) # Saves as a PDF




library(dplyr)
library(readxl)
library(readxl)


my_data <- readRDS("Active_members.rds")


my_data <- my_data %>%
  select(c(w24_0511_lopenr_person, idrett2, foedselsaar, 11:19))

#Long data#
library(tidyverse)
my_data <- my_data %>%
  pivot_longer(cols = 4:12, names_to = "year", values_to = "deltar")

my_data$year <- as.numeric(my_data$year)

my_data$alder <- my_data$year - my_data$foedselsaar

my_data <- my_data %>%
  filter(alder >= 26 & alder <= 70)


my_data$year <- as.numeric(my_data$year)
my_data$alder <- my_data$year - my_data$foedselsaar

collapsed_data <- my_data %>%
  group_by(year, idrett2) %>%
  summarise(
    nsmd = sum(deltar)
  )


members <- read_excel("N:/durable/data/Medlemmer_alle.xlsx")
members <- members %>%
  filter(age == "old")

members$`2015` <- as.numeric(members$`2015`)
members$`2016` <- as.numeric(members$`2016`)
members$`2017` <- as.numeric(members$`2017`)
members$`2018` <- as.numeric(members$`2018`)


members <- members %>%
  select(2:10, idrett2)

members <- members %>%
  pivot_longer(cols = 1:9, names_to = "year", values_to = "nif")

members$year <- as.numeric(members$year)

master <- left_join(collapsed_data, members, by = c("idrett2", "year"))

master$share <- master$nsmd/master$nif

master <- master %>%
  filter(!is.na(share))

master2 <- master %>%
  mutate(share = ifelse(nif == 0, 0, share))


df_filtered <- master2 


english <- read_excel("N:/durable/data/Engelske_navn_idretter.xlsx")
df_top_english <- left_join(df_filtered, english, by = "idrett2")


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

top_all_sports_3plot <- ggplot(df_top_english, aes(x = year, y = share, group = english, fill = cat), color = "grey80") +
  #geom_area(alpha = 0.3, show.legend = FALSE) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  facet_wrap(~ english) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1.2)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    title = "26-70",
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

ggsave("SFigure3C_26.pdf", plot = top_all_sports_3plot, width = 20, height = 12) # Saves as a PDF


rm()
gc()


#ALL SPORTS 13-19#

my_data <- readRDS("Active_members.rds")


my_data <- my_data %>%
  select(c(w24_0511_lopenr_person, idrett2, foedselsaar, 11:19))

#Long data#
library(tidyverse)
my_data <- my_data %>%
  pivot_longer(cols = 4:12, names_to = "year", values_to = "deltar")

my_data$year <- as.numeric(my_data$year)

my_data$alder <- my_data$year - my_data$foedselsaar

my_data <- my_data %>%
  filter(alder >= 13 & alder <= 19)


my_data$year <- as.numeric(my_data$year)
my_data$alder <- my_data$year - my_data$foedselsaar

collapsed_data <- my_data %>%
  group_by(year, idrett2) %>%
  summarise(
    nsmd = sum(deltar)
  )


members <- read_excel("N:/durable/data/Medlemmer_alle.xlsx")
members <- members %>%
  filter(age == "young")

members$`2015` <- as.numeric(members$`2015`)
members$`2016` <- as.numeric(members$`2016`)
members$`2017` <- as.numeric(members$`2017`)
members$`2018` <- as.numeric(members$`2018`)


members <- members %>%
  select(2:10, idrett2)

members <- members %>%
  pivot_longer(cols = 1:9, names_to = "year", values_to = "nif")

members$year <- as.numeric(members$year)

master <- left_join(collapsed_data, members, by = c("idrett2", "year"))

master$share <- master$nsmd/master$nif

master <- master %>%
  filter(!is.na(share))

master2 <- master %>%
  mutate(share = ifelse(nif == 0, 0, share))


df_filtered <- master2 


english <- read_excel("N:/durable/data/Engelske_navn_idretter.xlsx")
df_top_english <- left_join(df_filtered, english, by = "idrett2")


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

sfig3a <- ggplot(df_top_english, aes(x = year, y = share, group = english, fill = cat), color = "grey80") +
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

ggsave("SFigure3A_13-19.pdf", plot = sfig3a, width = 10, height = 6) # Saves as a PDF


#################
#S-Figure 4 Ages#
#################

my_data <- readRDS("Active_members.rds")


my_data <- my_data %>%
  select(c(w24_0511_lopenr_person, idrett2, foedselsaar, 11:19))

#Long data#
library(tidyverse)
my_data <- my_data %>%
  pivot_longer(cols = 4:12, names_to = "year", values_to = "deltar")

my_data$year <- as.numeric(my_data$year)

my_data$alder <- my_data$year - my_data$foedselsaar

rm(list = setdiff(ls(), "my_data"))
gc()


collapsed_data <- my_data %>%
  group_by(year, idrett2) %>%
  summarise(
    nsmd = sum(deltar)
  )

library(readxl)

members <- read_excel("N:/durable/data/Medlemmer_alle.xlsx")
members <- members %>%
  filter(age == "young")

members$`2015` <- as.numeric(members$`2015`)
members$`2016` <- as.numeric(members$`2016`)
members$`2017` <- as.numeric(members$`2017`)
members$`2018` <- as.numeric(members$`2018`)


members <- members %>%
  select(2:10, idrett2)

members <- members %>%
  pivot_longer(cols = 1:9, names_to = "year", values_to = "nif")

members$year <- as.numeric(members$year)

master <- left_join(collapsed_data, members, by = c("idrett2", "year"))

master$share <- master$nsmd/master$nif

master <- master %>%
  filter(!is.na(share))

master2 <- master %>%
  mutate(share = ifelse(nif == 0, 0, share))

master2 <- master2 %>%
  mutate(coverage = case_when(
    share >= 0.8 ~ "Excellent coverage (80-100%)",
    share >= 0.6 ~ "Good coverage (60-79%)",
    share >= 0.4 ~ "Fair coverage (40-59%)",
    TRUE         ~ "Poor coverage (0-39%)"
  ))

share_sports <- master2 %>% 
  select(idrett2, year, coverage, share)


df_filtered <- my_data 

top10_idrett2 <- df_filtered %>%
  group_by(idrett2) %>%
  summarise(total_nsmd = sum(deltar, na.rm = TRUE)) %>%
  arrange(desc(total_nsmd)) %>%
  slice_head(n = 10) %>%
  pull(idrett2)

# 2. Keep only rows with those sports
df_top <- df_filtered %>%
  filter(idrett2 %in% top10_idrett2)


df_pct <- df_top %>%
  group_by(idrett2, year, alder) %>%
  summarise(total_nsmd = sum(deltar, na.rm = TRUE))

share_sports$year <- as.numeric(share_sports$year)
df_pct$year <- as.numeric(df_pct$year)

df <- left_join(df_pct, share_sports, by = c("idrett2", "year"))

english <- read_excel("N:/durable/data/Engelske_navn_idretter.xlsx")
df_x <- left_join(df, english, by = "idrett2")


df_x2 <- df_x %>%
  group_by(english, coverage, alder) %>%
  summarise(n = sum(total_nsmd), .groups = "drop") %>%
  group_by(english, coverage) %>%
  mutate(pct = n / sum(n) * 100)

library(ggplot2)
library(viridis)
library(scales)

library(showtext)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()

df_x2 <- df_x2 %>%
  mutate(coverage = fct_relevel(coverage, "Excellent coverage (80-100%)", "Good coverage (60-79%)", "Fair coverage (40-59%)", "Poor coverage (0-39%)"))


df_x2 <- df_x2 %>%
  mutate(
    cat = case_when(
      coverage == "Excellent coverage (80-100%)" ~ ">80%",
      coverage == "Good coverage (60-79%)" ~ "60-79%",
      coverage == "Fair coverage (40-59%)" ~ "40-59%",
      coverage == "Poor coverage (0-39%)" ~ "<40%"
    ),
    cat = factor(cat, levels = c("<40%", "40-59%", "60-79%", ">80%"))
  )


# Create the overlapping density plot
age_graph <- ggplot(df_x2, aes(x = as.factor(alder), y = pct, fill = cat, group = cat)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  facet_wrap(~ english, scales = "free") +
  # Use a sophisticated color palette
  scale_fill_manual(
    name = "Coverage",
    values = c(
      "<40%" = "#d73027",    # red
      "40-59%" = "#fc8d59",   # orange
      "60-79%" = "#d9ef8b",   # light green
      ">80%" = "#1a9850"      # dark green
    )) +
  # Format x-axis as currency
  #scale_x_continuous(labels = label_currency(prefix = "$", suffix = "K", scale = 1e-3),
  #                   breaks = pretty_breaks(n = 6)) +
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    # Grid and background
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5),
    panel.grid.major.y = element_line(color = "grey95", size = 0.3),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Text styling
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(size = 14, color = "grey40", margin = margin(b = 25)),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    
    
    # Plot margins
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Labels
  labs(
    #title = "Age Distribution by Sports Data Coverage Completeness",
    #subtitle = "Comparing age patterns between years with complete vs. incomplete data coverage",
    x = "Age",
    y = "Density",
  ) +
  
  # Add guides
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.9)),
  )

age_graph

ggsave("SFigure4_Age.pdf", plot = age_graph, width = 12, height = 8) # Saves as a PDF



#Geographical coverage#

#Hente inn fylke#
fylke <- read.csv("N:/durable/data/ssb/W25_2904_TIDSPUNKTBESTEMTE_LOPENR.csv")

fylke <- fylke %>%
  select(c(w24_0511_lopenr_person, 8:17))

pers_fylke <- fylke %>%
  pivot_longer(cols = 2:11, names_to = "year", values_to = "fylke")

pers_fylke <- pers_fylke %>%
  mutate(year = sub("bostedsfylke_01_01_", "", year))

#Since fylke are registered on 1. january and membership on 31 december, we take fylke - 1 year
pers_fylke$year <- as.numeric(pers_fylke$year) - 1
pers_fylke$year <- as.factor(pers_fylke$year)


library(readxl)
fylke_name <- read_excel("N:/durable/data/Fylker.xlsx")

fylke_pre_2020 <- fylke_name %>%
  select(Fylkesnr_2019, Fylkesnr_2020, Fylkesnavn_2020)

fylke_post_2024 <- fylke_name %>%
  select(Fylkesnr_2024, Fylkesnr_2020, Fylkesnavn_2020)

fylke_pre_2020$fylke <- as.numeric(fylke_pre_2020$Fylkesnr_2019)
fylke_post_2024$fylke <- as.numeric(fylke_post_2024$Fylkesnr_2024)

fylke_pre_2020 <- fylke_pre_2020 %>%
  select(-Fylkesnr_2019)

fylke_post_2024 <- fylke_post_2024 %>%
  select(-Fylkesnr_2024)

fylke_konvertering <- rbind(fylke_pre_2020, fylke_post_2024)

fylke_konvertering <- fylke_konvertering %>%
  mutate(Fylkesnavn_2020 = if_else(Fylkesnavn_2020 == "TR??NEDELAG", "TR??NDELAG", Fylkesnavn_2020))

#Add manually Viken, Troms og Finnmark and Vestfold and Telemark#
df <- tibble(
  Fylkesnr_2020 = c("30", "38", "54"),
  Fylkesnavn_2020 = c("VIKEN", "VESTFOLD OG TELEMARK", "TROMS OG FINNMARK"),
  fylke = c(30, 38, 54)
)

fylke_konvertering <- rbind(fylke_konvertering, df)

fylke_konvertering <- fylke_konvertering %>%
  distinct()

long_data_fylke <- pers_fylke %>%
  left_join(fylke_konvertering, by = "fylke")


long_data_fylke$year <- as.numeric(long_data_fylke$year)
long_data_fylke$year <- long_data_fylke$year + 2013

df2 <- my_data %>%
  left_join(long_data_fylke, by = c("w24_0511_lopenr_person", "year"))

df2 <- left_join(df2, share_sports, by = c("idrett2", "year"))

english <- read_excel("N:/durable/data/Engelske_navn_idretter.xlsx")
df_x <- left_join(df2, english, by = "idrett2")

subset <- df_x %>%
  filter(english == "Football" | english == "Handball" |
           english == "Skiing" | english == "Gymnastics" |
           english == "Athletics" | english == "Basketball" | 
           english == "Volleyball" | english == "Martial arts" | 
           english == "Bandy" | english == "Equestrian")

subset <- subset %>%
  filter(deltar == 1)

df_pct <- subset %>%
  group_by(english, coverage, Fylkesnavn_2020) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(english, coverage) %>%
  mutate(pct = n / sum(n) * 100)

library(ggplot2)
library(viridis)
library(scales)

library(showtext)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()

library(forcats)
df_pct <- df_pct %>%
  mutate(coverage = fct_relevel(coverage, "Excellent coverage (80-100%)", "Good coverage (60-79%)", "Fair coverage (40-59%)", "Poor coverage (0-39%)"))


df_pct <- df_pct %>%
  filter(!is.na(Fylkesnavn_2020))

df_pct <- df_pct %>%
  mutate(
    cat = case_when(
      coverage == "Excellent coverage (80-100%)" ~ ">80%",
      coverage == "Good coverage (60-79%)" ~ "60-79%",
      coverage == "Fair coverage (40-59%)" ~ "40-59%",
      coverage == "Poor coverage (0-39%)" ~ "<40%"
    ),
    cat = factor(cat, levels = c("<40%", "40-59%", "60-79%", ">80%"))
  )

# Create the overlapping density plot
county_graph <- ggplot(df_pct, aes(x = as.factor(Fylkesnavn_2020), y = pct, fill = cat, group = cat)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  coord_flip() +
  facet_wrap(~ english, scales = "free") +
  # Use a sophisticated color palette
  scale_fill_manual(
    name = "Coverage",
    values = c(
      "<40%" = "#d73027",    # red
      "40-59%" = "#fc8d59",   # orange
      "60-79%" = "#d9ef8b",   # light green
      ">80%" = "#1a9850"      # dark green
    )) +
  # Format x-axis as currency
  #scale_x_continuous(labels = label_currency(prefix = "$", suffix = "K", scale = 1e-3),
  #                   breaks = pretty_breaks(n = 6)) +
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    # Grid and background
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5),
    panel.grid.major.y = element_line(color = "grey95", size = 0.3),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Text styling
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(size = 14, color = "grey40", margin = margin(b = 25)),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 8),
    
    # Plot margins
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Labels
  labs(
    #title = "County Distribution by Sports Data Coverage Completeness",
    #subtitle = "Comparing county patterns between years with complete vs. incomplete data coverage",
    x = "County",
    y = "Density",
  ) +
  
  # Add guides
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.9)),
  )

county_graph

ggsave("SFigure5_Counties.pdf", plot = county_graph, width = 18, height = 14) # Saves as a PDF

#Income graph#
#Income#
income <- read.csv("N:/durable/data/ssb/W24_0511_INNT.csv", encoding = "UTF-6", sep = ";")

income <- income %>%
  filter(aargang >= 2014 & aargang <= 2023)

income$aargang <- income$aargang + 1

income <- income %>%
  select(c(w24_0511_lopenr_person, lopenr_husholdning, aargang, saminnt))

#income2 <- income %>%
#  group_by(lopenr_husholdning, aargang) %>%
#  summarise(income = sum(saminnt, na.rm = TRUE), .groups = "drop")

income_father <- income
income_father$lopenr_far <- income_father$w24_0511_lopenr_person
income_father$father_household <- income_father$lopenr_husholdning
income_father$year <- as.character(income_father$aargang)
income_father$saminnt_far <- income_father$saminnt


income_mother <- income
income_mother$lopenr_mor <- income_mother$w24_0511_lopenr_person
income_mother$mother_household <- income_mother$lopenr_husholdning
income_mother$year <- as.character(income_mother$aargang)
income_mother$saminnt_mor <- income_mother$saminnt


income_father <- income_father %>%
  select(lopenr_far, father_household, year, saminnt_far)

income_mother <- income_mother %>%
  select(lopenr_mor, mother_household, year, saminnt_mor)

#rm(income, income2)

gc()

message("Income created.")

income_father$year <- as.numeric(income_father$year)
income_mother$year <- as.numeric(income_mother$year)

#long_data$year <- as.numeric(long_data$year)

df2$year <- as.numeric(as.character(df2$year))

#Retrieve mothers and fathers#
mother_and_father <- read.csv("N:/durable/data/ssb/W24_0511_FASTE_OPPL_UTEN_FDATO.csv")
mother_and_father <- mother_and_father %>%
  select(w24_0511_lopenr_person, lopenr_mor, lopenr_far)

df2 <- df2 %>%
  left_join(mother_and_father, by = "w24_0511_lopenr_person")

df <- left_join(df2, income_mother, by = c("lopenr_mor", "year"))
df <- left_join(df, income_father, by = c("lopenr_far", "year"))

rm(long_data, income_father, income_mother)
gc()

#share_sports$year <- as.numeric(share_sports$year)
#df2 <- left_join(df2, share_sports, by = c("idrett2", "year"))

df <- df %>%
  mutate(parental_income = rowSums(across(c(saminnt_far, saminnt_mor)), na.rm = TRUE))

df <- df %>%
  mutate(parental_income = ifelse(parental_income < 0, 0, parental_income))

#Since income is lagged one year, KPI needs to be the year after,
#Hence the KPI for 2023 reported here is the actual KPI for 2022

df <- df %>%
  mutate(KPI = case_when(
    year == 2023 ~ 122.8,
    year == 2022 ~ 116.1,
    year == 2021 ~ 112.2,
    year == 2020 ~ 110.8,
    year == 2019 ~ 108.4,
    year == 2018 ~ 105.5,
    year == 2017 ~ 103.6,
    year == 2016 ~ 100.0,
    year == 2015 ~ 97.9,
    TRUE ~ NA_real_  # default for unmatched years
  ))

df$KPI_2022 <- 122.8
df$income_2022 <- df$parental_income * (df$KPI_2022/df$KPI)

english <- read_excel("N:/durable/data/Engelske_navn_idretter.xlsx")
df_x <- left_join(df, english, by = "idrett2")

subset <- df_x %>%
  filter(english == "Football" | english == "Handball" |
           english == "Skiing" | english == "Gymnastics" |
           english == "Athletics" | english == "Basketball" | 
           english == "Volleyball" | english == "Martial arts" | 
           english == "Bandy" | english == "Equestrian")

subset <- subset %>%
  filter(deltar == 1)

library(ggplot2)
library(scales)

library(showtext)
font_add(family = "Outfit", regular = "N:/durable/fonts/Outfit/static/Outfit-Regular.ttf")
showtext_auto()

library(forcats)
subset <- subset %>%
  mutate(coverage = fct_relevel(coverage, "Excellent coverage (80-100%)", "Good coverage (60-79%)", "Fair coverage (40-59%)", "Poor coverage (0-39%)"))


subset <- subset %>%
  mutate(
    cat = case_when(
      coverage == "Excellent coverage (80-100%)" ~ ">80%",
      coverage == "Good coverage (60-79%)" ~ "60-79%",
      coverage == "Fair coverage (40-59%)" ~ "40-59%",
      coverage == "Poor coverage (0-39%)" ~ "<40%"
    ),
    cat = factor(cat, levels = c("<40%", "40-59%", "60-79%", ">80%"))
  )

# Create the overlapping density plot
income_plot <- ggplot(subset, aes(x = income_2022, fill = cat, color = cat)) +
  geom_density(alpha = 0.3, size = 0.8) +
  facet_wrap(~ english, scales = "free") +
  # Use a sophisticated color palette
  scale_fill_manual(
    name = "Coverage",
    values = c(
      "<40%" = "#d73027",    # red
      "40-59%" = "#fc8d59",   # orange
      "60-79%" = "#d9ef8b",   # light green
      ">80%" = "#1a9850"      # dark green
    )) +
  scale_color_manual(
    values = c(
      "<40%" = "#d73027",    # red
      "40-59%" = "#fc8d59",   # orange
      "60-79%" = "#d9ef8b",   # light green
      ">80%" = "#1a9850"      # dark green
    )) +
  # Format x-axis as currency
  #scale_x_continuous(labels = label_currency(prefix = "$", suffix = "K", scale = 1e-3),
  #                   breaks = pretty_breaks(n = 6)) +
  scale_x_log10(limits=c(500000, 5000000)) +
  # Clean theme
  theme_minimal(base_size = 14) +
  theme(
    # Grid and background
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.5),
    panel.grid.major.y = element_line(color = "grey95", size = 0.3),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Text styling
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(size = 14, color = "grey40", margin = margin(b = 25)),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    
    # Plot margins
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Labels
  labs(
    #title = "Income Distribution by Sports Data Coverage Completeness",
    #subtitle = "Comparing socioeconomic patterns between years with complete vs. incomplete data coverage\nIncome as both parents' brutto income measured in 2022 NOK",
    x = "Annual Income",
    y = "Density",
  ) +
  
  # Add guides
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.8)),
    color = "none"
  )

income_plot

ggsave("SFigure6_income.pdf", plot = income_plot, width = 15, height = 8) # Saves as a PDF


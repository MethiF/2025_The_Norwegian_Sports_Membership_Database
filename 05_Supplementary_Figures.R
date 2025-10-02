###############
#S-FIGURE 2--4#
###############

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



long_data <- long_data %>%
  filter(alder > 12 & alder < 20)

collapsed_df <- long_data %>%
  group_by(idrett2, year) %>%
  summarise(members = sum(deltar), .groups = "drop")

gc()

collapsed_df$year <- as.factor(collapsed_df$year)

library(readxl)
share_sports <- read_excel("C:/Users/frme/OneDrive - Folkehelseinstituttet/Dokumenter/PhD/Cohort Profile/coverage_share.xlsx")

df <- long_data

df <- left_join(df, share_sports, by = c("idrett2", "year"))

subset <- df %>%
  filter(idrett2 == "Football" | idrett2 == "Handball" |
           idrett2 == "Skiing" | idrett2 == "Gymnastics" |
           idrett2 == "Athletics" | idrett2 == "Basketball" | 
           idrett2 == "Volleyball" | idrett2 == "Martial arts" | 
           idrett2 == "Bandy" | idrett2 == "Swimming")

subset <- subset %>%
  filter(deltar == 1)

df_pct <- subset %>%
  group_by(idrett2, coverage, alder) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(idrett2, coverage) %>%
  mutate(pct = n / sum(n) * 100)



library(forcats)
df_pct <- df_pct %>%
  mutate(coverage = fct_relevel(coverage, "Excellent coverage (80-100%)", "Good coverage (60-79%)", "Fair coverage (40-59%)", "Poor coverage (0-39%)"))

df_pct <- df_pct %>%
  mutate(coverage = fct_relevel(coverage, "Excellent coverage (80-100%)", "Good coverage (60-79%)", "Fair coverage (40-59%)", "Poor coverage (0-39%)"))

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
age_graph <- ggplot(df_pct, aes(x = as.factor(alder), y = pct, fill = cat, group = cat)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  facet_wrap(~ idrett2, scales = "free") +
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

##########
#COUNTIES#
##########

pers_fylke <- df_fylke %>%
  select(c(w24_0511_lopenr_person, 7:16))

pers_fylke <- pers_fylke %>%
  pivot_longer(cols = 2:11, names_to = "year", values_to = "fylke")

pers_fylke <- pers_fylke %>%
  mutate(year = sub("bostedsfylke_01_01_", "", year))

#Since fylke are registered on 1. january and membership on 31 december, we take fylke - 1 year
pers_fylke$year <- as.numeric(pers_fylke$year) - 1
pers_fylke$year <- as.factor(pers_fylke$year)


library(readxl)
fylke_name <- read_excel("C:/Users/frme/OneDrive - Folkehelseinstituttet/Dokumenter/PhD/Fylker.xlsx")

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


#Add manually Viken, Troms og Finnmark and Vestfold and Telemark#
df <- tibble(
  Fylkesnr_2020 = c("30", "38", "54"),
  Fylkesnavn_2020 = c("VIKEN", "VESTFOLD OG TELEMARK", "TROMS OG FINNMARK"),
  fylke = c(30, 38, 54)
)

fylke_konvertering <- rbind(fylke_konvertering, df)

fylke_konvertering <- fylke_konvertering %>%
  distinct()

pers_fylke$fylke <- as.numeric(pers_fylke$fylke)

fylke_konvertering <- fylke_konvertering[-31, ]

long_data2 <- pers_fylke %>%
  left_join(fylke_konvertering, by = "fylke")

long_data$year <- as.factor(long_data$year)

long_data$w24_0511_lopenr_person <- long_data$ID

df2 <- long_data %>%
  left_join(long_data2, by = c("w24_0511_lopenr_person", "year"))

share_sports$year <- as.factor(share_sports$year)

df_x <- left_join(df2, share_sports, by = c("idrett2", "year"))

subset <- df_x %>%
  filter(idrett2 == "Football" | idrett2 == "Handball" |
           idrett2 == "Skiing" | idrett2 == "Gymnastics" |
           idrett2 == "Athletics" | idrett2 == "Basketball" | 
           idrett2 == "Volleyball" | idrett2 == "Martial arts" | 
           idrett2 == "Bandy" | idrett2 == "Swimming")

subset <- subset %>%
  filter(deltar == 1)


df_pct <- subset %>%
  group_by(idrett2, coverage, Fylkesnavn_2020) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(idrett2, coverage) %>%
  mutate(pct = n / sum(n) * 100)


library(ggplot2)
library(viridis)
library(scales)


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
  facet_wrap(~ idrett2, scales = "free") +
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



##############
#INCOME GRAPH#
##############


#Income (should be done better, just a test!)#
income <- df_income %>%
  filter(aargang >= 2014 & aargang <= 2023)

income$aargang <- income$aargang + 1

income <- income %>%
  select(c(w24_0511_lopenr_person, aargang, saminnt))

#income2 <- income %>%
#  group_by(lopenr_husholdning, aargang) %>%
#  summarise(income = sum(saminnt, na.rm = TRUE), .groups = "drop")

income_father <- income
income_father$lopenr_far <- income_father$w24_0511_lopenr_person
income_father$year <- as.character(income_father$aargang)
income_father$saminnt_far <- income_father$saminnt

income_mother <- income
income_mother$lopenr_mor <- income_mother$w24_0511_lopenr_person
income_mother$year <- as.character(income_mother$aargang)
income_mother$saminnt_mor <- income_mother$saminnt


income_father <- income_father %>%
  select(lopenr_far, year, saminnt_far)

income_mother <- income_mother %>%
  select(lopenr_mor, year, saminnt_mor)


income_father$year <- as.numeric(income_father$year)
income_mother$year <- as.numeric(income_mother$year)

#long_data$year <- as.numeric(long_data$year)

df2$year <- as.numeric(as.character(df2$year))

#Add parents#
df3 <- left_join(df2, df_parents, by = "w24_0511_lopenr_person")

df <- left_join(df3, income_mother, by = c("lopenr_mor", "year"))
df <- left_join(df, income_father, by = c("lopenr_far", "year"))


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

subset <- df %>%
  filter(idrett2 == "Football" | idrett2 == "Handball" |
           idrett2 == "Skiing" | idrett2 == "Gymnastics" |
           idrett2 == "Athletics" | idrett2 == "Basketball" | 
           idrett2 == "Volleyball" | idrett2 == "Martial arts" | 
           idrett2 == "Bandy" | idrett2 == "Swimming")


share_sports$year <- as.numeric(as.character(share_sports$year))
subset <- left_join(subset, share_sports, by = c("idrett2", "year"))



subset <- subset %>%
  filter(deltar == 1)

library(ggplot2)
library(scales)

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
  facet_wrap(~ idrett2, scales = "free") +
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




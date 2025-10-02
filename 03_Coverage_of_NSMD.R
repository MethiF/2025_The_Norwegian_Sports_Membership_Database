# Figure 2a, 2b, S-Figure 1: Coverage of NSMD by year #

library(readxl)
share_sports <- read_excel("C:/Users/frme/OneDrive - Folkehelseinstituttet/Dokumenter/PhD/Cohort Profile/NSMD_NIF_members.xlsx")

remove <- c("Company sport", "Curling", "Golf", "Student sports", "Squash", "Multisport", "Swimming")

#Medlemmer totalt alle idretter#
alle <- share_sports %>%
  filter(!idrett2 %in% remove)

collapsed_df2 <- alle %>%
  group_by(year) %>%
  summarise(members = sum(NSMD),
            nif = sum(NIF),
            .groups = "drop")

collapsed_df2$share <- collapsed_df2$members/collapsed_df2$nif
collapsed_df2$year <- as.factor(collapsed_df2$year)
collapsed_df2$pct <- collapsed_df2$share*100


collapsed_df2 <- collapsed_df2 %>%
  mutate(
    percentage_label = paste0(round(pct, 1), "%")
  )

#Figure 2a: Coverage of NSMD by year.

ggplot(collapsed_df2, aes(x = year, y = share, group = 1)) +
  geom_area(alpha = 0.3, show.legend = FALSE, fill = "#099eee") +
  geom_line(size = 1, colour = "#122eee", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_point(data = collapsed_df2, aes(x = year, y = share)) +
  geom_text(data = collapsed_df2, aes(x = year, y = share, label = percentage_label), 
            vjust = -1.5, hjust = +0.5, size = 5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    x = "Year",
    y = "Share (%)",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0)
  )




#Figure 2b: Coverage of top 10 sports in NSMD by year.

top_10 <- c("Football", "Handball", "Skiing", "Athletics", "Gymnastics", "Martial arts", "Swimming", "Volleyball", "Basketball", "Bandy")

collapsed_df3 <- alle %>%
  group_by(year, idrett2) %>%
  summarise(members = sum(NSMD),
            nif = sum(NIF),
            .groups = "drop")

collapsed_df3$share <- collapsed_df3$members/collapsed_df3$nif
collapsed_df3$year <- as.factor(collapsed_df3$year)
collapsed_df3$pct <- collapsed_df3$share*100


top_10_data <- collapsed_df3 %>%
  filter(idrett2 %in% top_10)


top_10_data <- top_10_data %>%
  mutate(
    cat = case_when(
      share < 0.40 ~ "<40%",
      share >= 0.40 & share < 0.60 ~ "40-59%",
      share >= 0.60 & share < 0.80 ~ "60-79%",
      share >= 0.80 ~ ">80%"
    ),
    cat = factor(cat, levels = c("<40%", "40-59%", "60-79%", ">80%"))
  )

ggplot(top_10_data, aes(x = year, y = share, group = idrett2, fill = cat), color = "grey80") +
  #geom_area(alpha = 0.3, show.legend = FALSE) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  facet_wrap(~ idrett2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1.2)) +
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
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0)
  )



#S-Figure 1: Coverage by type of sport

collapsed_df4 <- share_sports %>%
  group_by(year, idrett2) %>%
  summarise(members = sum(NSMD),
            nif = sum(NIF),
            .groups = "drop")

collapsed_df4$share <- collapsed_df4$members/collapsed_df4$nif
collapsed_df4$year <- as.factor(collapsed_df4$year)
collapsed_df4$pct <- collapsed_df4$share*100

collapsed_df4 <- collapsed_df4 %>%
  mutate(
    cat = case_when(
      share < 0.40 ~ "<40%",
      share >= 0.40 & share < 0.60 ~ "40-59%",
      share >= 0.60 & share < 0.80 ~ "60-79%",
      share >= 0.80 ~ ">80%"
    ),
    cat = factor(cat, levels = c("<40%", "40-59%", "60-79%", ">80%"))
  )



ggplot(collapsed_df4, aes(x = year, y = share, group = idrett2, fill = cat), color = "grey80") +
  #geom_area(alpha = 0.3, show.legend = FALSE) +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  facet_wrap(~ idrett2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1.2)) +
  scale_x_discrete(labels = function(x) paste0("'", substr(as.character(x), 3, 4))) +
  labs(
    #title = "Share of Members in NSMD vs. NIF's reports",
    #subtitle = "Ages 13 to 19",
    x = "Year",
    y = "Share (%)",
    #caption = "Dashed line represent 100%. Members were counted as being a registered member at the end of the year (e.g. 31st of December 2023 for 2023)."
  ) +
  scale_fill_manual(
    name = "Coverage",
    values = c(
      "<40%" = "#d73027",    # red
      "40-59%" = "#fc8d59",   # orange
      "60-79%" = "#d9ef8b",   # light green
      ">80%" = "#1a9850"      # dark green
    )) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0)
  )


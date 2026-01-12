#01_Table_1_Descriptives#

#NB! This code provides S-Table 2. Just cut the top 10 sports to be used in Table 1.#

# Data preparation
prepare_membership_data <- function(data) {
  
  # Convert dates and calculate variables
  data_clean <- data %>%
    mutate(
      # Convert dates if they're not already Date objects
      Start_date = as.Date(startdate),
      End_date = as.Date(enddate),
      
      # Calculate age at membership start
      Age_at_Start = year(Start_date) - foedselsaar,
      
      # Calculate membership duration in days
      Duration_Days = as.numeric(End_date - Start_date) + 1,
      
      # Ensure gender is properly formatted
      Gender = as.factor(kjoenn),
      
      # Create age categories
      Age_Category = case_when(
        Age_at_Start <= 12 ~ "Children (0-12 years)",
        Age_at_Start >= 13 & Age_at_Start <= 19 ~ "Adolescents (13-19 years)",
        Age_at_Start >= 20 & Age_at_Start <= 39 ~ "Young adults (20-39 years)",
        Age_at_Start >= 40 & Age_at_Start <= 59 ~ "Middle-aged (40-59 years)",
        Age_at_Start >= 60 ~ "Older adults (>60 years)"
      ),
      
      # Create duration categories
      Duration_Category = case_when(
        Duration_Days <= 30 ~ "Very short (1-30 days)",
        Duration_Days >= 31 & Duration_Days <= 90 ~ "Short (31-90 days)",
        Duration_Days >= 91 & Duration_Days <= 365 ~ "Medium (91-365 days)",
        Duration_Days > 365 ~ "Long (>365 days)"
      )
    ) %>%
    # Remove invalid ages or durations
    filter(Age_at_Start >= 0 & Age_at_Start <= 100,
           Duration_Days > 0) # max 10 years
  
  return(data_clean)
}




create_descriptive_table <- function(data) {
  
  # Summary statistics by group
  summary_stats <- data %>%
    group_by(Group) %>%
    summarise(
      n_memberships = n(),
      pct_memberships = round(n() / nrow(data[data$Group == "All memberships", ]) * 100, 1),
      mean_duration = round(mean(Duration_Days, na.rm = TRUE), 0),
      sd_duration = round(sd(Duration_Days, na.rm = TRUE), 0),
      median_duration = round(median(Duration_Days, na.rm = TRUE), 0),
      q25_duration = round(quantile(Duration_Days, 0.25, na.rm = TRUE), 0),
      q75_duration = round(quantile(Duration_Days, 0.75, na.rm = TRUE), 0),
      unique_individuals = n_distinct(lno),
      female_n = sum(kjoenn == 2, na.rm = TRUE),
      female_pct = round(sum(kjoenn == 2, na.rm = TRUE) / n() * 100, 1),
      .groups = "drop"
    )
  
  # Age category breakdown
  age_breakdown <- data %>%
    count(Group, Age_Category) %>%
    group_by(Group) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  
  # Duration category breakdown
  duration_breakdown <- data %>%
    count(Group, Duration_Category) %>%
    group_by(Group) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  
  # Top 10 most common sports by overall n, then get their counts by group
  top_sports_list <- data %>%
    count(idrett2) %>%
    arrange(desc(n)) %>%
    pull(idrett2)
  
  sports_breakdown <- data %>%
    filter(idrett2 %in% top_sports_list) %>%
    count(Group, idrett2) %>%
    group_by(Group) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  
  # Printing table
  cat("Table 1: Descriptive Characteristics of Norwegian Sports Federation Memberships\n")
  cat("=", rep("=", 100), "\n", sep = "")
  
  cat(sprintf("%-50s %-20s %-20s\n", 
              "Characteristic", "All memberships", "Active memberships"))
  cat(rep("-", 95), "\n", sep = "")
  
  # Helper to print rows for both groups
  print_row <- function(label, var_all, var_active) {
    cat(sprintf("%-50s %-20s %-20s\n", label, var_all, var_active))
  }
  
  # --- Extract group-specific values ---
  summary_all <- summary_stats[summary_stats$Group == "All memberships", ]
  summary_active <- summary_stats[summary_stats$Group == "Active memberships", ]
  
  # ---- Total memberships ----
  print_row(
    "Total memberships, n (%)",
    paste0(format(summary_all$n_memberships, big.mark = ","), " (100.0)"),
    paste0(format(summary_active$n_memberships, big.mark = ","), " (",
           round(summary_active$n_memberships / summary_all$n_memberships * 100, 1), ")")
  )
  
  # ---- Unique individuals ----
  print_row(
    "Unique individuals, n (%)",
    paste0(format(summary_all$unique_individuals, big.mark = ","), " (100.0)"),
    paste0(format(summary_active$unique_individuals, big.mark = ","), " (",
           round(summary_active$unique_individuals / summary_all$unique_individuals * 100, 1), ")")
  )
  
  # ---- Female N (%) ----
  print_row(
    "Female, n (%)",
    paste0(summary_all$female_n, " (", summary_all$female_pct, ")"),
    paste0(summary_active$female_n, " (", summary_active$female_pct, ")")
  )
  
  # ---- Membership duration ----
  print_row(
    "Mean duration (SD)",
    paste0(summary_all$mean_duration, " (", summary_all$sd_duration, ")"),
    paste0(summary_active$mean_duration, " (", summary_active$sd_duration, ")")
  )
  
  print_row(
    "Median duration (IQR)",
    paste0(summary_all$median_duration, " (",
           summary_all$q25_duration, "-", summary_all$q75_duration, ")"),
    paste0(summary_active$median_duration, " (",
           summary_active$q25_duration, "-", summary_active$q75_duration, ")")
  )
  
  # ---- Age categories ----
  cat("\nAge categories, n (%)\n")
  
  for(cat in unique(age_breakdown$Age_Category)) {
    all_row <- age_breakdown %>% filter(Group == "All memberships", Age_Category == cat)
    act_row <- age_breakdown %>% filter(Group == "Active memberships", Age_Category == cat)
    
    print_row(
      paste0("  ", cat),
      if(nrow(all_row)>0) paste0(all_row$n, " (", all_row$pct, ")") else "",
      if(nrow(act_row)>0) paste0(act_row$n, " (", act_row$pct, ")") else ""
    )
  }
  
  # ---- Duration categories ----
  cat("\nDuration categories, n (%)\n")
  
  for(cat in unique(duration_breakdown$Duration_Category)) {
    all_row <- duration_breakdown %>% filter(Group == "All memberships", Duration_Category == cat)
    act_row <- duration_breakdown %>% filter(Group == "Active memberships", Duration_Category == cat)
    
    print_row(
      paste0("  ", cat),
      if(nrow(all_row)>0) paste0(all_row$n, " (", all_row$pct, ")") else "",
      if(nrow(act_row)>0) paste0(act_row$n, " (", act_row$pct, ")") else ""
    )
  }
  
  # ---- Top 10 sports ----
  cat("\nTop 10 most common sports, n (%)\n")
  for(sport in top_sports_list) {
    all_row <- sports_breakdown %>% filter(Group == "All memberships", idrett2 == sport)
    act_row <- sports_breakdown %>% filter(Group == "Active memberships", idrett2 == sport)
    
    print_row(
      paste0("  ", sport),
      if(nrow(all_row) > 0) paste0(all_row$n, " (", all_row$pct, ")") else "",
      if(nrow(act_row) > 0) paste0(act_row$n, " (", act_row$pct, ")") else ""
    )
  }
  
  
  
  return(list(
    summary_stats = summary_stats,
    age_breakdown = age_breakdown,
    duration_breakdown = duration_breakdown,
    sports_breakdown = sports_breakdown
    
  ))
}


final2 <- readRDS("All_members.rds")


collapsed <- final2 %>%
  group_by(idrett2) %>%
  summarise(n = n())

membership_data <- final2 

membership_data$startdate <- as.Date(membership_data$startdate)

membership_data$age <- year(membership_data$startdate) - membership_data$foedselsaar 

all_members <- prepare_membership_data(membership_data)

strict <- readRDS("Active_members.rds")


active_members <- prepare_membership_data(strict)

population <- read.csv("N:/durable/data/ssb/W24_0511_FASTE_OPPL_UTEN_FDATO.csv")
population <- population %>%
  select(w24_0511_lopenr_person, fodeland)

all_members2 <- all_members %>%
  left_join(population, by = "w24_0511_lopenr_person")

sum(all_members2$fodeland == 0)

active_members2 <- active_members %>%
  left_join(population, by = "w24_0511_lopenr_person")

sum(active_members2$fodeland == 0)

all_members2$Group <- "All memberships"
active_members2$Group <- "Active memberships"

all_members2 <- all_members2 %>%
  select(idrett2, w24_0511_lopenr_person, foedselsaar, kjoenn, Start_date, End_date, Age_at_Start, Duration_Days, Gender, Age_Category, Duration_Category, fodeland, Group)

active_members2 <- active_members2 %>%
  select(idrett2, w24_0511_lopenr_person, foedselsaar, kjoenn, Start_date, End_date, Age_at_Start, Duration_Days, Gender, Age_Category, Duration_Category, fodeland, Group)


clean_data <- rbind(all_members2, active_members2)

clean_data$lno <- clean_data$w24_0511_lopenr_person

sink("Table1_STable2.txt")
descriptive_results <- create_descriptive_table(clean_data)
sink()


rm()
gc()



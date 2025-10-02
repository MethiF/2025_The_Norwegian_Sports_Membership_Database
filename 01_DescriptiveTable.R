# Table 2: Descriptive table #

# Load required libraries
library(dplyr)
library(lubridate)
library(gt)
library(gtsummary)
library(tidyverse)
library(readxl)

# Assuming your dataset is called 'membership_data' with columns:
# ID, Start_date, End_date, gender, foedselsaar

# Data preparation
prepare_membership_data <- function(data) {
  
  # Convert dates and calculate variables
  data_clean <- data %>%
    mutate(
      # Convert dates if they're not already Date objects
      Start_date = as.Date(Start_date),
      End_date = as.Date(End_date),
      
      # Calculate age at membership start
      Age_at_Start = year(Start_date) - foedselsaar,
      
      # Calculate membership duration in days
      Duration_Days = as.numeric(End_date - Start_date) + 1,
      
      # Ensure gender is properly formatted
      Gender = as.factor(gender),
      
      # Create age categories
      Age_Category = case_when(
        Age_at_Start <= 12 ~ "Children (0-12 years)",
        Age_at_Start >= 13 & Age_at_Start <= 19 ~ "Adolescents (13-19 years)",
        Age_at_Start >= 20 & Age_at_Start <= 39 ~ "Young adults (20-39 years)",
        Age_at_Start >= 40 & Age_at_Start <= 59 ~ "Middle-aged (40-59 years)",
        Age_at_Start >= 60 ~ "Older adults (???60 years)"
      ),
      
      # Create duration categories
      Duration_Category = case_when(
        Duration_Days <= 30 ~ "Very short (1-30 days)",
        Duration_Days >= 31 & Duration_Days <= 90 ~ "Short (31-90 days)",
        Duration_Days >= 91 & Duration_Days <= 365 ~ "Medium (91-365 days)",
        Duration_Days > 365 ~ "Long (>365 days)"
      ),
      
      # Active participation (???90 days)
      Active_Participation = ifelse(Duration_Days >= 90, "Yes", "No")
    ) %>%
    # Remove invalid ages or durations
    filter(Age_at_Start >= 0 & Age_at_Start <= 100,
           Duration_Days > 0) # max 10 years
  
  return(data_clean)
}

# Create descriptive statistics function
create_descriptive_table <- function(data) {
  
  # Calculate overall statistics
  total_memberships <- nrow(data)
  unique_individuals <- n_distinct(data$ID)
  
  # Create summary statistics by gender
  summary_stats <- data %>%
    group_by(Gender) %>%
    summarise(
      n_memberships = n(),
      pct_memberships = round(n() / total_memberships * 100, 1),
      mean_duration = round(mean(Duration_Days, na.rm = TRUE), 0),
      sd_duration = round(sd(Duration_Days, na.rm = TRUE), 0),
      median_duration = round(median(Duration_Days, na.rm = TRUE), 0),
      q25_duration = round(quantile(Duration_Days, 0.25, na.rm = TRUE), 0),
      q75_duration = round(quantile(Duration_Days, 0.75, na.rm = TRUE), 0),
      .groups = 'drop'
    )
  
  # Calculate overall statistics
  overall_stats <- data %>%
    summarise(
      n_memberships = n(),
      pct_memberships = 100.0,
      mean_duration = round(mean(Duration_Days, na.rm = TRUE), 0),
      sd_duration = round(sd(Duration_Days, na.rm = TRUE), 0),
      median_duration = round(median(Duration_Days, na.rm = TRUE), 0),
      q25_duration = round(quantile(Duration_Days, 0.25, na.rm = TRUE), 0),
      q75_duration = round(quantile(Duration_Days, 0.75, na.rm = TRUE), 0)
    ) %>%
    mutate(Gender = "Overall")
  
  # Combine overall and gender-specific stats
  all_stats <- bind_rows(overall_stats, summary_stats)
  
  # Create age category breakdown
  age_breakdown <- data %>%
    count(Gender, Age_Category) %>%
    group_by(Gender) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  
  # Add overall age breakdown
  age_overall <- data %>%
    count(Age_Category) %>%
    mutate(pct = round(n / sum(n) * 100, 1),
           Gender = "Overall")
  
  age_breakdown_all <- bind_rows(age_overall, age_breakdown)
  
  # Create duration category breakdown
  duration_breakdown <- data %>%
    count(Gender, Duration_Category) %>%
    group_by(Gender) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup()
  
  # Add overall duration breakdown
  duration_overall <- data %>%
    count(Duration_Category) %>%
    mutate(pct = round(n / sum(n) * 100, 1),
           Gender = "Overall")
  
  duration_breakdown_all <- bind_rows(duration_overall, duration_breakdown)
  
  # Active participation breakdown
  active_breakdown <- data %>%
    count(Gender, Active_Participation) %>%
    group_by(Gender) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    ungroup() %>%
    filter(Active_Participation == "Yes")
  
  # Add overall active participation
  active_overall <- data %>%
    count(Active_Participation) %>%
    mutate(pct = round(n / sum(n) * 100, 1),
           Gender = "Overall") %>%
    filter(Active_Participation == "Yes")
  
  active_breakdown_all <- bind_rows(active_overall, active_breakdown)
  
  # Print formatted table stratified by gender
  cat("Table 1: Descriptive Characteristics of Norwegian Sports Federation Memberships\n")
  cat("=", rep("=", 100), "\n", sep = "")
  
  # Create formatted table with gender columns
  gender_levels <- unique(data$Gender)
  if(length(gender_levels) == 0) gender_levels <- c("Male", "Female")
  
  # Print header
  cat(sprintf("%-50s %-20s %-20s %-20s %-10s\n", "Characteristic", "Overall", 
              gender_levels[1], gender_levels[2], "P-value"))
  cat(rep("-", 120), "\n", sep = "")
  
  # Total memberships
  cat(sprintf("%-50s %-20s", "Total memberships, n (%)", 
              paste0(format(total_memberships, big.mark = ","), " (100.0)")))
  
  for(gender in gender_levels) {
    stats <- summary_stats[summary_stats$Gender == gender, ]
    if(nrow(stats) > 0) {
      cat(sprintf(" %-20s", paste0(format(stats$n_memberships, big.mark = ","), " (", stats$pct_memberships, ")")))
    } else {
      cat(sprintf(" %-20s", ""))
    }
  }
  cat(" -\n")
  
  # Unique individuals by gender
  unique_by_gender <- data %>%
    group_by(Gender) %>%
    summarise(unique_ind = n_distinct(ID), .groups = 'drop')
  
  cat(sprintf("%-50s %-20s", "Unique individuals, n (%)", 
              paste0(format(unique_individuals, big.mark = ","), " (100.0)")))
  
  for(gender in gender_levels) {
    gender_unique <- unique_by_gender[unique_by_gender$Gender == gender, ]
    if(nrow(gender_unique) > 0) {
      pct_unique <- round(gender_unique$unique_ind / unique_individuals * 100, 1)
      cat(sprintf(" %-20s", paste0(format(gender_unique$unique_ind, big.mark = ","), " (", pct_unique, ")")))
    } else {
      cat(sprintf(" %-20s", ""))
    }
  }
  cat(" -\n")
  
  # Age statistics
  overall <- all_stats[all_stats$Gender == "Overall", ]
  
  # Age categories
  cat(sprintf("%-50s %-20s", "Age categories, n (%)", ""))
  cat(" <0.001\n")
  
  age_order <- c("Children (0-12 years)", "Adolescents (13-19 years)", 
                 "Young adults (20-39 years)", "Middle-aged (40-59 years)", 
                 "Older adults (???60 years)")
  
  for(age_cat in age_order) {
    overall_age <- age_breakdown_all[age_breakdown_all$Gender == "Overall" & 
                                       age_breakdown_all$Age_Category == age_cat, ]
    
    if(nrow(overall_age) > 0) {
      cat(sprintf("%-50s %-20s", paste0("  ", age_cat), 
                  paste0(format(overall_age$n, big.mark = ","), " (", overall_age$pct, ")")))
      
      for(gender in gender_levels) {
        gender_age <- age_breakdown_all[age_breakdown_all$Gender == gender & 
                                          age_breakdown_all$Age_Category == age_cat, ]
        if(nrow(gender_age) > 0) {
          cat(sprintf(" %-20s", paste0(format(gender_age$n, big.mark = ","), " (", gender_age$pct, ")")))
        } else {
          cat(sprintf(" %-20s", ""))
        }
      }
      cat("\n")
    }
  }
  
  # Duration statistics
  cat(sprintf("%-50s %-20s", "Membership duration", ""))
  cat("\n")
  
  cat(sprintf("%-50s %-20s", "  Mean days (SD)", 
              paste0(overall$mean_duration, " (", overall$sd_duration, ")")))
  
  for(gender in gender_levels) {
    stats <- all_stats[all_stats$Gender == gender, ]
    if(nrow(stats) > 0) {
      cat(sprintf(" %-20s", paste0(stats$mean_duration, " (", stats$sd_duration, ")")))
    } else {
      cat(sprintf(" %-20s", ""))
    }
  }
  cat(" <0.001\n")
  
  cat(sprintf("%-50s %-20s", "  Median days (IQR)", 
              paste0(overall$median_duration, " (", overall$q25_duration, "-", overall$q75_duration, ")")))
  
  for(gender in gender_levels) {
    stats <- all_stats[all_stats$Gender == gender, ]
    if(nrow(stats) > 0) {
      cat(sprintf(" %-20s", paste0(stats$median_duration, " (", stats$q25_duration, "-", stats$q75_duration, ")")))
    } else {
      cat(sprintf(" %-20s", ""))
    }
  }
  cat(" <0.001\n")
  
  # Duration categories
  cat(sprintf("%-50s %-20s", "Duration categories, n (%)", ""))
  cat(" <0.001\n")
  
  duration_order <- c("Very short (1-30 days)", "Short (31-90 days)", 
                      "Medium (91-365 days)", "Long (>365 days)")
  
  for(dur_cat in duration_order) {
    overall_dur <- duration_breakdown_all[duration_breakdown_all$Gender == "Overall" & 
                                            duration_breakdown_all$Duration_Category == dur_cat, ]
    
    if(nrow(overall_dur) > 0) {
      cat(sprintf("%-50s %-20s", paste0("  ", dur_cat), 
                  paste0(format(overall_dur$n, big.mark = ","), " (", overall_dur$pct, ")")))
      
      for(gender in gender_levels) {
        gender_dur <- duration_breakdown_all[duration_breakdown_all$Gender == gender & 
                                               duration_breakdown_all$Duration_Category == dur_cat, ]
        if(nrow(gender_dur) > 0) {
          cat(sprintf(" %-20s", paste0(format(gender_dur$n, big.mark = ","), " (", gender_dur$pct, ")")))
        } else {
          cat(sprintf(" %-20s", ""))
        }
      }
      cat("\n")
    }
  }
  
  # Active participation
  overall_active <- active_breakdown_all[active_breakdown_all$Gender == "Overall", ]
  if(nrow(overall_active) > 0) {
    cat(sprintf("%-50s %-20s", "Active participation (???90 days), n (%)", 
                paste0(format(overall_active$n, big.mark = ","), " (", overall_active$pct, ")")))
    
    for(gender in gender_levels) {
      gender_active <- active_breakdown_all[active_breakdown_all$Gender == gender, ]
      if(nrow(gender_active) > 0) {
        cat(sprintf(" %-20s", paste0(format(gender_active$n, big.mark = ","), " (", gender_active$pct, ")")))
      } else {
        cat(sprintf(" %-20s", ""))
      }
    }
    cat(" <0.001\n")
  }
  
  return(list(
    summary_stats = all_stats,
    age_breakdown = age_breakdown_all,
    duration_breakdown = duration_breakdown_all,
    active_breakdown = active_breakdown_all
  ))
}


# Main execution function
analyze_membership_data <- function(raw_data) {
  
  # Prepare data
  cat("Preparing data...\n")
  clean_data <- prepare_membership_data(raw_data)
  
  # Create descriptive table
  cat("\nCreating descriptive statistics...\n")
  descriptive_results <- create_descriptive_table(clean_data)
  
  return(list(
    clean_data = clean_data,
    descriptive_results = descriptive_results
  ))
}



# Function to filter and adjust dataset based on age criteria
filter_by_age <- function(df) {
  
  # Convert dates to Date objects if they're not already
  df$start_date <- as.Date(df$Start_date)
  df$end_date <- as.Date(df$End_date)
  
  # Define the observation period
  period_start <- as.Date("2015-01-01")
  period_end <- as.Date("2024-12-31")
  
  # Calculate age bounds for each person during the observation period
  df <- df %>%
    mutate(
      # Age at start of observation period (2015)
      age_at_2015 = 2015 - foedselsaar,
      # Age at end of observation period (2024)
      age_at_2024 = 2024 - foedselsaar,
      
      # Calculate when person turns 10 and 70
      date_turns_10 = as.Date(paste0(foedselsaar + 10, "-01-01")),
      date_turns_71 = as.Date(paste0(foedselsaar + 71, "-01-01")),
      
      # Determine valid age window within observation period
      valid_start = pmax(period_start, date_turns_10),
      valid_end = pmin(period_end, date_turns_71 - 1) # -1 because we want up to 70, not including 71
    )
  
  # Filter out people who are never in the 10-70 age range during 2015-2024
  df_filtered <- df %>%
    filter(
      age_at_2024 >= 10,  # At least 10 by end of period
      age_at_2015 <= 70   # At most 70 at start of period
    ) %>%
    filter(valid_start <= valid_end)  # Valid age window exists
  
  # Check if person's observation period overlaps with their valid age window
  df_filtered <- df_filtered %>%
    filter(
      start_date <= valid_end,  # Start before valid window ends
      end_date >= valid_start   # End after valid window starts
    )
  
  # Adjust start and end dates to fit within valid age window
  df_final <- df_filtered %>%
    mutate(
      start_date_adjusted = pmax(start_date, valid_start),
      end_date_adjusted = pmin(end_date, valid_end)
    ) %>%
    select(ID, foedselsaar, gender, idrett2, fodeland,
           Start_date = start_date_adjusted, 
           End_date = end_date_adjusted)
  
  return(df_final)
}


#Filter by age#
result <- filter_by_age(membership_data)

less_than_0days <- result %>%
  filter(End_date < Start_date)

results <- analyze_membership_data(result)

result3 <- prepare_membership_data(result)

#Gender distribution for Table 1#
collapsed <- result3 %>%
  group_by(idrett2, gender) %>%
  summarise(n = n())

#Top sports for Table 1#
all <- result3 %>%
  group_by(idrett2) %>%
  summarise(n = n())

x <- sum(all$n)
all$pct <- format((all$n/x)*100, scientific = FALSE)

#Born in Norway for Table 1#
collapsed_fodeland <- result3 %>%
  group_by(gender) %>%
  summarise(n = sum(fodeland == "Norway"))

collapsed_fodeland_all <- result3 %>%
  summarise(n = sum(fodeland == "Norway"))

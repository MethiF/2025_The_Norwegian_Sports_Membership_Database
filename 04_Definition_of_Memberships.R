#Definitions of memberships: Wide, Intermediate, Strict: Table 3, S-Table 2 #

library(dplyr)
library(tidyverse)
library(data.table)

result <- filter_by_age(membership_data)

master <- result
master$w24_0511_lopenr_person <- master$ID

master$start <- as.numeric(master$Start_date)
master$end <- as.numeric(master$End_date)

#Remove those with start date before end date
master2 <- master %>%
  filter(start <= end)

sub <- master2 %>%
  select(w24_0511_lopenr_person, idrett2, start, end)

setDT(sub)
sub2 <- copy(sub)

setkey(sub, w24_0511_lopenr_person, idrett2, start, end)
setkey(sub2, w24_0511_lopenr_person, idrett2, start, end)

result <- foverlaps(sub, sub2, by.x = c("w24_0511_lopenr_person", "idrett2", "start", "end"),
                    type = "any", nomatch = NULL)


filtered_data <- result %>%
  rowwise() %>%
  mutate(min_value = min(c(start, end, i.start, i.end), na.rm = TRUE),
         max_value = max(c(start, end, i.start, i.end), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(w24_0511_lopenr_person, idrett2, start) %>%
  summarise(min = min(min_value, na.rm = TRUE),
            max = max(max_value, na.rm = TRUE),
            .groups = "drop")

final <- filtered_data %>%
  distinct(w24_0511_lopenr_person, idrett2, min, max, .keep_all = TRUE)

final <- final %>%
  select(w24_0511_lopenr_person, idrett2, min, max)

final <- final %>%
  rename(start = min, end = max)

final$startdate <- as.Date(final$start)
final$enddate <- as.Date(final$end)

#Set end date to October 31, 2024 for those with higher end date#
final <- final %>%
  mutate(enddate = ifelse(enddate > ymd("2024-10-31"), ymd("2024-10-31"), enddate))

final$enddate <- as.Date(final$enddate)

#Calculate days per membership#
final$days <- final$end - final$start

final <- final %>%
  select(w24_0511_lopenr_person, idrett2, startdate, enddate, days)

#Remove unused dataframes
rm(filtered_data, master, master2, nif_medlemmer, result, sub, sub2)
gc()


#This code needs to run 3 times for handball.

#264722

final$start <- as.numeric(final$startdate)
final$end <- as.numeric(final$enddate)

sub <- final %>%
  select(w24_0511_lopenr_person, idrett2, start, end)

#Remove those with start date before end date
sub <- sub %>%
  filter(start <= end)

setDT(sub)
sub2 <- copy(sub)

setkey(sub, w24_0511_lopenr_person, idrett2, start, end)
setkey(sub2, w24_0511_lopenr_person, idrett2, start, end)

result <- foverlaps(sub, sub2, by.x = c("w24_0511_lopenr_person", "idrett2", "start", "end"),
                    type = "any", nomatch = NULL)


filtered_data <- result %>%
  rowwise() %>%
  mutate(min_value = min(c(start, end, i.start, i.end), na.rm = TRUE),
         max_value = max(c(start, end, i.start, i.end), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(w24_0511_lopenr_person, idrett2, start) %>%
  summarise(min = min(min_value, na.rm = TRUE),
            max = max(max_value, na.rm = TRUE),
            .groups = "drop")

final <- filtered_data %>%
  distinct(w24_0511_lopenr_person, idrett2, min, max, .keep_all = TRUE)

final <- final %>%
  select(w24_0511_lopenr_person, idrett2, min, max)

final <- final %>%
  rename(start = min, end = max)

final$startdate <- as.Date(final$start)
final$enddate <- as.Date(final$end)

#Set end date to October 31, 2024 for those with higher end date#
final <- final %>%
  mutate(enddate = ifelse(enddate > ymd("2024-10-31"), ymd("2024-10-31"), enddate))

final$enddate <- as.Date(final$enddate)

#Calculate days per membership#
final$days <- final$end - final$start

final <- final %>%
  select(w24_0511_lopenr_person, idrett2, startdate, enddate, days)

#And third time#

final$start <- as.numeric(final$startdate)
final$end <- as.numeric(final$enddate)

sub <- final %>%
  select(w24_0511_lopenr_person, idrett2, start, end)

#Remove those with start date before end date
sub <- sub %>%
  filter(start <= end)

setDT(sub)
sub2 <- copy(sub)

setkey(sub, w24_0511_lopenr_person, idrett2, start, end)
setkey(sub2, w24_0511_lopenr_person, idrett2, start, end)

result <- foverlaps(sub, sub2, by.x = c("w24_0511_lopenr_person", "idrett2", "start", "end"),
                    type = "any", nomatch = NULL)


filtered_data <- result %>%
  rowwise() %>%
  mutate(min_value = min(c(start, end, i.start, i.end), na.rm = TRUE),
         max_value = max(c(start, end, i.start, i.end), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(w24_0511_lopenr_person, idrett2, start) %>%
  summarise(min = min(min_value, na.rm = TRUE),
            max = max(max_value, na.rm = TRUE),
            .groups = "drop")

final <- filtered_data %>%
  distinct(w24_0511_lopenr_person, idrett2, min, max, .keep_all = TRUE)

final <- final %>%
  select(w24_0511_lopenr_person, idrett2, min, max)

final <- final %>%
  rename(start = min, end = max)

final$startdate <- as.Date(final$start)
final$enddate <- as.Date(final$end)

#Set end date to October 31, 2024 for those with higher end date#
final <- final %>%
  mutate(enddate = ifelse(enddate > ymd("2024-10-31"), ymd("2024-10-31"), enddate))

final$enddate <- as.Date(final$enddate)

#Calculate days per membership#
final$days <- final$end - final$start

final <- final %>%
  select(w24_0511_lopenr_person, idrett2, startdate, enddate, days)



member_ever <- final %>%
  group_by(idrett2) %>%
  summarise(n = n())

member_ninety <- final %>%
  filter(days >= 90) %>%
  group_by(idrett2) %>%
  summarise(n = n())

library(dplyr)
library(lubridate)
library(readxl)





library(dplyr)
library(lubridate)
library(readxl)

sport_seasons <- read_excel("C:/Users/frme/OneDrive - Folkehelseinstituttet/Dokumenter/PhD/Cohort Profile/sesonger.xlsx")

#sport_seasons <- read_excel("N:/durable/data/sesonger.xlsx")
sport_seasons$sport <- sport_seasons$idrett2

master <- left_join(final, sport_seasons, by = "idrett2")

master <- master %>%
  mutate(member_one_year = ifelse(
    
    #Summer seasons#
    (season_start_month == 3 & startdate <= ymd("2015-03-01") & enddate >= ymd("2015-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2016-03-01") & enddate >= ymd("2016-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2017-03-01") & enddate >= ymd("2017-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2018-03-01") & enddate >= ymd("2018-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2019-03-01") & enddate >= ymd("2019-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2020-03-01") & enddate >= ymd("2020-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2021-03-01") & enddate >= ymd("2021-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2022-03-01") & enddate >= ymd("2022-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2023-03-01") & enddate >= ymd("2023-11-01")) |
      (season_start_month == 3 & startdate <= ymd("2024-03-01") & enddate == ymd("2024-10-31")) |
      
      #Winter seasons#
      (season_start_month == 9 & startdate <= ymd("2015-09-01") & enddate >= ymd("2016-05-01")) |
      (season_start_month == 9 & startdate <= ymd("2016-09-01") & enddate >= ymd("2017-05-01")) |
      (season_start_month == 9 & startdate <= ymd("2017-09-01") & enddate >= ymd("2018-05-01")) |
      (season_start_month == 9 & startdate <= ymd("2018-09-01") & enddate >= ymd("2019-05-01")) |
      (season_start_month == 9 & startdate <= ymd("2019-09-01") & enddate >= ymd("2020-05-01")) |
      (season_start_month == 9 & startdate <= ymd("2020-09-01") & enddate >= ymd("2021-05-01")) |
      (season_start_month == 9 & startdate <= ymd("2021-09-01") & enddate >= ymd("2022-05-01")) |
      (season_start_month == 9 & startdate <= ymd("2022-09-01") & enddate >= ymd("2023-05-01")) |
      (season_start_month == 9 & startdate <= ymd("2023-09-01") & enddate >= ymd("2024-05-01")) , 1, 0
  ))



lisens <- df_licenses

lisens$date <- sub(" \\d{2}:\\d{2}:\\d{2}\\.\\d+$", "", lisens$dato)
lisens$date <- ymd(lisens$date)

lisens$licence_end <- lisens$date

lisens$licence_start <- lisens$licence_end - years(1)

lisens <- lisens %>%
  select(w24_0511_lopenr_person, idrett2, licence_start, licence_end) %>%
  distinct()


wide_data <- lisens %>%
  group_by(w24_0511_lopenr_person, idrett2) %>%
  mutate(license_number = row_number()) %>%
  pivot_wider(
    names_from = license_number,
    values_from = c(licence_start, licence_end),
    names_glue = "lisens{license_number}_{.value}"
  )



master2 <- left_join(master, wide_data, by = c("w24_0511_lopenr_person", "idrett2"))

merged_data <- master2

#In the original data people are registered with multiple licences. We therefore need to loop this several times.
#Do not need to do this now#
x <- 1:1

# Initialize the overlap variable
merged_data$license_membership_overlap <- 0

# Loop through each license number
for(i in x) {
  start_col <- paste0("lisens", i, "_licence_start")
  end_col <- paste0("lisens", i, "_licence_end")
  
  merged_data <- merged_data %>%
    mutate(
      license_membership_overlap = case_when(
        license_membership_overlap == 1 ~ 1,  # Keep existing 1s
        (!is.na(!!sym(start_col)) & !is.na(!!sym(end_col)) & 
           !is.na(startdate) & !is.na(enddate) &
           !!sym(start_col) <= enddate & !!sym(end_col) >= startdate) ~ 1,
        TRUE ~ license_membership_overlap
      )
    )
}


new_master <- merged_data %>%
  select(w24_0511_lopenr_person, idrett2, startdate, enddate, days, member_one_year, license_membership_overlap)

new_master <- new_master %>%
  mutate(strict = ifelse(member_one_year == 1 | license_membership_overlap == 1, 1, 0))

strict_by_sport <- new_master %>%
  filter(strict == 1) %>%
  group_by(idrett2) %>%
  summarise(strict = n())

member_ever <- new_master %>%
  filter(days >= 1) %>%
  group_by(idrett2) %>%
  summarise(wide = n())

member_ninety <- new_master %>%
  filter(days >= 90) %>%
  group_by(idrett2) %>%
  summarise(intermediate = n())

all <- new_master %>%
  group_by(idrett2) %>%
  summarise(all = n())

table_data <- left_join(all, member_ever, by = "idrett2")
table_data <- left_join(table_data, member_ninety, by = "idrett2")
table_data <- left_join(table_data, strict_by_sport, by = "idrett2")

table_data <- table_data %>%
  filter(!is.na(strict))


#Data for Table 3 and S-Table 3#.
table_data <- table_data %>%
  mutate(
    wide_pct = paste0(wide, " (", round((wide/all)*100, 1), "%)"),
    intermediate_pct = paste0(intermediate, " (", round((intermediate/all)*100, 1), "%)"),
    strict_pct = paste0(strict, " (", round((strict/all)*100, 1), "%)")
  )


#End this with creating data for descriptives
members_gender_birth <- membership_data %>%
  group_by(ID, gender, foedselsaar) %>%
  select(ID, gender, foedselsaar) %>%
  distinct()

members_gender_birth <- members_gender_birth %>%
  rename(w24_0511_lopenr_person = ID)

for_descriptives <- left_join(new_master, members_gender_birth, by = "w24_0511_lopenr_person")


################################################################
#CHECK CORRELATIONS BETWEEN SEASONAL PARTICIPATION AND LICENCES#
################################################################

birth_year <- membership_data %>%
  select(ID, foedselsaar) %>%
  distinct()

birth_year <- birth_year %>%
  rename(w24_0511_lopenr_person = ID)


check_licence_correlation <- new_master %>%
  left_join(birth_year, by = "w24_0511_lopenr_person")

check_licence_correlation$start_year <- year(check_licence_correlation$startdate)
check_licence_correlation$start_age <- check_licence_correlation$start_year - check_licence_correlation$foedselsaar

lisensidretter <- df_licenses %>%
  select(idrett2) %>%
  distinct()

lisensidretter$lisens_idrett <- 1

check_licence_correlation <- check_licence_correlation %>%
  left_join(lisensidretter, by = "idrett2")

correlation_df <- check_licence_correlation %>%
  filter(lisens_idrett == 1 & start_age >= 13 & strict == 1 & start_age <= 19)

mean(correlation_df$license_membership_overlap[correlation_df$member_one_year == 1], na.rm = TRUE)
mean(correlation_df$member_one_year[correlation_df$license_membership_overlap == 1], na.rm = TRUE)

##########################################
##S-Table X: Characteristics of members##
#########################################

fodeland <- df_persons %>%
  select(c(w24_0511_lopenr_person, fodeland))

for_descriptives <- left_join(for_descriptives, fodeland, by = "w24_0511_lopenr_person")

for_descriptives <- for_descriptives %>%
  rename(ID = w24_0511_lopenr_person,
         Start_date = startdate,
         End_date = enddate)


for_descriptives <- for_descriptives %>%
  mutate(wide = ifelse(days >= 1, 1, 0),
         intermediate = ifelse(days >= 90, 1, 0),
         strict = ifelse(member_one_year == 1 | license_membership_overlap == 1, 1, 0))


#Remove golf, studentidrett og bedriftsidrett#
for_descriptives <- for_descriptives %>%
  filter(idrett2 != "Golf" & idrett2 != "Company" & idrett2 != "Student sports" & idrett2 != "Multisports")

clean_data <- prepare_membership_data(for_descriptives)

wide <- clean_data %>%
  filter(wide == 1)

intermediate <- clean_data %>%
  filter(intermediate == 1)

strict <- clean_data %>%
  filter(strict == 1)

create_descriptive_table(wide)
create_descriptive_table(intermediate)
create_descriptive_table(strict)

table(wide$Gender)
table(intermediate$Gender)
table(strict$Gender)

wide$norge <- wide$fodeland == "Norway"
intermediate$norge <- intermediate$fodeland == "Norway"
strict$norge <- strict$fodeland == "Norway"


sum(wide$norge)
sum(intermediate$norge)
sum(strict$norge)





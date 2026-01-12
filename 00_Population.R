#00_Population#


library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)

Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

setwd("N:/durable/codes/Study 1 Fredrik - The Norwegian Sports Membership Database/")


list_federations <- function() {
  feds <- data.table::fread("N:/durable/data/pop_nif_medlemmer.csv", select =
                              "idrett2")
  feds <- feds[!is.na(idrett2)]
  return(print(unique(feds)))
}

list_activity <- function() {
  activity <- data.table::fread("N:/durable/data/pop_nif_medlemmer.csv", select =
                                  "idrett")
  activity <- activity[!is.na(idrett)]
  return(print(unique(activity)))
}


#FORKLARING CLEAN NIF FUNKSJON
#        Argument:
#        >federation = forbund. Hvis man setter federation = 'all' inkluderer man alle forbund
#        >sep_by_federation = skal idrettsmedlemskapene sl??s sammen etter forbund. Default er nei (F).
#        >sep_by_activity = skal idrettsmedlemskapene sl??s sammen etter forbund. Default er ja (T).
#        Man velger ja(T) hvis man kun er interessert i om personen er medlem i ett idrettsforbund (uten ?? vite hvilket) p?? et tidspunkt
#        >drop_endatebefore2019 = inkluderer ikke medlemskap som slutter f??r 2019. Default satt til ja (true)
#        >days_inbetween = maksimum antall dager som er mellom to medlemskap for at man sl??r sammen medlemskapene. Satt til 0, dvs at man ikke sl??r sammen medlemskap
#        >set_enddate_to_311024 = T by default. Velger om man beholder opprinnelig sluttdato eller setter den til denne datoen (da vi fikk dataene fra NIF)")

# federation <- "Klatring"
# activity<- "all"
# sep_by_federation = T
# sep_by_activity = F
# drop_endatebefore2019 = T
# days_inbetween = 0
# set_enddate_to_311024 = T
clean_nif <- function(federation,
                      activity,
                      sep_by_federation = T,
                      sep_by_activity = F,
                      drop_endatebefore2019 = T,
                      days_inbetween = 0,
                      set_enddate_to_311024 = T) {
  Sys.setlocale("LC_ALL", "nb-NO.UTF-8")
  #Retrieve sport members#
  nif_medlemmer <- as.data.table(fread(
    "N:/durable/data/pop_nif_medlemmer.csv",
    encoding = "UTF-8",
    drop = "V1"
  ))
  nif_medlemmer <- nif_medlemmer[!is.na(idrett2)]
  
  if (sum(!(federation %in% c("all"))) > 0) {
    nif_medlemmer <- nif_medlemmer[idrett2 %in% federation]
  }
  if (sum(!(activity %in% c("all")) > 0)) {
    nif_medlemmer <- nif_medlemmer[idrett %in% activity]
  }
  nif_medlemmer[, dato1 := as.Date(sub(" \\d{2}:\\d{2}:\\d{2}\\.\\d+$", "", dato1))][, dato2 := as.Date(sub(" \\d{2}:\\d{2}:\\d{2}\\.\\d+$", "", dato2))]
  #Drop those with end date before 2019
  if (drop_endatebefore2019 == T) {
    nif_medlemmer <- nif_medlemmer[dato2 > as.Date("2019-12-31") |
                                     is.na(dato2)]
  }
  nif_medlemmer[, dur_days_uncencored := as.numeric(dato2 - dato1)]
  
  nif_medlemmer[, dato2_c := as.Date(ifelse(is.na(dato2) &
                                              !is.na(dato1), as.Date("2024-10-31"), dato2))]
  #End dates after 2024-10-31
  nif_medlemmer[, dato2_c := as.Date(ifelse(
    dato2_c > as.Date("2024-10-31"),
    as.Date("2024-10-31"),
    dato2_c
  ))]
  
  #Remove those with start date before end date
  nif_medlemmer <- nif_medlemmer[dato2_c >= dato1 | is.na(dato1)]
  #No end date?
  setnames(nif_medlemmer, 'w24_0511_lopenr_person', 'lno')
  master <- nif_medlemmer[, c('lno', 'dato1', 'idrett', 'idrett2', 'dato2_c')]
  setorder(master, 'lno', 'dato1')
  setnames(master, c('dato1', 'dato2_c'), c('start', 'end'))
  master[, start := as.numeric(start)]
  master[, end := as.numeric(end)]
 
  master2 <- master
  master_int <- nrow(master2) + 1
  master2[, end := end + days_inbetween]
  
  #For all federations and merged memberships
  if (sum(federation %in% c('all')) > 0 & sep_by_federation == F) {
    var_forovverlaps <- c("lno", "start", "end")
    cleanvars <- c("lno", "start")
  }
  #Without merging membership of different federations
  if (sep_by_federation == T & sep_by_activity == F) {
    var_forovverlaps <- c("lno", "idrett2", "start", "end")
    cleanvars <- c("lno", "start","idrett2")
  }
  
  #Without merging membership of different federations and activities
  if (sep_by_federation == T & sep_by_activity == T) {
    var_forovverlaps <- c("lno","idrett2", "idrett",  "start", "end")
    cleanvars <- c("lno", "start","idrett2", "idrett")
  }
  
  
  
  while (nrow(master2) < master_int) {
    master_int <- nrow(master2)
    sub <- master2[, ..var_forovverlaps]
    sub2 <- copy(sub)
    setkeyv(sub, var_forovverlaps)
    setkeyv(sub2, var_forovverlaps)
    result <- foverlaps(sub,
                        sub2,
                        by.x = var_forovverlaps,
                        type = "any",
                        nomatch = NULL)
    filtered_data <- result %>%
      rowwise() %>%
      mutate(min_value = min(c(start, end, i.start, i.end), na.rm = TRUE),
             max_value = max(c(start, end, i.start, i.end), na.rm = TRUE)) %>%
      ungroup() %>% #Her legge inn en loop?
      group_by(across(all_of(cleanvars))) %>%
      summarise(
        min = min(min_value, na.rm = TRUE),
        max = max(max_value, na.rm = TRUE),
        .groups = "drop"
      )
    final <- filtered_data %>%
      distinct(., .keep_all = TRUE) %>%
      select(-start)%>%
      rename(start = min, end = max)%>%
      select(all_of(var_forovverlaps))
    
    
    master2 <- as.data.table(final)
    #master_int <- nrow(master2)
  }
  
  master2[, startdate := as.Date(start)]
  master2[, enddate := as.Date(end - days_inbetween)]
  
  
  #Set end date to October 31, 2024 for those with higher end date
  if (set_enddate_to_311024 == T) {
    master2[, enddate := if_else(enddate > as.Date("2024-10-31"),
                                 as.Date("2024-10-31"),
                                 enddate)]
  }
  
  master2[, duration_membership := interval(startdate, enddate)]
  master2[, days := as.numeric(time_length(duration_membership, unit = "days"))]
  master2[, months := as.numeric(time_length(duration_membership, unit =
                                               "months"))]
  
  return(master2)
}

sport <- clean_nif(federation = "all", activity = "all", sep_by_federation = T, sep_by_activity = T, drop_endatebefore2019 = F)


#################
#ALL MEMBERSHIPS#
#################

nif_medlemmer <- sport

nif_medlemmer$w24_0511_lopenr_person <- nif_medlemmer$lno

#Remove those not in sport#
nif_medlemmer <- nif_medlemmer %>%
  filter(!is.na(idrett2))

#Keep only those between 13 and 19 between 2015 and 2023: 
#Hente fodeaar#
alder <- read.csv("N:/durable/data/ssb/W24_0511_FASTE_OPPL_UTEN_FDATO.csv")
alder <- alder %>%
  select(c(w24_0511_lopenr_person, foedselsaar, kjoenn))

membership_data <- left_join(nif_medlemmer, alder, by = "w24_0511_lopenr_person")

#Set end date to October 31, 2024 for those with higher end date#
final2 <- membership_data %>%
  mutate(
    startdate = ifelse(startdate < ymd("2015-01-01"), ymd("2015-01-01"), startdate),
    enddate = ifelse(enddate > ymd("2024-10-31"), ymd("2024-10-31"), enddate)
  )

final2$startdate <- as.Date(final2$startdate)
final2$enddate <- as.Date(final2$enddate)


saveRDS(final2, "All_members.rds") #Prev called mellomlagret_aktiviteter_new.rds

rm(list = setdiff(ls(), "final2"))

####################
#ACTIVE MEMBERSHIPS#
####################


#final2 <- readRDS("All_members.rds")


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


library(dplyr)
library(lubridate)
library(readxl)

sport_seasons <- read_excel("N:/durable/data/sesonger.xlsx")
sport_seasons$sport <- sport_seasons$idrett2

master3 <- left_join(final2, sport_seasons, by = "idrett2")

master3 <- master3 %>%
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


nif_lisens <- read.csv("N:/durable/data/pop_nif_lisens.csv", encoding = "UTF-6")

nif_lisens <- nif_lisens %>%
  filter(!is.na(idrett))

lisens <- nif_lisens %>%
  rename(idrett2 = idrett)

lisens <- lisens %>%
  select(w24_0511_lopenr_person, dato, idrett2)

lisenser <- nif_lisens %>%
  group_by(idrett) %>%
  summarise(n = n())

print(unique(lisens$idrett2))


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



master4 <- left_join(master3, wide_data, by = c("w24_0511_lopenr_person", "idrett2"))

merged_data <- master4


x <- 1:21

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
  select(w24_0511_lopenr_person, idrett2, startdate, kjoenn, enddate, days, member_one_year, license_membership_overlap, foedselsaar)

new_master <- new_master %>%
  mutate(strict = ifelse(member_one_year == 1 | license_membership_overlap == 1, 1, 0))

new_master <- new_master %>%
  filter(strict == 1)


new_master <- new_master %>%
  mutate(
    `2015` = ifelse(startdate <= ymd("2015-10-02") & enddate >= ymd("2015-04-01"), 1, 0),
    `2016` = ifelse(startdate <= ymd("2016-10-02") & enddate >= ymd("2016-04-01"), 1, 0),
    `2017` = ifelse(startdate <= ymd("2017-10-02") & enddate >= ymd("2017-04-01"), 1, 0),
    `2018` = ifelse(startdate <= ymd("2018-10-02") & enddate >= ymd("2018-04-01"), 1, 0),
    `2019` = ifelse(startdate <= ymd("2019-10-02") & enddate >= ymd("2019-04-01"), 1, 0),
    `2020` = ifelse(startdate <= ymd("2020-10-02") & enddate >= ymd("2020-04-01"), 1, 0),
    `2021` = ifelse(startdate <= ymd("2021-10-02") & enddate >= ymd("2021-04-01"), 1, 0),
    `2022` = ifelse(startdate <= ymd("2022-10-02") & enddate >= ymd("2022-04-01"), 1, 0),
    `2023` = ifelse(startdate <= ymd("2023-10-02") & enddate >= ymd("2023-04-01"), 1, 0))



new_master <- new_master %>%
  filter(idrett2 != "Testaktivitet")

saveRDS(new_master, "Active_members.rds") # Prev called strict_new.rds




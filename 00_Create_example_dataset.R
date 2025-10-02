##00_Create_Example_Dataset##

set.seed(123)  # For reproducibility

# Number of rows
n <- 10000

# 1) Create person IDs with specified distribution
# We want mean of 2 appearances per person, so approximately 5000 unique persons
# with some appearing once and some up to 30 times

# Generate number of appearances for each person using a distribution
# that gives us a mean of 2
n_persons <- 5000
appearances <- rpois(n_persons, lambda = 1.5) + 1  # Poisson + 1 to ensure at least 1
appearances <- pmin(appearances, 30)  # Cap at 30

# Adjust to get exactly 10000 rows
while(sum(appearances) != n) {
  if(sum(appearances) < n) {
    # Add appearances to random persons
    idx <- sample(which(appearances < 30), 1)
    appearances[idx] <- appearances[idx] + 1
  } else {
    # Remove appearances from random persons with more than 1
    idx <- sample(which(appearances > 1), 1)
    appearances[idx] <- appearances[idx] - 1
  }
}

# Create the person IDs
w24_0511_lopenr_pers <- unlist(lapply(1:n_persons, function(i) {
  rep(sprintf("P%d", i), appearances[i])
}))

# Shuffle to randomize order
w24_0511_lopenr_pers <- sample(w24_0511_lopenr_pers)

# 2) Create start dates
start_date <- as.Date("2010-01-01")
end_date_range <- as.Date("2024-10-31")
dato1 <- as.character(sample(seq(start_date, end_date_range, by = "day"), n, replace = TRUE))

# 3) Create end dates (average 500 days after start, some NA if after 2024-10-31)
dato1_date <- as.Date(dato1)
# Generate days to add with mean of 500
days_to_add <- rpois(n, lambda = 500)
dato2_date <- dato1_date + days_to_add

# Set to NA if after 2024-10-31
dato2 <- ifelse(dato2_date > as.Date("2024-10-31"), 
                NA, 
                as.character(dato2_date))

# 4) Create sports distribution based on Norwegian popularity
sports <- c(
  "Football", "Handball", "Skiing", "Athletics", "Swimming", 
  "Gymnastics", "Ice hockey", "Volleyball", "Basketball", "Cycling",
  "Golf", "Tennis", "Orienteering", "Biathlon", "Climbing",
  "Table tennis", "Martial arts", "Equestrian", "Sailing", "Dancing",
  "Shooting", "Rowing", "Skating", "Rugby", "Triathlon",
  "Badminton", "Archery", "Curling", "Bandy", "Multisport",
  "Functional Fitness", "Powerlifting", "Judo", "Boxing", "Fencing",
  "Motorsport", "Canoeing", "Weightlifting", "Wrestling", "Air sport",
  "Waterski and wakeboard", "Squash", "Cricket", "Bowling", "Baseball and softball",
  "Diving", "Kickboxing", "Student sports", "Boardsports", "Company sport",
  "Casting", "Sled dog racing", "Billiards", "American sports", "Luge, bobsleigh and skeleton"
)

# Create weights (exponentially decreasing for Norwegian popularity)
weights <- exp(-seq(0, 4, length.out = 55))
idrett2 <- sample(sports, n, replace = TRUE, prob = weights)

# Create the dataframe
nif_medlemmer <- data.frame(
  w24_0511_lopenr_person = w24_0511_lopenr_pers,
  dato1 = dato1,
  dato2 = dato2,
  idrett2 = idrett2,
  stringsAsFactors = FALSE
)


#Create descriptive data#
unique_persons <- unique(nif_medlemmer$w24_0511_lopenr_person)
n_unique <- length(unique_persons)

set.seed(124)  # For reproducibility

# Create birth years with mean around 2000
# Using normal distribution with some reasonable spread
birth_year <- round(rnorm(n_unique, mean = 2000, sd = 12))
# Ensure reasonable range (e.g., 1950-2020)
birth_year <- pmin(pmax(birth_year, 1950), 2020)

# Create gender (55% boys, 45% girls)
gender <- sample(c("Male", "Female"), n_unique, replace = TRUE, prob = c(0.55, 0.45))

# Create country of birth
# 91% Norway, 9% other common countries for people living in Norway
countries <- c(
  "Norway",           # 91%
  "Poland",           # Most common immigrant group
  "Lithuania",
  "Sweden",
  "Somalia",
  "Pakistan",
  "Iraq",
  "Germany",
  "Philippines",
  "Syria",
  "Denmark",
  "Eritrea",
  "Thailand",
  "United Kingdom",
  "Afghanistan"
)

# Weights: 91% Norway, remaining 9% distributed among others
weights <- c(0.91, rep(0.09/14, 14))

country_of_birth <- sample(countries, n_unique, replace = TRUE, prob = weights)

# Create the person-level dataframe
df_persons <- data.frame(
  w24_0511_lopenr_person = unique_persons,
  foedselsaar = birth_year,
  kjoenn = gender,
  fodeland = country_of_birth,
  stringsAsFactors = FALSE
)

################
##LICENCE DATA##
################
set.seed(124)  # For reproducibility

# Sports that require licenses
license_sports <- c(
  "American sports", "Bandy", "Table tennis", "Bowling", "Boardsports", 
  "Wrestling", "Cricket", "Dancing", "Diving", "Athletics", 
  "Functional Fitness", "Gymnastics", "Sled dog racing", "Handball", 
  "Ice hockey", "Judo", "Kickboxing", "Climbing", "Air sport", 
  "Motorsport", "Orienteering", "Equestrian", "Rowing", "Rugby", 
  "Sailing", "Skiing", "Biathlon", "Skating", "Baseball and softball", 
  "Squash", "Swimming", "Tennis", "Triathlon", "Waterski and wakeboard", 
  "Volleyball"
)

# Get all unique persons
all_persons <- unique(df_persons$w24_0511_lopenr_person)
n_all_persons <- length(all_persons)

# Select 20% of the population to have licenses
n_with_licenses <- round(n_all_persons * 0.20)
persons_with_licenses <- sample(all_persons, n_with_licenses, replace = FALSE)

# For each person with license, assign 1-3 licenses (most have 1, some have more)
n_licenses_per_person <- sample(1:3, n_with_licenses, replace = TRUE, 
                                prob = c(0.7, 0.25, 0.05))

# Create the license dataset
license_list <- lapply(1:n_with_licenses, function(i) {
  person <- persons_with_licenses[i]
  n_lic <- n_licenses_per_person[i]
  
  # Sample sports for this person
  sports <- sample(license_sports, n_lic, replace = FALSE)
  
  # Generate license end dates (between 2010 and 2030)
  # Most licenses are valid for 1-2 years, so dates cluster around recent years
  dato <- as.character(sample(seq(as.Date("2020-01-01"), 
                                  as.Date("2030-12-31"), 
                                  by = "day"), 
                              n_lic, 
                              replace = TRUE))
  
  data.frame(
    w24_0511_lopenr_person = person,
    dato = dato,
    idrett2 = sports,
    stringsAsFactors = FALSE
  )
})

# Combine all licenses into one dataframe
df_licenses <- do.call(rbind, license_list)


##Create counties dataset##

set.seed(124)  # For reproducibility

# Get all unique persons
all_persons <- unique(df_persons$w24_0511_lopenr_person)
n_persons <- length(all_persons)

# Define valid county codes for each time period
counties_2010_2019 <- sprintf("%02d", c(1:12, 14:20))  # 01-20 except 13
counties_2020_2023 <- c("03", "11", "15", "18", "30", "34", "38", "42", "46", "50", "54")
counties_2024 <- c("03", "11", "15", "18", "31", "32", "33", "35", "39", "40", "42", "46", "50", "55", "56")

# Initialize the dataframe with person IDs
df_fylke <- data.frame(w24_0511_lopenr_person = all_persons, stringsAsFactors = FALSE)

# Create columns for each year from 2010 to 2024
for (year in 2010:2024) {
  col_name <- sprintf("bostedsfylke_01_01_%d", year)
  
  if (year <= 2019) {
    # For 2010-2019: use codes 01-20 (except 13)
    df_fylke[[col_name]] <- sample(counties_2010_2019, n_persons, replace = TRUE)
  } else if (year <= 2023) {
    # For 2020-2023: use reform codes
    df_fylke[[col_name]] <- sample(counties_2020_2023, n_persons, replace = TRUE)
  } else {
    # For 2024: use new codes
    df_fylke[[col_name]] <- sample(counties_2024, n_persons, replace = TRUE)
  }
}

# Add some continuity: people are more likely to stay in the same county
# Let's make it so there's ~70% chance of staying in same county year-to-year
for (i in 1:n_persons) {
  for (year in 2011:2024) {
    prev_col <- sprintf("bostedsfylke_01_01_%d", year - 1)
    curr_col <- sprintf("bostedsfylke_01_01_%d", year)
    
    # 70% chance to keep the same county (if it's valid for current year)
    if (runif(1) < 0.70) {
      prev_county <- df_fylke[i, prev_col]
      
      # Check if previous county is valid for current year
      if (year <= 2019) {
        valid_counties <- counties_2010_2019
      } else if (year <= 2023) {
        valid_counties <- counties_2020_2023
      } else {
        valid_counties <- counties_2024
      }
      
      # Only keep same county if it's still valid
      if (prev_county %in% valid_counties) {
        df_fylke[i, curr_col] <- prev_county
      }
    }
  }
}


set.seed(124)  # For reproducibility

# Get all unique persons
all_persons <- unique(df_persons$w24_0511_lopenr_person)
n_persons <- length(all_persons)

# ==========================================
# Dataset 1: Parent linkage
# ==========================================

# Create unique parent IDs (not overlapping with person IDs)
# Since person IDs are P1, P2, etc., we'll use F1, F2... for fathers and M1, M2... for mothers
n_unique_fathers <- n_persons  # Each person has their own father
n_unique_mothers <- n_persons  # Each person has their own mother

# Generate father and mother IDs
lopenr_far <- sprintf("F%d", 1:n_unique_fathers)
lopenr_mor <- sprintf("M%d", 1:n_unique_mothers)

# Some persons might have missing parent info (e.g., 5% missing father, 3% missing mother)
lopenr_far[sample(1:n_persons, round(n_persons * 0.05))] <- NA
lopenr_mor[sample(1:n_persons, round(n_persons * 0.03))] <- NA

# Create parent linkage dataset
df_parents <- data.frame(
  w24_0511_lopenr_person = all_persons,
  lopenr_far = lopenr_far,
  lopenr_mor = lopenr_mor,
  stringsAsFactors = FALSE
)


# ==========================================
# Dataset 2: Parent incomes
# ==========================================

# Get all unique parent IDs (excluding NAs)
all_fathers <- unique(df_parents$lopenr_far[!is.na(df_parents$lopenr_far)])
all_mothers <- unique(df_parents$lopenr_mor[!is.na(df_parents$lopenr_mor)])
all_parents <- c(all_fathers, all_mothers)
n_all_parents <- length(all_parents)

# Create years
years <- 2010:2022
n_years <- length(years)

# Initialize list to store income data
income_list <- list()

for (i in 1:n_all_parents) {
  parent_id <- all_parents[i]
  
  # Determine if this is a father or mother (for income distribution)
  is_father <- grepl("^F", parent_id)
  
  # Base income varies by gender (reflecting wage gap)
  if (is_father) {
    base_income <- rnorm(1, mean = 550000, sd = 200000)  # Fathers average ~550k NOK
  } else {
    base_income <- rnorm(1, mean = 450000, sd = 180000)  # Mothers average ~450k NOK
  }
  
  # Ensure positive income
  base_income <- max(base_income, 100000)
  
  # Generate income for each year with some growth and variation
  for (year in years) {
    # Income grows slightly over time (2% per year on average)
    years_since_2010 <- year - 2010
    income_growth_factor <- 1.02^years_since_2010
    
    # Add yearly variation
    yearly_variation <- rnorm(1, mean = 1, sd = 0.1)
    
    # Calculate income for this year
    income <- round(base_income * income_growth_factor * yearly_variation, 0)
    
    # Ensure minimum income
    income <- max(income, 50000)
    
    # Add to list
    income_list[[length(income_list) + 1]] <- data.frame(
      w24_0511_lopenr_person = parent_id,
      aargang = year,
      saminnt = income,
      stringsAsFactors = FALSE
    )
  }
}

# Combine all income records
df_income <- do.call(rbind, income_list)

# Sort by person ID and year
df_income <- df_income[order(df_income$w24_0511_lopenr_person, df_income$aargang), ]
rownames(df_income) <- NULL


library(dplyr)

#Remove those not in sport#
nif_medlemmer <- nif_medlemmer %>%
  filter(!is.na(idrett2))

#Keep only those between 13 and 19 between 2015 and 2023: 
#Hente fodeaar#
#alder <- read.csv("N:/durable/data/ssb/W24_0511_FASTE_OPPL_UTEN_FDATO.csv")
alder <- df_persons %>%
  select(c(w24_0511_lopenr_person, foedselsaar, kjoenn, fodeland))

membership_data <- left_join(nif_medlemmer, alder, by = "w24_0511_lopenr_person")

membership_data <- membership_data %>%
  select(w24_0511_lopenr_person, idrett2, foedselsaar, fodeland, kjoenn, dato1, dato2)

rm(alder, nif_medlemmer)
gc()

membership_data$ID <- membership_data$w24_0511_lopenr_person
membership_data <- membership_data %>%
  mutate(gender = kjoenn)

membership_data$Start_date <- sub(" \\d{2}:\\d{2}:\\d{2}\\.\\d+$", "", membership_data$dato1)
membership_data$End_date <- sub(" \\d{2}:\\d{2}:\\d{2}\\.\\d+$", "", membership_data$dato2)

library(lubridate)

membership_data$Start_date <- ymd(membership_data$Start_date)
membership_data$End_date <- ymd(membership_data$End_date)

membership_data <- membership_data %>%
  mutate(End_date = ifelse(is.na(End_date) & !is.na(Start_date), ymd("2024-12-31"), End_date))

membership_data$End_date <- as.Date(membership_data$End_date)

membership_data <- membership_data %>%
  select(ID, foedselsaar, fodeland, gender, idrett2, Start_date, End_date)


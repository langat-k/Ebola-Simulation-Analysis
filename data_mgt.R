library(rio)
install.packages("rio")
here()
library(here)
library(tidyverse)
##manually select a file
manselectfile <- import(file.choose())
import("stroke.dta")
import(here( "stroke.dta"), na = c(NA "", " ")

stroketrue <- import("stroke.dta") %>% 
  names()
    
install.packages("clipr")
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)
 
#import linelist data set
linelist_raw <- import("linelist_raw.xlsx")

#view column names 
names(linelist_raw)

#clean column names using janitor
linelist <- linelist_raw %>% 
  janitor::clean_names() %>% 
  #rename date columns to appropriate format
  rename(date_infection = infection_date,
         date_hospitalization = hosp_date,
         date_outcome = date_of_outcome) %>% 
  #remove row_num, x28 and merged_header columns
  select(-c(row_num, x28, merged_header)) %>% 
  #remove duplicates
  distinct() %>% 
  #add bmi column
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>% 
  #change class of date,generation and age columns
  mutate(across(contains("date"), as.Date),
         generation = as.numeric(generation),
         age = as.numeric(age)) %>% 
  
  #


##tidy select helper functions
# move date_onset and date_hospitalization to beginning
linelist %>% 
  select(date_onset, date_hospitalization, everything()) %>% 
  names()
#remove row_num, x28 and merged_header columns
View(linelist)
#change age column to numeric
class(linelist$age)
linelist$age <- as.numeric(linelist$age)

#normalize age to mean of all rows
linelist %>% mutate(age_norm = age/mean(age, na.rm = T))

#calculation of the cumulative sum of number o cases per day
cumul_case_counts <- linelist %>% 
  count(date_onset) %>% 
  mutate(cumulative_cases = cumsum(n))
#view first ten rows
head(cumul_case_counts, 10)
                         
##clean hospital column
#view unique values as a table
table(linelist$hospital, useNA = "always")
#recode the messy values
linelist <- linelist %>% 
  #for reference OLD = NEW
  mutate(hospital = recode(hospital,
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  ))
table(linelist$hospital, useNA = "always")

#create a date of death column, which is NA if patient has not died
linelist <- linelist %>% 
  mutate(date_death = if_else(outcome == "Death", date_outcome, NA))

#create age_years column from age and age_unit columns
linelist <- linelist %>% 
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age
  ))

#create a case column based on confirmed and sspet cases
linelist <- linelist %>% 
  mutate(case_status = case_when(
  #above 20 blood count is confirmed
    ct_blood > 20 ~ "Confirmed",
    #if an epidemiological link is present and has fever
    # the patient is a suspect case
    !is.na(source)& fever == "yes" ~ "Suspect",
    #mark for investigation any other patient not included in above logic
    TRUE ~ "To investigate"
    
  ))

#replace missing values in hospital variable with "Missing"
linelist <- linelist %>% 
  mutate(hospital = replace_na(hospital, "Missing"))
#examine distribution of age
hist(linelist$age_years)



#pipe
linelist <- linelist_raw %>% 
  janitor::clean_names() %>% 
  #rename date columns to appropriate format
  rename(date_infection = infection_date,
         date_hospitalization = hosp_date,
         date_outcome = date_of_outcome) %>% 
  #remove row_num, x28 and merged_header columns
  select(-c(row_num, x28, merged_header)) %>% 
  #remove duplicates
  distinct() %>% 
  #add bmi column
  mutate(bmi = wt_kg / (ht_cm/100)^2) %>% 
  #change class of date,generation and age columns
  mutate(across(contains("date"), as.Date),
         generation = as.numeric(generation),
         age = as.numeric(age)) %>% 
  
  # add a column describing delay to hospitalization
  mutate(days_onset_hosp = as.numeric(date_hospitalization - date_onset)) %>% 
  #clean hospital column
  mutate(hospital = recode(hospital,
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
  )) %>% 
  mutate(hospital = replace_na(hospital, "Missing")) %>% 
  #create age_years column from age and age_unit dolumns 
  mutate(age_years = case_when(
    age_unit == "years" ~ age,
    age_unit == "months" ~ age/12,
    is.na(age_unit) ~ age,
    TRUE ~ NA_real_)) %>% 
  #categorize age column
  mutate(age_cat = age_categories(age_years, breakers = c(0,5,10,15,20,30,40,50,60,70)
 )) %>% 
  #keep only rows where case_id is not missing
  filter(!is.na(caseid),
         #only keep the second outbreak
         date_onset > as.Date("2013-06-01") | is.na(date_onset) & !hospital %in% c("Hospital A","Hospital B","Hospital C")))
#rowwise
linelist %>% 
  rowwise() %>% 
  mutate(num_NA_dates = sum(is.na(c_across(contains("date")))) %>% 
           ungroup() %>% 
           select(num_NA_dates, contains("date")) %>%
                    view()

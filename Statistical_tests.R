pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable     # converting tables to HTML
)

# import dataset 
linelist <- import("linelist_cleaned.rds")

# compare mean age by outcome using t-test
t.test(age_years ~ gender,data = linelist)


# compare age distribution by outcome 
# using wilcox test
wilcox.test(age_years ~ outcome, data = linelist)

# compare the proportions in each group
#with a chi squared test
chisq.test(linelist$gender, linelist$outcome)

#get summary statistics 
linelist %>% 
  group_by(hospital) %>% 
  rstatix::get_summary_stats(age,temp, type = "common")

# one sample t-test of age by gender
linelist %>% 
  group_by(gender) %>% 
  t_test(age_years ~ 1, mu = 18)

# compare the mean age by patient outcome
linelist %>% 
  select(age_years,outcome) %>% 
  tbl_summary(
    statistic = age_years ~ "{mean} ({sd})",
    by = outcome) %>% 
  add_p(age_years ~ "t.test")

## CORRELATIONS
correlation_tab <- linelist %>% 
  select(generation, age, ct_blood, days_onset_hosp, wt_kg, ht_cm) %>%   # keep numeric variables of interest
  correlate()      # create correlation table (using default pearson)

correlation_tab   

# remove mirror effect from the table
correlation_tab <- correlation_tab %>% shave()

#plot the correlations
rplot(correlation_tab)
ggsave("Correlation_plot.png")

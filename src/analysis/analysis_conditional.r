# packages needed
rm(list = ls())
source("project_paths.r")

library(tidyverse)
library(plm)
library(lmtest)
library(broom)
# library(flextable)
# library(officer)
library(knitr)

source(paste(PATH_IN_ANALYSIS, "unconditionalModels.R", sep="/"))
source(paste(PATH_IN_ANALYSIS, "conditionalModels.R", sep="/"))



outcomes <- read_rds(paste(PATH_OUT_DATA, "outcomes.rds", sep="/"))
events_by_time <-  read_rds(paste(PATH_OUT_DATA, "events_by_time.rds", sep="/"))
demographics <-read_rds(paste(PATH_OUT_DATA, "demographics.rds", sep="/"))

knitr::opts_chunk$set(include = FALSE,
                      eval = TRUE,
                      echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      fig.path = "figures/")



# 21 life events included


lifevents <- c('lefnw', 'lefni', 'lefrd', 'lejob', 'leprm', 'lertr', 'lemar',
               'leprg', 'lebth', 'ledsc', 'lesep', 'lemvd', 'leins', 'lepcm',
               'lejls', 'lejlf', 'ledfr', 'ledrl', 'leinf', 'lercl', 'levio')

# 10 time points included
timelevels = c('pre36', 'pre24', 'pre12', 'post03', 'post06', 'post09', 
               'post12', 'post24', 'post36', 'post48')

# name of events
renamevents <- list(
    Widowed = "ledsc",    
    Separated = "lesep",
    Money_lost = "lefnw",
    Jailed = "lejls",
    Attacked = "levio",
    Health_shock = "leins",
    Reconciled = "lercl", 
    Fired = "lefrd", 
    Family_harmed = "leinf",   
    Robbed = "lepcm",
    Friend_died = "ledfr",
    Relative_died = "ledrl",
    Relative_jailed = "lejlf",
    Moved = "lemvd",
    Hired = "lejob",
    Promoted = "leprm",
    Retired = "lertr",
    Money_gained = "lefni",
    Pregnant = "leprg",
    Childbirth = "lebth",
    Married = "lemar"
    )

# good event names
good_events <- c(
  "Married", "Childbirth", "Money_gained", 
  "Retired", "Pregnant", "Promoted", 
  "Hired", "Moved", "Reconciled"
  )

# bad event names
bad_events <- c(
  "Widowed", "Separated", "Money_lost",
  "Health_shock", "Attacked", "Fired", 
  "Family_harmed", "Friend_died", "Robbed"
  )

  






#Table 1. List of 22 life events and their description*  

tribble(
  ~Event, ~Description,
  "Widowed", "death of a spouse or a child",
  "Separated", "separated or divorced from a spouse or long-term partner",
  "Money lost", "major worsening in financial situation (e.g. bankruptcy)",
  "Jailed", "detained in jail / correctional facility",
  "Attacked", "victim of physical violence (e.g. assault)",
  "Health shock", "serious personal injury or illness (e.g., disability)",
  "Reconciled", "reconciled with spouse/long-term partner after separation",
  "Fired", "fired or made redundant by an employer",
  "Family harmed", "serious injury or illness to a close family member",
  "Robbed", "victim of property crime (e.g. theft, house breaking)",
  "Friend died", "death of a close friend",
  "Relative died", "death of a close family member (e.g. parent or sibling)",
  "Relative jailed", "jail for a close friend or relative",
  "Moved", "changed residence",
  "Hired", "changed jobs (i.e. employer)",
  "Promoted", "promoted at work",
  "Retired", "retired from workforce",
  "Money gained", "major gain in finances (e.g., lottery win, inheritance)",
  "Pregnant", "you (or your partner) got pregnant",
  "Married", "got married",
  "Childbirth","birth (or adoption) of a child"
  ) %>%
  kable()



outcomes %>%
  filter(code == 'losatyh') %>% 
  mutate(
    Y = case_when(
      val < 0 ~ NA_integer_,
      TRUE ~ as.integer(val)
    )
  ) %>% 
  select(-code, -val) -> cognitive.df

outcomes %>%
  filter(code == 'gh9_sum') %>% 
  mutate(
    Y = case_when(
      val < 0 ~ NA_integer_, # no negatives in gh9_sum
      TRUE ~ as.integer(val)
    )
  ) %>% 
  select(-code, -val) -> affect.df

# outcomes %>%
#   filter(code == 'mcs') %>%
#   select(-code, Y = val) -> affect.df

# load the list of life event data (for preprocessing see Appendix)
#events_by_time <- read_rds('src/data/events_by_time.rds')


#### Add covariates ####
# Load the preprocessed demographic data (for preprocessing see Appendix)
#demographics <- read_rds('src/data/demographics.rds')

# socioeconomic status (hhda10)
demographics %>%
  filter(code == 'hhda10') %>%
  mutate(
    val = case_when(
      val < 0 ~ NA_integer_, # recode missing values as NA
      TRUE ~ as.integer(val) 
    )
  ) %>% 
  group_by(xwaveid) %>%
  arrange(xwaveid, wave) %>%
  fill(val, .direction = 'down') %>%
  fill(val, .direction = 'up') %>%
  select(xwaveid, wave, seifa = val) %>%
  replace_na(list(seifa = 0)) -> seifa.df

# level of highest education (edhigh1)
demographics %>%
  filter(code == 'edhigh1') %>%
  mutate(
    reval = case_when(
      val < 0 ~ NA_real_,   # recode missing values as NA
      val == 10 ~ NA_real_, # recode undetermined values as NA
      val == 1 ~  3,        # PhD
      val == 2 ~  2,        # Grad diploma
      val == 3 ~  2,        # Bachelors
      val == 4 ~  1,        # Diploma
      val == 5 ~  1,        # Certificate
      val == 8 ~  0,        # Year 12
      val == 9 ~ -1         # Year 10
    )
  ) %>%
  group_by(xwaveid) %>%
  arrange(xwaveid, wave) %>%
  fill(reval, .direction = 'down') %>%
  fill(reval, .direction = 'up') %>%
  select(xwaveid, wave, edu = reval) %>%
  replace_na(list(edu = 0)) -> edu.df

# age (decade of life)
demographics %>%
  filter(code == 'hgage') %>%
  mutate(age = floor(val/10)) %>%
  select(xwaveid, wave, age) -> age.df

demographics %>%
  filter(code == 'female') %>%
  mutate(sex = val) %>%
  select(xwaveid,sex) -> female.df

# Join the covariates together into a single data frame
# (they will be converted to change scores later)
edu.df %>%
  left_join(age.df, by = c("xwaveid", "wave")) %>%
  left_join(seifa.df, by = c("xwaveid", "wave")) %>%
  left_join(female.df, by = c("xwaveid"))-> covariates

write_rds(age.df,paste(PATH_OUT_DATA, "age.rds", sep="/")) # save for supplementary analysis
write_rds(female.df,paste(PATH_OUT_DATA, "sex.rds", sep="/")) # save for supplementary analysis

##delete the next line
write_rds(covariates,paste(PATH_OUT_DATA, "covariates.rds", sep="/")) # save for supplementary analysis
rm(edu.df,age.df,seifa.df,demographics,outcomes)

#write_rds(affect.df,paste(PATH_OUT_DATA, "affectdf.rds", sep="/")) # save for supplementary analysis
marker="female"
cognitive.results <- conditionalModels(cognitive.df, events_by_time, covariates, timelevels, lifevents,marker)
affective.results <- conditionalModels(affect.df, events_by_time, covariates, timelevels, lifevents,marker)

# Combine coeficient results
cognitive.coefs <- cognitive.results$coefs
affective.coefs <- affective.results$coefs
cognitive.coefs$outcome <- 'health'
affective.coefs$outcome <- 'emotional'
bind_rows(cognitive.coefs, affective.coefs) -> results

# Calculate confidence intervals on clustered errors
results$upper <- results$estimate - results$std.error*qt(p = 0.1, df = results$n-1)
results$lower <- results$estimate + results$std.error*qt(p = 0.1, df = results$n-1)

# Save results 
write_csv(results,paste(PATH_OUT_DATA, "conditional_results_female.csv", sep="/"))

marker="male"
cognitive.results <- conditionalModels(cognitive.df, events_by_time, covariates, timelevels, lifevents,marker)
affective.results <- conditionalModels(affect.df, events_by_time, covariates, timelevels, lifevents,marker)

# Combine coeficient results
cognitive.coefs <- cognitive.results$coefs
affective.coefs <- affective.results$coefs
cognitive.coefs$outcome <- 'health'
affective.coefs$outcome <- 'emotional'
bind_rows(cognitive.coefs, affective.coefs) -> results

# Calculate confidence intervals on clustered errors
results$upper <- results$estimate - results$std.error*qt(p = 0.1, df = results$n-1)
results$lower <- results$estimate + results$std.error*qt(p = 0.1, df = results$n-1)

# Save results 
write_csv(results,paste(PATH_OUT_DATA, "conditional_results_male.csv", sep="/"))

marker="seifa_top_half"
cognitive.results <- conditionalModels(cognitive.df, events_by_time, covariates, timelevels, lifevents,marker)
affective.results <- conditionalModels(affect.df, events_by_time, covariates, timelevels, lifevents,marker)

# Combine coeficient results
cognitive.coefs <- cognitive.results$coefs
affective.coefs <- affective.results$coefs
cognitive.coefs$outcome <- 'health'
affective.coefs$outcome <- 'emotional'
bind_rows(cognitive.coefs, affective.coefs) -> results

# Calculate confidence intervals on clustered errors
results$upper <- results$estimate - results$std.error*qt(p = 0.1, df = results$n-1)
results$lower <- results$estimate + results$std.error*qt(p = 0.1, df = results$n-1)

# Save results 
write_csv(results,paste(PATH_OUT_DATA, "conditional_results_seifa_top_half.csv", sep="/"))

marker="seifa_bottom_half"
cognitive.results <- conditionalModels(cognitive.df, events_by_time, covariates, timelevels, lifevents,marker)
affective.results <- conditionalModels(affect.df, events_by_time, covariates, timelevels, lifevents,marker)

# Combine coeficient results
cognitive.coefs <- cognitive.results$coefs
affective.coefs <- affective.results$coefs
cognitive.coefs$outcome <- 'health'
affective.coefs$outcome <- 'emotional'
bind_rows(cognitive.coefs, affective.coefs) -> results

# Calculate confidence intervals on clustered errors
results$upper <- results$estimate - results$std.error*qt(p = 0.1, df = results$n-1)
results$lower <- results$estimate + results$std.error*qt(p = 0.1, df = results$n-1)

# Save results 
write_csv(results,paste(PATH_OUT_DATA, "conditional_results_seifa_bottom_half.csv", sep="/"))
marker="all"
cognitive.results <- conditionalModels(cognitive.df, events_by_time, covariates, timelevels, lifevents,marker)
affective.results <- conditionalModels(affect.df, events_by_time, covariates, timelevels, lifevents,marker)

covar.results <- list(cognitive = cognitive.results$covar, affective = affective.results$covar)
write_rds(covar.results,paste(PATH_OUT_DATA, "conditional_covar.rds", sep="/"))

# Combine coeficient results
cognitive.coefs <- cognitive.results$coefs
affective.coefs <- affective.results$coefs
cognitive.coefs$outcome <- 'health'
affective.coefs$outcome <- 'emotional'
bind_rows(cognitive.coefs, affective.coefs) -> results

# Calculate confidence intervals on clustered errors
results$upper <- results$estimate - results$std.error*qt(p = 0.1, df = results$n-1)
results$lower <- results$estimate + results$std.error*qt(p = 0.1, df = results$n-1)

# Save results 
write_csv(results,paste(PATH_OUT_DATA, "conditional_results.csv", sep="/"))

#### Comparing the total impact of life events
#In order to compare the total impact of life events, taking into account both magnitude and duration of effect, 
#we calculated the area-under-the-curve (AUC) for each event in each model (as well as variance). The AUC was calculated by:  


## Results  
#### Subjective wellbeing



#### Effect of life events on subjective wellbeing
#Unconditional effect of life events*. Figures 1 & 2 below show the effect of each life event on cognitive and affective wellbeing,
#ignoring any concurrent life events (or any other covariates apart from year).  

#We a priori defined 9 positively and 9 negatively valenced events and present them in separate figures to facilitate
#comparison between events of the same valence (although this was not born out for reconciliation which had an anticipatory negative effect 
#and no positive effect). *Married*, *Childbirth* and a major financial gain  (*Money gained*)
 #produce the largest positive impact on wellbeing, while *Widowed*, *Separated* and major financial loss (*Money lost*) produced the largest negative impact on wellbeing.   


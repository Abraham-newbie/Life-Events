# packages needed
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


outcomes %>%
  filter(code == 'losatsf') %>% 
  mutate(
    Y = case_when(
      val < 0 ~ NA_integer_, # no negatives in gh9_sum
      TRUE ~ as.integer(val)
    )
  ) %>% 
  select(-code, -val) -> safety.df


outcomes %>%
  filter(code == 'losatfs') %>% 
  mutate(
    Y = case_when(
      val < 0 ~ NA_integer_, # no negatives in gh9_sum
      TRUE ~ as.integer(val)
    )
  ) %>% 
  select(-code, -val) -> finance.df


outcomes %>%
  filter(code == 'firisk') %>% 
  mutate(
    Y = case_when(
      val < 0 ~ NA_integer_, # no negatives in gh9_sum
      TRUE ~ as.integer(val)
    )
  ) %>% 
  select(-code, -val) -> risk_aversion.df


# outcomes %>%
#   filter(code == 'mcs') %>%
#   select(-code, Y = val) -> affect.df

# load the list of life event data (for preprocessing see Appendix)
#events_by_time <- read_rds('src/data/events_by_time.rds')




# These models include all available observations of each event as a single
# independent variable (+ wave) in each model, thus representing the uncondit-
# ional effect of life events, and ignoring any mediating effects of other 
# events as well as the risk of contamination from unobserved events. 
#

# Our helpful wrapper returns the coefficients, residuals and covariance matrix

cognitive.results <- unconditionalModels(cognitive.df, events_by_time, timelevels, lifevents)
affective.results <- unconditionalModels(affect.df, events_by_time, timelevels, lifevents)

cognitive.resids <- cognitive.results$resids
affective.resids <- affective.results$resids
cognitive.resids$outcome <- 'health'
affective.resids$outcome <- 'emotional'
resid.results <- bind_rows(cognitive.resids, affective.resids) 
write_rds(resid.results,paste(PATH_OUT_DATA, "unconditional_resid.rds", sep="/"))

covar.results <- list(cognitive = cognitive.results$covar, 
                      affective = affective.results$covar)

write_rds(covar.results,paste(PATH_OUT_DATA, "unconditional_covar.rds", sep="/"))

# Get the coefficients and combine them into a table of ordered time points
cognitive.coefs <- cognitive.results$coefs
affective.coefs <- affective.results$coefs
cognitive.coefs$outcome <- 'health'
affective.coefs$outcome <- 'emotional'
bind_rows(cognitive.coefs, affective.coefs) -> results

# Calculate confidence intervals on clustered errors
results$upper <- results$estimate - 
                      results$std.error*qt(p = 0.1, df = results$n-1)
results$lower <- results$estimate + 
                      results$std.error*qt(p = 0.1, df = results$n-1)

# Save results 
write_csv(results,paste(PATH_OUT_DATA, "unconditional_results.csv", sep="/"))





safety_effect.results <- unconditionalModels(safety.df, events_by_time, timelevels, lifevents)
finance_effect.results <- unconditionalModels(finance.df, events_by_time, timelevels, lifevents)



# Get the coefficients and combine them into a table of ordered time points
safety_effect.coefs <- safety_effect.results$coefs
finance_effect.coefs <- finance_effect.results$coefs
safety_effect.coefs$outcome <- 'safety'
finance_effect.coefs$outcome <- 'finance'
bind_rows(safety_effect.coefs, finance_effect.coefs) -> results

# Calculate confidence intervals on clustered errors
results$upper <- results$estimate - 
                      results$std.error*qt(p = 0.1, df = results$n-1)
results$lower <- results$estimate + 
                      results$std.error*qt(p = 0.1, df = results$n-1)
write_csv(results,paste(PATH_OUT_DATA, "unconditional_results_finance_and_safety.csv", sep="/"))


##########


risk_results <- unconditionalModels(risk_aversion.df, events_by_time, timelevels, lifevents)

# Get the coefficients and combine them into a table of ordered time points
risk_effect.coefs <-risk_results$coefs
finance_effect.coefs <- finance_effect.results$coefs
risk_effect.coefs$outcome <- 'risk'
finance_effect.coefs$outcome <- 'finance'
bind_rows(risk_effect.coefs, finance_effect.coefs) -> results

# Calculate confidence intervals on clustered errors
results$upper <- results$estimate - 
                      results$std.error*qt(p = 0.1, df = results$n-1)
results$lower <- results$estimate + 
                      results$std.error*qt(p = 0.1, df = results$n-1)
write_csv(results,paste(PATH_OUT_DATA, "unconditional_results_finance_and_risk.csv", sep="/"))

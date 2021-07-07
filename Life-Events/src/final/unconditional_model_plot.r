'
Plots the unconditional models to show short and long term effects of
Life events.

'


rm(list=ls())
source("project_paths.r")
 require(dplyr)
 require(tidyverse)
 require(plm)
 require(lmtest)
 require(broom)
source(paste(PATH_IN_FINAL, "psth.R", sep="/"))

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

read_csv(
    paste(PATH_OUT_DATA, "unconditional_results.csv", sep="/"), 
    col_types = cols(
        code = col_character(),
        term = col_character(),
        estimate = col_double(),
        std.error = col_double(),
        statistic = col_double(),
        p.value = col_double(),
        std.error = col_double(),
        df = col_double(),
        n = col_double(),
        N = col_double(),
        outcome = col_character(),
        upper = col_double(),
        lower = col_double()
        )
    ) %>%
  mutate(
      # term = factor(term, levels = timelevels, ordered = TRUE)
      # code = factor(code)
      code = fct_recode(code, !!!renamevents)
      ) -> results

# levels(results$code) <- renamevents

#### Plot coefficients from unconditional model ####
png(filename=paste(PATH_OUT_FIGURES, "unconditional_model_positive_events_plot.png", sep="/"))
figure1_str <- c('Unconditional effect of positive events on wellbeing')
psth(results, good_events, figure1_str)
dev.off()


png(filename=paste(PATH_OUT_FIGURES, "unconditional_model_negative_events_plot.png", sep="/"))
figure2_str <- c('Unconditional effect of negative events on wellbeing')
psth(results, bad_events, figure2_str)
dev.off()

######



#rm(list=ls())
#source("project_paths.r")
 require(dplyr)
 require(tidyverse)
 require(plm)
 require(lmtest)
 require(broom)
source(paste(PATH_IN_FINAL, "psth.R", sep="/"))


read_csv(
    paste(PATH_OUT_DATA, "unconditional_results_finance_and_safety.csv", sep="/"), 
    col_types = cols(
        code = col_character(),
        term = col_character(),
        estimate = col_double(),
        std.error = col_double(),
        statistic = col_double(),
        p.value = col_double(),
        std.error = col_double(),
        df = col_double(),
        n = col_double(),
        N = col_double(),
        outcome = col_character(),
        upper = col_double(),
        lower = col_double()
        )
    ) %>%
  mutate(
      # term = factor(term, levels = timelevels, ordered = TRUE)
      # code = factor(code)
      code = fct_recode(code, !!!renamevents)
      ) -> results

# levels(results$code) <- renamevents

#### Plot coefficients from unconditional model ####
png(filename=paste(PATH_OUT_FIGURES, "unconditional_model_positive_events_financial_and_safety_plot.png", sep="/"))
figure1_str <- c('Unconditional effect of negative events on Sense of Financial \nSecurity and Safety')
psth(results, good_events, figure1_str)
dev.off()



png(filename=paste(PATH_OUT_FIGURES, "unconditional_model_negative_events_financial_and_safety_plot.png", sep="/"))
figure2_str <- c('Unconditional effect of negative events on Sense of Financial \nSecurity and Safety (HILDA 2002 to 2016)')
psth(results, bad_events, figure2_str)
dev.off()






read_csv(
    paste(PATH_OUT_DATA, "unconditional_results_finance_and_risk.csv", sep="/"), 
    col_types = cols(
        code = col_character(),
        term = col_character(),
        estimate = col_double(),
        std.error = col_double(),
        statistic = col_double(),
        p.value = col_double(),
        std.error = col_double(),
        df = col_double(),
        n = col_double(),
        N = col_double(),
        outcome = col_character(),
        upper = col_double(),
        lower = col_double()
        )
    ) %>%
  mutate(
      # term = factor(term, levels = timelevels, ordered = TRUE)
      # code = factor(code)
      code = fct_recode(code, !!!renamevents)
      ) -> results

# levels(results$code) <- renamevents

#### Plot coefficients from unconditional model ####
png(filename=paste(PATH_OUT_FIGURES, "unconditional_model_positive_events_financial_security_and_risk_aversion_plot.png", sep="/"))
figure1_str <- c('Unconditional effect of positive events on Sense of Financial \nSecurity and Risk aversion ')
psth(results, good_events, figure1_str)
dev.off()

png(filename=paste(PATH_OUT_FIGURES, "unconditional_model_negative_events_financial_security_and_risk_aversion_plot.png", sep="/"))
figure2_str <- c('Unconditional effect of negative events on Sense of Financial \nSecurity  and Risk Aversion (HILDA 2002 to 2016)')
psth(results, bad_events, figure2_str)
dev.off()
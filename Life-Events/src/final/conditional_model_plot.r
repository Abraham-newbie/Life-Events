'
conditional_model_plot module plots the conditional models to show short and long term effects of
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
    paste(PATH_OUT_DATA, "conditional_results.csv", sep="/"), 
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
png(filename=paste(PATH_OUT_FIGURES, "conditional_model_positive_events_plot.png", sep="/"))
figure1_str <- c('Conditional effect of positive events on health satisfaction')
psth(results, good_events, figure1_str)
dev.off()

png(filename=paste(PATH_OUT_FIGURES, "conditional_model_negative_events_plot.png", sep="/"))
figure2_str <- c('Conditional effect of negative events on health satisfaction')
psth(results, bad_events, figure2_str)
dev.off()


read_csv(
    paste(PATH_OUT_DATA, "conditional_results_female.csv", sep="/"), 
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
png(filename=paste(PATH_OUT_FIGURES, "conditional_model_positive_events_females_plot.png", sep="/"))
figure1_str <- c('Conditional effect of positive events on health satisfaction for \nfemales')
psth(results, good_events, figure1_str)
dev.off()

png(filename=paste(PATH_OUT_FIGURES, "conditional_model_negative_events_females_plot.png", sep="/"))
figure2_str <- c('Conditional effect of negative events on health satisfaction for \nfemales')
psth(results, bad_events, figure2_str)
dev.off()



read_csv(
    paste(PATH_OUT_DATA, "conditional_results_male.csv", sep="/"), 
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
good_events<-good_events[good_events != "Pregnant"]
#### Plot coefficients from unconditional model ####
png(filename=paste(PATH_OUT_FIGURES, "conditional_model_positive_events_males_plot.png", sep="/"))
figure1_str <- c('Conditional effect of positive events on health satisfaction for males')
psth(results, good_events, figure1_str)
dev.off()


png(filename=paste(PATH_OUT_FIGURES, "conditional_model_negative_events_males_plot.png", sep="/"))
figure2_str <- c('Conditional effect of negative events on health satisfaction for males')
psth(results, bad_events, figure2_str)
dev.off()

###


read_csv(
    paste(PATH_OUT_DATA, "conditional_results_seifa_top_half.csv", sep="/"), 
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
png(filename=paste(PATH_OUT_FIGURES, "conditional_model_positive_events_seifa_top_half_plot.png", sep="/"))
figure1_str <- c('Conditional effect of positive events on health satisfaction \nfor top 50% Socio-economic bracket')
psth(results, good_events, figure1_str)
dev.off()


png(filename=paste(PATH_OUT_FIGURES, "conditional_model_negative_events_seifa_top_half_plot.png", sep="/"))
figure2_str <- c('Conditional effect of negative events on health satisfaction \nfor top 50% Socio-economic bracket')
psth(results, bad_events, figure2_str)
dev.off()



read_csv(
    paste(PATH_OUT_DATA, "conditional_results_seifa_bottom_half.csv", sep="/"), 
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


#### Plot coefficients from unconditional model ####
png(filename=paste(PATH_OUT_FIGURES, "conditional_model_positive_events_seifa_bottom_half_plot.png", sep="/"))
figure1_str <- c('Conditional effect of positive events on health satisfaction \nfor bottom 50% Socio-economic bracket')
psth(results, good_events, figure1_str)
dev.off()




png(filename=paste(PATH_OUT_FIGURES, "conditional_model_negative_events_seifa_bottom_half_plot.png", sep="/"))
figure2_str <- c('Conditional effect of negative events on health satisfaction \nfor bottom 50% socio-economic bracket')
psth(results, bad_events, figure2_str)
dev.off()

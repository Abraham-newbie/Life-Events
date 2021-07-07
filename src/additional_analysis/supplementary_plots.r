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



outcomes <- read_rds(paste(PATH_OUT_DATA, "outcomes.rds", sep="/"))
events_by_time <-  read_rds(paste(PATH_OUT_DATA, "events_by_time.rds", sep="/"))
demographics <-read_rds(paste(PATH_OUT_DATA, "demographics.rds", sep="/"))

outcomes['year']= apply(outcomes, 1, function(x) (which(letters == x['wave']) + 2000))



#demographics$wave <- apply(outcomes, 1, function(x) (which(letters == x['wave']) + 2000))

outcomes %>%
  filter(code == 'losat') %>% 
  mutate(
    life_satisfaction = case_when(
      val < 0 ~ NA_integer_,
      TRUE ~ as.integer(val)
    )
  ) %>% 
  dplyr::select(-code, -val) -> cognitive.df


cognitive_mean_by_year.df=cognitive.df %>%
  group_by(year) %>%
  dplyr::summarize(Mean = mean(life_satisfaction, na.rm=TRUE)) # grouping by year




demographics %>%
  filter(code == 'hgage') %>%
  mutate(age = floor(val/10)) %>%
  dplyr::select(xwaveid, wave, age) -> age.df

demographics %>%
  filter(code == 'female') %>%
  mutate(sex = val) %>%
  dplyr::select(xwaveid,sex) -> female.df

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
  dplyr::select(xwaveid, wave, seifa = val) %>%
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
  dplyr::select(xwaveid, wave, edu = reval) %>%
  replace_na(list(edu = 0)) -> edu.df

edu.df %>%
  left_join(age.df, by = c("xwaveid", "wave")) %>%
  left_join(seifa.df, by = c("xwaveid", "wave")) %>%
  left_join(female.df, by = c("xwaveid"))-> covariates

main_df=cognitive.df %>%
  left_join(covariates, by = c("xwaveid", "wave")) 


main_df=main_df%>% filter(!is.na(life_satisfaction))

means_female=main_df %>%
  group_by(year) %>%filter(sex==1)%>%
  dplyr::summarize(Mean_female = mean(life_satisfaction, na.rm=TRUE)) %>% dplyr::select(-year)# grouping by year


means_male=main_df %>%
  group_by(year) %>%filter(sex==0)%>%
  dplyr::summarize(Mean_male = mean(life_satisfaction, na.rm=TRUE)) %>% dplyr::select(-year)# grouping by year



means_all=main_df %>%
  group_by(year) %>%
  dplyr::summarize(Mean_all = mean(life_satisfaction, na.rm=TRUE)) # grouping by year


means_to_plot <-cbind(means_all,means_male,means_female)


x_axis_labels <- min(means_to_plot[,'year']):max(means_to_plot[,'year'])

filename=paste(PATH_OUT_FIGURES, "average_life_satisfaction_all.png", sep="/")

ggplot(means_to_plot, aes(x=year)) + 
  geom_line(aes(y = Mean_female, color = "female")) + 
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
  geom_line(aes(y = Mean_male, color="Males"), linetype="twodash") +
  geom_line(aes(y = Mean_all, color="All"), linetype="twodash",size=2)+
   scale_color_manual(values = c(
  'Y1' = 'darkblue',
  'Y2' = 'red',
  'Y2' = 'black')) +
  labs(color = 'Y series')+ggtitle("Average Life Satisfaction of Australians (2002-2016)")+ labs(y="Life Satisfaction", x = "Year")

ggsave(filename)

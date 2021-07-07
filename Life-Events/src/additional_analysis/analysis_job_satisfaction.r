# packages needed
rm(list = ls())
source("project_paths.r")

library(dplyr)
library(tidyverse)
library(plm)
library(lmtest)
library(broom)
# library(flextable)
# library(officer)
#install.packages("stargazer",repos = "http://cran.us.r-project.org")
#install.packages("corrplot",repos = "http://cran.us.r-project.org")
#install.packages("plotmo",repos = "http://cran.us.r-project.org")
library(knitr)
library(stargazer)
library(MASS)
library(readr)
library(tidyr)

tribble(
  ~Event, ~Description,
  "jomcsb","The company I work for will still be in business 5 years from now",
  "jomcd", "My job is complex and difficult",
  "jomfw", "Freedom to decide when to work",
  "jomls", "I have a lot of say about what happens on my job",
  "jomfd", "I have a lot of freedom to decide how I do my own work",
  "jomns", "My job often requires me to learn new skills",
  "jompf", "I get paid fairly for the things I do in my job",
  "jomsf", "I have a secure future in my job",
  "jomus", "I use many of my skills and abilities in my current job",
  "jomms", "My job is more stressful than I had ever imagined",
  "jomwf", "I worry about the future of my job",
  "jomvar", "I worry about the future of my job",
  "jomtime", "I dont have enough time to do everything in my job",
  "jomrpt", "My job requires me to do the same things over and over again", 
  "jomflx", "My working times can be flexible",
  "jomdw", "I have a lot of choice in deciding what I do at work",
) %>%
  kable()






tribble(
  ~Event, ~Description,
  "jbmsall","Overall job satisfaction",
  "jbmsflx", "The flexibility to balance work and non-work satisfaction",
  "jbmshrs", "The hours you work satisfaction (plot)",
  "jbmspay", "Total pay satisfaction (plot)+estimate",
  "jbmssec", "Job security satisfaction",
  "jbmswrk", "The work itself satisfaction",
  "jbmtuea", "IDV: Union membership or employee association (dont know=no) ",
) %>%
  kable()

getwd()

#loading in the relevant data



outcomes <- read_rds(paste(PATH_OUT_DATA, "job_sat.rds", sep="/"))

independent_vars_subjective <- read_rds(paste(PATH_OUT_DATA, "subj_vars_job_sat.rds", sep="/"))



#preprocessing the necessary data for the upcoming regressions.
outcomes %>%
  filter(code == 'jbmsall') %>% 
  mutate(
    Y = case_when(
      val < 0 ~ NA_integer_,
      TRUE ~ as.integer(val)
    )
  ) %>% 
  dplyr::select(-code,-val) -> job_sat.df


independent_vars_subjective %>%
  mutate(
    val = case_when(
      val < 0 ~ NA_integer_,
      TRUE ~ as.integer(val)
    )
  )-> independent_vars


  
data_wide <- spread(independent_vars,code,val)


#forming the data frame by joining the outcome and independent variables
job_sat.df %>%
  left_join(data_wide, by = c("xwaveid", "wave")) -> full.df



full.df %>%
  ungroup() %>%
  dplyr::select(-Y, -xwaveid) %>%
  dplyr::select(-wave,everything()) %>% 
  colnames() -> var.names 




var.names <-var.names[var.names!='wave']

#running the fixed effect panel regression for all the variables,the positive events and the negative events.

full.formula <- as.formula(paste('Y ~', paste(var.names, collapse = ' + ')))
fe <- plm(full.formula, full.df, index = c('wave'), method = 'within')
fe_trunc_pos <- plm(Y~ jomcsb+jomfw+jomfd+jomns+jompf+jomdw+jomvar, full.df, index = c('wave'), method = 'within')
fe_trunc_negative <- plm(Y~ jomcd+jomms+jomwf+jomrpt, full.df, index = c('wave'), method = 'within')
#Figure 1: 
star_fe <-stargazer(fe_trunc_pos,fe_trunc_negative,type="latex",title= "Panel Fixed Effect Regression Results",
                    covariate.labels = c("Company stability","Freedom when to work","Freedom how to work","Learn new Skills",
                                         "Fair wage","Freedom what to do","Interesting work","Work complex and difficult","Job stress","Worry job stability",
                                          "Repetitive"
                                           ),out=paste(PATH_OUT_FIGURES, "figure1_pos_neg_linear_reg_job.tex", sep="/"))
                    
#shows the relationship between positive and negative variables on job satisfaction



#now let us use ordered logit to rerun the regression using all the variables

full.formula <-as.formula(paste('factor(Y) ~', paste(paste(var.names,collapse = ' + '),'+factor(wave)')))
m <- polr(full.formula, data = full.df, Hess=TRUE)

m_pos<-polr(factor(Y)~ jomcsb+jomfw+jomfd+jomns+jompf+jomdw+jomvar+factor(wave), data = full.df, Hess=TRUE)

m_neg<-polr(factor(Y)~ jomcd+jomms+jomwf+jomrpt+factor(wave), data = full.df, Hess=TRUE)

summary(m)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
exp(coef(m))
stargazer(m)
print(full.formula)


#Figure 2: 

star_fe <-stargazer(fe_trunc_pos,fe_trunc_negative,m_pos,m_neg,keep=var.names,type="latex",
                    title= "Panel Fixed Effect Regression Results (Feature Selected)",
                    no.space = TRUE,
                    column.sep.width = "3pt",
                    covariate.labels = c("Company stability","Freedom when to work",
                                         "Freedom how to work","Learn new Skills",
                                         "Fair wage","Freedom what to do","Interesting work",
                                         "Work complex and difficult","Job stress",
                                         "Worry job stability",
                                         "Repetitive"),
                    dep.var.labels   = c("Job Satisfaction","Job Satisfaction"),out=paste(PATH_OUT_FIGURES, "figure2_pos_neg_linear_reg_ordered_logit_job.tex", sep="/"))

# however there is high correlation and it is best to first filter out the variables to a manageable number using some kind
# of penalized regression or elastic net methods.

###

##correlation plot 
#install.packages("corrplot")
library(corrplot)
#install.packages("ggplot2")
library(ggplot2)

full.df %>%
  filter(wave=='e')%>% 
  subset(select = -c(wave,xwaveid)) -> wave_e
summary(wave_e)

cc = cor(wave_e, method = "spearman",use="pairwise.complete.obs")
#drawing the correlation plots

jpeg(file = paste(PATH_OUT_FIGURES, "figure_3_job_variables_corr_plot.png", sep="/"),width = 465, height = 225, units='mm', res = 300)
#Figure 3: 
corrplot(cc,method = "shade")
while (!is.null(dev.list()))  dev.off()

##feature selection
#install.packages("glmnet")
#install.packages("plotmo")
library(glmnet)
library(plotmo)

df_temp_dataframe <-as.data.frame(wave_e) 
df_temp_dataframe <-na.omit(df_temp_dataframe)

library(caret)

# Model Building : Elastic Net Regression 
train_control <- trainControl(method = "repeatedcv", 
                              number = 5, 
                              repeats = 5, 
                              search = "random", 
                              verboseIter = TRUE) 




invisible(elastic_model_all <- train(Y~ ., 
                           data = df_temp_dataframe, 
                           method = "glmnet", 
                           preProcess = c("center", "scale"), 
                           tuneLength = 25, 
                           trControl = train_control))

elastic_model_all$bestTune
#alpha and lambda that is best performing for entire data set

cat("Within entire data set,the number of variables have been shrunk to 11")
CF_all <- as.matrix(coef(elastic_model_all$finalModel, elastic_model_all$bestTune$lambda))
as.data.frame(CF_all[CF_all!=0,])




##rerunning the regressions with filtered variables

fe_filt <- plm(Y~jomflex+jomls+jomms+jompf+jomsf+jomus+jomvar, full.df, index = c('wave'), method = 'within')
m_filt <- polr(factor(Y)~jomflex+jomls+jomms+jompf+jomsf+jomus+jomvar+factor(wave), data = full.df, Hess=TRUE)
#figure 5




star_fe_5 <-stargazer(fe_filt,m_filt,keep=var.names,type="latex",
                    title= "Panel Fixed Effect Regression Results",
                    column.sep.width = "3pt",
                    covariate.labels = c("Flexible Work Times","Freedom to make Decisions",
                                         "Job Stress","Fair Wage",
                                         "Worry job stability","Personal Skills utilized","Interesting work"
                                          ),
                    dep.var.labels   = c("Job Satisfaction","Job Satisfaction"),out=paste(PATH_OUT_FIGURES, "figure5_pos_neg_linear_reg_ordered_logit_filtered_job.tex", sep="/"))
                    
                    

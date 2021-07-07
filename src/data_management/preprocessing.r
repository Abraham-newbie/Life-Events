'
The file preprocessing.r contains instructions to fetch the required variables from 
the HILDA Panel data, from the waves from 2002 to 2016. The variables being
fetched include all the outcomes of interest,the life events we are interested
in as well as many control variables and variables of interest for additional 
plotting.
'
source("project_paths.r")

library(tidyverse)
library(haven)


#### Helper functions ####
source(paste(PATH_IN_DATA_MANAGEMENT, "GetRaws.r", sep="/"))
source(paste(PATH_IN_DATA_MANAGEMENT, "GetMCS.r", sep="/"))
source(paste(PATH_IN_DATA_MANAGEMENT, "GetEventTimes.r", sep="/"))


#### Load data ####
filepaths =c( paste(PATH_IN_DATA,"Combined_b180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_c180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_d180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_e180c.dta", sep="/"),
paste(PATH_IN_DATA,  "Combined_f180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_g180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_h180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_i180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_j180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_k180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_l180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_m180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_n180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_o180c.dta", sep="/"),
paste(PATH_IN_DATA, "Combined_p180c.dta", sep="/"))


hilda <- list()
for (pathtofile in filepaths) {
  df <- read_dta(pathtofile)
  hilda <- append(hilda, list(df))
  cat('.')
}
##new variables

#Filtering out the new variables from the HILDA data set

life_shocks <-list(
"lebth","ledfr","ledhm","ledrl","ledsc","lefni","lefnw","lefrd","leinf","leins","lejlf","lejls","lejob","lemar","lepcm","leprg","leprm","lercl","lertr","lesep","levio") #life event variables



cog.vars<-list("ctbds","ctwps","ctsds") #cognitive variables


rem.vars<-list("hhrpid","hhpxid"
                ,"ghmh","ghvt","pddepr","pdeff","pdenerv","pderles", "pdhless",
                "pdnerv" ,"pdrless", "pdsad","pdtired" ,"pdwless","esbrd",
                "pnagree", "pnconsc", "pnemote", "pnextrv", "pnopene","esbrd","esdtl","hgsex",
                "tifeftf","tifditf",
                "tifeftp", "tifeftn","tifditp", "tifditn","hifdip", "hifdin","hifeftp","hifeftn",
                "hwassei","hwdebti","hwnwip", "hwnwin","tcr","firisk","firiska", "fisavep",
                "lstrust","pntrisk","fmfsch","fmmsch","edhigh1","mrcms","mrchgmt","mrchgyr") #remaining variables

#remaining

var.codes<-c(life_shocks,cog.vars,rem.vars)



GetRaws(hilda,var.codes)-> hilda_life_events_new_vars
write_rds(hilda_life_events_new_vars,paste(PATH_OUT_DATA, "hilda_life_events_new_vars.rds", sep="/"))




#### Get Variables ####
# Get the life events
lifevents <- c('lefnw', 'lefni', 'lefrd', 'lejob', 'leprm', 'lertr', 'lemar',
               'leprg', 'lebth', 'ledsc', 'lesep', 'lemvd', 'leins', 'lepcm',
               'lejls', 'lejlf', 'ledfr', 'ledrl', 'leinf', 'lercl', 'levio')
#dropped ledhm 7 years missing (maybe removed in latest wave)


events_by_time <- GetEventTimes(lifevents, hilda)

write_rds(events_by_time,paste(PATH_OUT_DATA, "events_by_time.rds", sep="/"))

for (df in events_by_time) {
  print(dim(df))
}
# Store the events to use as covariates
# To be done


# Get outcome variables
mcs <- GetMCS(hilda)

GetRaws(hilda, c(
  'gh9a', # Vitality: feel full of life (lower is better)*
  'gh9b', # Mental H: Been a nervous person (higher is better)
  'gh9c', # Mental H: Felt so down in the dumps (higher is better)
  'gh9d', # Mental H: Felt calm and peaceful (lower is better)*
  'gh9e', # Vitality: Have a lot of energy (lower is better)*
  'gh9f', # Mental H: Felt down (higher is better)
  'gh9g', # Vitality: Felt worn out (higher is better)
  'gh9h', # Mental H: Been happy (lower is better)*
  'gh9i'  # Vitality: Felt tired (higher is better)
  )
) -> gh9

reversed_items <- c('gh9a', 'gh9d', 'gh9e', 'gh9h')

gh9 %>%
  # Recode missing to NA
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%  # if less than zero then change to NA-real,otherwise do nothing
  # Reverse score
  mutate_at(reversed_items, ~ 7 - .) -> gh9_items  #reverses as 7 minus the current value

write_rds(gh9_items,paste(PATH_OUT_DATA, "gh9_items.rds", sep="/"))


gh9_items %>%
  gather(code, val, -xwaveid, -wave) %>% #reconverts to long format
  # Impute average if less than half missing (see ghmh data dictionary)  i.e, if less than 5 wave values values missing for each person
  group_by(xwaveid, wave) %>%
  mutate(
    sum_na = sum(is.na(val)),
    mean_na = mean(val, na.rm = TRUE),
    imputed = ifelse(is.na(val) & sum_na < 5, mean_na, val),
    sum_imputed = sum(imputed),
    code = "gh9_sum"
  ) %>% 
  select(xwaveid, wave, code, val = sum_imputed) %>%
  distinct() %>%  #Select only unique/distinct rows from a data frame
  ungroup() -> gh9_imputed 

GetRaws(hilda, c('losatyh', 'ghmh','losatsf','losatfs','firisk','losat')) %>%  # losat-self assessed , ghmh- mental health
  bind_rows(mcs) %>%
  bind_rows(gh9_imputed) -> outcomes


write_rds(outcomes,paste(PATH_OUT_DATA, "outcomes.rds", sep="/"))

#### Get demographic covariates ####
GetRaws(hilda, c('hgage', 'hhda10', 'edhigh1')) -> covariates #hgage-age,hhda10-Decile of Index of relative socioeconomic disadvantage,edhigh1-History: Highest education level achieved

GetRaws(hilda, 'hgsex') %>%
  group_by(xwaveid) %>%
  summarise(sex = round(mean(val))) %>%
  mutate(val = sex - 1, #male is 0,female is 1
         code = 'female') %>%
  select(xwaveid, code, val) %>%
  bind_rows(covariates) %>%
  arrange(xwaveid, code, wave) -> demographics

write_rds(demographics,paste(PATH_OUT_DATA, "demographics.rds", sep="/"))




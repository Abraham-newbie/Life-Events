
source("project_paths.r")

library(tidyverse)
library(haven)


#### Helper functions ####
source(paste(PATH_IN_DATA_MANAGEMENT, "GetRaws.r", sep="/"))


#### Load data ####
filepaths =c( 
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

job_satisfaction <-GetRaws(hilda,"jbmsall")
objective_job_list<-c("jbmsch","jbmsflx","jbmshrs","jbmsl","jbmspay","jbmssec","jbmsvsr",
"jbmswrk","jbmtuea","jbmmwpsz","jbn","jbnewjs","jboccyr","jbprhr")

subjective_job_list<-c("jomcsb","jomcd","jomfw","jomls","jomfd","jomns","jompf","jomsf","jomus","jomms","jomwf","jomvar","jomtime","jomrpt","jomflex","jomdw")
addntl_controls_list<-c("hhstate")

independent_vars_job_objective<-GetRaws(hilda,objective_job_list)
independent_vars_job_subjective<-GetRaws(hilda,subjective_job_list)
addntl_controls<-GetRaws(hilda,addntl_controls_list)


write_rds(independent_vars_job_objective,paste(PATH_OUT_DATA, "obj_vars_job_sat.rds", sep="/"))
write_rds(independent_vars_job_subjective,paste(PATH_OUT_DATA, "subj_vars_job_sat.rds", sep="/"))
write_rds(addntl_controls,paste(PATH_OUT_DATA, "controls_job_sat.rds", sep="/"))
write_rds(job_satisfaction,paste(PATH_OUT_DATA, "job_sat.rds", sep="/"))






GetEventTimes <- function(eventcodes, datalist) {
  
  require(dplyr)
  #source('src/GetRaws.R')
  
  df.list <- list()

  for (eventcode in eventcodes) {
    #### Recode data so event = 1, no event = 0, missing = NA ####
    #
    codeq <- paste0(eventcode, 'q', seq(4)) # get the quarterly codes
    #
    # Creating the event data frame...
    GetRaws(datalist, c(eventcode, codeq)) %>% 
      spread(key = code, value = val) %>% 
      # missing values replaced with NA
      mutate_at(eventcode, funs(case_when(. < 0 ~ NA_real_, 
                                          TRUE ~ as.numeric(.)))) %>%
      mutate_at(eventcode, funs(. - 1)) %>%
      mutate_at(codeq, funs(case_when(. < 0 ~ NA_real_,
                                      TRUE ~ as.numeric(.)))) -> df
    
    # rename our column variables to make selection easier
    colnames(df) <- c("xwaveid", "wave", "annual", "post03", 
                            "post06", "post09", "post12")
    
    # set the column order to allow column indexing below
    colorder <- c('xwaveid', 'wave', 'pre36', 'pre24', 'pre12', 'post03', 
                  'post06', 'post09', 'post12', 'post24', 'post36', 'post48')
    
    # Add lead and lag variables based on annual event record
    # pre36 = 2-3 years before event
    # pre24 = 1-2 years before event
    # pre12 = 0-1 years before event
    # post03 = 0-3 months after event
    # post06 = 3-6 months after event
    # post09 = 6-9 months after event
    # post12 = 9-12 months after event
    # post24 = 1-2 years after event
    # post36 = 2-3 years after event
    # post48 = 3-4 years after event
    
    df %>%  
      group_by(xwaveid) %>%
      # The annual event record is used to create yearly leads and lags (pre12 
      # to post36). If the annual event record is missing (even if quarterly 
      # event records are present), then leads and lags will be missing
      mutate( 
        pre12  = dplyr::lead(annual, n = 1, order_by = wave),
        pre24  = dplyr::lead(annual, n = 2, order_by = wave),
        pre36  = dplyr::lead(annual, n = 3, order_by = wave),
        post24 = dplyr::lag(annual, n = 1, order_by = wave),
        post36 = dplyr::lag(annual, n = 2, order_by = wave),
        post48 = dplyr::lag(annual, n = 3, order_by = wave)
      ) %>%
      ungroup() %>%
      select(-annual) %>%
      select(one_of(colorder)) %>%
      mutate(rowsums = rowSums(.[3:12], na.rm = TRUE)) %>%
      filter(rowsums > 0) %>%  #This removes rows containing only zeroes, NAs are counted as zeros,i.e,removes persons with no values in all of the event variables.
      select(-rowsums) -> df
    
    df.list[eventcode] <- list(df)
    
  }
  
  return(df.list)
}

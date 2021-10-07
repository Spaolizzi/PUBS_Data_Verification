
##Load Packages
pacman::p_load(tidyverse, dependlab, readr)


setwd("~/github_repos/Cannon_Task_Inquisit/Data/")
cannon_data_19 <- data.table::fread("pubs_pilot_cannon_task_forpilot_6.30.21_raw_21_07_29.csv", fill = TRUE)
cannon_data_31 <- data.table::fread("pubs_pilot_cannon_task_forpilot_6.30.21_raw_21_07_29-31.csv", fill = TRUE)


setwd("~/github_repos/Cannon_Task_Inquisit/Data/")
cannon_data_19 <- data.table::fread("pubs_pilot_cannon_task_forpilot_6.30.21_raw_21_07_29.csv", fill = TRUE)
cannon_data_31 <- data.table::fread("pubs_pilot_cannon_task_forpilot_6.30.21_raw_21_07_29-31.csv", fill = TRUE)



tidy_cannon_data <- function(path){
  out <- list()
  for(i in names(cannon_data)){
    df <- cannon_data[[i]]
    charcols <- c("Date", "cond"); numcols <- names(df)[!names(df) %in% charcols]
    out[[i]] <- as.data.frame(do.call(cbind, lapply(df, unlist))) %>% mutate_at(vars(all_of(numcols)), funs(as.numeric)) %>% tibble() 
  }
  
  return(out)
}

x <- tidy_cannon_data("ADL_B_278.mat")
str(x)

view(cannon_data[1])

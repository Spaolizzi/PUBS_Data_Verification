grades <-read_csv("~/Downloads/gradebook_export-e0ad4931-7ee6-4522-8a0c-32dbd9d02c6d.csv")


test <- cannon_data %>% filter(!is.na(changepoint))


library(dplyr)
library(data.table)





test2 <- cannon_data %>% 
  select(subject, cond, blocknum, trialnum, Abs_PE, PE_angmu, shield_size, StaySwitch, 
         placementAngle, angmu, outcome) %>% mutate(angmu, angmu = ifelse(cond == "ODDBALL" & StaySwitch == 1, outcome, angmu)) %>%
  mutate (update_angmu = angmu - lag(angmu)) %>% 
  mutate (update_outcome = outcome - lag(outcome)) %>% 
  filter(StaySwitch == 1 | lead(StaySwitch == 1))
  
  
cannon_data <- cannon_data %>%
  mutate(angmu, angmu = ifelse(cond == "ODDBALL" & StaySwitch == 1, outcome, angmu)) %>%
  mutate (update_angmu = angmu - lag(angmu)) %>% 
 mutate (update_outcome = outcome - lag(outcome))


testcp <- test2 %>% filter(cond == "CHANGEPOINT") %>% na.omit

testodd <- test2 %>% filter(cond == "ODDBALL") %>% filter(subject == 925) %>% na.omit

          
                        
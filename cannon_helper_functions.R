tidy_cannon <- function(cannon_data) {
  cannon_data <- cannon_data_8.16 %>% 
    select(!c(build, experimentName, list.previouspos.currentvalue, list.Condition.currentvalue, ang)) %>% 
    group_by(subject, time) %>% mutate(subject, subject = ifelse(time = "21:26:09", 1, 2))## be by subject
  filter(trialcode == "cannon_outcome") %>% filter(!practiceblock < 6) %>% ##only data from trials
    mutate(picture.shield.currentitem, shield_size = as.numeric(parse_number(picture.shield.currentitem))) %>% ## make shield size numeric
    mutate_if(is.integer, as.numeric) ## make all integers numeric
  assign("cannon_data",cannon_data, envir = .GlobalEnv)
}
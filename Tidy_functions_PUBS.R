tidy_cannon <- function(cannon_data) {
  cannon_data <- cannon_data_8.16 %>% 
    select(!c(build, experimentName, list.previouspos.currentvalue, list.Condition.currentvalue, ang)) %>% 
    group_by(subject, time) %>% ## be by subject
    dplyr::filter(trialcode == "cannon_outcome") %>% filter(!practiceblock < 6) %>% ##only data from trials
    mutate(picture.shield.currentitem, shield_size = as.numeric(parse_number(picture.shield.currentitem))) %>% ## make shield size numeric
    mutate_if(is.integer, as.numeric) %>% mutate(placementAngle, ifelse(outcomeindex == 1, NA, placementAngle)) %>%
    mutate(cond = as.factor(cond))
  assign("cannon_data",cannon_data, envir = .GlobalEnv)
}

discrep <- function(angmu, r) {
  phi <- abs(angmu - r) %% 360
  if (phi > 180) { phi <- 360 - phi }
  return(phi)
}

create_vars <- function(cannon_data, set_vars){
  if("Abs_PE" %in% set_vars) {
    cannon_data$Abs_PE <- NA
    for(r in 1:nrow(cannon_data)){
      tmp <- discrep(cannon_data$outcome[r], cannon_data$placementAngle[r])
      cannon_data$Abs_PE[r] <- tmp}
  }
  if("PE_angmu" %in% set_vars){
    cannon_data$PE_angmu <- NA
    for(r in 1:nrow(cannon_data)){
      tmp <- discrep(cannon_data$angmu[r], cannon_data$placementAngle[r])
      cannon_data$PE_angmu[r] <- tmp}
  }
  if ("catch_miss" %in% set_vars) {
    cannon_data$catch_miss <- cannon_data$outcomeindex 
    for(r in 1:nrow(cannon_data)){
      if (cannon_data$catch_miss[r] == "1"){
        cannon_data$catch_miss[r] <- "noresp"
        #cannon_data$PE_angmu[r] <- NA
        #cannon_data$PE_raw[r] <- NA
        #cannon_data$trial.placeshield_mouse.latency[r] <- NA
      } else if (cannon_data$catch_miss[r] == 5){
        cannon_data$catch_miss[r] <- "hit"
      } else if (cannon_data$catch_miss[r] == 6){
        cannon_data$catch_miss[r] <- "miss"
      } 
    }
  }
  if ("changepoint" %in% set_vars){
    for(r in 1:nrow(cannon_data)){
     cannon_data$changepoint[r] <- ifelse(cannon_data$StaySwitch[r] == 1, cannon_data$trialnum[r], NA)
      }
  }
  
  for(r in 1:nrow(cannon_data)){
    cannon_data$cond_num <- NA
    cannon_data$cond_num[r] <- ifelse(cannon_data$cond[r] == "ODDBALL", 1, 2)
  }
 
  
  assign("cannon_data",cannon_data, envir = .GlobalEnv)
}

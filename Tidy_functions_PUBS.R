tidy_cannon <- function(cannon_data) {
  cannon_data <- cannon_data %>% 
    select(!c(build, experimentName, list.Condition.currentvalue)) %>% 
    group_by(subject, time) %>% ## be by subject
    dplyr::filter(trialcode == "cannon_outcome") %>% filter(!practiceblock < 6) %>% ##only data from trials
    mutate(picture.shield.currentitem, shieldsize = as.numeric(parse_number(picture.shield.currentitem))) %>% ## make shield size numeric
    mutate_if(is.integer, as.numeric) %>%
    mutate(cond = as.factor(cond))
  
  #numeric columns 
  cols.num <- c("placementAngle","prev_placementAngle", "shield_size", "subject")
  cannon_data[cols.num] <- sapply(cannon_data[cols.num],as.numeric)
  sapply(cannon_data, class)
  return(cannon_data)
}

discrep <- function(angmu, r) {
  if(is.na(angmu)){
    phi <- NA
  } else if (is.na(angmu)){
    phi <- NA
  } else {
    phi <- abs(angmu - r) %% 360
    if (phi > 180) { phi <- 360 - phi }
    
  }
  return(phi)
}

create_vars <- function(cannon_data, set_vars){
  if("Abs_PE" %in% set_vars) {
    cannon_data$Abs_PE <- NA
    for(r in 1:nrow(cannon_data)){
      if(is.na(cannon_data$placementAngle[r])){
        cannon_data$Abs_PE[r] <- NA
      } else if(is.na(cannon_data$outcome[r])){
        cannon_data$Abs_PE[r] <- NA
      } else {
        cannon_data$Abs_PE[r] <- discrep(cannon_data$outcome[r], cannon_data$placementAngle[r])
      }
    }
  }
  if("PE_angmu" %in% set_vars){
    cannon_data$PE_angmu <- NA
    for(r in 1:nrow(cannon_data)){
      if(is.na(cannon_data$placementAngle[r])){
        tmp <- NA
      } else {
      tmp <- discrep(cannon_data$angmu[r], cannon_data$placementAngle[r])
      cannon_data$PE_angmu[r] <- tmp}
    }
  }
  if ("catch_miss" %in% set_vars) {
    cannon_data$catch_miss <- cannon_data$outcomeindex 
    for(r in 1:nrow(cannon_data)){
      if (cannon_data$catch_miss[r] == 1){ ## check 
        if(cannon_data$trial.placeshield_mouse.latency[r] == 2500){
          cannon_data$catch_miss[r] <- "noresp"
          cannon_data$PE_angmu[r] <- NA
          cannon_data$Abs_PE[r] <- NA
          cannon_data$placementAngle[r] <- NA
          cannon_data$trial.placeshield_mouse.latency[r] <- NA
        }
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
  cannon_data$cond_num <- NA
  for(r in 1:nrow(cannon_data)){
    cannon_data$cond_num[r] <- ifelse(cannon_data$cond[r] == "ODDBALL", 1, 2)
  }
  return(cannon_data)
}


check_irreg <- function(cannon_data){
  cannon_data$Irreg <- NA
  for(r in 1:nrow(cannon_data)){
    if(is.na(cannon_data$Abs_PE[r])) {
      if(cannon_data$outcomeindex[r] != 1){
        cannon_data$Irreg[r] <- "CHECK_NA"
      } else {
        cannon_data$Irreg[r] <- NA
      }
    } else if(is.na(cannon_data$hitmiss[r])){
      cannon_data$Irreg[r] <- NA
    } else if(is.na(cannon_data$shield_size[r])) {
      cannon_data$Irreg[r] <- NA
    } else {
        if (cannon_data$Abs_PE[r] <= (cannon_data$shield_size[r]/2) && cannon_data$hitmiss[r] == 0){
        cannon_data$Irreg[r] <- "CHECK_MISS"
      } else if (cannon_data$Abs_PE[r] >= 30 & cannon_data$hitmiss[r] == 1){
        cannon_data$Irreg[r] <- "CHECK_HIT"
      }
    }
  }
  return(cannon_data)
}












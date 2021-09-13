tidy_cannon <- function(data) {
  data <- data %>% 
    select(!c(build, experimentName, list.Condition.currentvalue)) %>% 
    group_by(subject, time) %>% ## be by subject
    dplyr::filter(trialcode == "cannon_outcome") %>% filter(!practiceblock < 6) %>% ##only data from trials
    mutate(picture.shield.currentitem, shieldsize = as.numeric(parse_number(picture.shield.currentitem))) %>% ## make shield size numeric
    mutate_if(is.integer, as.numeric) %>%
    mutate(cond = as.factor(cond))
  
  #numeric columns 
  cols.num <- c("placementAngle","prev_placementAngle", "shield_size", "subject")
  data[cols.num] <- sapply(data[cols.num],as.numeric)
  sapply(data, class)
  return(data)
}



tidy_reversal <- function(reversal_data) {
  reversal_data <- reversal_data %>% 
    select(!c(build, experimentName, totalcorrect)) %>% 
    group_by(subject, time) %>% ## be by subject
    filter(!stimulusitem1 == "Press SPACE to continue")
  reversal_data <- as.data.table(reversal_data)
  reversal_data <- reversal_data[ , stimulusitem := shift(stimulusitem1, 1)]
  reversal_data <- reversal_data %>% 
    mutate_if(is.integer, as.numeric) %>%
    filter(!stimulusitem == "+")
  return(reversal_data)
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



create_vars <- function(data, set_vars){
  if (numbercorrect %in% set_vars){
    data$consecutivecorrect <- 
  }
  if("Abs_PE" %in% set_vars) {
    data$Abs_PE <- NA
    for(r in 1:nrow(data)){
      if(is.na(data$placementAngle[r])){
        data$Abs_PE[r] <- NA
      } else if(is.na(data$outcome[r])){
        data$Abs_PE[r] <- NA
      } else {
        data$Abs_PE[r] <- discrep(data$outcome[r], data$placementAngle[r])
      }desc
    }
  }
  if("PE_angmu" %in% set_vars){
    data$PE_angmu <- NA
    for(r in 1:nrow(data)){
      if(is.na(data$placementAngle[r])){
        tmp <- NA
      } else {
      tmp <- discrep(data$angmu[r], data$placementAngle[r])
      data$PE_angmu[r] <- tmp}
    }
  }
  if ("catch_miss" %in% set_vars) {
    data$catch_miss <- data$outcomeindex 
    for(r in 1:nrow(data)){
      if (data$catch_miss[r] == 1){ ## check 
        if(data$trial.placeshield_mouse.latency[r] == 2500){
          data$catch_miss[r] <- "noresp"
          data$PE_angmu[r] <- NA
          data$Abs_PE[r] <- NA
          data$placementAngle[r] <- NA
          data$trial.placeshield_mouse.latency[r] <- NA
        }
      } else if (data$catch_miss[r] == 5){
        data$catch_miss[r] <- "hit"
      } else if (data$catch_miss[r] == 6){
        data$catch_miss[r] <- "miss"
      } 
    }
  }
  if ("changepoint" %in% set_vars){
    for(r in 1:nrow(data)){
     data$changepoint[r] <- ifelse(data$StaySwitch[r] == 1, data$trialnum[r], NA)
      }
  }
  data$cond_num <- NA
  for(r in 1:nrow(data)){
    data$cond_num[r] <- ifelse(data$cond[r] == "ODDBALL", 1, 2)
  }
  return(data)
}


check_irreg <- function(data){
  data$Irreg <- NA
  for(r in 1:nrow(data)){
    if(is.na(data$Abs_PE[r])) {
      if(data$outcomeindex[r] != 1){
        data$Irreg[r] <- "CHECK_NA"
      } else {
        data$Irreg[r] <- NA
      }
    } else if(is.na(data$hitmiss[r])){
      data$Irreg[r] <- NA
    } else if(is.na(data$shield_size[r])) {
      data$Irreg[r] <- NA
    } else {
        if (data$Abs_PE[r] <= (data$shield_size[r]/2) && data$hitmiss[r] == 0){
        data$Irreg[r] <- "CHECK_MISS"
      } else if (data$Abs_PE[r] >= 30 & data$hitmiss[r] == 1){
        data$Irreg[r] <- "CHECK_HIT"
      }
    }
  }
  return(data)
}












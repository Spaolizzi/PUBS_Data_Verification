
tidy_cannon <- function(data) {
  data <- data %>% 
    select(!c(build, experimentName, list.Condition.currentvalue)) %>% 
    group_by(subject, time) %>% ## be by subject
    dplyr::filter(trialcode == "cannon_outcome") %>% filter(!practiceblock < 6) %>% ##only data from trials
    mutate(picture.shield.currentitem, shieldsize = as.numeric(parse_number(picture.shield.currentitem))) %>% ## make shield size numeric
    mutate_if(is.integer, as.numeric) %>% #convert all integers to numeric
    mutate(cond = as.factor(cond))
  
  #convert non-numeric columns  to numeric
  cols.num <- c("placementAngle","prev_placementAngle", "shield_size", "subject")
  data[cols.num] <- sapply(data[cols.num],as.numeric)
  sapply(data, class)
  return(data)
}

create_vars_reversal <- function(data){
  if(data$date < "2021-08-31"){
   data<- data %>%
      group_by(subject, block_number, trial_number) %>% ##group by subject and block to prevent weirdness
      mutate(trial_number = as.numeric(trial_number)) %>% mutate(block_number = as.numeric(block_number)) %>% mutate(reversalnumber = as.numeric(reversalnumber)) %>% ##change needed things to numeric
      arrange(subject, block_number, trial_number) %>% ## arrange in order based on grouping
      mutate(rt = ifelse(rightleftcorrect == "left", as.numeric(trial.presentation_left.latency), as.numeric(trial.presentation_right.latency))) %>% ## combine latencies to create overall trial latency
      mutate(trial_response_num = as.numeric(trial.choice.response)) %>% ## combine responses to create overall trial response
      mutate(trial_response = ifelse(trial_response_num == 33, "left", "right")) %>% mutate(trial_response, trial_response = ifelse(trial_response_num != 33 || trial_response_num != 36 , "none", trial_response)) %>% #change name of response variable
      mutate(total_trialnum = ifelse(block_number > 1, trial_number + (50*(block_number-1)), trial_number)) %>% #add running counter for total trials
      mutate(isResponse_num =ifelse(isResponseCorrect == -1, 0, isResponseCorrect)) %>% 
      mutate(reversal_trial = ifelse(trial_number == reversalnumber , trial_number, NA)) %>% #change name of response variable
      select(!c(trial.presentation_left.latency, trial.presentation_right.latency, trial.presentation_left.response, stimulusitem1, trial.presentation_right.response)) %>%
      group_by(subject, block_number) %>% mutate(isResponse_num, numbercorrect = cumsum(isResponse_num)) %>%
      mutate(ConsecutiveCorrect = sequence(rle(as.character(isResponse_num))$lengths)) %>%
      ungroup() %>%
      group_by(subject, block_number, task_phase) %>%
      mutate(phase_trialnum = ifelse(task_phase == "Reversal", cumsum(task_phase == "Reversal"), cumsum(task_phase == "Acquisiton"))) %>%  ## number of trials since reversal
      mutate(numbercorrect_phase = ifelse(task_phase == "Reversal", as.numeric(cumsum(isResponseCorrect == 1)), as.numeric(cumsum(isResponseCorrect == 1)))) %>% ## number of correct choices by phase
      mutate(ResponseCorrect = as.numeric(isResponseCorrect)) %>% ## transform to numeric
      mutate(reached_criterion = ifelse(as.numeric(numbercorrect_phase) == 10, phase_trialnum, NA)) %>% ## did subjects get 10 consecutive answers correct
      mutate(percent_correct_phase = numbercorrect_phase/max(phase_trialnum))  %>% # percent of correct choices by phase (acquisiton or reversal)
      #mutate(diff_numbercorrect_phase = numbercorrect_phase - lag(numbercorrect_phase)) %>%
      ungroup() %>%
      group_by(subject, block_number) %>%
      mutate(numbercorrect = cumsum(isResponse_num)) %>%
      ungroup() %>% 
      group_by(subject, block_number, task_phase, grp = lag(cumsum(isResponseCorrect == -1), default = 0)) %>%
      mutate(ConsecutiveCorrect = ifelse(isResponseCorrect == -1, 0, cumsum(isResponseCorrect))) %>%
      mutate(ConsecutiveCorrect = ifelse(isResponseCorrect == 0, NA, ConsecutiveCorrect)) %>%
      filter(!block_number > 4) %>%
      ungroup() %>% select(-grp)
  } else if(data$date > "2021-08-31"){
    data<- data %>%
      group_by(subject, block_number, trial_number) %>% ##group by subject and block to prevent weirdness
      mutate(trial_number = as.numeric(trial_number)) %>% mutate(block_number = as.numeric(block_number)) %>% mutate(reversalnumber = as.numeric(reversalnumber)) %>% ##change needed things to numeric
      arrange(subject, block_number, trial_number) %>% ## arrange in order based on grouping
      mutate(rt = as.numeric(trial.choice.latency)) %>% ## combine latencies to create overall trial latency
      mutate(trial_response_num = as.numeric(trial.choice.response)) %>% ## combine responses to create overall trial response
      mutate(trial_response = ifelse(trial_response_num == 33, "left", as.character(trial_response_num))) %>% mutate(trial_response, trial_response = ifelse(trial_response_num == 36, "right", trial_response)) %>% mutate(trial_response, trial_response = ifelse(trial_response == 0, "noresponse", trial_response)) %>% #change name of response variable
      mutate(total_trialnum = ifelse(block_number > 1, trial_number + (50*(block_number-1)), trial_number)) %>% #add running counter for total trials
      mutate(isResponse_num =ifelse(isResponseCorrect == -1, 0, isResponseCorrect)) %>% 
      mutate(reversal_trial = ifelse(trial_number == reversalnumber , trial_number, NA)) %>% #change name of response variable
      group_by(subject, block_number) %>% mutate(isResponse_num, numbercorrect = cumsum(isResponse_num)) %>%
      mutate(ConsecutiveCorrect = sequence(rle(as.character(isResponse_num))$lengths)) %>%
      ungroup() %>%
      group_by(subject, block_number, task_phase) %>%
      mutate(phase_trialnum = ifelse(task_phase == "Reversal", cumsum(task_phase == "Reversal"), cumsum(task_phase == "Acquisiton"))) %>%  ## number of trials since reversal
      mutate(numbercorrect_phase = ifelse(task_phase == "Reversal", as.numeric(cumsum(isResponseCorrect == 1)), as.numeric(cumsum(isResponseCorrect == 1)))) %>% ## number of correct choices by phase
      mutate(ResponseCorrect = as.numeric(isResponseCorrect)) %>% ## transform to numeric
      mutate(reached_criterion = ifelse(as.numeric(numbercorrect_phase) == 8, phase_trialnum, NA)) %>% ## did subjects get 8 consecutive answers correct
      mutate(percent_correct_phase = numbercorrect_phase/max(phase_trialnum))  %>% # percent of correct choices by phase (acquisiton or reversal)
      #mutate(diff_numbercorrect_phase = numbercorrect_phase - lag(numbercorrect_phase)) %>%
      ungroup() %>%
      group_by(subject, block_number) %>%
      mutate(numbercorrect = cumsum(isResponse_num)) %>%
      ungroup() %>% 
      group_by(subject, block_number, task_phase, grp = lag(cumsum(isResponseCorrect == -1), default = 0)) %>%
      mutate(ConsecutiveCorrect = ifelse(isResponseCorrect == -1, 0, cumsum(isResponseCorrect))) %>%
      mutate(ConsecutiveCorrect = ifelse(isResponseCorrect == 0, NA, ConsecutiveCorrect)) %>%
      filter(!block_number > 5) %>%
      ungroup() %>% select(-grp)
    }
}

check_tidy <- function(data){
  check <- data %>% group_by(subject) %>%
  summarise(count = n_distinct(time),
            no_resp = )
  return(check)
}

tidy_reversal <- function(data) {
  if(data$date < "2021-08-31"){
    data <- data %>% 
      select(!c(build, experimentName, totalcorrect)) %>% 
      group_by(subject, time) %>% ## be by subject
      filter(!stimulusitem1 == "Press SPACE to continue")
    data <- as.data.table(data)
    data <- data[ , stimulusitem := shift(stimulusitem1, 1)]
    data <- data %>% 
      mutate_if(is.integer, as.numeric) %>%
      filter(!stimulusitem == "+")
  } else if (data$date > "2021-08-31"){
    data <- data %>% 
      select(!c(build, experimentName, totalcorrect)) %>% 
      group_by(subject, time) %>% ## be by subject
      filter(!stimulusitem1 == "Press SPACE to continue")
    data <- as.data.table(data)
    data <- data[ , stimulusitem := shift(stimulusitem1, 1)]
    data <- data %>% 
      mutate_if(is.integer, as.numeric) %>%
      filter(!stimulusitem == "+")
  }
  return(data)
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
  if("Abs_PE" %in% set_vars) {
    data$Abs_PE <- NA
    for(r in 1:nrow(data)){
      if(is.na(data$placementAngle[r])){
        data$Abs_PE[r] <- NA
      } else if(is.na(data$outcome[r])){
        data$Abs_PE[r] <- NA
      } else {
        data$Abs_PE[r] <- discrep(data$outcome[r], data$placementAngle[r])
      }
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
      if(data$catch_miss[r] == 1){ ## check 
        if (data$trial.placeshield_mouse.latency[r] == 2500){
          data$trial.placeshield_mouse.latency[r] <- NA
        } 
      } else if (data$catch_miss[r] == 5){
        data$catch_miss[r] <- "hit"
      } else if (data$catch_miss[r] == 6){
        data$catch_miss[r] <- "miss"
      } 
    }
    for(r in 1:nrow(data)) {
      if(is.na(data$trial.placeshield_mouse.latency[r])){
        data$catch_miss[r] <- "noresp"
        data$PE_angmu[r] <- NA
        data$Abs_PE[r] <- NA
        data$placementAngle[r] <- NA
        data$trial.placeshield_mouse.latency[r] <- NA
      }
    }
  }
  if ("changepoint" %in% set_vars){
    data$changepoint <- NA
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




# get_payment <- function(){
#   for (i in subject){
#     if(){
#     }
#   }
# }



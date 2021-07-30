
##Load Packages
pacman::p_load(tidyverse, readr, janitor, ggplot2,wesanderson, cowplot)

##First things first: load data. Eventually ask nate for find.unproccessed code

setwd("~/github_repos/PUBS_Data_Verification/")
reversal_data <- data.table::fread("pubs_pilot_reversal_task_forpilot_7.4.21_raw_21_07_29.csv", fill = TRUE)
reversal_data <- reversal_data %>% row_to_names(4) %>% group_by(subject) %>% filter(n() >= 200)

#drop rows that aren't currently useful
reversal_data <- reversal_data %>% select(!c(practiceInstructionIndex,blockName, 
                                             ITI, numTrialEachBlock, instructionIndex, 
                                             isThisTrialPractice, trialCounter, blockNumber, build, experimentName)) 

##long pipe for tidying data
distinct <- reversal_data %>% 
  group_by(subject, block_number, trial_number) %>% ##group by subject and block to prevent weirdness
  mutate(trial_number = as.numeric(trial_number)) %>%  mutate(block_number = as.numeric(block_number)) %>% mutate(reversalnumber = as.numeric(reversalnumber)) %>% ##change needed things to numeric
  arrange(subject, block_number, trial_number) %>% slice(which.max(centsearned)) %>% ## arrange in order based on grouping
  mutate(trial_latency = ifelse(rightleft == "left", trial.presentation_left.latency, trial.presentation_right.latency)) %>% ## combine latencies to create overall trial latency
  mutate(trial_response = ifelse(rightleft == "left", trial.presentation_left.response, trial.presentation_right.response)) %>% ## combine responses to create overall trial response
  mutate(trial_response = ifelse(trial_response == 45, "left", trial_response)) %>% #change name of response variable
  mutate(trial_response = ifelse(trial_response == 50, "right", trial_response)) %>% #change name of response variable
  mutate(trial_response = ifelse(trial_response == 0, "noresponse", trial_response)) %>% #change name of response variable
  mutate(total_trialnum = ifelse(block_number > 1, trial_number + (80*(block_number-1)), trial_number)) %>% #add running counter for total trials
  mutate(reversal_trial = ifelse(trial_number == reversalnumber , trial_number, NA)) %>% #change name of response variable
  mutate(new_contingency = ifelse(trial_number == 0 || reversal_trial == trial_number, 1, NA)) %>%
  select(!c(trial.presentation_left.latency, trial.presentation_right.latency, trial.presentation_left.response, trial.presentation_right.response)) %>% # delete extra columns for conciseness
  filter(!total_trialnum > 240 && !block_number > 3) %>%
  group_by(subject, block_number) %>% mutate(percent_correct_block = as.numeric(numbercorrect)/80) %>%
  mutate(task_phase = ifelse(trial_number <= reversalnumber, "Acquisition", "Reversal")) %>% 
  ##dplyr::mutate(isResponseCorrect = ifelse(isResponseCorrect == 1, 1, 0)) %>%
  dplyr::filter(!trial_response == "noresponse")
  
  distinct <- distinct %>% mutate(numberoftimesleft = ifelse(trial_response == "noresponse", 0, numberoftimesleft)) %>% 
  mutate(numberoftimesright = ifelse(trial_response == "noresponse", 0, numberoftimesright))

  
  


subjects <- unique(distinct$subject)


##create plot for responsexreversal?
plot_list <- list()

for (i in subjects) {
   j <- ggplot(distinct %>% dplyr::filter(subject == i), aes(x=trial_number, y=trial_response, colour = isResponseCorrect)) + 
     geom_point() + 
     geom_vline(aes(xintercept = reversal_trial, color = "Reversal Point")) + 
     scale_color_manual(labels = c("Correct", "Incorrect", "Reversal Point"), values = (wes_palette("Cavalcanti1")[c(5,4,2)])) + xlab("Trial Number") + ylab("Response Type") + ggtitle(i) +
     facet_wrap(~block_number)
plot_list[[i]] <- j

}


pdf("~/github_repos/PUBS_Data_Verification/Reversal_Pilot.pdf", width = 12, height = 5)
plot_list
dev.off()


###variables for statistical summaries

#what percentage did people get correct by block, not including missed responses?
remove_noresponse <- distinct %>%
  filter(!trial_response == "noresponse") %>% group_by(subject, block_number) %>%
  group_by(subject, block_number, task_phase) %>% mutate(percent_correct_block = max(as.numeric(numbercorrect))/(max(as.numeric(numberoftimesleft)) + max(as.numeric(numberoftimesright)))) 

obj <- remove_noresponse %>% summarise(max = max(percent_correct_block, na.rm=TRUE))

#how long did it take people to get to >10 correct? ## collect this automatically next round!!

remove_noresponse <- remove_noresponse %>% mutate(reached_criterion = ifelse(as.numeric(numbercorrect) > 10, trial_number, NA))

remove_noresponse %>% dplyr::filter(!reached_criterion == 0) %>% summarise(reached_criterion = min(reached_criterion, na.rm=TRUE))

#how many trials did they need to perform better than chance?

distinct %>%
  group_by(subject, block_number) %>% 
  summarise(max = max(percent_correct_block, na.rm=TRUE))

remove_noresponse %>% summarise(max = max(percent_correct_block, na.rm=TRUE))


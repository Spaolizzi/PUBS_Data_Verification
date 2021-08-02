
##Load Packages
pacman::p_load(tidyverse, readr, janitor, ggplot2,wesanderson, cowplot)

##First things first: load data. Eventually ask nate for find.unproccessed code
setwd("~/github_repos/PUBS_Data_Verification/")
reversal_data <- data.table::fread("pubs_pilot_reversal_task_forpilot_7.4.21_raw_21_07_29.csv", fill = TRUE)
reversal_data <- reversal_data %>% row_to_names(4) %>% group_by(subject) %>% filter(n() >= 200)

#drop rows that aren't useful - will delete from main script
reversal_data <- reversal_data %>% select(!c(practiceInstructionIndex,blockName, 
                                             ITI, numTrialEachBlock, instructionIndex, 
                                             isThisTrialPractice, trialCounter, blockNumber, build, experimentName,
                                             picture.Left.currentvalue, picture.Right.currentvalue)) 

##long pipe for tidying data, not by block
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
  select(!c(trial.presentation_left.latency, trial.presentation_right.latency, trial.presentation_left.response, trial.presentation_right.response)) %>% # delete extra columns for conciseness
  filter(!total_trialnum > 240 && !block_number > 3) %>% filter(!trial_number == 0) %>% #removing things we don't need
  group_by(subject, block_number) %>% mutate(percent_correct_block = as.numeric(numbercorrect)/80) %>% ##percentage correct across block
  mutate(task_phase = ifelse(trial_number <= reversalnumber, "Acquisition", "Reversal")) #note which block they're in
  
##doesn't really do what I hoped it would
##distinct <- distinct %>% mutate(numberoftimesleft = ifelse(trial_response == "noresponse", 0, numberoftimesleft)) %>% 
  ##mutate(numberoftimesright = ifelse(trial_response == "noresponse", 0, numberoftimesright))


subjects <- unique(distinct$subject)

distinct <- distinct %>% group_by(subject, block_number) %>% 
  mutate(numbercorrect = as.numeric(numbercorrect)) %>%
  mutate(phase_trialnum = ifelse(task_phase == "Reversal", trial_number - reversalnumber, trial_number)) %>%
  mutate(numbercorrect_acq = ifelse(as.numeric(trial_number) == as.numeric(reversalnumber), numbercorrect, 0)) %>%
  mutate(numbercorrect_reversal = ifelse(task_phase == "Reversal", numbercorrect - max(numbercorrect_acq), 0)) %>%
  mutate(numbercorrect_phase = ifelse(task_phase == "Reversal", numbercorrect_reversal, numbercorrect)) %>% 
  mutate(ResponseCorrect = as.numeric(isResponseCorrect)) %>%
  select(!c(numbercorrect_acq, numbercorrect_reversal)) %>% #phase_numbercorrect
  group_by(subject, block_number, task_phase) %>%
  mutate(reached_criterion = ifelse(as.numeric(numbercorrect_phase) > 10, phase_trialnum, NA)) %>%
  mutate(percent_correct_phase = as.numeric(numbercorrect_phase)/max(as.numeric(phase_trialnum)))  %>% 
  mutate(diff_numbercorrect_phase = numbercorrect_phase - lag(numbercorrect_phase)) %>%
  

test <- distinct %>% group_by(subject, block_number, ResponseCorrect) %>% arrange(subject, block_number, total_trialnum) %>% mutate(ConsecutiveCorrect = cumsum(ResponseCorrect))
  



example <- distinct %>% group_by(block_number) %>% select(subject, block_number, trial_number, ResponseCorrect) %>% slice(2:4)

obj <- distinct %>% summarise(reached_criterion = min(reached_criterion, na.rm=TRUE), percent_correct_phase = max(percent_correct_phase), trial_number = max(phase_trialnum))


plot_subjects <- list()


##create plot for responsexreversal? Overall look at the task
# Plot --------------------------------------------------------------------


for (i in subjects) {
  
  scatter <- ggplot(distinct %>% dplyr::filter(subject == i), aes(x=trial_number, y=trial_response, colour = isResponseCorrect)) + 
     geom_point() + 
     geom_vline(aes(xintercept = reversal_trial, color = "Reversal Point")) + 
     scale_color_manual(labels = c("Incorrect", "Correct", "Reversal Point"),values = (wes_palette("Cavalcanti1")[c(5,4,2)])) + 
     xlab("Trial Number") + ylab("Response Type") + ggtitle(i) +
     facet_wrap(~block_number)
  scatter[[i]] <- scatter

hist <- obj %>% filter(subject == i) %>% group_by(block_number) %>% ggplot(aes(y=percent_correct_phase, x = task_phase, fill = task_phase)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = (wes_palette("Cavalcanti1")[c(4,2)])) + 
  ggtitle(i) + ylab("Percentage Correct") + xlab("") +  facet_grid(~ block_number)
 hist[[i]] <- hist

 plot_subjects[[i]] <- list(scatter, hist) 
}

pdf("~/github_repos/PUBS_Data_Verification/Reversal_Pilot.pdf", width = 12, height = 5)
plot_subjects
dev.off()

# split by block ----------------------------------------------------------


acquisition <- distinct %>% group_by(subject, block_number, task_phase, trial_number) %>% filter(task_phase == "Acquisition")
reversal <- distinct %>% group_by(subject, block_number, task_phase, trial_number) %>% filter(task_phase == "Reversal")


summary <- acquisition %>% arrange(subject, block_number, trial_number) %>%
  group_by(subject, block_number, isResponseCorrect) %>% 
  dplyr::mutate(isResponseCorrect = ifelse(isResponseCorrect == 1, 1, 0)) %>%
  dplyr::filter(!trial_response == "noresponse") %>% summarise(consecutivecorrect = sum(isResponseCorrect), trial_number = max(trial_number))

reversal %>% arrange(subject, block_number, trial_number) %>%
  group_by(subject, block_number, isResponseCorrect) %>% 
  dplyr::mutate(isResponseCorrect = ifelse(isResponseCorrect == 1, 1, 0)) %>%
  
  dplyr::filter(!trial_response == "noresponse") %>% summarise(consecutivecorrect = sum(isResponseCorrect))



# ###transformations for statistical summaries ----------------------------


#what percentage did people get correct by block, including missed responses?
# #distinct <- distinct %>%
#   group_by(subject, block_number, task_phase) %>%
#   
#   
# 
# obj <- distinct %>% summarise(percent_correct = max(percent_correct_block, na.rm=TRUE), trial_number = max(phase_trialnum))

#how long did it take people to get to >10 correct? ## collect this automatically next round!!

 
 
 pdf("~/github_repos/PUBS_Data_Verification/Reversal_Pilot_histograms.pdf", width = 12, height = 5)
 plot_list_hist
 dev.off()
 

   
         
         #how many trials did they need to perform better than chance?
         
         distinct %>%
           group_by(subject, block_number) %>% 
           summarise(max = max(percent_correct_block, na.rm=TRUE))
         
         distinct %>% summarise(max = max(percent_correct_block, na.rm=TRUE))
         
         
         dplyr::mutate(isResponseCorrect = ifelse(isResponseCorrect == 1, 1, 0)) %>%
           dplyr::filter(!trial_response == "noresponse")
         
         
         
         
         ##Trying to calculate consecutivecorrect
         reversal_test <- distinct %>% arrange(subject, block_number, trial_number) %>%
           group_by(subject, block_number, isResponseCorrect) %>% 
           dplyr::mutate(isResponseCorrect = ifelse(isResponseCorrect == 1, 1, 0)) %>%
           dplyr::filter(!trial_response == "noresponse")
         
         #consecutive correct
         for (i in subjects){
           for (r in 1:nrow(distinct)) {
             if (distinct$isResponseCorrect == 1) {
               distinct$consecutivechoice[r] = reversal_test$consecutivechoice[r] + 1
             } else if (reversal_test$isResponseCorrect == -1)  {
               reversal_test$consecutivechoice[r] = 0
             }
           }
         }
        
         
         
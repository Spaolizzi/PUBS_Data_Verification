

load()

earnings <- earnings %>% mutate(total = (centsearned_r + centsearned_c)/100) 

       
       
setwd("~/github_repos/PUBS_Data_Verification/")
qualtrics <- read_csv("Qualtrics_Data_augpilot.csv")

qualtrics <- qualtrics %>% dplyr::filter(!StartDate < "2021-08-30") %>% dplyr::filter(Progress == "100")
qualtrics<- qualtrics %>%  mutate(Duration_mins = as.numeric(`Duration (in seconds)`)/60)

qualtrics <- arrange(qualtrics,Duration_minss)
qualtrics_payment <- qualtrics %>% arrange(qualtrics,Duration_mins) %>% select(Duration_mins, RandomID, Progress)
View(qualtrics_payment)
qualtrics_randomdraw <- qualtrics %>% dplyr::select(., contains("_select")) %>% select(!contains("Click")) %>% select(!contains("submit"))
View(qualtrics_randomdraw)

qualtrics$payout_colnum <- NA
qualtrics$payout_colname <- NA
qualtrics$payout_resp <- NA


for(i in 1:nrow(qualtrics)){
  coln<- floor(runif(1, min=2, max=60))
  element <- i
  qualtrics$payout_colnum[i] <- coln 
  qualtrics$payout_colname[i] <- colnames(qualtrics_randomdraw[coln]) 
}

qualtrics$payout_resp <- unlist(qualtrics_randomdraw[i, qualtrics$payout_col])

for_payment <- qualtrics %>% select(RandomID, payout_colnum,payout_colname, payout_resp, Duration_minss)


for_payment <- for_payment %>% separate(payout_colname, into = c("Likelihood", "Amount"), sep = "_", extra = "drop") %>% 
  mutate(Answer = ifelse(grepl("L", Amount), "F", "J")) %>% 
  mutate(Side = str_sub(Amount, -1)) %>% 
  mutate(Likelihood = as.numeric(Likelihood)/100) %>%
  mutate(Amount = as.numeric(str_sub(Amount, start = 1, end =-2))) %>%
  mutate(Amount = ifelse(Amount < 2.50, Amount*100, Amount)) %>%
  mutate(Probability = runif(10)) %>%
  mutate(Correct = ifelse(payout_resp == Answer, 1, 0)) %>% mutate(true_payout = ifelse(Probability < Likelihood, Amount*Correct, 0)) %>%
  rename(subject = RandomID) %>% mutate(subject = as.numeric(subject)) 

qualtrics_earnings <- for_payment %>% select(subject, true_payout, Duration_minss)

payment <- left_join(qualtrics_earnings, reversal_earnings, by = "subject")
payment <- left_join(payment, cannon_earnings, by = "subject")

payment$total <- (payment$true_payout + payment$centsearned_c + payment$centsearned_r)/100

sum(payment$total)



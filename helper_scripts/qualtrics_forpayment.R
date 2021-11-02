# Load Data and Packages --------------------------------------------------
library(tidyverse)       
setwd("~/github_repos/PUBS_Data_Verification/sample_data/")
qualtrics <- read_csv("PUBS_Batch1_Q_Samp.csv") #download newest qualtrics data and load
cloudresearch <- read_csv("PUBS_Batch1_CR_Samp.csv") #download newest cloudresearch data and load
load("~/github_repos/PUBS_Data_Verification/Payment/Cannon.Rdata")  #run cannon markdown and load
load("~/github_repos/PUBS_Data_Verification/Payment/Reversal.Rdata") #run reversal markdown and load payment data

# Step 1: Combine inquisit earnings ---------------------------------
earnings <- left_join(cannon_earnings, reversal_earnings, by = 'subject') %>%
  mutate(total = (centsearned_r + centsearned_c)/100) #calculate inquisit earnings across both tasks

# Step 2: Filter only complete qualtrics surverys -------------------------
qualtrics <- qualtrics %>%
  dplyr::filter(Progress == "100")
qualtrics<- qualtrics %>%  
  mutate(Duration_mins = as.numeric(`Duration (in seconds)`)/60)
qualtrics <- arrange(qualtrics,Duration_mins)


# Select columns for payout from risk task ---------------------------------

qualtrics_randomdraw <- qualtrics %>% dplyr::select(., contains("_select")) %>% select(!contains("Click")) %>% select(!contains("submit"))

# Select/Create Q columns relevant for payment -----------------------------------
qualtrics <- qualtrics %>% arrange(qualtrics,Duration_mins) %>% select(Duration_mins, RandomID, workerId, Progress)
View(payment)
qualtrics$payout_colnum <- NA
qualtrics$payout_colname <- NA
qualtrics$payout_resp <- NA
for(i in 1:nrow(qualtrics)){
  coln<- floor(runif(1, min=2, max=60))
  element <- i
  qualtrics$payout_colnum[i] <- coln 
  qualtrics$payout_colname[i] <- colnames(qualtrics_randomdraw[coln]) 
}

qualtrics$payout_resp <- unlist(qualtrics_randomdraw[i, qualtrics$payout_colnum])

# Combine all bonus payments into final CSV -------------------------------
for_payment <- qualtrics %>% select(RandomID, workerId, payout_colnum,payout_colname, payout_resp, Duration_mins)

for_payment <- for_payment %>% 
  separate(payout_colname, into = c("Likelihood", "Amount"), sep = "_", extra = "drop") %>% 
  mutate(Answer = ifelse(grepl("L", Amount), "F", "J")) %>% 
  mutate(Side = str_sub(Amount, -1)) %>% 
  mutate(Likelihood = as.numeric(Likelihood)/100) %>%
  mutate(Amount = as.numeric(str_sub(Amount, start = 1, end =-2))) %>%
  mutate(Amount = ifelse(Amount < 2.50, Amount*100, Amount)) %>%
  mutate(Probability = runif(nrow(for_payment))) %>%
  mutate(Correct = as.numeric(payout_resp)) %>%
  mutate(true_payout = ifelse(Probability < Likelihood, Amount*Correct, 0)) %>%
  rename(subject = RandomID, AmazonIdentifier = workerId) %>% mutate(subject = as.numeric(subject)) 

# Format Bonus Payments for export to CloudResearch -----------------------
  
# First, pull all cloud research IDs and approval status
cloudresearch <- cloudresearch %>% rename(AmazonIdentifier = workerId) 
  # next, Join payment with earnings from cannon and reversal tasks 
    # NOTE: If something fails here, you probably haven't gotten the newest data from IQ yet!!
payment <- left_join(for_payment, earnings, by = "subject")


 #Tweaking to generate CSV for upload to CloudResearch
cloudresearch_final <- merge(cloudresearch, payment,  by = "AmazonIdentifier", all = TRUE) %>%
 mutate(`Bonus Amount` = ifelse(StartTime >= "8/30/2021 10:41:31 PM", 1.41, total)) # originally run with mean(payment$total, na.rm = TRUE) in place of 1.41

#final CSV: Return only workers whose HITs have been Approved (must check over inquisit responses before approval!)
  #N.B, probably worth adding a "yes" column to earnings CSV
approved_pilot_1 <- cloudresearch %>% 
    dplyr::select(AmazonIdentifier, `Bonus Amount`, ApprovalStatus) %>% 
    filter(ApprovalStatus == "Approved")


write_csv(cloudresearch_final, "~/github_repos/PUBS_Data_Verification/sample_data/PUBS_Batch1_CR_Samp.csv") #keep this consistent with 
write_csv(approved_pilot_1, "~/github_repos/PUBS_Data_Verification/Payment/Payment_10.26.21.csv")

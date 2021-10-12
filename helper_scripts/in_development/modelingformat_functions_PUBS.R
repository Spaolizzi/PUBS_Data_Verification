###For_Modeling add-ons
pacman::p_load(R.matlab)

load("~/github_repos/PUBS_Data_Verification/Markdowns/cannon_processing.Rdata")
load("~/github_repos/Cannon_Task_Inquisit/Data/cannon_proc.RData")
#res <- readMat("~/github_repos/AdaptiveLearning_Data/ADL_B_000342.mat")

#pull files in we might need (i.e., demographics vaariables)
Demographics <- read_csv("~/github_repos/PUBS_Data_Verification/sample_data/Qualtrics_Data_OctPilot.csv")
Demographics <- Demographics %>% select(gen, Q242_1, RandomID) %>% 
  rename(subject = RandomID, sex = gen,age = Q242_1) %>% 
  mutate(subject = as.numeric(subject))

##cut uneccessary trials from iq_names

#check what's not reconciled
reconcile_names <- c(NA)
cleaned_names <- colnames(cannon_data)
model_names
for(i in 1:length(model_names)){
  if (!is.element(model_names[i], cleaned_names)) {
    reconcile_names <- append(reconcile_names, model_names[i], after = length(reconcile_names)-1)
  }
}

cannon_data <- left_join(cannon_data, Demographics, by = "subject")


##Rename columns already present
    cannon_data <- rename(cannon_data, 
                          ID = subject,
                          Date = date,
                          allASS = shield_size,
                          block =	blocknum,
                          hit = hitmiss, 
                          predT	= trial.placeshield_mouse.latency,
                          pred = placementAngle,
                          accPerf	= totalearnings,
                          trial = trialnum)
cannon_data <- cannon_data %>% group_by(subject, mutate(cannon_data,
                      UPMin = 0,
                      UPNorm = 0
                      UPPlus = 0,
                      catchTrial= 0,
                      
                )
  cannon_data <- mutate(cannon_data,
                        oddballProb = abs(percentTrialsStay -1),
                        actRew = hit*.5,
                        rew = .5,
                        oddBall = ifelse(cond == "ODDBALL", StaySwitch, NA),
                        cp = ifelse(cond == "CHANGEPOINT", StaySwitch, NA))

#next round of updates - will involve more mutating to resolve  
reconcile_updated_names <- c(NA)
updated_names <- colnames(cannon_data)
for(i in 1:length(model_names)){
  if (!is.element(model_names[i], updated_names)) {
    reconcile_updated_names <- append(reconcile_updated_names, model_names[i], after = length(reconcile_updated_names))
  }
}


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
  cannon_data <- cannon_data %>% group_by(ID) %>% mutate(
                          oddballProb = abs(percentTrialsStay -1),
                          predErrMin  = min(predErr),
                          predErrNorm = mean(predErr),
                          predErrPlus  = max(predErr))
  cannon_data <- mutate(cannon_data,
                        actRew = hit*.5,
                        rew = .5,
                        UPMin = 0,
                        UPNorm = 0,
                        UPPlus = 0,
                        catchTrial= 0,
                        sigma = 18.775,
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

cannon_data <- cannon_data[, model_names]
bad_rows <- c()
for (i in 1:nrow(cannon_data)){
  if (is.na(cannon_data$predErr[i])){
    bad_rows <- append(bad_rows, i)
  }
}

cannon_data <- cannon_data[-bad_rows, ]

cannon_data 


cannon_data_split[[i]] <- as.list(split(cannon_data, cannon_data$ID))


CDS_names <- as.numeric(names(cannon_data_split))


 for(i in 1:length(CDS_names)){
  write_csv(cannon_data_split[[i]],
            file.path("~/github_repos/Cannon_Task_Inquisit/Data/cannon_proc_formodeling",
                      paste0("Cannon_",CDS_names[c(i)], ".csv")))
}



samp <- read_csv("~/github_repos/Cannon_Task_Inquisit/Data/cannon_proc_formodeling/Cannon_1.csv")


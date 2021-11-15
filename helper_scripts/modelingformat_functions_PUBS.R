###For_Modeling add-ons
pacman::p_load(R.matlab, tidyverse, qualtRics)

load("~/github_repos/PUBS_Data_Verification/Markdowns/cannon_processing.Rdata")
load("~/github_repos/Cannon_Task_Inquisit/Data/cannon_proc_pilot.RData")
load("~/github_repos/PUBS_Data_Verification/Qualtrics_Data//PUBS_Batch2_QR_Proc.Rdata")
#res <- readMat("~/github_repos/AdaptiveLearning_Data/ADL_B_000342.mat")

#pull files in we might need (i.e., demographics vaariables)
Demographics <- measures_df %>% mutate(subject = RandomID) %>% select(subject, age, gen, Eth, PAI_Tot, PAI_AI, PAI_IP, PAI_NR,PAI_SH, STAI_Tot, STAI_S, STAI_T,
                                                                      PSWQ_Tot, BFAS_V, BFAS_W, BFAS_Tot, PID_anh, PID_anx, PID_attn, PID_callous, PID_deceit, 
                                                                      PID_depres, PID_distr,PID_ecc,PID_emo_lab, PID_grnd, PID_host,PID_impuls,PID_int_avd, 
                                                                      PID_irr, PID_man,PID_perc_dysreg, PID_persev, PID_rest_aff,PID_perf,PID_rt,PID_sep_insec, 
                                                                      PID_sub,PID_sus,PID_unusual, PID_wthdrwl, PID_neg_aff, PID_detach, PID_antag, PID_disinhib, 
                                                                      PID_psycho) %>% rename(sex = gen)
cannon_data <- left_join(cannon_data, Demographics, by = "subject")
#check what's not reconciled
reconcile_names <- c(NA)
cleaned_names <- colnames(cannon_data)
model_names
for(i in 1:length(model_names)){
  if (!is.element(model_names[i], cleaned_names)) {
    reconcile_names <- append(reconcile_names, model_names[i], after = length(reconcile_names)-1)
  }
}


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
                        cp = ifelse(cond == "CHANGEPOINT", StaySwitch, NA)) %>% 
    add_column(boatType = NA, 
               initiationRTs = NA, 
               memErr = NA, memErrMin = NA, 
               memErrNorm = NA, memErrPlus = NA, 
               outT = NA, timestampOffset = NA, 
               timestampOnset = NA, timestampPrediction = NA, 
               triggers = NA, vola = NA)
  
  

#next round of updates - will involve more mutating to resolve  
reconcile_updated_names <- c()
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

cannon_data_split <- as.list(split(cannon_data, f = cannon_data$ID))


CDS_names <- as.numeric(names(cannon_data_split))


 for(i in 1:length(CDS_names)){
  write_csv(cannon_data_split[[i]],
            file.path("~/github_repos/Cannon_Task_Inquisit/Data/cannon_proc_formodeling",
                      paste0("Cannon_",CDS_names[c(i)], ".csv")))
}



#samp <- read_csv("~/github_repos/Cannon_Task_Inquisit/Data/cannon_proc_formodeling/Cannon_1.csv")


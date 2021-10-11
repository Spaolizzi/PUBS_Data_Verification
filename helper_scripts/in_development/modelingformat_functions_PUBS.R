###For_Modeling add-ons
pacman::p_load(R.matlab)

load("~/github_repos/PUBS_Data_Verification/Markdowns/cannon_processing.Rdata")


res <- readMat("~/github_repos/AdaptiveLearning_Data/ADL_B_000342.mat")

names(res$DataOddball.000342)
##cut uneccessary trials from iq_names
cleaned_names
model_names




reconcile_names <- c(NA)

for(i in 1:length(model_names)){
  if (!is.element(model_names[i], cleaned_names)) {
    reconcile_names <- append(reconcile_names, model_names[i], after = length(reconcile_names))
  }
}

for (i in 1:length(cleaned_names)) {
  if(cleaned_names[i] == "subject"){
    cannon_data <- rename(cannon_data, 
                          ID = subject,
                          Date = date,
                          allASS = shield_size,
                          cp = StaySwitch,
                          oddBall = StaySwitch,
                          block =	blocknum,
                          hit = hitmiss, 
                          predT	= trial.placeshield_mouse.latency,
                          pred = placementAngle,
                          accPerf	= totalearnings,
                          trial = trialnum)
  }
  cannon_data <- mutate(cannon_data,
                        oddballProb = abs(percentTrialsStay -1),
                        actRew = ".5")
                        
}

reconcile_names <- c(NA)
updated_names <- colnames(cannon_data)
for(i in 1:length(model_names)){
  if (!is.element(model_names[i], updated_names)) {
    reconcile_names <- append(reconcile_names, model_names[i], after = length(reconcile_names))
  }
}


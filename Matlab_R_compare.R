setwd("~/github_repos/PUBS_Data_Verification/")
pacman::p_load(R.matlab, tidyverse)


res <- readMat("logistic_demo.mat")
#res$binX <- res$binX[res$sel==1,]
#res$anyUp <- res$anyUp[res$sel==1]
df_applysel <- as.data.frame(cbind(res$anyUp, res$binX, res$sel)) %>% setNames(c("anyUp", "blockCond", "modRU","modSurp", "blockCondxmodSurp", "hit", "sel")) %>% filter(!sel == "0") %>% round(digits = 15)

apply(df_applysel, 2, mean)
apply(df_applysel, 2, sd)
 
fit <- glm(anyUp ~ blockCond + modRU + modSurp + blockCondxmodSurp + hit, df_applysel, family=binomial, maxit = 1000)

 summary(fit)
 
 fit$coefficients

 resid_compare <- cbind(as.data.frame(res$STATS[11]), as.data.frame(fit$residuals)) %>% 
   setNames(c("matlab", "R")) %>% mutate(Logical = ifelse(matlab != R, FALSE, TRUE))
 coeff_compare <- cbind(as.data.frame(res$STATS[1]), as.data.frame(fit$coefficients)) %>% 
   setNames(c("matlab", "R")) %>% mutate(Logical = ifelse(matlab != R, FALSE, TRUE))
 wt_compare <- cbind(as.data.frame(res$STATS[15]), as.data.frame(fit$weights)) %>% 
   setNames(c("matlab", "R")) %>% mutate(Logical = ifelse(matlab != R, FALSE, TRUE))


  
 
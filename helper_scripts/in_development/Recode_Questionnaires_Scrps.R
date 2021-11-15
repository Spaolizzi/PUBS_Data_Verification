
##LEFTOVER MANUAL SCORING FROM PUBS: OLD DON'T USE!


# PAI <- PAI %>%
#   mutate(across(
#         starts_with("PAI") & ends_with(c("7","12","14","19","20","24")),
#         ~recode(., `1` ="3",
#                 `2` = "2" ,
#                 `3` = "1",
#                 `4` = "0")
#         )) %>%
#   mutate(across(
#         starts_with("PAI") & !ends_with(c("7","12","14","19","20","24")),
#         ~recode(., `1` = "0",
#                 `2` = "1" ,
#                 `3`= "2",
#                 `4` = "3")))
# 
# 
# PSWQ <- PSWQ %>%
#   mutate(across(
#         starts_with("PSWQ") & !ends_with(c("2", "4", "5", "6", "7", "9", "12", "13", "14", "15", "16")),
#         ~recode(., `1` = "5",
#                 `2` = "4" ,
#                 `3` = "3",
#                 `4` = "2",
#                 `5` = "1")
#         )) %>%
#   mutate(across(
#         starts_with("PAI") & ends_with(c("2", "4", "5", "6", "7", "9", "12", "13", "14", "15", "16")),
#          ~recode(., `1` = "1",
#                 `2` = "2" ,
#                 `3` = "3",
#                 `4` = "4",
#                 `5` = "5")))
# 
# # PAI <- PAI %>%
#   mutate(across(
#         starts_with("PAI") & ends_with(c("7","12","14","19","20","24")),
#         ~recode(., "False, not at all true" = "3",
#                                 "Slightly True" = "2" ,
#                                 "Mainly True" = "1",
#                                 "Very True" = "0",
#                 .default = .)
#         )) %>%
#   mutate(across(
#         starts_with("PAI") & !ends_with(c("7","12","14","19","20","24")),
#         ~recode(., "False, not at all true" = "0",
#                                 "Slightly True" = "1" ,
#                                 "Mainly True" = "2",
#                                 "Very True" = "3",
#                 .default = .)
#         ))
# PAI <- as.data.frame(lapply(PAI, as.numeric))
# PSWQ <- as.data.frame(lapply(PSWQ, as.numeric))



# #PTs over PAI Cutoff
# #seperate into subscales
# PAI$PAI_Score <- NULL
# for (i in 1:nrow(PAI)){
#   PAI$PAI_Tot[i] <- sum(PAI[i, 2:25], na.rm = TRUE)
# }
# 
# 
# #need to sort out a few questions, this should be about 100 qs
# #need to sort into subscales
# PID$PID_Tot <- NULL
# for (i in 1:nrow(PAI)){
#   PID$PID_Tot[i] <- sum(PID[i, 2:110], na.rm = TRUE)
# }
# 
# 
# #seperate into subscales
# STAI$STAI_Tot <- NULL
# for (i in 1:nrow(PAI)){
#   STAI$STAI_Tot[i] <- sum(STAI[i, 2:41], na.rm = TRUE)
# }
# 
# 
# #seperate BFAS into subscales
# BFAS$BFAS_Tot <- NULL
# for (i in 1:nrow(PAI)){
#   BFAS$BFAS_Tot[i] <- sum(BFAS[i, 2:21], na.rm = TRUE)
# }
# 
# 
# #PTs over PAI Cutoff
# PSWQ$PSWQ_Tot <- NULL
# for (i in 1:nrow(PAI)){
#   PSWQ$PSWQ_Tot[i] <- sum(PSWQ[i, 2:17], na.rm = TRUE)
# }

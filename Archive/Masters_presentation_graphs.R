library(ggplot2)
library(dplyr)
library(tidyverse)
library(wesanderson)

vec <- rnorm(1000, mean = 38, sd = 5)
data_frame <- as.data.frame(vec) %>% dplyr::mutate(number = c(1:1000))

p3 <- ggplot(data = data.frame(x = c(0, 80)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 38, sd = 5), size=2, colour = "tan") +
                  scale_color_manual(values = wes_palette("Moonrise2")[c(1)]) + ylab("") +
                  scale_y_continuous(name = "", breaks = seq(0, .1, 0.1), limits=c(0, .11)) + 
  scale_x_continuous(name = "")
p3


vec3 <- c(.04761905)
num <- c(1:80)
data_frame <- as.data.frame(num) %>% dplyr::mutate(prob = ifelse(num <= 50 & num >= 30, .04761905, 0))


p2 <- ggplot(data_frame, aes(x = num, y = prob)) + 
  geom_line(size=2, aes(colour = "red")) +
  scale_color_manual(values = wes_palette("Moonrise2")[c(2)]) + 
  scale_y_continuous(name = "Probability of Reversal",
                     breaks = seq(0, .1, 0.1),
                     limits=c(0, .11)) +
  scale_x_continuous(name = "Trial") + theme(legend.position = "none")
p2

vec <- c(.0125)
num3 <- c(1:80)
data_frame <- as.data.frame(num3)
data_frame$prob3 <- vec


p1 <- ggplot(data_frame, aes(x = num3, y = prob3)) + geom_line(size=2, aes(color = "blue")) +
  scale_color_manual(values = wes_palette("Moonrise2")[c(1)]) +
  scale_y_continuous(name = "",
                     breaks = seq(0, .1, 0.1),
                     limits=c(0, .11)) + scale_x_continuous(name = "") + theme(legend.position = "none")
p1


title <- ggdraw() + 
  draw_label(
    "Distribution of priors on reversal points",
    fontface = 'bold',
    x = 0,
    hjust = -1.15
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
library(cowplot)
grid <- plot_grid(p2,p1,p3, labels = c('M1', 'M2', 'M3'), label_size = 12, nrow = 1, ncol = 3)

grid

cowplot <- plot_grid(
  title, grid,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

pdf("~/Desktop/cowplot.pdf", width = 12, height = 5)
cowplot
dev.off()




# Bar graphs --------------------------------------------------------------





temperature_trendline <- data.frame(c(1:27))
temperature_trendline$value <- temperature_trendline$c.1.27.
temperature_trendline$c.1.27. <- NULL

temperature_trendline$worry_acq <- log(temperature_trendline$value, 100)
temperature_trendline$worry_rev <- c(.28, .29, .3, .31, .35, .36, .38, .37, .36, .38, 
                                     .35, .36, .26, .27, .25, .31, .27, .33,.28, .29, .24,
                                     .26, .27, .25, .28, .24, .23)


temperature_trendline$bpd_acq <-   c(.10, .21, .238, .25, .2, .23, .20, .29, .25, .20, .25, .22, .26,
                                     .27, .28, .20 , .29, .22, .20, .25, .22, .26, .22, .28, .20, .29, .27)
temperature_trendline$bpd_rev <-   c(.20, .23, .28, .25, .20, .29, .20 , .29, .22, .20, .25, .24, .29,
                                     .26, .20, .29, .27, .23, .24, .22, .25,.12, .26, .23, .28, .22, .28)
temperature_trendline$nothigh_rev <- log(temperature_trendline$value, 200)
temperature_trendline$nothigh_acq <- log(temperature_trendline$value, 250)


data_long <- gather(temperature_trendline, condition, measurement, worry_acq:nothigh_acq, factor_key=TRUE)
Reversal <- data_long %>% dplyr::filter(!grepl("acq", condition))
Aquisition <- data_long %>% filter(!grepl("rev", condition))

tp1 <- ggplot(Reversal, aes(x = value,y = measurement, fill = condition)) + theme_minimal() + 
  scale_color_manual(labels = c("Elevated Worry", "Elevated BPD", "Not Elevated"), values = wes_palette("Moonrise2")[c(2,1,3)]) +
  geom_path(aes(colour = condition), size = 2) + 
  expand_limits(y=0) + theme(legend.position='none') +
    theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) + 
  ggtitle("Reversal Phase") + ylab("") + xlab("")
tp1

tp2 <- ggplot(Aquisition, aes(x = value, y = measurement, fill = condition)) + theme_minimal() + 
  scale_color_manual(labels = c("Elevated Worry", "Elevated BPD", "Not Elevated"), values = wes_palette("Moonrise2")[c(2,1,3)]) +
  geom_path(aes(colour = condition), size = 2)  +
   theme(legend.position='none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) + xlab("") + ylab("") +  ggtitle("Acquisiton Phase") 
tp2

#####Bar graph

bargraph_frame <- as.data.frame(c(.7, .5, .3))
bargraph_frame$Learning_Rate_rev <- c(.8, .55, .4)
bargraph_frame$group <- c("Elevated BPD", "Elevated Worry", "Low Elevation")
bargraph_frame$error <- c(.03,.03,.03)
colnames(bargraph_frame) <- c("Learning_Rate_Acq","Learning_Rate_Rev", "Group", "Error")

bg1 <- ggplot(bargraph_frame, aes(x = Learning_Rate_Acq, y = Group, fill=Group)) + 
  geom_bar(stat="identity", color="black") +
  scale_fill_manual(labels = c("Elevated BPD", "Not Elevated", "Elevated Worry"), values = wes_palette("Moonrise2")[c(1,3,2)]) + 
  geom_errorbar(aes(xmin=(Learning_Rate_Acq-Error), xmax=(Learning_Rate_Acq + Error)), width=.2,
                    position=position_dodge(.9)) + coord_flip() + theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) + ylab("") + xlab("") +
  theme(
    legend.position = c(.97, .97),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(3, 3, 3, 3)
  )
bg1


legend <- get_legend(
  # create some space to the left of the legend
  bg1 + theme(legend.box.margin = margin(0, 0, 0,30))
)


grid <- plot_grid(tp2, tp1, labels = c('', ''), label_size = 12, nrow = 2, ncol = 1)
grid<- plot_grid(grid, bg1 + theme(plot.margin = margin(.2, .2, .2, .2, "cm")),  labels = c('', ''), label_size = 12, nrow = 1, ncol = 2)
grid

grid <- plot_grid(grid, legend, rel_widths = c(3, .4), ncol = 2)



cowplot <- plot_grid(
  title, grid,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)



pdf("~/Desktop/H1_RL.pdf", width = 12, height = 5)
grid
dev.off()









# H2 ----------------------------------------------------------------------
#Reversal <- data_long %>% dplyr::filter(!grepl("acq", condition))
#Aquisition <- data_long %>% filter(!grepl("rev", condition))

trendline <- data.frame(c(1:100))
trendline$value <- trendline$c.1.100.
trendline$c.1.100. <- NULL

trendline$changepoint_pd <- c(rnorm(5, mean = 252, sd = 15), rnorm(15, mean = 97, sd = 15) ,
                              rnorm(13, mean = 164, sd = 15), 
                              rnorm(11, mean = 263, sd = 15), rnorm(17, mean = 110, sd = 15), 
                              rnorm(10, mean = 48, sd = 15),
                              rnorm(14, mean = 125, sd = 15), rnorm(15, mean = 146, sd = 15))
trendline$changepoint_worry <-  rnorm(100, mean = 150, sd = 35)



trendline$oddball_pd <-  c(rnorm(6, mean = 136, sd = 15), rnorm(4, mean = 260, sd = 10), 
                           rnorm(9, mean = 109, sd = 5), rnorm(12, mean = 130, sd = 15), 
                           rnorm(7, mean = 125, sd = 8),rnorm(6, mean = 105, sd = 8),  
                           rnorm(3, mean = 255, sd = 4),
                           rnorm(9, mean = 113, sd = 4), rnorm(15, mean = 130, sd = 10), 
                           rnorm(6, mean = 45, sd = 4), rnorm(5, mean = 138, sd = 10), 
                           rnorm(5, mean = 108, sd = 15), rnorm(6, mean = 125, sd = 15), 
                           rnorm(7, mean = 128, sd = 17))
trendline$oddball_pd[35:37] <- c(170,160, 165)


trendline$oddball_worry <-  rnorm(100, mean = 122, sd = 15)
trendline$oddball_obs <- rnorm(100, mean = 120, sd = 15)


trendline$changepoint_obs <-c(rnorm(5, mean = 252, sd = 15), rnorm(15, mean = 97, sd = 15) ,rnorm(13, mean = 164, sd = 15), 
                              rnorm(11, mean = 263, sd = 15), rnorm(17, mean = 110, sd = 15), rnorm(10, mean = 48, sd = 15),
                              rnorm(14, mean = 125, sd = 15), rnorm(15, mean = 146, sd = 15))
trendline$oddball_obs[5] <- 252
trendline$oddball_obs[20] <- 97
trendline$oddball_obs[33] <- 164
trendline$oddball_obs[44] <- 263
trendline$oddball_obs[61] <- 110
trendline$oddball_obs[71] <- 48
trendline$oddball_obs[85] <- 125

vec <- c(rnorm(5, mean = 260, sd = 10), rnorm(10, mean = 99, sd = 5) ,rnorm(13, mean = 160, sd = 15), 
         rnorm(11, mean = 255, sd = 8), rnorm(17, mean = 113, sd = 4), rnorm(10, mean = 48, sd = 15),
         rnorm(14, mean = 125, sd = 15), rnorm(15, mean = 146, sd = 15))


data_long <- gather(trendline, condition, measurement, changepoint_pd:changepoint_obs, factor_key=TRUE)
Oddball_pd <- data_long %>% dplyr::filter(!grepl("changepoint", condition)) %>% 
  dplyr::filter(!grepl("worry", condition))
Oddball_worry <- data_long %>% dplyr::filter(!grepl("changepoint", condition)) %>% 
  dplyr::filter(!grepl("pd", condition))
Changepoint_pd <- data_long %>% dplyr::filter(!grepl("oddball", condition)) %>% 
  dplyr::filter(!grepl("worry", condition))
Changepoint_worry <- data_long %>% dplyr::filter(!grepl("oddball", condition)) %>% 
  dplyr::filter(!grepl("pd", condition))
 



pd_o <- ggplot(trendline, aes(x = value, y = oddball_pd)) +
  geom_path(aes(y = oddball_pd, colour = "red"), size = 2)  + 
  geom_point(aes(y = oddball_obs, colour = "blue"), size = 2)  +
  scale_color_manual(labels = c("Participant Behavior", "Observations"), 
                     values = wes_palette("Moonrise2")[c(4,1)]) +
  expand_limits(y=0) + theme(legend.position='none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) + 
  ggtitle("") + ylab("") + xlab("")
pd_o



pd_c <- ggplot(trendline, aes(x = value, y = changepoint_pd)) +
  geom_path(aes(y = changepoint_pd, colour = "red"), size = 2)  + 
  geom_point(aes(y = changepoint_obs, fill = changepoint_obs), colour = "black", size = 2)  +
  expand_limits(y=0) + theme(legend.position='none') +
  scale_color_manual(labels = c("Participant Behavior", "Observations"), 
                     values = wes_palette("Moonrise2")[c(1,4)]) +
  expand_limits(y=0) + theme(legend.position='none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) + 
  ggtitle("") + ylab("") + xlab("")
pd_c


wr_o <- ggplot(trendline, aes(x = value, y = oddball_worry)) +
  geom_path(aes(y = oddball_worry, colour = "red"), size = 2)  + 
  geom_point(aes(y = oddball_obs, , colour = "blue"), size = 2)  +
  expand_limits(y=0) + theme(legend.position='none') +
  scale_color_manual(labels = c("Participant Behavior", "Observations"), 
                     values = wes_palette("Moonrise2")[c(4, 2)]) +
  expand_limits(y=0) + theme(legend.position='none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) + 
  ggtitle("Oddball Condition") + ylab("") + xlab("")

wr_o

wr_c <- ggplot(trendline, aes(x = value, y = changepoint_worry)) +
  geom_path(aes(y = changepoint_worry, colour = "black"), size = 2)  + 
  geom_point(aes(y = changepoint_obs,, colour = "blue"), size = 2) +
  expand_limits(y=0) + theme(legend.position='none') +
  scale_color_manual(labels = c("Participant Behavior", "Observations"), 
                     values = wes_palette("Moonrise2")[c(2,4)]) +
  expand_limits(y=0) + theme(legend.position='none') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank()) + 
  ggtitle("Change-Point Condiiton") + ylab("") + xlab("")
wr_c



title <- ggdraw() + 
  draw_label(
    "Simulated Participant Behavior in a Predictive Inference Task",
    fontface = 'bold',
    x = 0,
    hjust = -1.15
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 12)
  )


legend <- get_legend(
  # create some space to the left of the legend
  bg1 + theme(legend.box.margin = margin(0, 0, 0,30))
)


grid <- plot_grid(wr_c, wr_o, pd_c, pd_o, labels = c('', '', '', ''), label_size = 12, nrow = 2, ncol = 2)
#grid<- plot_grid(grid, bg1 + theme(plot.margin = margin(.2, .2, .2, .2, "cm")),  labels = c('', ''), label_size = 12, nrow = 1, ncol = 2)
grid

grid <- plot_grid(grid, legend, rel_widths = c(3, .4), ncol = 2)




pdf("~/Desktop/H2_RL.pdf", width = 12, height = 5)
grid
dev.off()




bpd <- c(rnorm(100, mean = 40, sd = 15))

simulate_anxiety <- c(rnorm(100, mean = 28, sd = 10))
                      
                  
data_frame <- cbind(bpd, simulate_anxiety)

plot(data_frame)



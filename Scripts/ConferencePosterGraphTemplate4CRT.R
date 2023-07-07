# This script contains code for easily adaptable plots related to a four-choice reaction time task.
# It also contains shows how to export your graphs at a standard size/resolution, which is great
# for things like conference posters.


#library(data.table) # No used functions found
library(plyr) # here ddply mutate
#library(zoo) # No used functions found
#library(tidyverse) # No used functions found
library(tidyr) # %>% gather
library(rstatix) # %>% gather mutate
#library(interactions) # No used functions found
#library(lme4) # No used functions found
library(ggplot2) # ggplot aes geom_line theme_bw geom_point geom_errorbar labs scale_color_manual scale_linetype_manual scale_x_discrete
library(ggpubr) # %>% mutate ggerrorplot
library(here) # here
library(rio) # import
library(ggthemes)

# Exp 1 = RT by Bin & Condition plot

# Import data
exp1Data <- import(here("data","Experiment1DataGoalChoiceRT.csv"),setclass="tbl_df")

# Set experimental condition as a factor
exp1Data$condition <- as.factor(exp1Data$condition)

# The design is repeated measures, so here we're taking those 5 variables and gathering them so that
# bin (1-5) is one variable, and meanRT (mean response time) is the other
exp1Gather <- exp1Data %>%
  gather(key = "Bin", value = "BinRT", meanRank1, meanRank2, meanRank3, meanRank4, meanRank5)
exp1Gather$Bin = as.factor(exp1Gather$Bin)

#exp1 <- exp1Gather %>%
#  pivot_wider(names_from = Bin, values_from = BinRT)

# Taking the data set and calculating final N, mean, SD, and SE for mean response time
tg <- ddply(exp1Gather, c("condition", "Bin"), summarise,
            N=length(!is.na(BinRT)),
            mean=mean(BinRT),
            sd=sd(BinRT),
            se=sd/sqrt(N)) 
tg$Bin <- as.numeric(tg$Bin)

# tiff helps us create graphs with a standard size and resolution to be used on a poster presentation
tiff("plote1a", units="in", width=6, height=3, res=450)
# insert ggplot code

plot <- ggplot(data=tg, aes(x=Bin, y=mean, color= condition, linetype=condition)) +
  geom_point() +
  geom_line(aes(linetype=condition, color=condition)) +
  theme_bw() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Bin", y = "Response Time (in seconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name= "Condition", labels = c("Control", "Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", "Goal"), values=c(4,1))
plot

dev.off()
# Task-Unrelated Thoughts by Condition
exp1Data %>%
  mutate(TUTprop = NA)
exp1Data$TUTprop <-  (exp1Data$sumTUT)/10

tiff("plote1b", units="in", width=5, height=4, res=450)
ggerrorplot(exp1Data, x = "condition", y = "TUTprop", 
            desc_stat = "mean_se", 
            color = "condition", 
            panel.labs = list(condition = c("Control", "Goal"))) +
  labs(x = "Condition", y = "Proportion of Task-Unrelated Thoughts",
       color="Condition") +
  scale_color_manual(labels = c("Control", "Goal"), values = c("black", "dark green")) +
  scale_x_discrete(labels=c("control" = "Control", "goal" = "Goal"))
dev.off()
# EXPERIMENT 2

exp2Data <- import(here("data","Experiment2DataHOTonline.csv"),setclass="tbl_df")

# Exp 1 = RT by Bin & Condition plot
exp2Data$condition <- as.factor(exp2Data$condition)
exp2Gather <- exp2Data %>%
  gather(key = "Bin", value = "BinRT", meanRank1, meanRank2, meanRank3, meanRank4, meanRank5)
exp2Gather$Bin = as.factor(exp2Gather$Bin)

exp2 <- exp2Gather %>%
  pivot_wider(names_from = Bin, values_from = BinRT)

exp2RankSum <- ddply(exp2Gather, c("condition", "Bin"), summarise,
                     N=length(!is.na(BinRT)),
                     mean=mean(BinRT),
                     sd=sd(BinRT),
                     se=sd/sqrt(N))  
exp2RankSum$Bin <- as.numeric(exp2RankSum$Bin)

tiff("plote2a", units="in", width=6, height=3, res=450)
exp2Plot <- ggplot(data=exp2RankSum, aes(x=Bin, y=mean, color= condition, linetype = condition)) +
  geom_line() +
  theme_bw() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Bin", y = "Response Time (in seconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name= "Condition", labels = c("Control", " HOT Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", " HOT Goal"), values=c(4,1)) 
exp2Plot
dev.off()
# Task-Unrelated Thoughts by Condition
exp2Data %>%
  mutate(TUTprop = NA)
exp2Data$TUTprop <-  (exp2Data$sumTUT)/10

tiff("plote2c", units="in", width=5, height=4, res=450)
ggerrorplot(exp2Data, x = "condition", y = "TUTprop", 
            desc_stat = "mean_se", 
            color = "condition")+
  labs(x = "Condition", y = "Proportion of Task-Unrelated Thoughts",
       color="Condition") +
  scale_color_manual(labels = c("Control", "HOT Goal"), values = c("black", "dark green")) +
  scale_x_discrete(labels=c("control" = "Control", "hotgoal" = "HOT Goal"))
dev.off()
# Response Time by Time Block
exp2Data$condition <- as.factor(exp2Data$condition)
exp2Gather <- exp2Data %>%
  gather(key = "Block", value = "BlockRT", gbTime1, gbTime2, gbTime3)
exp2Gather$Block = as.factor(exp2Gather$Block)

exp2Gather[,81] <- ifelse(exp2Gather[,81] == "gbTime1", 1, ifelse(exp2Gather[,81] == "gbTime2", 2, ifelse(exp2Gather[,81] == "gbTime3", 3, 999)))

exp2BlockSumm <- ddply(exp2Gather, c("condition", "Block"), summarise,
                       N=length(!is.na(BlockRT)),
                       mean=mean(BlockRT),
                       sd=sd(BlockRT),
                       se=sd/sqrt(N)) 
exp2BlockSumm$Block <- as.numeric(exp2BlockSumm$Block)

tiff("plote2b", units="in", width=6, height=3, res=450)
plot2 <- ggplot(data=exp2BlockSumm, aes(x=Block, y=mean, color= condition, linetype = condition)) +
  geom_line(aes(linetype=condition, color=condition)) +
  theme_bw() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Block", y = "Response Time (in seconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name="Condition", labels = c("Control", "HOT Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", "HOT Goal"), values=c(4,1)) 
plot2

plot2 + scale_x_discrete(name ="Block", 
                         limits=c("1","2","3"))

dev.off()
# EXPERIMENT 3

exp3Data <- import(here("data","Experiment3Data _HOTinPerson.sav"),setclass="tbl_df")
# RT by Bin & Condition plot

exp3Data$condition <- as.factor(exp3Data$condition)
exp3Gather <- exp3Data %>%
  gather(key = "Bin", value = "BinRT", meanRank1, meanRank2, meanRank3, meanRank4, meanRank5)
exp3Gather$Bin = as.factor(exp3Gather$Bin)

exp3Gather[,19] <- ifelse(exp3Gather[,19] == "meanRank1", 1, 
                          ifelse(exp3Gather[,19] == "meanRank2", 2, 
                                 ifelse(exp3Gather[,19] == "meanRank3", 3, 
                                        ifelse(exp3Gather[,19] == "meanRank4", 4, 
                                               ifelse(exp3Gather[,19] == "meanRank5", 5, 999)))))

exp3RankSum <- ddply(exp3Gather, c("condition", "Bin"), summarise,
                     N=length(!is.na(BinRT)),
                     mean=mean(BinRT),
                     sd=sd(BinRT),
                     se=sd/sqrt(N))        

exp3RankSum$Bin <- as.numeric(exp3RankSum$Bin)
exp3RankSum$mean <- exp3RankSum$mean/1000
exp3RankSum$se <- exp3RankSum$se/1000

tiff("plote3a", units="in", width=6, height=3, res=450)
exp3Plot <- ggplot(data=exp3RankSum, aes(x=Bin, y=mean, color= condition, linetype = condition)) +
  geom_line() +
  theme_bw() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Bin", y = "Response Time (in seconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name= "Condition", labels = c("Control", " HOT Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", " HOT Goal"), values=c(4,1)) 
exp3Plot
dev.off()

# Task-Unrelated Thoughts by Condition
exp3Data %>%
  mutate(TUTprop = NA)
exp3Data$TUTprop <-  (exp3Data$sumTUT)/10

tiff("plote3c", units="in", width=5, height=4, res=450)
ggerrorplot(exp3Data, x = "condition", y = "TUTprop", 
            desc_stat = "mean_se", 
            color = "condition")+
  labs(x = "Condition", y = "Proportion of Task-Unrelated Thoughts",
       color="Condition") +
  scale_color_manual(labels = c("Control", "HOT Goal"), values = c("black", "darkgreen")) +
  scale_x_discrete(labels=c("control" = "Control", "goal" = "HOT Goal"))
dev.off()

# Response Time by Time Block
exp3Data$condition <- as.factor(exp3Data$condition)
exp3Gather <- exp3Data %>%
  gather(key = "Block", value = "BlockRT", rtBlock1, rtBlock2, rtBlock3)
exp3Gather$Block = as.factor(exp3Gather$Block)

exp3Gather[,22] <- ifelse(exp3Gather[,22] == "rtBlock1", 1, ifelse(exp3Gather[,22] == "rtBlock2", 2, ifelse(exp3Gather[,22] == "rtBlock3", 3, 999)))

exp3BlockSumm <- ddply(exp3Gather, c("condition", "Block"), summarise,
                       N=length(!is.na(BlockRT)),
                       mean=mean(BlockRT),
                       sd=sd(BlockRT),
                       se=sd/sqrt(N)) 
exp3BlockSumm$Block <- as.numeric(exp3BlockSumm$Block)
exp3BlockSumm$mean <- exp3BlockSumm$mean/1000
exp3BlockSumm$se <- exp3BlockSumm$se/1000

tiff("plote3b", units="in", width=6, height=3, res=450)
plot2 <- ggplot(data=exp3BlockSumm, aes(x=Block, y=mean, color= condition, linetype = condition)) +
  geom_line(aes(linetype=condition, color=condition)) +
  theme_bw() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  labs(x = "Block", y = "Response Time (in seconds)",
       color="Condition", linetype="Condition") +
  scale_color_manual(name="Condition", labels = c("Control", "HOT Goal"), values = c("black", "dark green")) +
  scale_linetype_manual(name="Condition", labels = c("Control", "HOT Goal"), values=c(4,1)) 
plot2

plot2 + scale_x_discrete(name ="Block", 
                         limits=c("1","2","3"))
dev.off()


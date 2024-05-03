
library(tidyverse)
library(ggplot2)
library(plotrix)
library(ggpubr)
library(BayesFactor)
library(car)
options(scipen = 999)
install.packages("bayestestR")
library(bayestestR)


data <- read.csv("data_young_old_pd.csv")
data <- na.omit(data) 
data$group <- as.factor(data$group)

counts <- data %>%
  group_by(group) %>%
  summarize(count = n_distinct(subj))

# trial wise 
data_mean_pg <- data %>% 
  group_by(trial,group) %>%
  summarize(mean_logmIKI = mean(logmIKI),
            se_logmIKI = std.error(logmIKI),
            mean_mIKI = mean(exp(logmIKI)),
            se_mIKI = std.error(exp(logmIKI)))


ggplot(data_mean_pg, aes(x = trial, y = mean_logmIKI, color = group)) +
  geom_line(size=0.2) +
  geom_errorbar(aes(ymin = mean_logmIKI - se_logmIKI, ymax = mean_logmIKI + se_logmIKI), 
                width=0.5,size=0.3) +
  labs(title="Time Course of logmIKI", x = "Trial", y = "Mean logmIKI", color = "Group") +
  scale_color_manual(values = c("black","blue","red"), 
                     labels=c('Older Healthy Adults', 'Young Healthy Adults', "Parkinson's Disease"))

ggplot(data_mean_pg, aes(x = trial, y = mean_mIKI, color = group)) +
  geom_line(size=0.2) +
  geom_errorbar(aes(ymin = mean_mIKI - se_mIKI, ymax = mean_mIKI + se_mIKI), 
                width=0.5,size=0.3) +
  labs(title="Time Course of mIKI", x = "Trial", y = "Mean mIKI", color = "Group") +
  scale_color_manual(values = c("black","blue","red"), 
                     labels=c('Older Healthy Adults', 'Young Healthy Adults', "Parkinson's Disease"))

# trial average
data_mean_pp <- data %>% 
  group_by(group,subj) %>%
  summarize(mean_logmIKI2 = mean(logmIKI),
            mean_mIKI2 = mean(exp(logmIKI)))

means <- data_mean_pp %>%
  group_by(group) %>%
  summarize(meanlogmIKI = mean(mean_logmIKI2),
            meanmIKI = mean(mean_mIKI2))


one <- ggplot(data_mean_pp, aes(x=mean_logmIKI2)) + 
  geom_histogram() +
  facet_wrap(~group, scales="free") +
  ggtitle("Histogram of trial-average logmIKI by Group")
      
two <- ggplot(data_mean_pp, aes(x=mean_mIKI2)) + 
  geom_histogram() +
  facet_wrap(~group, scales="free") +
  ggtitle("Histogram of trial-average mIKI by Group")

three <- ggplot(data_mean_pg, aes(x=mean_logmIKI)) + 
  geom_histogram() +
  facet_wrap(~group, scales="free") +
  ggtitle("Histogram of trial-wise logmIKI by Group")


four <- ggplot(data_mean_pg, aes(x=mean_mIKI)) + 
  geom_histogram() +
  facet_wrap(~group, scales="free") +
  ggtitle("Histogram of trial-wise mIKI by Group")

ggarrange(one, two + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

resultlog <- leveneTest(mean_logmIKI ~ group, data_mean_pg)
result <- leveneTest(mean_mIKI ~ group, data_mean_pg)

resultlog_pp <- leveneTest(mean_logmIKI2 ~ group, data_mean_pp)
result_pp <- leveneTest(mean_mIKI2 ~ group, data_mean_pp)

ggplot(data_mean_pg, aes(x = group, y = mean_mIKI)) +
  geom_boxplot() +
  labs(x = "Group", y = "Mean LogmIKI") +
  ggtitle("Boxplot of Mean LogmIKI by Group")


bf_data <- data_mean_pp %>%
  select(subj,group,mean_logmIKI2)

bf_data$group <- as.factor(bf_data$group)


bf_data$mean_logmIKI2 <- as.numeric(bf_data$mean_logmIKI2)
bf_data$group <- as.factor(bf_data$group)

anova <- aov(mean_logmIKI2 ~ group, data = bf_data)
summary <- summary(anova)

bf_anova <- anovaBF(mean_logmIKI2 ~ group, data = bf_data, whichRandom = "none")


ttest <- pairwise.t.test(bf_data$mean_logmIKI2, bf_data$group, p.adjust.method = "bonferroni")



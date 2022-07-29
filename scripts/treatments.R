# load packages
library(arrow)
library(tidyverse)    # basic functions
library(cowplot)      # arrange multiple plots in one figure
library(Hmisc)
library(caret)
library(e1071)
library(ROCR)
library(ggpubr)
library(rstatix)      # conduct Wilcoxon signed rank test 
library(broom)
library(readxl)       # read excel sheet


# load data
data <- read_feather(snakemake@input[["data"]])


############################################################################################################################################################################

################################################# T R E A T M E N T ##############################################################################################################

############################################################################################################################################################################

# select relevant variables
df_treatment <- data %>%
  dplyr::select(c(v_16, c_0001, v_92, v_96, v_100))

df_treatment$v_92[df_treatment$v_92 == "-77"] <- 0 
df_treatment$v_96[df_treatment$v_96 == "-77"] <- 0
df_treatment$v_100[df_treatment$v_100 == "-77"] <- 0

# compute post-treatment score
df_treatment <- df_treatment %>% 
  rowwise() %>%
  mutate(t_post = sum(v_92, v_96, v_100, na.rm = TRUE))

# tidy up data frame
df_treatment <- df_treatment %>% 
  mutate(treatment = c_0001)

df_treatment$treatment <- factor(df_treatment$treatment, ordered = FALSE)

df_treatment$c_0001 <- NULL
df_treatment$v_92 <- NULL
df_treatment$v_96 <- NULL
df_treatment$v_100 <- NULL

df_treatment <- df_treatment %>% 
  mutate(t_pre = v_16)

df_treatment$v_16 <- NULL

### compute paired-sample Wilcoxon test 
### Q: Is there a significant difference in the two means before & after treatment?

# transform to long data: 
df_treatment_l <- df_treatment %>% 
  gather(key = "time", value = "score", t_pre, t_post)

# assumptions: 
# differences between paired samples should be distributed symmetrically around the median 

df_treatment_diff <- df_treatment %>%
  mutate(differences = t_post - t_pre)

# plot
gghistogram(df_treatment_diff, x = "differences", y = "..density..", 
            fill = "gray", bins = 5, add.density = TRUE)
# visual eval: differences are approximately symmetrical 

# Q: is there a statistically significant change in acceptance score? 
# make 3 data sets to compare 3 treatments 

df_treatment_l1 <- df_treatment_l %>% 
  filter(treatment == "1")
df_treatment_l2 <- df_treatment_l %>% 
  filter(treatment == "2")
df_treatment_l3 <- df_treatment_l %>% 
  filter(treatment == "3")

#Wilcoxon signed rank test
stat.test1 <- df_treatment_l1 %>% 
  rstatix::wilcox_test(score ~ time, paired = TRUE) %>% 
  add_significance()
stat.test1

stat.test2 <- df_treatment_l2 %>% 
  rstatix::wilcox_test(score ~ time, paired = TRUE) %>% 
  add_significance()
stat.test2

stat.test3 <- df_treatment_l3 %>% 
  rstatix::wilcox_test(score ~ time, paired = TRUE) %>% 
  add_significance()
stat.test3

# results: 
# A: not statistically significant
# B: p<01 (**)
# C: p<01 (**)

# Q: effect size 
df_treatment_l %>% 
  wilcox_effsize(score ~ time, paired = TRUE)
df_treatment_l1 %>% 
  wilcox_effsize(score ~ time, paired = TRUE)
df_treatment_l2 %>% 
  wilcox_effsize(score ~ time, paired = TRUE)
df_treatment_l3 %>% 
  wilcox_effsize(score ~ time, paired = TRUE)
# a small effect size is detected; r = 0.105, r=0.055, r = 0.114, r = 0.143

# interpretation results: 
# acceptance score before treatment is significantly different from score after treatment 
# with p-value <0.001 and effect size r = .....


### differences in Likert scores (for visualisation) 
str(df_treatment_diff)

### turn around scales so that increase in value is equivalent to increase in acceptance
# FIXME lines 124--145 are broken and therefore commented
#df_treatment_diff <- df_treatment_diff %>% 
#  mutate(diff = differences/-1)

#df_treatment_diff$diff <- factor(df_treatment_diff$diff)

#df_treatment_diff <- df_treatment_diff %>% 
#  mutate(treatment = ifelse(df_treatment_diff$treatment == "1", "A) Change is inevitable", 
#                            ifelse(df_treatment_diff$treatment == "2", "B) EVs are better than you think", "C) ICEVs are worse than you think")))

#p_treat1 <- ggplot(df_treatment_diff, aes(x = treatment)) +
#  geom_bar(aes(y= (..count..), fill = factor(diff, levels = c("4", "3", "2", "1", "0", "-1", "-2", "-3", "-4"), labels = c("+4", "+3", "+2", "+1", "none", "-1", "-2", "-3", "-4"))), width = .5) +
#  scale_fill_manual("Change in \n Likert score", values = c("lightsteelblue1", "lightsteelblue2", "lightsteelblue3", "lightsteelblue4", "gray25", 
#                                                            "coral4", "coral3", "coral2", "coral")) +
#  theme_light() +
#  labs(x = "", y = "N") +
#  theme(axis.text = element_text(size = 22)) +
#  theme(axis.text = element_text(size = 22)) +
#  theme(axis.title.y = element_text(vjust = 0.9, angle = 0, size = 15)) + 
#  theme(legend.title = element_text(size = 20)) +
#  theme(legend.position = "none")

#p_treat1


############### treatment effect of EC proposal - follow up survey data

# import data of follow-up survey
df2 <- read.csv(snakemake@input[["follow_up_data"]], 
                header = TRUE, 
                sep = ";")

# recode answers so that 1 = treatment (i.e. have heard of EC proposal), and 0= no treatment (haven't heard, or aren't sure), no answer = NA
df2 <- df2 %>% 
  mutate(T_EC = ifelse(df2$v_115 == "1", "1", 
                       ifelse(df2$v_115 == "0", "NA", "0")))

# recode answers so that 1 = treatment (think that CC & floods are connected), and 0= no treatment (don't think so, or aren't sure)
df2 <- df2 %>% 
  mutate(T_CC = ifelse(df2$v_116 == "1", "1", 
                       ifelse(df2$v_116 == "0", "NA", "0")))

# v_16 is acceptance (framed exactly the same as in first study) - turning around scales so that higher number -> higher acceptance
df2 <- df2 %>% 
  mutate(t2a_post = ifelse(df2$v_16 == "1", "5", 
                          ifelse(df2$v_16 == "2", "4", 
                                 ifelse(df2$v_16 == "3", "3", 
                                        ifelse(df2$v_16 == "4", "2", 
                                               ifelse(df2$v_16 == "5", "1", NA))))))

# the same for perceived inevitability...
df2 <- df2 %>% 
  mutate(t2i_post = ifelse(df2$v_17 == "1", "5", 
                          ifelse(df2$v_17 == "2", "4", 
                                 ifelse(df2$v_17 == "3", "3", 
                                        ifelse(df2$v_17 == "4", "2", 
                                               ifelse(df2$v_17 == "5", "1", NA))))))

df2 <- df2 %>% 
  mutate(tic_2 = p_0001)

# select only relevant variables 
df2 <- df2 %>% 
  dplyr::select(c(tic_2, t2a_post, t2i_post, v_17, T_EC, T_CC))


### import excel file that matches IDs of first and second survey
df3 <- read_excel(snakemake@input[["tic_matched3"]])

df3 <- df3 %>% 
  filter(tic_2 != "NA")

df2 <- merge(df2, df3, by = "tic_2", all.x = TRUE, all.y = TRUE) 

df1 <- data %>% 
  dplyr::select(c(p_0001, v_16, v_17))

df1 <- df1 %>% 
  mutate(t2a_pre = ifelse(df1$v_16 == "1", "5", 
                           ifelse(df1$v_16 == "2", "4", 
                                  ifelse(df1$v_16 == "3", "3", 
                                         ifelse(df1$v_16 == "4", "2", 
                                                ifelse(df1$v_16 == "5", "1", NA))))))

# the same for perceived inevitability...
df1 <- df1 %>% 
  mutate(t2i_pre = ifelse(df1$v_17 == "1", "5", 
                           ifelse(df1$v_17 == "2", "4", 
                                  ifelse(df1$v_17 == "3", "3", 
                                         ifelse(df1$v_17 == "4", "2", 
                                                ifelse(df1$v_17 == "5", "1", NA))))))
# tidy up data frame
df1 <- df1 %>% 
  mutate(tic_1 = p_0001)

df2 <- merge(df1, df2, by = "tic_1")

df2$p_0001 <- NULL
df2$v_16 <- NULL
df2$v_17.x <- NULL
df2$v_17.y <- NULL

df2$t2a_pre <- as.numeric(df2$t2a_pre)
df2$t2i_pre <- as.numeric(df2$t2i_pre)
df2$t2a_post <- as.numeric(df2$t2a_post)
df2$t2i_post <- as.numeric(df2$t2i_post)

# calculate differences between pre and post score for acceptance & inevitability 
df2 <- df2 %>% 
  mutate(diff_a = t2a_post - t2a_pre)

df2 <- df2 %>% 
  mutate(diff_i = t2i_post - t2i_pre)

df2 <- df2 %>% 
  filter(T_EC != "NA")

#plot
p_treat2 <- ggplot(df2, aes(x = T_EC)) +
  geom_bar(aes(y= (..count..), fill = factor(diff_a, levels = c("4", "3", "2", "1", "0", "-1", "-2", "-3", "-4"), labels = c("+4", "+3", "+2", "+1", "none", "-1", "-2", "-3", "-4"))), width = .5) +
  scale_fill_manual("Change in \n Likert score", values = c("lightsteelblue1", "lightsteelblue2", "lightsteelblue3", "lightsteelblue4", "gray25", 
                                                            "coral4", "coral3", "coral2", "coral")) +
  theme_light() +
  labs(x = "", y = "N") +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.text = element_text(size = 22)) +
  theme(legend.text = element_text(size = 16)) +
  theme(axis.title.y = element_text(vjust = 0.9, angle = 0, size = 15)) + 
  theme(legend.title = element_text(size = 18))

ggsave(snakemake@output[["plot"]], p_treat2)


df2 %>% 
  group_by(T_EC) %>% count(diff_a)

# Wilcoxon test 
### Q: Is there a significant difference in the two means before & after treatment?

# transform to long data: 
df2_l <- df2 %>% 
  gather(key = "time", value = "score", t2a_pre, t2a_post)


### make 2 data sets to compare treatment w/ control
df2_l1 <- df2_l %>% 
  filter(T_EC == "1")
df2_l0 <- df2_l %>% 
  filter(T_EC == "0")

# Q: significance
stat.test1 <- df2_l1 %>% 
  rstatix::wilcox_test(score ~ time, paired = TRUE) %>% 
  add_significance()
stat.test1

stat.test0 <- df2_l0 %>% 
  rstatix::wilcox_test(score ~ time, paired = TRUE) %>% 
  add_significance()
stat.test0
# results: no statistically significant effect in either group (p=0.19 & p=0.87)

# Q: effect size 
df2_l1 %>% 
  wilcox_effsize(score ~ time, paired = TRUE)
df2_l0 %>% 
  wilcox_effsize(score ~ time, paired = TRUE)
# results: tiny effect size (0.0390 for treatment & 0.00699 for non-treatment), but not significant anyway


### combine two plots on treatment effects 

#plot_grid(p_treat1, NULL, p_treat2, # FIXME not possible because of problem in line 124
#          ncol = 3, nrow = 1, 
#          rel_widths = c(1, .08, 1),
#          labels = c("a.", "", "b."), 
#          label_size =20)

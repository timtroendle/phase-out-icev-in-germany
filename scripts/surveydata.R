### load packages

library(tidyverse)
library(circlize)
library(RColorBrewer)
library(networkD3)
library(likert)
library(hrbrthemes)
library(psych)
library(data.table)
library(pspearman)
library(rgdal)
library(sp)
library(rgeos)
library(plotrix)
library(ggpubr)
library(readxl)
library(cowplot)




### import data of first survey (June 2021)

df <- read.csv("C://Users//jhoppe//polybox//Research//Scripts, summaries, drafts, proposal//Proposal 4, Public opinion on phasing out ICEs//Survey Data//210602SurveyData.csv", 
               header = TRUE, 
               sep = ";")


#######################################################################################################################################################################

### WILLINGNESS TO ABANDON CAR / TO SWITCH TO AN EV 


# make sankey diagram of willingness to abandon car / switch to EV 
# "yes" / "maybe"/ "no" regards willing to give up car 
# "Yes" / "Maybe" / "No" regards willingness to switch to EV 
# cat1/cat2: subgroups

### everyone (cat1), whether or not they would be willing to give up car (cat2)
sankey1 <- df %>% 
  count(v_72 == 2, v_72 == 1 & v_79 == 0, v_79 == 1) ### everyone
sankey1$cat1 <- c("all", "all", "all")
sankey1$cat2 <- c("no", "maybe", "yes")

### all maybe willing to give up car (cat1), whether or not they would be willing to change to EV (cat2)
sankey2 <- df %>% 
  count(v_72 == 1 & v_79 == 0 & v_82 == 2, v_72 == 1 & v_79 == 0 & v_82 == 1 & v_90 == 0, v_72 == 1 & v_79 == 0 & v_90 == 1)
sankey2$cat1 <- c("NA", "maybe", "maybe", "maybe")
sankey2$cat2 <- c("NA", "No", "Maybe", "Yes")

### all not willing to give up car (cat1), whether or not they would be willing to change to EV (cat2)
sankey3 <- df %>% 
  count(v_79 == 1 & v_82 == 2, v_79 == 1 & v_82 == 1 & v_90 == 0, v_79 == 1 & v_90 == 1)
sankey3$cat1 <- c("NA", "no", "no", "no")
sankey3$cat2 <- c("NA", "No", "Maybe", "Yes")


sankey <- merge(sankey1, sankey2, all.y = TRUE, all.x = TRUE)
sankey <- merge(sankey, sankey3, all.y = TRUE, all.x = TRUE)

sankey <- sankey[1:3] ### keep only relevant columns

colnames(sankey) <- c("value", "cat1", "cat2")

sankey <- sankey %>% 
  group_by(value, cat1, cat2) %>%
  filter(cat1 != "NA") %>% 
  filter(cat2 != "NA")

# Create node data frame 
nodes <- data.frame(name = c(as.character(sankey$cat1),
                         as.character(sankey$cat2)) %>% unique())


sankey$IDcat1 <- match(sankey$cat1, nodes$name)-1
sankey$IDcat2 <- match(sankey$cat2, nodes$name)-1

my_colour <- 'd3.scaleOrdinal() .domain(["all", "yes", "maybe", "no", "Yes", "Maybe", "No"]) .range([
"gray", "#00A7A2", "#32B9A3", "#7FC9A4", "#9AD0A3", "#CFE3A2", "#E6EAA2"])'


# make Sankey network 

sankeyNetwork(Links = sankey, Nodes = nodes, 
              Source = "IDcat1", Target = "IDcat2", 
              Value = "value", NodeID = "name",  
              sinksRight = FALSE, nodeWidth = 40, fontSize = 13, nodePadding = 20, 
              colourScale = my_colour)


### optional: display percentages 

###

### WOULD ABANDON CAR IF... (bar chart)

df_strfct <- df %>%
  filter(v_72 == 1) %>% 
  dplyr::select(c(v_72, v_73, v_74, v_75, v_76, v_77, v_80, v_79))

df_strfct_sum <- data.frame(colSums(df_strfct))
colnames(df_strfct_sum) <- c("Count_ICE_ab")

df_strfct_sum$reason <- c(
  v_72 = "I find it hard to do without a car",
  v_73 = "...if public transport was better developed, cheaper, or more reliable", 
  v_74 = "...if there were more car sharing options in my region", 
  v_75 = "...if walking and cycling paths were better developed and safer", 
  v_76 = "...if I would get from A to B equally fast and conveniently as w/ car",
  v_77 = "...if my living conditions (health, family, job) were different", 
  v_80 = "...other", 
  v_79 = "I couldn't do w/o car")

df_strfct_sum$Perc <- df_strfct_sum$Count_ICE_ab / (1198-423) *100

df_strfct_sum <- cbind(Reason = rownames(df_strfct_sum), df_strfct_sum)
rownames(df_strfct_sum) <- 1:nrow(df_strfct_sum)

df_strfct_sum2 <- df_strfct_sum %>% 
  filter(Reason %in% c("v_73", "v_74", "v_75", "v_76", "v_77", "v_80"))

ggplot(df_strfct_sum2, aes(x = reorder(reason, Perc), y = Perc)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip() +
  labs(title = "I would be willing to give up my car...", 
       x = "", 
       y = "Percent") +
  theme_ipsum()


###

### WOULD SWITCH TO EV IF...(bar chart)

df_strEV <- df %>%
  filter(v_82 == 1) %>% 
  dplyr::select(c(v_82, v_83, v_84, v_85, v_86, v_88, v_90))

df_strEV_sum <- data.frame(colSums(df_strEV))
colnames(df_strEV_sum) <- c("Count_EV_swi")

df_strEV_sum$reason <- c(
  v_82 = "I'd find it difficult to switch to an EV",
  v_83 = "...if the range of EVs was better", 
  v_84 = "...if EVs were cheaper", 
  v_85 = "...if the charging infrastructure was better developed", 
  v_86 = "...if negative environmental effects of EVs were smaller",
  v_88 = "...other", 
  v_90 = "I couldn't switch to an EV")

df_strEV_sum$Perc <- df_strEV_sum$Count_EV_swi / (754-157) *100

df_strEV_sum <- cbind(Reason = rownames(df_strEV_sum), df_strEV_sum)
rownames(df_strEV_sum) <- 1:nrow(df_strEV_sum)

df_strEV_sum2 <- df_strEV_sum %>% 
  filter(Reason %in% c("v_83", "v_84", "v_85", "v_86", "v_88"))

ggplot(df_strEV_sum2, aes(x = reorder(reason, Perc), y = Perc)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip() +
  labs(title = "I would be willing to switch to an EV...", 
       x = "", 
       y = "Percent") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_ipsum()



###################################################################################################################################################################


### SOCIO-DEMOGRAPHICS (Car-related factors)

# make variable containing number of cars numeric to aggregate later

df$v_102 <- as.numeric(df$v_102) # no of diesel
df$v_104 <- as.numeric(df$v_104) # no of petrol
df$v_106 <- as.numeric(df$v_106) # no of hybrid
df$v_108 <- as.numeric(df$v_108) # no of EVs


### tidy up data 
# set both answer "0" (no answer) and "1" (zero no of respective car) as having zero cars per type (respondents didn't set always set answer to zero, 
# ...but sometimes set no answer)
# assumption: they have zero cars of that type if they skipped the answer option (almost none skipped question as a whole)


df <- df %>% 
  mutate(v_diesel = ifelse(df$v_102 %in% c("0", "1"), "0", 
                           ifelse(df$v_102 == "2", "1", 
                                  ifelse(df$v_102 == "3", "2", 
                                         ifelse(df$v_102 == "4", "3", 
                                                ifelse(df$v_102 == "5", "4", "5"))))))

df <- df %>% 
  mutate(v_petrol = ifelse(df$v_104 %in% c("0", "1"), "0", 
                           ifelse(df$v_104 == "2", "1", 
                                  ifelse(df$v_104 == "3", "2", 
                                         ifelse(df$v_104 == "4", "3", 
                                                ifelse(df$v_104 == "5", "4", "5"))))))

df <- df %>% 
  mutate(v_hybrid = ifelse(df$v_106 %in% c("0", "1"), "0", 
                           ifelse(df$v_106 == "2", "1", 
                                  ifelse(df$v_106 == "3", "2", 
                                         ifelse(df$v_106 == "4", "3", 
                                                ifelse(df$v_106 == "5", "4", "5"))))))

df <- df %>% 
  mutate(v_EV = ifelse(df$v_108 %in% c("0", "1"), "0", 
                           ifelse(df$v_108 == "2", "1", 
                                  ifelse(df$v_108 == "3", "2", 
                                         ifelse(df$v_108 == "4", "3", 
                                                ifelse(df$v_108 == "5", "4", "5"))))))



df$v_diesel <- as.numeric(df$v_diesel)
df$v_petrol <- as.numeric(df$v_petrol)
df$v_hybrid <- as.numeric(df$v_hybrid)
df$v_EV <- as.numeric(df$v_EV)

### calculate total number of cars
df$v_cars <- df$v_diesel + df$v_petrol + df$v_hybrid + df$v_EV

### calculate total number of ICEs
df$v_ICE <- df$v_diesel + df$v_petrol + df$v_hybrid 

### calculate total number of pure ICEs (petrol & diesel)
df$v_pureICE <- df$v_diesel + df$v_petrol


### overview of all car ownership variables: 
df_cars <- df %>%
  dplyr::select(c(lfdn, v_diesel, v_petrol, v_hybrid, v_EV, v_cars, v_ICE, v_pureICE))




#####################################################################################################################################################################

### ATTITUDE TOWARDS ICEV PHASE-OUT 

# make new data frame to display absolute & relative frequencies

df_attitude <- as.data.frame(table(df$v_16))
df_attitude$Percentage <- df_attitude$Freq/1663*100


# plot frequencies (bar chart) for attitude towards phase-out 

ggplot(df_attitude, aes(x = Var1, y = Freq)) +
         geom_bar(stat = "identity") +
  labs(title = "", 
       x = "", 
       y = "Count") +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"), 
                   labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 18))


### Lollipop charts: support for ICEV phase-out, differentiated by different variables 

### display attitude per AGE GROUP, differentiate by gender 

df_att_age_gen <- df %>% 
  dplyr::select(c(v_3, v_114, v_16))

df_att_age_gen <- df_att_age_gen %>% 
  mutate(v_114 = ifelse(df_att_age_gen$v_114 == "1", "<30", 
                        ifelse(df_att_age_gen$v_114 == "2", "30-39", 
                               ifelse(df_att_age_gen$v_114 == "3", "40-49", 
                                      ifelse(df_att_age_gen$v_114 == "4", "50-59", 
                                             ifelse(df_att_age_gen$v_114 == "5", "60+", NA))))))


df_att_sum <- aggregate(x = df_att_age_gen$v_16, 
                        by = list(df_att_age_gen$v_3, df_att_age_gen$v_114), 
                        FUN = mean)

colnames(df_att_sum) <- c("Gender", "Agegroup", "Value")

df_att_sum <- reshape(df_att_sum, idvar = "Agegroup", timevar = "Gender", direction = "wide")
colnames(df_att_sum) <- c("Age", "male", "female", "diverse", "NA")

df_att_sum$male <- as.numeric(df_att_sum$male)
df_att_sum$female <- as.numeric(df_att_sum$female)


ggplot(df_att_sum) +
  geom_segment(aes(x = Age, xend = Age, y = male, yend = female), color = "grey50", size = 1) +
  geom_point(aes(x = Age, y = male), colour = "darkslategrey", size = 6, alpha = 0.7) +
  geom_point(aes(x = Age, y = female), colour = "steelblue3", size = 6, alpha = 0.7) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Attitude towards ICEV phase-out", 
       x = "Agegroup",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose"))



### display attitude per INCOME GROUP, differentiate by gender 

df_income <- as.data.frame(table(df$v_5))

df_att_inc_gen <- df %>% 
  dplyr::select(c(v_3, v_5, v_16))

df_att_inc_gen <- df_att_inc_gen %>% 
  mutate(v_5 = ifelse(df_att_inc_gen$v_5 == "1", "<10'000", 
                      ifelse(df_att_inc_gen$v_5 == "2", "10-30'000", 
                             ifelse(df_att_inc_gen$v_5 == "3", "30-50'000", 
                                    ifelse(df_att_inc_gen$v_5 == "4", "50-70'000", 
                                           ifelse(df_att_inc_gen$v_5 == "5", "70-100'000", 
                                                  ifelse(df_att_inc_gen$v_5 == "6", "100-150'000", 
                                                         ifelse(df_att_inc_gen$v_5 == "7", "150'000+", NA))))))))

df_att_inc_gen$v_5 <- factor(df_att_inc_gen$v_5, order = TRUE, levels = c("<10'000", "10-30'000", "30-50'000", "50-70'000", "70-100'000", 
                                                                          "100-150'000", "150'000+"))


df_att_sum_inc <- aggregate(x = df_att_inc_gen$v_16, 
                            by = list(df_att_inc_gen$v_3, df_att_inc_gen$v_5), 
                            FUN = mean)

colnames(df_att_sum_inc) <- c("Gender", "Income group", "Value")

df_att_sum_inc <- reshape(df_att_sum_inc, idvar = "Income group", timevar = "Gender", direction = "wide")
colnames(df_att_sum_inc) <- c("Income", "male", "female", "diverse", "NA")

df_att_sum_inc$male <- as.numeric(df_att_sum_inc$male)
df_att_sum_inc$female <- as.numeric(df_att_sum_inc$female)


ggplot(df_att_sum_inc) +
  geom_segment(aes(x = Income, xend = Income, y = male, yend = female), color = "grey50", size = 1) +
  geom_point(aes(x = Income, y = male), colour = "darkslategrey", size = 6, alpha = 0.7) +
  geom_point(aes(x = Income, y = female), colour = "steelblue3", size = 6, alpha = 0.7) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Annual household income", 
       x = "EURO",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose"))



### display attitude per LOCATION, differentiate by gender 


df_att_loc_gen <- df %>% 
  dplyr::select(c(v_3, v_2, v_16))

df_att_loc_gen <- df_att_loc_gen %>% 
  mutate(v_2 = ifelse(df_att_loc_gen$v_2 == "1", "urban", 
                      ifelse(df_att_loc_gen$v_2 == "2", "semi-urban", 
                             ifelse(df_att_loc_gen$v_2 == "3", "rural", NA))))


df_att_sum_loc <- aggregate(x = df_att_loc_gen$v_16, 
                            by = list(df_att_loc_gen$v_3, df_att_loc_gen$v_2), 
                            FUN = mean)

colnames(df_att_sum_loc) <- c("Gender", "Location", "Value")

df_att_sum_loc <- reshape(df_att_sum_loc, idvar = "Location", timevar = "Gender", direction = "wide")
colnames(df_att_sum_loc) <- c("Location", "no answer", "male", "female", "diverse", "NA")

df_att_sum_loc$male <- as.numeric(df_att_sum_loc$male)
df_att_sum_loc$female <- as.numeric(df_att_sum_loc$female)


ggplot(df_att_sum_loc) +
  geom_segment(aes(x = Location, xend = Location, y = male, yend = female), color = "grey50", size = 1) +
  geom_point(aes(x = Location, y = male), colour = "darkslategrey", size = 6, alpha = 0.7) +
  geom_point(aes(x = Location, y = female), colour = "steelblue3", size = 6, alpha = 0.7) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Place of residence", 
       x = "",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose"))



### display attitude per FREQUENCY OF DRIVING, differentiate by gender 

# first: collapse categories to have only 3 
df <- df %>% 
  mutate(v_9b = ifelse(df$v_9 == "1", "never", 
                       ifelse(df$v_9 %in% c("2", "3"), "1-4 days", 
                              ifelse(df$v_9 %in% c("4", "5"), "5-7 days", NA))))

df$v_9b <- factor(df$v_9b, order = TRUE, levels = c("never", "1-4 days", "5-7 days"))

df_att_dri_gen <- df %>% 
  dplyr::select(c(v_3, v_9b, v_16))


df_att_sum_dri <- aggregate(x = df_att_dri_gen$v_16, 
                            by = list(df_att_dri_gen$v_3, df_att_dri_gen$v_9b), 
                            FUN = mean)

colnames(df_att_sum_dri) <- c("Gender", "Driving Frequency", "Value")

df_att_sum_dri <- reshape(df_att_sum_dri, idvar = "Driving Frequency", timevar = "Gender", direction = "wide")
colnames(df_att_sum_dri) <- c("Driv_Freq", "no answer", "male", "female", "diverse", "NA")

df_att_sum_dri$male <- as.numeric(df_att_sum_dri$male)
df_att_sum_dri$female <- as.numeric(df_att_sum_dri$female)


ggplot(df_att_sum_dri) +
  geom_segment(aes(x = Driv_Freq, xend = Driv_Freq, y = male, yend = female), color = "grey50", size = 1) +
  geom_point(aes(x = Driv_Freq, y = male), colour = "darkslategrey", size = 6, alpha = 0.7) +
  geom_point(aes(x = Driv_Freq, y = female), colour = "steelblue3", size = 6, alpha = 0.7) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Weekly car use", 
       x = "no. of days",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose")) +
  scale_x_discrete(limits=rev) 



### display attitude per PARTY PROXIMITY, differentiate by gender 


df_att_par_gen <- df %>% 
  dplyr::select(c(v_3, v_6, v_16))

df_att_par_gen <- df_att_par_gen %>% 
  mutate(v_6 = ifelse(df_att_par_gen$v_6 == "1", "CDU/CSU", 
                      ifelse(df_att_par_gen$v_6 == "2", "SPD", 
                             ifelse(df_att_par_gen$v_6 == "3", "Greens", 
                                    ifelse(df_att_par_gen$v_6 == "4", "Die Linke", 
                                           ifelse(df_att_par_gen$v_6 == "5", "FDP", 
                                                  ifelse(df_att_par_gen$v_6 == "6", "AfD", 
                                                         ifelse(df_att_par_gen$v_6 == "7", "none", NA))))))))


df_att_sum_par <- aggregate(x = df_att_par_gen$v_16, 
                            by = list(df_att_par_gen$v_3, df_att_par_gen$v_6), 
                            FUN = mean)

colnames(df_att_sum_par) <- c("Gender", "Party", "Value")

df_att_sum_par <- reshape(df_att_sum_par, idvar = "Party", timevar = "Gender", direction = "wide")
colnames(df_att_sum_par) <- c("Party", "male", "female", "diverse", "NA")

df_att_sum_par$male <- as.numeric(df_att_sum_par$male)
df_att_sum_par$female <- as.numeric(df_att_sum_par$female)

df_att_sum_par$Party <- factor(df_att_sum_par$Party, levels = c("Greens", "Die Linke", "CDU/CSU", "SPD", "none", "FDP", "AfD")) ### important! reorder!
### ordering currently following data on N=700, so update with full dataset 

ggplot(df_att_sum_par) +
  geom_segment(aes(x = Party, xend = Party, y = male, yend = female), color = "grey50", size = 1) +
  geom_point(aes(x = Party, y = male), colour = "darkslategrey", size = 6, alpha = 0.7) +
  geom_point(aes(x = Party, y = female), colour = "steelblue3", size = 6, alpha = 0.7) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Party proximity", 
       x = "",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose")) +
  scale_x_discrete(limits=rev)


###################################################################################################################################################################

### correlation between support for and and perceived inevitability of ICEV phase-out

spearman.test(df$v_16,df$v_17)


###################################################################################################################################################################

### PERCEIVED INEVITABILITY

# make new data frame to display absolute & relative frequencies 

df_inevitability <- as.data.frame(table(df$v_17))
df_inevitability$Percentage <- df_inevitability$Freq/1663*100

df_likelihood <- as.data.frame(table(df$v_17)) 

df_likelihood[df_likelihood == 0] <- NA

# plot: bar chart of perceived likelihood of ICEV phase-out

df_likelihood %>% 
  filter(Var1 != "0") %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "", 
       x = "", 
       y = "Count") +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"), 
                   labels = c("inevitable", "rather likely", "neither likely \n nor unlikely", "rather unlikely", "unrealistic")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 18))

# display perceived inevitability (average) per agegroup, differentiate by gender 

df_inev_age_gen <- df %>% 
  dplyr::select(c(v_3, v_114, v_17))

df_inev_age_gen <- df_inev_age_gen %>% 
  mutate(v_114 = ifelse(df_inev_age_gen$v_114 == "1", "<30", 
                        ifelse(df_inev_age_gen$v_114 == "2", "30-39", 
                               ifelse(df_inev_age_gen$v_114 == "3", "40-49", 
                                      ifelse(df_inev_age_gen$v_114 == "4", "50-59", 
                                             ifelse(df_inev_age_gen$v_114 == "5", "60+", NA))))))


df_inev_sum <- aggregate(x = df_inev_age_gen$v_17, 
                         by = list(df_inev_age_gen$v_3, df_inev_age_gen$v_114), 
                         FUN = mean)

colnames(df_inev_sum) <- c("Gender", "Agegroup", "Value")

df_inev_sum <- reshape(df_inev_sum, idvar = "Agegroup", timevar = "Gender", direction = "wide")
colnames(df_inev_sum) <- c("Age", "male", "female", "diverse", "NA")

df_inev_sum$male <- as.numeric(df_inev_sum$male)
df_inev_sum$female <- as.numeric(df_inev_sum$female)

# plot with lollipop chart: 
ggplot(df_inev_sum) +
  geom_segment(aes(x = Age, xend = Age, y = male, yend = female), color = "grey50", size = 1) +
  geom_point(aes(x = Age, y = male), colour = "darkslategrey", size = 6, alpha = 0.7) +
  geom_point(aes(x = Age, y = female), colour = "steelblue3", size = 6, alpha = 0.7) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Perceived likelihood of an ICEV phase-out", 
       x = "Agegroup",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(2.25,3.75))



#####################################################################################################################################################################



### OUTCOME EXPECTATIONS

# using the likert package 

df_outex <- df %>%    # create new data frame with all relevant variables
  dplyr::select(c(v_18, v_19, v_20, v_21, v_22, v_23, v_24))

df_outex[df_outex == 0] <- NA


df_outex$v_18 <- factor(df_outex$v_18, levels = c("5", "4", "3", "2", "1"), 
                        labels = c("very negative", "rather negative", "neutral", "rather positive", "very positive"), ordered = TRUE)
df_outex$v_19 <- factor(df_outex$v_19, levels = c("5", "4", "3", "2", "1"), 
                        labels = c("very negative", "rather negative", "neutral", "rather positive", "very positive"), ordered = TRUE)
df_outex$v_20 <- factor(df_outex$v_20, levels = c("5", "4", "3", "2", "1"), 
                        labels = c("very negative", "rather negative", "neutral", "rather positive", "very positive"), ordered = TRUE)
df_outex$v_21 <- factor(df_outex$v_21, levels = c("5", "4", "3", "2", "1"), 
                        labels = c("very negative", "rather negative", "neutral", "rather positive", "very positive"), ordered = TRUE)
df_outex$v_22 <- factor(df_outex$v_22, levels = c("5", "4", "3", "2", "1"), 
                        labels = c("very negative", "rather negative", "neutral", "rather positive", "very positive"), ordered = TRUE)
df_outex$v_23 <- factor(df_outex$v_23, levels = c("5", "4", "3", "2", "1"), 
                        labels = c("very negative", "rather negative", "neutral", "rather positive", "very positive"), ordered = TRUE)
df_outex$v_24 <- factor(df_outex$v_24, levels = c("5", "4", "3", "2", "1"), 
                        labels = c("very negative", "rather negative", "neutral", "rather positive", "very positive"), ordered = TRUE)



names(df_outex) <- c(
  v_18 = "Individual", 
  v_19 = "Society", 
  v_20 = "Avg. citizen", 
  v_21 = "German automakers",
  v_22 = "Economy", 
  v_23 = "Environment", 
  v_24 = "Public health") 

lik_outex <- likert(df_outex)
lik_outex # print

summary(lik_outex)

xtable(lik_outex)

# plot likert-type bars per group:
p_outex <- plot(lik_outex, center = 3, text.size = 8,
     colors = c("tan4", "tan3", "khaki2", "darkseagreen3", "darkseagreen4"), 
     group.order= c("Environment", "Public health", "Society", "Economy", "Individual", "Avg. citizen", "German automakers")) + 
  theme(axis.text = element_text(size = 26)) +
  theme(axis.text = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_blank()) +
  labs(y = "")

# Note: I'm editing the label outside RStudio

# density plot: (not currently used)
plot(lik_outex, type = "density", bw = 0.5) + ggtitle(title)


##############################################################################################################################################################


### POLICY INSTRUMENT EVALUATION

# using the likert package 

df_policy <- df %>%   # create new dataframe with all relevant variables
  dplyr::select(c(v_54, v_56, v_57, v_58, v_59, v_60, v_112, v_62))


df_policy[df_policy == 0] <- NA



df_policy$v_54 <- factor(df_policy$v_54, levels = c("5", "4", "3", "2", "1"), 
                         labels = c("strongly disapprove", "rather disapprove", "neither nor", "rather approve", "strongly approve"), 
                         ordered = TRUE)
df_policy$v_56 <- factor(df_policy$v_56, levels = c("5", "4", "3", "2", "1"), 
                         labels = c("strongly disapprove", "rather disapprove", "neither nor", "rather approve", "strongly approve"), 
                         ordered = TRUE)
df_policy$v_57 <- factor(df_policy$v_57, levels = c("5", "4", "3", "2", "1"), 
                         labels = c("strongly disapprove", "rather disapprove", "neither nor", "rather approve", "strongly approve"), 
                         ordered = TRUE)
df_policy$v_58 <- factor(df_policy$v_58, levels = c("5", "4", "3", "2", "1"), 
                         labels = c("strongly disapprove", "rather disapprove", "neither nor", "rather approve", "strongly approve"), 
                         ordered = TRUE)
df_policy$v_59 <- factor(df_policy$v_59, levels = c("5", "4", "3", "2", "1"), 
                         labels = c("strongly disapprove", "rather disapprove", "neither nor", "rather approve", "strongly approve"), 
                         ordered = TRUE)
df_policy$v_60 <- factor(df_policy$v_60, levels = c("5", "4", "3", "2", "1"), 
                         labels = c("strongly disapprove", "rather disapprove", "neither nor", "rather approve", "strongly approve"), 
                         ordered = TRUE)
df_policy$v_112 <- factor(df_policy$v_112, levels = c("5", "4", "3", "2", "1"), 
                         labels = c("strongly disapprove", "rather disapprove", "neither nor", "rather approve", "strongly approve"), 
                         ordered = TRUE)
df_policy$v_62 <- factor(df_policy$v_62, levels = c("5", "4", "3", "2", "1"), 
                         labels = c("strongly disapprove", "rather disapprove", "neither nor", "rather approve", "strongly approve"), 
                         ordered = TRUE)


names(df_policy) <- c(
  v_54 = "Lower EV purchase price (-20%)", 
  v_56 = "Lower public transit price (-20%)", 
  v_57 = "Increase ICEV purchase price (+20%)", 
  v_58 = "Inner-city ICEV driving ban (2025)",
  v_59 = "Mandate EV-only registrations (2030)", 
  v_60 = "Mandate EV-only company cars (2028)", 
  v_112 = "No ICEs may be registered from 2030", 
  v_62 = "ICEV driving ban (2040)") 
str(df_policy)

lik_policy <- likert(df_policy)
lik_policy # print

summary(lik_policy)

xtable(lik_policy)

# plot per policy instrument
p_policy <- plot(lik_policy, center = 3, text.size = 8,
     colors = c("tan4", "tan3", "khaki2", "darkseagreen3", "darkseagreen4"),
     group.order = c("Lower public transit price (-20%)",
                     "Lower EV purchase price (-20%)",
                     "Mandate EV-only company cars (2028)",
                     "ICEV driving ban (2040)", 
                     "Increase ICEV purchase price (+20%)", 
                     "Mandate EV-only registrations (2030)", 
                     "Inner-city ICEV driving ban (2025)")) + 
  theme(axis.text = element_text(size = 26)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_blank()) +
  labs(y = "")



# density plot option:
plot(lik_policy, type = "density", bw = 0.5) + ggtitle(title)


################################## 

### combine policy & outcome expectation plots into one
# I'm adding a third plot (with low height) to increase white space in between plots

plot_grid(p_outex, NULL, p_policy, 
          labels = c("a)", "", "b)"),
          label_size = 22,
          ncol = 1, nrow = 3, 
          rel_heights = c(1, 0.15, 1)) 








### perceived effectiveness of different policy instruments 

df_effect <- as.data.frame(table(df$v_63))

df_effect$Percentage <- df_effect$Freq/1663*100




###############################################################################################################################################################

### SOCIO-EMOTIONAL FACTORS / BELIEFS

### scales: v43 & 44, v45 & 46 & 47, v48 & 49 & 91, v51 & 52 & 53

df_soefct <- df %>% 
  dplyr::select(c(lfdn, v_16, v_43, v_44, v_45, v_46, v_47, v_48, v_49, v_91, v_51, v_52, v_53))

df_soefct[df_soefct == 0] <- NA

###check consistency of scales via Cronbach's alpha
alpha(subset(df_soefct, select = c(v_43, v_44)))          #0.54       ### questionable 
alpha(subset(df_soefct, select = c(v_45, v_46, v_47)))    #0.87
alpha(subset(df_soefct, select = c(v_48, v_49, v_91)))    #0.79       ### results suggest to consider dropping v_48, but still ok
alpha(subset(df_soefct, select = c(v_49, v_91)))          #0.90       ### when left out: ICEVs are responsible for env problems 
alpha(subset(df_soefct, select = c(v_51, v_52, v_53)))    #0.85

### combine individual items into factors
df_soefct$v_soe1 <- (df_soefct$v_43 + df_soefct$v_44)  / 2
df_soefct$v_soe2 <- (df_soefct$v_45 + df_soefct$v_46 + df_soefct$v_47) /3
df_soefct$v_soe3 <- (df_soefct$v_49 + df_soefct$v_91) /2
df_soefct$v_soe4 <- (df_soefct$v_51 + df_soefct$v_52 + df_soefct$v_53) /3

df_soefct$v_soe1 <- as.numeric(df_soefct$v_soe1) # set as numeric
df_soefct$v_soe2 <- as.numeric(df_soefct$v_soe2)
df_soefct$v_soe3 <- as.numeric(df_soefct$v_soe3)
df_soefct$v_soe4 <- as.numeric(df_soefct$v_soe4)

df_soefct_long <- melt(setDT(df_soefct), id.vars = c("lfdn", "v_16"), variable.name = "soe_scale") # change data form

df_soefct_long$v_16 <- factor(df_soefct_long$v_16, levels = c("5", "4", "3", "2", "1"), ordered = TRUE)

# display agreement to belief statements & differentiate by support for ICEV phase-out
df_soefct_long %>% 
  filter(soe_scale %in% c("v_soe1", "v_soe2", "v_soe3", "v_soe4")) %>%
  ggplot(aes(x = soe_scale, y = value, fill=factor(v_16))) +
  geom_boxplot(size = .8, width=.75) +
  scale_y_continuous(trans = "reverse", labels = c("fully agree", "mostly agree", "neutral", "mostly disagree", "fully disagree")) +
  scale_x_discrete(labels = c("No need for \n regulation", "ICEVs are part \n of Germany", "ICEVs are major \n cause of env. problems", 
                              "ICEVs elicit \n positive emotions")) +
  labs(title = "", 
       x = "", 
       y = "") +
  scale_fill_manual("Attitude towards \n ICEV phase-out", labels = c("- -", "-", "o", "+", "++"), values = c("5" = "coral4", "4" = "coral2", 
                                                                                                             "3" = "lemonchiffon3", "2" = "steelblue3", 
                                                                                                             "1" = "steelblue4")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_ipsum() +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 24)) +
  theme(legend.text = element_text(size = 22)) +
  theme(legend.title = element_text(size = 22))



############################################################################################################################################################

### map of Germany, displaying average support for ICEV phase-out by Bundesland 

# Note: I'm not using this due to low N in several Bundesl�nder

str(df$participant_region)
head(df$participant_region, n=100)

df_region <- df %>%    # filter to only have German Bundesl�nder
  dplyr::select(c(v_16, participant_region)) %>% 
  filter(participant_region != "-99") %>% 
  filter(participant_region != "Zurich") %>% 
  filter(participant_region != "North Holland")

df_region_sum <- aggregate(x = df_region$v_16, 
                           by = list(df_region$participant_region), 
                           FUN = mean)

df_region_sum$ID_1 <- c(14, 6, 7, 9, 4, 2, 5, 8, 11, 13, 3, 15, 10, 1, 12, 16)  # add identifying numbers
df_region_sum$HASC <- c("DE.BR", "DE.SL", "DE.BW", "DE.SN", "DE.BY", "DE.NI", "DE.BE", "DE.SH", "DE.HE", "DE.MV", "DE.NW", "DE.HH", "DE.ST", "DE.TH", "DE.HB", "DE.RP")

df_region_sum$ID_1 <- as.integer(df_region_sum$ID_1)

sample_ID <- readRDS("C://Users//jhoppe//Downloads//DEU_adm1.rds")

df_region_sum <- sp::merge(x = sample_ID@data, y = df_region_sum, by.x="ID_1", all.x = TRUE)

df_region_sum[order(-df_region_sum$x), c("ID_1", "x", "HASC", "Group.1")]

clrs <- c('#F4F1A2','#F4F1A2','#E6EAA2','#E6EAA2',
          '#CFE3A2','#CFE3A2','#9AD0A3','#9AD0A3',
          '#7FC9A4','#7FC9A4','#32B9A3','#32B9A3',
          '#00A7A2','#00A7A2','#00667E','#00667E')

df_region_sum$x <- as.numeric(df_region_sum$x)

polygon_DE <- SpatialPolygonsDataFrame(sample_ID, df_region_sum)

# plot attotude towards ICEV phase-out
spplot(polygon_DE, zcol = "x", main = "", col.regions = clrs)


################################# repeat analysis with perceived inevitability 

df_region2 <- df %>% 
  dplyr::select(c(v_17, participant_region)) %>% 
  filter(participant_region != "-99") %>% 
  filter(participant_region != "Zurich") %>% 
  filter(participant_region != "North Holland")

df_region_sum2 <- aggregate(x = df_region2$v_17, 
                           by = list(df_region2$participant_region), 
                           FUN = mean)

df_region_sum2$ID_1 <- c(14, 6, 7, 9, 4, 2, 5, 8, 11, 13, 3, 15, 10, 1, 12, 16)
df_region_sum2$HASC <- c("DE.BR", "DE.SL", "DE.BW", "DE.SN", "DE.BY", "DE.NI", "DE.BE", "DE.SH", "DE.HE", "DE.MV", "DE.NW", "DE.HH", "DE.ST", "DE.TH", "DE.HB", "DE.RP")

df_region_sum2$ID_1 <- as.integer(df_region_sum2$ID_1)

sample_ID <- readRDS("C://Users//jhoppe//Downloads//DEU_adm1.rds")

df_region_sum2 <- sp::merge(x = sample_ID@data, y = df_region_sum2, by.x="ID_1", all.x = TRUE)

df_region_sum2$x <- as.numeric(df_region_sum2$x)

polygon_DE <- SpatialPolygonsDataFrame(sample_ID, df_region_sum2)

spplot(polygon_DE, zcol = "x", main = "", col.regions = clrs)




### check how representative maps are by comparing share of respondents per region to actual share of population

df_regionrep <- as.data.frame(table(df$participant_region))
df_regionrep$share <- df_regionrep$Freq /(1663-27)*100

### this is (with few exceptions) fairly representative wrt population of states, but 3 states are only made up of 16 respondents, so results are sensitive... 
### to outliers in regions with small N

################################################################################################################################################################

### TREATMENT EFFECTS

### bar charts for each group of support (proponents, opponents, neutral)

# Narrative A: Change is inevitable

df_condA <- df %>% 
  dplyr::select(c(lfdn, v_16, v_92, c_0001)) %>% 
  filter(c_0001 == 1)

df_condA_pre <- as.data.frame(table(df_condA$v_16))
df_condA_post <- as.data.frame(table(df_condA$v_92))

aa <- c(41.4, 23.8, 34.8, 42.0, 25.8, 32.3)
ab <- c("1pre", "1pre", "1pre", "post", "post", "post")
ac <- c("++/+", "o", "-/--", "++/+", "o", "-/--")
df_A <- data.frame(aa, ab, ac)

level_order <- c("++/+", "o", "-/--")

df_A %>% 
  ggplot(aes(x = factor(ac, level = level_order), y= aa, fill = ab)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  scale_fill_manual("Condition", values = c("1pre" = "lightblue3", "post" = "lightblue4")) +
  labs(title = "Attitude - Change is inevitable", 
       x = "", 
       y = "(%)") +
  theme_ipsum() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20)) +
  coord_cartesian(ylim = c(0, 50))

# Narrative B: EVs are better than you think

df_condB <- df %>% 
  dplyr::select(c(lfdn, v_16, v_96, c_0001)) %>% 
  filter(c_0001 == 2)

df_condB_pre <- as.data.frame(table(df_condB$v_16))
df_condB_post <- as.data.frame(table(df_condB$v_96))

ba <- c(48.2, 17.9, 33.9, 50.7, 20.4, 28.9)
bb <- c("1pre", "1pre", "1pre", "post", "post", "post")
bc <- c("++/+", "o", "-/--", "++/+", "o", "-/--")
df_B <- data.frame(ba, bb, bc)

df_B %>% 
  ggplot(aes(x = factor(bc, level = level_order), y= ba, fill = bb)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  scale_fill_manual("Condition", values = c("1pre" = "lightblue3", "post" = "lightblue4")) +
  labs(title = "Attitude - EVs are better than you think", 
       x = "", 
       y = "(%)") +
  theme_ipsum() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20)) +
  coord_cartesian(ylim = c(0, 50))


# Narrative C: ICEVs are worse than you think

df_condC <- df %>% 
  dplyr::select(c(lfdn, v_16, v_100, c_0001)) %>% 
  filter(c_0001 == 3)

df_condC_pre <- as.data.frame(table(df_condC$v_16))
df_condC_post <- as.data.frame(table(df_condC$v_100))

ca <- c(47.0, 21.8, 31.2, 49.6, 20.4, 30.0)
cb <- c("1pre", "1pre", "1pre", "post", "post", "post")
cc <- c("++/+", "o", "-/--", "++/+", "o", "-/--")
df_C <- data.frame(ca, cb, cc)

df_C %>% 
  ggplot(aes(x = factor(cc, level = level_order), y= ca, fill = cb)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  scale_fill_manual("Condition", values = c("1pre" = "lightblue3", "post" = "lightblue4")) +
  labs(title = "Attitude - ICEVs are worse than you think", 
       x = "", 
       y = "(%)") +
  theme_ipsum() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20)) +
  coord_cartesian(ylim = c(0, 50))



#######################


### study treatment effect via average treatment effect (compare means of pre and post treatment answers) 

df_treatment <- df %>% 
  dplyr::select(c(lfdn, v_16, v_92, v_96, v_100, c_0001))

df_treatment_pre <- df %>% 
  dplyr::select(c(v_16, c_0001))

df_treatment_pre$cond <- " pre"

df_treatment_post <- df %>% 
  dplyr::select(c(v_92, v_96, v_100, c_0001))

df_treatment_post$cond <- "post"

df_treatment_post[df_treatment_post == -77] <- 0

df_treatment_post <- df_treatment_post %>% 
  mutate(v_16_post = v_92 + v_96 + v_100)

df_treatment_post$v_92 <- NULL
df_treatment_post$v_96 <- NULL
df_treatment_post$v_100 <- NULL
df_treatment_post$lfdn <- NULL

df_treatment_post <- df_treatment_post %>% 
  rename(v_16 = v_16_post)

df_treatment_all <- merge(df_treatment_pre, df_treatment_post, all.x = TRUE, all.y = TRUE)

df_treatment_all <- df_treatment_all %>% 
  mutate(c_0001 = ifelse(df_treatment_all$c_0001 == "1", "Change is \n inevitable", 
         ifelse(df_treatment_all$c_0001 == "2", "EVs are better \n than you think", 
                ifelse(df_treatment_all$c_0001 == "3", "ICEVs are worse \n than you think", "NA"))))

ggerrorplot(df_treatment_all, x = c_0001, y = v_16, desc_stat = "mean_se")

ggerrorplot(df_treatment_all, x = "c_0001", y = "v_16", 
            desc_stat = "mean_se", 
            color = "cond",
            palette = "Paired",
            size = 1.9,
            width = 0,
            error.plot = "errorbar",
            position = position_dodge(0.4),
                       add = "mean") +
  scale_y_reverse() +
  labs(title = "", 
       x = "", 
       y = "") +
 coord_cartesian(ylim=c(3.1, 2.6))


### summary statistic by group(i.e. condition) 

group_by(df_treatment_all, cond) %>% 
  summarise(count= n(),
            median = median(v_16, nar.rm = TRUE), 
            IQR = IQR(v_16, na.rm = TRUE))

### compute paired-sample Wilcoxon test 
### Q: Is there a significant difference in the two means before & after treatment?


beforeA <- subset(df_treatment_all, cond == " pre" & c_0001 == "Change is \n inevitable", v_16, drop = TRUE)
afterA <- subset(df_treatment_all, cond == "post" & c_0001 == "Change is \n inevitable", v_16, drop = TRUE)

result_wilcoxA <- wilcox.test(beforeA, afterA, paired = TRUE)


beforeB <- subset(df_treatment_all, cond == " pre" & c_0001 == "EVs are better \n than you think", v_16, drop = TRUE)
afterB <- subset(df_treatment_all, cond == "post" & c_0001 == "EVs are better \n than you think", v_16, drop = TRUE)

result_wilcoxB <- wilcox.test(beforeB, afterB, paired = TRUE)


beforeC <- subset(df_treatment_all, cond == " pre" & c_0001 == "ICEVs are worse \n than you think", v_16, drop = TRUE)
afterC <- subset(df_treatment_all, cond == "post" & c_0001 == "ICEVs are worse \n than you think", v_16, drop = TRUE)

result_wilcoxC <- wilcox.test(beforeC, afterC, paired = TRUE)


result_wilcoxA
result_wilcoxB
result_wilcoxC





###############################################################################################################################################

################################################## follow up survey ###########################################################################

###############################################################################################################################################





df2 <- read.csv("C://Users//jhoppe//polybox//Research//Scripts, summaries, drafts, proposal//Proposal 4, Public opinion on phasing out ICEs//Survey Data//210816SurveyData2.csv", 
                header = TRUE, 
                sep = ";")

setnames(df2, "p_0001", "tic_2")

# recode answers so that 1 = treatment (i.e. have heard of EC proposal), and 0= no treatment (haven't heard, or aren't sure), no answer = NA
df2 <- df2 %>% 
  mutate(T_EC = ifelse(df2$v_115 == "1", "1", 
                       ifelse(df2$v_115 == "0", "NA", "0")))

# recode answers so that 1 = treatment (think that CC & floods are connected), and 0= no treatment (don't think so, or aren't sure)
df2 <- df2 %>% 
  mutate(T_CC = ifelse(df2$v_116 == "1", "1", 
                       ifelse(df2$v_116 == "0", "NA", "0")))

df2a <- df2 %>% 
  dplyr::select(c(tic_2, v_16, v_17))

df2b <- df2 %>% 
  dplyr::select(c(tic_2, T_EC, T_CC))



### import excel file that matches IDs (p_0001) of first and second survey

df3 <- read_excel("C://Users//jhoppe//polybox//Research//Scripts, summaries, drafts, proposal//Proposal 4, Public opinion on phasing out ICEs//Survey Data//tic_matched3.xlsx")


df1 <- df %>% 
  dplyr::select(c(p_0001, v_16, v_17))


setnames(df1, "p_0001", "tic_1")


df4 <- merge(df1, df3, by = "tic_1", all.x = TRUE, all.y = TRUE) 

df4 <- df4 %>% 
  filter(tic_1 != "-99") %>% 
  filter(tic_2 != "NA")

# adjust df so that I can use rbind (same columns)
df4$tic_1 <- NULL
df4$prepost <- "pre"
df2a$prepost <- "post"


df5 <- rbind(df4, df2a)

df5 <- merge(df5, df2b, by = "tic_2")

df5 <- df5 %>% 
  filter(T_EC != "NA") %>% 
  filter(T_CC != "NA")

df6 <- merge(df5, df3, by = "tic_2")

df7 <- df1 %>% 
  mutate(GROUP = ifelse(df1$v_16 %in% c("1", "2"), "P", 
                        ifelse(df1$v_16 == "3", "N", "O")))

df7$v_16 <- NULL
df7$v_17 <- NULL

df8 <- merge(df7, df6, by = "tic_1")
df8$v_16 <- as.numeric(df8$v_16)



df8$GROUP <- factor(df8$GROUP, order = TRUE, levels = c("O", "N", "P"))

df8_all <- aggregate(x = df8$v_16, 
                     by = list(df8$prepost, df8$GROUP),
                               FUN = mean)

colnames(df8_all) <- c("prepost", "GROUP", "Value")
df8_all <- reshape(df8_all, idvar = "GROUP", timevar = "prepost", direction = "wide")
colnames(df8_all) <- c("GROUP", "post", "pre")
df8_all$post <- as.numeric(df8_all$post)
df8_all$pre <- as.numeric(df8_all$pre)

ggplot(df8_all) +
  geom_segment(aes(x= GROUP, xend = GROUP, y = pre, yend = post), color = "grey50", size = 1) +
  geom_point(aes(x= GROUP, y = pre), colour = "red1", size = 6, alpha = 0.8) +
  geom_point(aes(x = GROUP, y = post), colour = "red4", size = 6, alpha = 0.8) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Change in attitude", 
       x = "Previous level of support",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose"))


### T_EC = 1 --> all those that have heared of EC proposal 
df8_EC1 <- df8 %>% 
  filter(T_EC == 1)

df8_EC1 <- aggregate(x = df8_EC1$v_16, 
                     by = list(df8_EC1$prepost, df8_EC1$GROUP),
                     FUN = mean)

colnames(df8_EC1) <- c("prepost", "GROUP", "Value")
df8_EC1 <- reshape(df8_EC1, idvar = "GROUP", timevar = "prepost", direction = "wide")
colnames(df8_EC1) <- c("GROUP", "post", "pre")
df8_EC1$post <- as.numeric(df8_EC1$post)
df8_EC1$pre <- as.numeric(df8_EC1$pre)

ggplot(df8_EC1) +
  geom_segment(aes(x= GROUP, xend = GROUP, y = pre, yend = post), color = "grey50", size = 1) +
  geom_point(aes(x= GROUP, y = pre), colour = "red1", size = 6, alpha = 0.8) +
  geom_point(aes(x = GROUP, y = post), colour = "red4", size = 6, alpha = 0.8) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Change in attitude among T_EC=1", 
       x = "Previous level of support",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose"))



### T_EC = 1 --> all those that have NOT heared of EC proposal 
df8_EC0 <- df8 %>% 
  filter(T_EC == 0)

df8_EC0 <- aggregate(x = df8_EC0$v_16, 
                     by = list(df8_EC0$prepost, df8_EC0$GROUP),
                     FUN = mean)

colnames(df8_EC0) <- c("prepost", "GROUP", "Value")
df8_EC0 <- reshape(df8_EC0, idvar = "GROUP", timevar = "prepost", direction = "wide")
colnames(df8_EC0) <- c("GROUP", "post", "pre")
df8_EC0$post <- as.numeric(df8_EC0$post)
df8_EC0$pre <- as.numeric(df8_EC0$pre)

ggplot(df8_EC0) +
  geom_segment(aes(x= GROUP, xend = GROUP, y = pre, yend = post), color = "grey50", size = 1) +
  geom_point(aes(x= GROUP, y = pre), colour = "red1", size = 6, alpha = 0.8) +
  geom_point(aes(x = GROUP, y = post), colour = "red4", size = 6, alpha = 0.8) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Change in attitude among T_EC=0", 
       x = "Previous level of support",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose"))





### T_EC = 1 --> all those that think floods & CC are related
df8_CC1 <- df8 %>% 
  filter(T_CC == 1)

df8_CC1 <- aggregate(x = df8_CC1$v_16, 
                     by = list(df8_CC1$prepost, df8_CC1$GROUP),
                     FUN = mean)

colnames(df8_CC1) <- c("prepost", "GROUP", "Value")
df8_CC1 <- reshape(df8_CC1, idvar = "GROUP", timevar = "prepost", direction = "wide")
colnames(df8_CC1) <- c("GROUP", "post", "pre")
df8_CC1$post <- as.numeric(df8_CC1$post)
df8_CC1$pre <- as.numeric(df8_CC1$pre)

ggplot(df8_CC1) +
  geom_segment(aes(x= GROUP, xend = GROUP, y = pre, yend = post), color = "grey50", size = 1) +
  geom_point(aes(x= GROUP, y = pre), colour = "red1", size = 6, alpha = 0.8) +
  geom_point(aes(x = GROUP, y = post), colour = "red4", size = 6, alpha = 0.8) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Change in attitude among T_CC=1", 
       x = "Previous level of support",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose"))



### T_EC = 1 --> all those that think floods & CC are NOT related
df8_CC0 <- df8 %>% 
  filter(T_CC == 0)

df8_CC0 <- aggregate(x = df8_CC0$v_16, 
                     by = list(df8_CC0$prepost, df8_CC0$GROUP),
                     FUN = mean)

colnames(df8_CC0) <- c("prepost", "GROUP", "Value")
df8_CC0 <- reshape(df8_CC0, idvar = "GROUP", timevar = "prepost", direction = "wide")
colnames(df8_CC0) <- c("GROUP", "post", "pre")
df8_CC0$post <- as.numeric(df8_CC0$post)
df8_CC0$pre <- as.numeric(df8_CC0$pre)

ggplot(df8_CC0) +
  geom_segment(aes(x= GROUP, xend = GROUP, y = pre, yend = post), color = "grey50", size = 1) +
  geom_point(aes(x= GROUP, y = pre), colour = "red1", size = 6, alpha = 0.8) +
  geom_point(aes(x = GROUP, y = post), colour = "red4", size = 6, alpha = 0.8) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Change in attitude among T_CC=0", 
       x = "Previous level of support",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5), labels = c("strongly support", "rather support", "neutral", "oppose", "strongly oppose"))


#####################

df5_sum <- aggregate(x = df5.2$v_16, 
                     by = list(df5.2$prepost, df5.2$T_EC),
                     FUN = mean)

colnames(df5_sum) <- c("prepost", "T_EC", "Value")
df5_sum <- reshape(df5_sum, idvar = "T_EC", timevar = "prepost", direction = "wide")
colnames(df5_sum) <- c("T_EC", "post", "pre")
df5_sum$post <- as.numeric(df5_sum$post)
df5_sum$pre <- as.numeric(df5_sum$pre)

ggplot(df5_sum) +
  geom_segment(aes(x= T_EC, xend = T_EC, y = pre, yend = post), color = "grey50", size = 1) +
  geom_point(aes(x= T_EC, y = pre), colour = "red2", size = 6, alpha = 0.7) +
  geom_point(aes(x = T_EC, y = post), colour = "red4", size = 6, alpha = 0.7) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Treatment effect (EC proposal)", 
       x = "Treatment (1=yes, 0=no)",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(1,5))
  


df5_sum <- aggregate(x = df5.2$v_16, 
                     by = list(df5.2$prepost, df5.2$T_CC),
                     FUN = mean)

colnames(df5_sum) <- c("prepost", "T_CC", "Value")
df5_sum <- reshape(df5_sum, idvar = "T_CC", timevar = "prepost", direction = "wide")
colnames(df5_sum) <- c("T_CC", "post", "pre")
df5_sum$post <- as.numeric(df5_sum$post)
df5_sum$pre <- as.numeric(df5_sum$pre)

ggplot(df5_sum) +
  geom_segment(aes(x= T_CC, xend = T_CC, y = pre, yend = post), color = "grey50", size = 1) +
  geom_point(aes(x= T_CC, y = pre), colour = "red2", size = 6, alpha = 0.9) +
  geom_point(aes(x = T_CC, y = post), colour = "red4", size = 6, alpha = 0.9) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Treatment effect (CC impacts)", 
       x = "Treatment (1=yes, 0=no)",
       y = "") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits= c(2,4))






















### code not currently used: 
### import data of follow up survey (August 16 2021 data)

df2 <- read.csv("C://Users//jhoppe//polybox//Research//Scripts, summaries, drafts, proposal//Proposal 4, Public opinion on phasing out ICEs//Survey Data//210816SurveyData2.csv", 
                header = TRUE, 
                sep = ";")

setnames(df2, "p_0001", "tic_2")

# recode answers so that 1 = treatment (i.e. have heared of EC proposal), and 0= no treatment (haven't heard, or aren't sure), no answer = NA
df2 <- df2 %>% 
  mutate(T_EC = ifelse(df2$v_115 == "1", "1", 
                       ifelse(df2$v_115 == "0", "NA", "0")))

# recode answers so that 1 = treatment (think that CC & floods are connected), and 0= no treatment (don't think so, or aren't sure)
df2 <- df2 %>% 
  mutate(T_CC = ifelse(df2$v_116 == "1", "1", 
                       ifelse(df2$v_116 == "0", "NA", "0")))

df2 <- df2 %>% 
  dplyr::select(c(tic_2, v_16, v_17, T_EC, T_CC))



### import excel file that matches IDs (p_0001) of first and second survey

df3 <- read_excel("C://Users//jhoppe//polybox//Research//Scripts, summaries, drafts, proposal//Proposal 4, Public opinion on phasing out ICEs//Survey Data//tic_matched3.xlsx")


df1 <- df %>% 
  dplyr::select(c(p_0001, v_16, v_17))

setnames(df1, "p_0001", "tic_1")

setnames(df2, "v_16", "v_16_post")
setnames(df2, "v_17", "v_17_post")

df4 <- merge(df1, merge(df2, df3, by = "tic_2", all.x = TRUE, all.y = TRUE), by = "tic_1", all.x = TRUE, all.y = TRUE)

df4 <- df4 %>% 
  filter(tic_2 != "NA") %>% 
  filter(tic_1 != "NA")



############################################3 works but I want to do it differently: 


# load packages
library(arrow)
library(tidyverse)    # basic functions
library(writexl)      # export to excel file 


# load data
data <- read.csv(snakemake@input[["data"]],
                 header = TRUE, 
                 sep = ";")

# select relevant variables
# see pdf document (download from Unipark) for associated variable names and response option)
data <- data %>% 
  dplyr::select(c(lfdn, p_0001, v_17, v_16, v_114, v_2, v_4, v_5, v_6, v_3, v_7, v_8, v_9, v_102, v_104, v_106, v_108, v_18, v_19, v_20, v_21, v_22, v_23, 
                  v_24, v_72, v_73, v_74, v_75, v_76, v_77, v_80, v_79, v_82, v_83, v_84, v_85, v_86, v_88, v_90, v_43, v_44, v_45, v_46, 
                  v_47, v_48, v_49, v_91, v_51, v_52, v_53, c_0001, v_92, v_96, v_100))

# download dataset with only relevant set of variables, for figshare
write_xlsx(data, snakemake@output[["figshare_data"]])


############################################# clean data #######################################################################################

str(data)

# recode missing data (-77 was default of Unipark)
data[data == "-77"] <- NA 

# recode outcome variable -> make binary 
# i.e. strongly approve, rather approve, neither approve nor disapprove --> acceptance [1];  rather disapprove, strongly disapprove --> non-acceptance[0])
data <- data %>% 
  mutate(acc = ifelse(data$v_16 %in% c("1", "2", "3"), "1", "0"))

data$acc <- factor(data$acc)

data$app <- as.ordered(data$v_16)
levels(data$app) <- c("Strongly approve", "Rather approve", "Neither approve nor disapprove",
                        "Rather disapprove", "Strongly disapprove")
data$app <- ordered(
    data$app,
    levels = c("Strongly disapprove", "Rather disapprove", "Neither approve nor disapprove",
               "Rather approve", "Strongly approve")
)

#################################################### explanatory variables #######################################################################

############################################### socio-demographic variables ####################################################################

# following data cleaning entails: renaming variables, replacing zeros that stand for NAs by NAs, and setting variables as factors

# age (agegroup)
data <- data %>% 
  mutate(age = v_114)
data$age[data$age == "0"] <- NA
data$age <- ordered(data$age)


# loc (location)
data <- data %>% 
  mutate(loc = v_2)
data$loc[data$loc == "0"] <- NA
data$loc <- factor(data$loc)


# chldn (no. of children)
data$v_4[data$v_4 == "0"] <- NA
data <- data %>% 
  mutate(chldn = v_4 -1)


# chld (yes[1] or no[0] to having at least one child)
data <- data %>% 
  mutate(chld = ifelse(data$chldn %in% c("1","2","3","4","5"), "1", 
                       ifelse(data$chldn == "NA", NA, "0")))
data$chld <- factor(data$chld)


# inc (income)
data <- data %>%
  mutate(inc = v_5)
data$inc[data$inc == "0"] <- NA
data$inc <- ordered(data$inc)


# ppref (Party pref.)
data <- data %>% 
  mutate(ppref = ifelse(data$v_6 == "1", "CDU/CSU", 
                        ifelse(data$v_6 == "2", "AaSPD", # Aa so that SPD is reference category
                               ifelse(data$v_6 == "3", "Die Grünen",
                                      ifelse(data$v_6 == "4", "Die Linke", 
                                             ifelse(data$v_6 == "5", "FDP", 
                                                    ifelse(data$v_6 == "6", "AfD", 
                                                           ifelse(data$v_6 == "7", "none/other", NA))))))))
data$ppref <- factor(data$ppref)



# gen (gender, 1 = male, 0= female & diverse)
data <- data %>% 
  mutate(gen = ifelse(data$v_3 %in% c("2", "3"), "0", 
                      ifelse(data$v_3 == "1", "1", NA)))
data$gen <- factor(data$gen)



############################# car related factors ##################################################################################################


# job (employment in car manufacturing industry, 1 = yes; 0= no)
data <- data %>% 
  mutate(job = ifelse(data$v_8 == "1", "1", 
                      ifelse(data$v_8 == "2", "0", NA)))
data$job <- factor(data$job)


# dfreq (frequency of car use, in times per week)
data <- data %>% 
  mutate(dfreq = v_9)
data$dfreq[data$dfreq == "0"] <- NA
data$dfreq <- ordered(data$dfreq)


# dlic (availability of driving license, whereby 0= no, 1 = yes)
data <- data %>% 
  mutate(dlic = ifelse(data$v_7 == "2", "0", 
                       ifelse(data$v_7 == "1", "1", NA)))
data$dlic <- factor(data$dlic)


# number if cars per type (die = diesel, pet = petrol, hyb = hybrid, ev = EV)
# note that it is assumed that no answer (value 0) means zero cars per type (respondents did not always set answer to zero, but rather skipped the answer
# ...per car type, but very few skipped the questions altogether) 

data <- data %>% 
  mutate(ndie = ifelse(data$v_102 %in% c("0", "1"), 0, 
                       ifelse(data$v_102 == "2", 1, 
                              ifelse(data$v_102 == "3", 2, 
                                     ifelse(data$v_102 == "4", 3, 
                                            ifelse(data$v_102 == "5", 4, 5))))))

data <- data %>% 
  mutate(npet = ifelse(data$v_104 %in% c("0", "1"), 0, 
                       ifelse(data$v_104 == "2", 1, 
                              ifelse(data$v_104 == "3", 2, 
                                     ifelse(data$v_104 == "4", 3, 
                                            ifelse(data$v_104 == "5", 4, 5))))))

data <- data %>% 
  mutate(nhyb = ifelse(data$v_106 %in% c("0", "1"), 0, 
                       ifelse(data$v_106 == "2", 1, 
                              ifelse(data$v_106 == "3", 2, 
                                     ifelse(data$v_106 == "4", 3, 
                                            ifelse(data$v_106 == "5", 4, 5))))))

data <- data %>% 
  mutate(nev = ifelse(data$v_108 %in% c("0", "1"), 0, 
                      ifelse(data$v_108 == "2", 1, 
                             ifelse(data$v_108 == "3", 2, 
                                    ifelse(data$v_108 == "4", 3, 
                                           ifelse(data$v_108 == "5", 4, 5))))))



# ncar (total number of cars)
data$ncar <- data$ndie + data$npet + data$nhyb + data$nev


# nice (total number of ICEVs)
data$nice <- data$ndie + data$npet + data$nhyb 


# npice (total number of pure ICEVs)
data$npice <- data$ndie + data$npet 


# wac (Willingn. to abandon car) (3=high, 2=medium, 1=low)

data <- data %>% 
  mutate(wac = ifelse(data$v_72 == "2", "3", 
                      ifelse(data$v_79 == "0", "2", "1")))
data$wac <- ordered(data$wac)

# wev (Willingn. to adopt EV; only among those that are != high to last category)

data <- data %>% 
  mutate(wev = ifelse(data$v_82 == "2", "3", 
                      ifelse(data$v_90 == "0", "2", 
                             ifelse(data$v_90 == "1", "1", NA))))
data$wev <- ordered(data$wev)


###################################### beliefs ##################################################################################################


# proba (problem attribution, via agreement to statements whether ICEVs are a) main contributor to CC and b) main contributor to air pollution
data$v_49[data$v_49 == "0"] <- NA
data$v_91[data$v_91 == "0"] <- NA

# envc (environmental concern, via statement on importance of climate and environmental protection)
data$v_48[data$v_48 == "0"] <- NA
data$v_48 <- ordered(data$v_48)

# pger (degree to which ICEVs are regarded as a part of Germany)
data$v_45[data$v_45 == "0"] <- NA
data$v_46[data$v_46 == "0"] <- NA
data$v_47[data$v_47 == "0"] <- NA

# emat (degree of emotional attachment)
data$v_51[data$v_51 == "0"] <- NA
data$v_52[data$v_52 == "0"] <- NA
data$v_53[data$v_53 == "0"] <- NA

# vera (degree of aversion to regulation)
data$v_44[data$v44 == "0"] <- NA
data$v_44 <- ordered(data$v_44)

# mlib (dgree market liberalism)
data$v_43[data$v_43 == "0"] <- NA
data$v_43 <- ordered(data$v_43)

# TODO invert likert items: 1 is highest, 5 is lowest

write_feather(data, snakemake@output[["preprocessed_data"]])

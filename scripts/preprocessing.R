install.packages("pixiedust", repos = "https://stat.ethz.ch/CRAN/") # cannot be installed using conda at this point

# load packages
library(tidyverse)    # basic functions
library(cowplot)      # arrange multiple plots in one figure
library(randomForest) # conduct random forest classification
library(Hmisc)
library(caret)
library(e1071)
library(ROCR)
library(margins)      # compute marginal effects for logistic regression
library(mfx)
library(gmodels)
library(pixiedust)    # create summary table for export
library(ggpubr)
library(rstatix)      # conduct Wilcoxon signed rank test 
library(broom)
library(readxl)       # read excel sheet
library(writexl)      # export to excel file 
library(styler)       # tidies up R script


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


#################################################### explanatory variables #######################################################################

############################################### socio-demographic variables ####################################################################

# following data cleaning entails: renaming variables, replacing zeros that stand for NAs by NAs, and setting variables as factors

# age (agegroup)
data <- data %>% 
  mutate(age = v_114)
data$age[data$age == "0"] <- NA
data$age <- factor(data$age)


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


# inc (income - collapse income groups into 4)
data <- data %>% 
  mutate(inc = ifelse(data$v_5 %in% c("1", "2"), "1", 
                      ifelse(data$v_5 == "3", "2", 
                             ifelse(data$v_5 %in% c("4", "5"), "3", 
                                    ifelse(data$v_5 %in% c("6", "7"), "4", NA)))))
data$inc <- factor(data$inc)


# ppref (Party pref.)
data <- data %>% 
  mutate(ppref = ifelse(data$v_6 == "1", "CDU/CSU", 
                        ifelse(data$v_6 == "2", "AaSPD", # Aa so that SPD is reference category
                               ifelse(data$v_6 == "3", "Die Gr�nen", 
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
data$dfreq <- factor(data$dfreq)


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
data$wac <- factor(data$wac)

# wev (Willingn. to adopt EV; only among those that are != high to last category)

data <- data %>% 
  mutate(wev = ifelse(data$v_82 == "2", "3", 
                      ifelse(data$v_90 == "0", "2", 
                             ifelse(data$v_90 == "1", "1", NA))))
data$wev <- factor(data$wev)


###################################### beliefs ##################################################################################################


# proba (problem attribution, via agreement to statements whether ICEVs are a) main contributor to CC and b) main contributor to air pollution
data$v_49[data$v_49 == "0"] <- NA
data$v_91[data$v_91 == "0"] <- NA

data <- data %>% 
  mutate(v_49_91 = (data$v_49 + data$v_91) /2)

data <- data %>% #(again, 3=high, 2=medium, 1=low) -> judgment call on cut-off points (but absolute values are of low importance)
  mutate(proba = ifelse(data$v_49_91 %in% c("1", "1.5", "2"), "3", 
                        ifelse(data$v_49_91 %in% c("2.5", "3", "3.5"), "2", 
                               ifelse(data$v_49_91 %in% c("4", "4.5", "5"), "1", NA))))
data$proba <- factor(data$proba)


# envc (environmental concern, via statement on importance of climate and environmental protection)
data <- data %>% 
  mutate(envc = ifelse(data$v_48 %in% c("1", "2"), "3", 
                       ifelse(data$v_48 == "3", "2", 
                              ifelse(data$v_48 %in% c("4", "5"), "1", NA))))
data$envc <- factor(data$envc)


# pger (degree to which ICEVs are regarded as a part of Germany)
data$v_45[data$v_45 == "0"] <- NA
data$v_46[data$v_46 == "0"] <- NA
data$v_47[data$v_47 == "0"] <- NA

data <- data %>% 
  mutate(v_45_46_47 = (data$v_45 + data$v_46 + data$v_47) / 3)

data <- data %>% 
  mutate(pger = ifelse(data$v_45_46_47 <= 2, "3", 
                       ifelse(data$v_45_46_47 > 2 & data$v_45_46_47 < 4, "2", 
                              ifelse(data$v_45_46_47 >= 4, "1", NA))))
data$pger <- factor(data$pger)


# emat (degree of emotional attachment)
data$v_51[data$v_51 == "0"] <- NA
data$v_51[data$v_52 == "0"] <- NA
data$v_53[data$v_53 == "0"] <- NA

data <- data %>% 
  mutate(v_51_52_53 = (data$v_51 + data$v_52 + data$v_53) / 3)

data <- data %>% 
  mutate(emat = ifelse(data$v_51_52_53 <= 2, "3", 
                       ifelse(data$v_51_52_53 > 2 & data$v_51_52_53 < 4, "2", 
                              ifelse(data$v_51_52_53 >= 4, "1", NA))))
data$emat <- factor(data$emat)


# vera (degree of aversion to regulation)
data <- data %>% 
  mutate(vera = ifelse(data$v_44 %in% c("1", "2"), "3", 
                       ifelse(data$v_44 == "3", "2", 
                              ifelse(data$v_44 %in% c("4", "5"), "1", NA))))
data$vera <- factor(data$vera)


# mlib (dgree market liberalism)
data <- data %>% 
  mutate(mlib = ifelse(data$v_43 %in% c("1", "2"), "3", 
                       ifelse(data$v_43 == "3", "2", 
                              ifelse(data$v_43 %in% c("4", "5"), "1", NA))))
data$mlib <- factor(data$mlib)

head(data)

##################################### random forest classification ###############################################################################

##################################################################################################################################################

##################################################################################################################################################

set.seed(12345)

# select all relevant variables, to be included in the model 
data_rf <- data %>% 
  dplyr::select(c(lfdn, acc, age, loc, chld, inc, ppref, gen, dlic, job, dfreq, npice, mlib, vera, pger, envc, emat, proba, wac, wev))

str(data_rf)

### impute missing values for NAs (there are very few, except for WEV variable) because random forest does not handle missing values
data.imputed <- rfImpute(acc ~ ., data = data_rf, iter = 6)


### make model, use imputed dataset 
RFmodela <- randomForest(acc ~ age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + mlib + vera + pger + envc + emat + proba + wac + wev, data = data.imputed, proximity = TRUE)

RFmodela
### OBB estimate or error rate: 19% -> this means that 81% of the OOB samples were correctly classified by the random forest 

# plot model (for ntree = 500, i.e. default)
plot(RFmodela)

# test whether number or trees (default = 500) was appropriate --> with 1000 trees 
RFmodelb <- randomForest(acc ~ age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + mlib + vera + pger + envc + emat + proba + wac + wev, data = data.imputed, ntree = 1000, proximity = TRUE, importance = TRUE)

RFmodelb
# OBB error rate is very similar to before (slightly lower)

# test whether no of variables tried at each split was appropriate 
oob.values <- vector(length = 10)
for(i in 1:10) {
  temp.model <- randomForest(acc ~ ., data=data.imputed, mtry = i, ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

oob.values
# results show smallest oob value for mtry = 4

RFmodel <- randomForest(acc ~ age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + mlib + vera + pger + envc + emat + proba + wac + wev, data = data.imputed, ntree = 1000, mtry = 4, proximity = TRUE, importance = TRUE)
# when turning on importance = TRUE, it computes both mean-decrease-in-impurity and permutation importances

RFmodel
# result: OBB = 18.64 -> 81.36 % were correctly classified 

# importance is the extractor function for variable importance measures (type 1 = mean decrease in accuracy)
imp <- importance(RFmodel, type = 1)       

# make this into ggplot bar chart 
imp <- as.data.frame((imp))

# rescale, so that max value = 1 & all others are relative to that (absolute values aren't interpretable)
imp$acc_resc <- imp$MeanDecreaseAccuracy/max(imp$MeanDecreaseAccuracy) 

# add variable names and variable categories, so I can group (facet wrap)& colour-code them later; also to make labels readable
imp <- cbind(imp, c("Age", "Location", "Child(ren)", "Income", "Party pref.", "Gender", "Driving license", "Job in car industry", 
                    "Weekly car use", "No. of pure ICEVs", "Approval market liberalism", "Disapproval regulation", "Cultural/econ. significance", 
                    "Environmental concern", "Emotional attachment", "Problem attribution", "Willingn. to abandon car", "Willingn. to adopt EV")) 

imp <- cbind(imp, c("1) Socio-\ndemographics", "1) Socio-\ndemographics","1) Socio-\ndemographics","1) Socio-\ndemographics","1) Socio-\ndemographics","1) Socio-\ndemographics",
                    "2) Car-related \nfactors", "2) Car-related \nfactors", "2) Car-related \nfactors", "2) Car-related \nfactors", 
                    "3) Values and \nbeliefs", "3) Values and \nbeliefs","3) Values and \nbeliefs","3) Values and \nbeliefs","3) Values and \nbeliefs","3) Values and \nbeliefs",
                    "2) Car-related \nfactors", "2) Car-related \nfactors"))


colnames(imp) <- c("MDA", "MDA_resc", "Variable", "Category")

# order so that they are displayed as most to least important variables per category
imp$Variable <- factor(imp$Variable, levels = c("Child(ren)", "Gender", "Income",  "Age", "Location", "Party pref.",
                                                "Job in car industry", "Weekly car use", "Driving license", "No. of pure ICEVs", 
                                                "Willingn. to abandon car", "Willingn. to adopt EV", 
                                                "Approval market liberalism", "Disapproval regulation", "Cultural/econ. significance", "Emotional attachment", 
                                                "Environmental concern", "Problem attribution"))


#plot
p_VarIm <- ggplot(imp, aes(x = Variable, y = MDA_resc, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "", 
       x = "", 
       y = "") +
  theme_light() +
  facet_wrap(~Category, scales = "free", ncol = 3) +
  scale_y_continuous(limits = c(0,1)) +
  scale_fill_manual("Variable Category", values = c("1) Socio-\ndemographics" = "skyblue4", "2) Car-related \nfactors" = "lightsteelblue4", "3) Values and \nbeliefs" = "lightcyan4")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(size = 10)) +
  theme(strip.text = element_text(size = 11)) 

p_VarIm


##################################################################################################################################################################################

####################################### binomial logistic regression #############################################################################################################

##################################################################################################################################################################################


# build model, using all the same explanatory variables as for RF classification 
# use imputed dataset from RF so that there are no missing values
BLRmodel <- glm(formula = acc ~ age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + mlib + vera + pger + envc + emat + proba + wac + wev, data = data.imputed, family = "binomial"(link = "logit"))

# view summary statistics
summary.glm(BLRmodel)

# create summary table for export
basetable <- dust(BLRmodel) %>% 
  sprinkle(col = 1:4, round = 2) %>% 
  sprinkle(cols = 5, fn = quote(pvalString(value))) %>% 
  sprinkle(rows = c(1:40), border_color = "black") %>%
  sprinkle_colnames("Term", "Coefficient", "SE", "z-value", "p-value") %>% 
  sprinkle_print_method("console")

basetable

# coefficients
exp(coef(BLRmodel))  
exp(confint(BLRmodel))

# assess model using cross validation
crossValSettings <- trainControl(method = "repeatedcv", number = 10, 
                                 savePredictions = TRUE)
crossVal <- train(as.factor(acc) ~ age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + mlib + vera + pger + envc + emat + proba + wac + wev, data = data.imputed, family = "binomial", 
                  method = "glm", trControl = crossValSettings, tuneLength = 2)
pred <- predict(crossVal, newdata = data.imputed)

confusionMatrix(data = pred, data.imputed$acc)
# results: accuracy: 81.6% (p<0.001)


# produce & plot ROC curves of model
#probFull <- predict(BLRmodel, data.imputed, type = "response")
#predictFull <- prediction(probFull, data.imputed$acc) # FIXME does not work as probFull is numeric
#perfFull <- performance(predictFull, measure = "tpr", x.measure = "fpr")

#plot(perfFull, col = "blue")


# calculate McFadden's Pseudo R^2 
# pull log-likelihood of the null model out of the logistic variable by getting the value for the null deviance & dividing it by -2
# & pull the log-likelihood for the advances model out of the logistic variable by getting the value for the residual deviance and dividing by -2
BLR.null <- BLRmodel$null.deviance/-2
BLR.proposed <- BLRmodel$deviance/-2

(BLR.null - BLR.proposed) / BLR.null
## this is pseudo R2 = 0.38 

# calculate a p-value for that R^2 using a Chi-square distribution 
1 - pchisq(2*(BLR.proposed - BLR.null), df = (length(BLRmodel$coefficients) -1))
# result: 0 -> p-value is so small, i.e. R^2 value is not due to chance 


# draw graph of predicted probability vs observed values for acceptance 
# create a new data frame that contains probabilities of acceptance along with actual acceptance
predicted.data <- data.frame(probability.of.acc = BLRmodel$fitted.values, acc = data.imputed$acc)

# sort df from low to high probabilities & add new column that has rank of each sample (from low to high probability)
predicted.data <- predicted.data[order(predicted.data$probability.of.acc, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

# plot 
ggplot(data=predicted.data, aes(x = rank, y = probability.of.acc)) +
  geom_point(aes(color = acc), alpha = .3, shape = 17, stroke = .5) +
  xlab("Index") + 
  ylab("Predicted probability of acceptance")


# check for multicollinearity in model by computing the 'variance inflation factor' (VIF)
# the VIF measures how much the variance of a regression coefficient is inflated because of multicollinearity in the model
car::vif(BLRmodel)
# result: all VIF scores are fairly low (all below 2) -> very low multicollinearity
# James et al. (2014) suggests that VIF scores above 5 are problematic; Hardman and Tal exclude variables with VIF score > 3. 


# deviance residuals 
plot(density(resid(BLRmodel, type = "deviance")))
plot(density(rstandard(BLRmodel, type = "deviance")))

par(mfrow=c(1,2))
plot(cooks.distance(BLRmodel), type = "h")


# check for strongly correlated explanatory variables using scatterplot variables 
# correlates all variables (it's big, so not run...): pairs(~., data = data.imputed, main = "simple scatterplot matrix")


### calculate marginal effects with margins package 
ME.BLR <- margins(BLRmodel, 
                  data = find_data(BLRmodel, parent.frame()), 
                  variables = NULL, # default NULL returns ME for all variables
                  at = NULL, 
                  type = c("response"), # type of ME to estimate
                  vcov = stats::vcov(BLRmodel), 
                  vce = c("delta", "simulation", "bootstrap", "none"),
                  unit_ses = FALSE, 
                  esp = 1e-02)

ME.BLR.sum <- summary(ME.BLR, level = 0.95, by_factor = TRUE)

# add variable names and names for different levels of variables, as well as variable categories
ME.BLR.sum <- cbind(ME.BLR.sum, c("Age \n(R: <30y)", "Age \n(R: <30y)", "Age \n(R: <30y)", "Age \n(R: <30y)", "Child(ren)", "Weekly car use", "Weekly car use", "Weekly car use", "Weekly car use", 
                                  "Driving license", "Emotional attachment", "Emotional attachment", "Environmental concern", "Environmental concern", "Gender", 
                                  "Income \n(R: <30'000???)", "Income \n(R: <30'000???)", "Income \n(R: <30'000???)", "Job in car industry", "Location \n(R: urban)", "Location \n(R: urban)", "Approval market liberalism",
                                  "Approval market liberalism", "No. of pure ICEVs", "Cultural/econ. significance", "Cultural/econ. significance", 
                                  "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", 
                                  "Problem attribution", "Problem attribution", "Disapproval regulation", "Disapproval regulation", "Willingn. to abandon car", 
                                  "Willingn. to abandon car", "Willingn. to adopt EV", "Willingn. to adopt EV"))


ME.BLR.sum <- cbind(ME.BLR.sum, c("30-39", "40-49", "50-59", "60+", "yes", "1-2", "3-4", "5-6", "7", 
                                  "yes", "medium", "high", "medium", "high", "male", 
                                  "30-50'000", "50-100'000", "100'000+", "yes", "semi-urban", "rural", "medium", 
                                  "high", "Number", "medium", "high", 
                                  "AfD", "CDU/CSU", "Die Gr�nen", "Die Linke", "FDP", "none/other",  
                                  "medium", "high", "medium", "high", "medium", 
                                  "high", "medium", "high"))

ME.BLR.sum <- cbind(ME.BLR.sum, c("Socio-demographics", "Socio-demographics","Socio-demographics","Socio-demographics","Socio-demographics", "Car-related factors", "Car-related factors", "Car-related factors", "Car-related factors",  
                                  "Car-related factors", "Values and beliefs", "Values and beliefs", "Values and beliefs", "Values and beliefs", "Socio-demographics", 
                                  "Socio-demographics", "Socio-demographics", "Socio-demographics", "Car-related factors", "Socio-demographics", "Socio-demographics", "Values and beliefs", 
                                  "Values and beliefs", "Car-related factors", "Values and beliefs","Values and beliefs",
                                  "Socio-demographics", "Socio-demographics","Socio-demographics","Socio-demographics","Socio-demographics","Socio-demographics",
                                  "Values and beliefs", "Values and beliefs", "Values and beliefs", "Values and beliefs", "Car-related factors", 
                                  "Car-related factors","Car-related factors","Car-related factors"))

colnames(ME.BLR.sum) <- c("factor", "AME", "SE", "z", "p", "lower", "upper", "Variable", "Value", "Dimension")


# plot marginal effects
ME.BLR.sum$AME <- as.numeric(ME.BLR.sum$AME)


#set different variables as factors, so that they appear in the same order as for RF variable importance plot 
ME.BLR.sum$Variable <- factor(ME.BLR.sum$Variable, levels = c("Party pref. \n(R: SPD)", "Location \n(R: urban)", "Age \n(R: <30y)", "Income \n(R: <30'000???)", "Gender", "Child(ren)", 
                                                                 "Willingn. to adopt EV", "Willingn. to abandon car", "No. of pure ICEVs", 
                                                                 "Driving license", "Weekly car use", "Job in car industry", "Problem attribution", "Environmental concern", "Emotional attachment", 
                                                                 "Cultural/econ. significance", "Disapproval regulation", "Approval market liberalism"))

### this is nonsense code, but orders Variable values in the order that best suits the plot 
ME.BLR.sum$Value <- factor(ME.BLR.sum$Value, levels = c("none/other", "AfD", "Die Linke", "FDP", "Die Gr�nen", "CDU/CSU", 
                                                       "rural", "semi-urban", "60+", "50-59", "40-49", "30-39", "<30", 
                                                        "100'000+", "50-100'000", "30-50'000", "yes", "male", "high", "Number", "medium", "7", "5-6", "3-4", "1-2"))           


# plot
p_AME <- ggplot(ME.BLR.sum, aes(x = Value, y = AME, colour = Dimension)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, colour = Dimension),
                width = .4, size = .9, 
                position = position_dodge(0.6)) +
  geom_point(stat = "identity", position = position_dodge(0.6), size = 3) +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "darkred", linetype = 2, size = 1) +
  labs(x = "") +
  theme_light() +
  theme(legend.position = "none") +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(c("Variable"), scales = "free", space = "free") +
  theme(strip.text.y.right = element_text(angle = 0), strip.text = element_text(colour = "black", size = 13)) + 
  theme(strip.background = element_rect(colour = "grey", fill = "grey90")) +
  theme(axis.text = element_text(size = 11)) +
  scale_colour_manual("", values = c("Socio-demographics" = "skyblue4", "Car-related factors" = "lightsteelblue4", "Values and beliefs" = "lightcyan4"))

p_AME  

### combine RF classification results & BLR model results -> plot together


plot_grid(p_VarIm, NULL, p_AME,
          ncol = 1, nrow = 3, 
          rel_heights = c(.3, .035, 1),
          labels = c("a.", "b.", ""), 
          label_size =20)


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
# FIXME lines 662--683 are broken and therefore commented
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

p_treat2


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

#plot_grid(p_treat1, NULL, p_treat2, # FIXME not possible because of problem in line 662
#          ncol = 3, nrow = 1, 
#          rel_widths = c(1, .08, 1),
#          labels = c("a.", "", "b."), 
#          label_size =20)

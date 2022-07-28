# load packages
library(arrow)
library(tidyverse)    # basic functions
library(randomForest) # conduct random forest classification


# load data
data <- read_feather(snakemake@input[["data"]])

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

ggsave(snakemake@output[["plot"]], p_VarIm)
write_feather(data.imputed, snakemake@output[["imputed_data"]])

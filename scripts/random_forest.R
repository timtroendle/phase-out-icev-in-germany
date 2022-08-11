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
  dplyr::select(c(lfdn, acc, app, age, loc, chld, inc, ppref, gen, dlic, job, dfreq, npice, mlib, vera, pger, envc, emat, proba, wac, wev))

str(data_rf)

### impute missing values for NAs (there are very few, except for WEV variable) because random forest does not handle missing values
data.imputed <- rfImpute(acc ~ ., data = data_rf, iter = 6, ntree = 1000)


RFmodel <- randomForest(acc ~ age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + mlib + vera + pger + envc + emat + proba + wac + wev, data = data.imputed, ntree = 1000, mtry = 4, proximity = TRUE, importance = TRUE)
# when turning on importance = TRUE, it computes both mean-decrease-in-impurity and permutation importances

RFmodel
# result: OBB = 18.64 -> 81.36 % were correctly classified 

# importance is the extractor function for variable importance measures (type 1 = mean decrease in accuracy)
imp <- importance(RFmodel, type = 1)       

# make this into ggplot bar chart 
imp <- as.data.frame((imp))
imp$MDA <- imp$MeanDecreaseAccuracy

# rescale, so that max value = 1 & all others are relative to that (absolute values aren't interpretable)
imp$MDA_resc <- imp$MDA / max(imp$MDA)

# add variable names and variable categories, so I can group (facet wrap)& colour-code them later; also to make labels readable
imp$Variable <- c("Age", "Location", "Child(ren)", "Income", "Party pref.", "Gender", "Driving license",
                  "Job in car industry", "Weekly car use", "No. of pure ICEVs", "Approval market liberalism",
                  "Disapproval regulation", "Cultural/econ. significance", "Environmental concern",
                  "Emotional attachment", "Problem attribution", "Willingn. to abandon car", "Willingn. to adopt EV")

imp$Category <- as.factor(c("1) Socio-demographics and politics", "1) Socio-demographics and politics",
                            "1) Socio-demographics and politics", "1) Socio-demographics and politics",
                            "1) Socio-demographics and politics","1) Socio-demographics and politics",
                            "2) Context and habits", "2) Context and habits", "2) Context and habits",
                            "2) Context and habits", "3) Values and beliefs", "3) Values and beliefs",
                            "3) Values and beliefs", "3) Values and beliefs", "3) Values and beliefs",
                            "3) Values and beliefs", "2) Context and habits", "2) Context and habits"))

# order so that they are displayed as most to least important variables per category
imp$Variable <- factor(imp$Variable, levels = c("Child(ren)", "Gender", "Income",  "Age", "Location", "Party pref.",
                                                "Job in car industry", "Weekly car use", "Driving license", "No. of pure ICEVs",
                                                "Willingn. to abandon car", "Willingn. to adopt EV",
                                                "Approval market liberalism", "Disapproval regulation", "Cultural/econ. significance", "Emotional attachment",
                                                "Environmental concern", "Problem attribution"))
imp$Category <- factor(
    imp$Category,
    levels = c("1) Socio-demographics and politics", "2) Context and habits", "3) Values and beliefs")
)

#plot
p_var_im <- (
    ggplot(imp, aes(y = Variable, x = MDA_resc, fill = Category))
    + geom_bar(stat = "identity")
    + labs(x = "", y = "")
    + theme_light()
    + facet_wrap(~Category, scales = "free_y", labeller = label_wrap_gen())
    + theme(legend.position = "none")
    + scale_fill_manual(values = snakemake@params[["colours"]])
)

ggsave(snakemake@output[["plot"]], p_var_im, dpi = 300, width = 8, height = 2.25, units = "in")
write_feather(imp, snakemake@output[["variable_importance"]])
write_feather(data.imputed, snakemake@output[["imputed_data"]])

install.packages("pixiedust", repos = "https://stat.ethz.ch/CRAN/") # cannot be installed using conda at this point

# load packages
library(arrow)
library(tidyverse)    # basic functions
library(cowplot)      # arrange multiple plots in one figure
library(Hmisc)
library(caret)
library(e1071)
library(ROCR)
library(margins)      # compute marginal effects for logistic regression
library(mfx)
library(gmodels)
library(pixiedust)    # create summary table for export
library(ggpubr)
library(broom)


# load data
data.imputed <- read_feather(snakemake@input[["imputed_data"]])


##################################################################################################################################################################################

####################################### binomial logistic regression #############################################################################################################

##################################################################################################################################################################################


# build model, using all the same explanatory variables as for RF classification 
# use imputed dataset from RF so that there are no missing values
BLRmodel <- glm(
    acc ~ age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + lib + pger + emat + proba + wac + wev,
    data = data.imputed,
    family = "binomial"(link = "logit")
)

# view summary statistics
summary.glm(BLRmodel)

# create summary table for export
basetable <- dust(BLRmodel) %>% 
  sprinkle(col = 1:4, round = 2) %>% 
  sprinkle(cols = 5, fn = quote(pvalString(value))) %>% 
  sprinkle(rows = c(1:36), border_color = "black") %>%
  sprinkle_colnames("Term", "Coefficient", "SE", "z-value", "p-value") %>% 
  sprinkle_print_method("console")

basetable

# coefficients
exp(coef(BLRmodel))  
exp(confint(BLRmodel))

# assess model using cross validation
crossValSettings <- trainControl(method = "repeatedcv", number = 10, 
                                 savePredictions = TRUE)
crossVal <- train(
    as.factor(acc) ~ age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + lib + pger + emat + proba + wac + wev,
    data = data.imputed,
    family = "binomial",
    method = "glm",
    trControl = crossValSettings,
    tuneLength = 2
)
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
ME.BLR.sum <- cbind(ME.BLR.sum, c(
    "Age \n(R: <30y)", "Age \n(R: <30y)", "Age \n(R: <30y)", "Age \n(R: <30y)",
    "Child(ren) \n(R: none)",
    "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)",
    "Driving license\n(R: no)",
    "Emotional attachment",
    "Gender \n(R: female)",
    "Income \n(R: <10'000 €)", "Income \n(R: <10'000 €)", "Income \n(R: <10'000 €)", "Income \n(R: <10'000 €)",
    "Income \n(R: <10'000 €)", "Income \n(R: <10'000 €)",
    "Job in car industry\n(R: no)",
    "Liberalism",
    "Location \n(R: urban)", "Location \n(R: urban)",
    "No. of pure ICEVs",
    "Cultural/econ. significance",
    "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)",
    "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)",
    "Problem attribution",
    "Willingn. to abandon car\n(R: low)", "Willingn. to abandon car\n(R: low)",
    "Willingn. to adopt EV\n(R: low)", "Willingn. to adopt EV\n(R: low)")
)


ME.BLR.sum <- cbind(ME.BLR.sum, c(
    sort(levels(data.imputed$age)[-1]),
    sort(levels(data.imputed$chld)[-1]),
    sort(levels(data.imputed$dfreq)[-1]),
    sort(levels(data.imputed$dlic)[-1]),
    "Number",
    sort(levels(data.imputed$gen)[-1]),
    sort(levels(data.imputed$inc)[-1]),
    sort(levels(data.imputed$job)[-1]),
    "Number",
    sort(levels(data.imputed$loc)[-1]),
    "Number",
    "Number",
    sort(levels(data.imputed$ppref)[-1]),
    "Number",
    sort(levels(data.imputed$wac)[-1]),
    sort(levels(data.imputed$wev)[-1]))
)

ME.BLR.sum <- cbind(ME.BLR.sum, c(
    "Socio-demographics", "Socio-demographics", "Socio-demographics", "Socio-demographics",
    "Socio-demographics",
    "Car-related factors", "Car-related factors", "Car-related factors", "Car-related factors",
    "Car-related factors",
    "Values and beliefs",
    "Socio-demographics",
    "Socio-demographics", "Socio-demographics", "Socio-demographics", "Socio-demographics",
    "Socio-demographics", "Socio-demographics",
    "Car-related factors",
    "Values and beliefs",
    "Socio-demographics", "Socio-demographics",
    "Car-related factors",
    "Values and beliefs",
    "Socio-demographics", "Socio-demographics", "Socio-demographics",
    "Socio-demographics", "Socio-demographics", "Socio-demographics",
    "Values and beliefs",
    "Car-related factors", "Car-related factors",
    "Car-related factors", "Car-related factors")
    )

colnames(ME.BLR.sum) <- c("factor", "AME", "SE", "z", "p", "lower", "upper", "Variable", "Value", "Dimension")


# plot marginal effects
ME.BLR.sum$AME <- as.numeric(ME.BLR.sum$AME)


#set different variables as factors, so that they appear in the same order as for RF variable importance plot
ME.BLR.sum$Variable <- factor(ME.BLR.sum$Variable, levels = c(
    "Party pref. \n(R: SPD)", "Location \n(R: urban)", "Age \n(R: <30y)", "Income \n(R: <10'000 €)",
    "Gender \n(R: female)", "Child(ren) \n(R: none)", "Willingn. to adopt EV\n(R: low)",
    "Willingn. to abandon car\n(R: low)", "No. of pure ICEVs", "Driving license\n(R: no)",
    "Weekly car use\n(R: 0)", "Job in car industry\n(R: no)", "Problem attribution",
    "Emotional attachment", "Cultural/econ. significance",
    "Liberalism")
)

### this is nonsense code, but orders Variable values in the order that best suits the plot 
# TODO uncomment
#ME.BLR.sum$Value <- factor(ME.BLR.sum$Value, levels = c("none/other", "AfD", "Die Linke", "FDP", "Die Grünen", "CDU/CSU",
#                                                       "rural", "semi-urban", "60+", "50-59", "40-49", "30-39", "<30",
#                                                        "100'000+", "50-100'000", "30-50'000", "yes", "male", "high", "Number", "medium", "7", "5-6", "3-4", "1-2"))

ME.BLR.sum$Dimension <- factor(ME.BLR.sum$Dimension, levels = c("Socio-demographics", "Car-related factors", "Values and beliefs"))

# plot
p_AME <- (
    ggplot(ME.BLR.sum, aes(y = Value, x = AME, col = Dimension))
    + geom_errorbar(aes(xmin = lower, xmax = upper),
                    width = .4, linewidth = .6,
                    position = position_dodge(0.6))
    + geom_point(stat = "identity", position = position_dodge(0.6), size = 1.5)
    + geom_vline(xintercept = 0, colour = "darkred", linetype = 2, linewidth = 0.75)
    + labs(y = "")
    + theme_light()
    + theme(legend.position = "none")
    + theme(panel.grid.major.y = element_blank())
    + facet_grid(c("Variable"), scales = "free", space = "free", labeller = label_wrap_gen())
    + theme(strip.text.y.right = element_text(angle = 0), strip.text = element_text(colour = "black"))
    + theme(strip.background = element_rect(colour = "grey", fill = "grey90"))
    + scale_colour_manual(values = snakemake@params[["colours"]])
)

ggsave(snakemake@output[["plot"]], p_AME, dpi = 300, width = 8, height = 10, units = "in")
write_feather(ME.BLR.sum, snakemake@output[["summary"]])

### combine RF classification results & BLR model results -> plot together


#plot_grid(p_VarIm, NULL, p_AME, # FIXME currently not possible as random forest plot not available
#          ncol = 1, nrow = 3, 
#          rel_heights = c(.3, .035, 1),
#          labels = c("a.", "b.", ""), 
#          label_size =20)

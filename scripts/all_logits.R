# Fit all possible four binary logits to data and plot marginal means.

library(arrow)
library(margins)
library(ggplot2)

imputed_data <- read_feather(snakemake@input[["imputed_data"]])

imputed_data$y1 <- 0
imputed_data$y2 <- 0
imputed_data$y3 <- 0
imputed_data$y4 <- 0

imputed_data$y1[imputed_data$app >= "Rather disapprove"] <- 1
imputed_data$y2[imputed_data$app >= "Neither approve nor disapprove"] <- 1
imputed_data$y3[imputed_data$app >= "Rather approve"] <- 1
imputed_data$y4[imputed_data$app >= "Strongly approve"] <- 1

predictors = "age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + mlib + vera + pger + envc + emat + proba + wac + wev"

fit1 <- glm(formula = paste("y1 ~ ", predictors), data = imputed_data, family = "binomial" (link = "logit"))
fit2 <- glm(formula = paste("y2 ~ ", predictors), data = imputed_data, family = "binomial" (link = "logit"))
fit3 <- glm(formula = paste("y3 ~ ", predictors), data = imputed_data, family = "binomial" (link = "logit"))
fit4 <- glm(formula = paste("y4 ~ ", predictors), data = imputed_data, family = "binomial" (link = "logit"))

me1 <- summary(margins(fit1))
me2 <- summary(margins(fit2))
me3 <- summary(margins(fit3))
me4 <- summary(margins(fit4))

# Prepare plotting

me1$model_name <- "At least 'Rather disapprove'"
me2$model_name <- "At least 'Neither approve nor disapprove'"
me3$model_name <- "At least 'Rather approve'"
me4$model_name <- "'Strongly approve'"

me <- rbind(me1, me2, me3, me4)

me$model_name <- ordered(
    me$model_name,
    c("At least 'Rather disapprove'", "At least 'Neither approve nor disapprove'",
      "At least 'Rather approve'", "'Strongly approve'")
)

level_names <- c("30-39", "40-49", "50-59", "60+", "yes", "1-2", "3-4", "5-6", "7",
                 "yes", "medium", "high", "medium", "high", "male",
                 "30-50'000", "50-100'000", "100'000+", "yes", "semi-urban", "rural", "medium",
                 "high", "Number", "medium", "high",
                 "AfD", "CDU/CSU", "Die Grünen", "Die Linke", "FDP", "none/other",
                 "medium", "high", "medium", "high", "medium",
                 "high", "medium", "high")
dimensions <- c(
    "Socio-demographics", "Socio-demographics", "Socio-demographics", "Socio-demographics", "Socio-demographics",
    "Car-related factors", "Car-related factors", "Car-related factors", "Car-related factors", "Car-related factors",
    "Values and beliefs", "Values and beliefs", "Values and beliefs", "Values and beliefs", "Socio-demographics",
    "Socio-demographics", "Socio-demographics", "Socio-demographics", "Car-related factors", "Socio-demographics",
    "Socio-demographics", "Values and beliefs", "Values and beliefs", "Car-related factors", "Values and beliefs",
    "Values and beliefs", "Socio-demographics", "Socio-demographics", "Socio-demographics", "Socio-demographics",
    "Socio-demographics", "Socio-demographics", "Values and beliefs", "Values and beliefs", "Values and beliefs",
    "Values and beliefs", "Car-related factors", "Car-related factors", "Car-related factors", "Car-related factors"
)
variable_names <- c(
    "Age \n(R: <30y)", "Age \n(R: <30y)", "Age \n(R: <30y)", "Age \n(R: <30y)", "Child(ren) \n(R: none)",
    "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)",
    "Driving license\n(R: no)", "Emotional attachment\n(R: low)", "Emotional attachment\n(R: low)",
    "Environmental concern\n(R: low)", "Environmental concern\n(R: low)", "Gender \n(R: female)",
    "Income \n(R: <30'000 €)", "Income \n(R: <30'000 €)", "Income \n(R: <30'000 €)", "Job in car industry\n(R: no)",
    "Location \n(R: urban)", "Location \n(R: urban)", "Approval market liberalism\n(R: low)",
    "Approval market liberalism\n(R: low)", "No. of pure ICEVs", "Cultural/econ. significance\n(R: low)",
    "Cultural/econ. significance\n(R: low)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)",
    "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)",
    "Problem attribution\n(R: low)", "Problem attribution\n(R: low)", "Disapproval regulation\n(R: low)",
    "Disapproval regulation\n(R: low)", "Willingn. to abandon car\n(R: low)",
    "Willingn. to abandon car\n(R: low)", "Willingn. to adopt EV\n(R: low)", "Willingn. to adopt EV\n(R: low)"
)


me$level <- rep(level_names, 4)
me$dimension <- rep(dimensions, 4)
me$variable <- rep(variable_names, 4)

me$variable <- factor(me$variable, levels = c(
    "Party pref. \n(R: SPD)", "Location \n(R: urban)", "Age \n(R: <30y)", "Income \n(R: <30'000 €)",
    "Gender \n(R: female)", "Child(ren) \n(R: none)", "Willingn. to adopt EV\n(R: low)",
    "Willingn. to abandon car\n(R: low)", "No. of pure ICEVs", "Driving license\n(R: no)",
    "Weekly car use\n(R: 0)", "Job in car industry\n(R: no)", "Problem attribution\n(R: low)",
    "Environmental concern\n(R: low)", "Emotional attachment\n(R: low)", "Cultural/econ. significance\n(R: low)",
    "Disapproval regulation\n(R: low)", "Approval market liberalism\n(R: low)")
)
me$level <- factor(me$level, levels = c(
    "none/other", "AfD", "Die Linke", "FDP", "Die Grünen", "CDU/CSU",
    "rural", "semi-urban", "60+", "50-59", "40-49", "30-39", "<30",
    "100'000+", "50-100'000", "30-50'000", "yes", "male", "high", "Number",
    "medium", "7", "5-6", "3-4", "1-2")
)

write_feather(me, snakemake@output[["data"]])

gg_me <- (
    ggplot(me, aes(y = level, x = AME))
    + geom_errorbar(aes(xmin = lower, xmax = upper),
                    width = .4, size = .6,
                    position = position_dodge(0.6))
    + geom_point(stat = "identity", position = position_dodge(0.6), size = 1.5)
    + geom_vline(xintercept = 0, colour = "darkred", linetype = 2, size = 0.75)
    + labs(y = "")
    + theme_light()
    + theme(legend.position = "none")
    + theme(panel.grid.major.y = element_blank())
    + facet_grid(
        rows = vars(variable),
        cols = vars(model_name),
        scales = "free_y",
        space = "free",
        labeller = label_wrap_gen()
    )
    + theme(strip.text.y.right = element_text(angle = 0), strip.text = element_text(colour = "black"))
    + theme(strip.background = element_rect(colour = "grey", fill = "grey90"))
)

ggsave(snakemake@output[["plot"]], gg_me, dpi = 300, width = 8, height = 10, units = "in")

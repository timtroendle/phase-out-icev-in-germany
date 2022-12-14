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

preds <- "age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + lib + pger + emat + proba + wac + wev"

fit1 <- glm(formula = paste("y1 ~ ", preds), data = imputed_data, family = "binomial" (link = "logit"))
fit2 <- glm(formula = paste("y2 ~ ", preds), data = imputed_data, family = "binomial" (link = "logit"))
fit3 <- glm(formula = paste("y3 ~ ", preds), data = imputed_data, family = "binomial" (link = "logit"))
fit4 <- glm(formula = paste("y4 ~ ", preds), data = imputed_data, family = "binomial" (link = "logit"))

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

level_names <- c(
    sort(levels(imputed_data$age)[-1]),
    sort(levels(imputed_data$chld)[-1]),
    sort(levels(imputed_data$dfreq)[-1]),
    sort(levels(imputed_data$dlic)[-1]),
    "Number",
    sort(levels(imputed_data$gen)[-1]),
    sort(levels(imputed_data$inc)[-1]),
    sort(levels(imputed_data$job)[-1]),
    "Number",
    sort(levels(imputed_data$loc)[-1]),
    "Number",
    "Number",
    sort(levels(imputed_data$ppref)[-1]),
    "Number",
    sort(levels(imputed_data$wac)[-1]),
    sort(levels(imputed_data$wev)[-1])
)
dimensions <- c(
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
    "Car-related factors", "Car-related factors"
)
variable_names <- c(
    "Age \n(R: <30y)", "Age \n(R: <30y)", "Age \n(R: <30y)", "Age \n(R: <30y)",
    "Child(ren) \n(R: none)",
    "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)", "Weekly car use\n(R: 0)",
    "Driving license\n(R: no)",
    "Emotional attachment (1-5)",
    "Gender \n(R: female)",
    "Income \n(R: <10'000 €)", "Income \n(R: <10'000 €)", "Income \n(R: <10'000 €)", "Income \n(R: <10'000 €)",
    "Income \n(R: <10'000 €)", "Income \n(R: <10'000 €)",
    "Job in car industry\n(R: no)",
    "Liberalism (1-5)",
    "Location \n(R: urban)", "Location \n(R: urban)",
    "No. of pure ICEVs",
    "Cultural/econ. significance (1-5)",
    "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)",
    "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)", "Party pref. \n(R: SPD)",
    "Problem attribution (1-5)",
    "Willingn. to abandon car\n(R: none)", "Willingn. to abandon car\n(R: none)",
    "Willingn. to adopt EV\n(R: none)", "Willingn. to adopt EV\n(R: none)"
)

me$level <- rep(level_names, 4)
me$dimension <- rep(dimensions, 4)
me$variable <- rep(variable_names, 4)

me$variable <- factor(me$variable, levels = c(
    "Party pref. \n(R: SPD)",
    "Location \n(R: urban)",
    "Age \n(R: <30y)",
    "Gender \n(R: female)",
    "Income \n(R: <10'000 €)",
    "Child(ren) \n(R: none)",
    "Willingn. to abandon car\n(R: none)",
    "Willingn. to adopt EV\n(R: none)",
    "Weekly car use\n(R: 0)",
    "No. of pure ICEVs",
    "Driving license\n(R: no)",
    "Job in car industry\n(R: no)",
    "Problem attribution (1-5)",
    "Liberalism (1-5)",
    "Emotional attachment (1-5)",
    "Cultural/econ. significance (1-5)"
))

me$dimension <- factor(
    me$dimension,
    levels = c("Socio-demographics", "Car-related factors", "Values and beliefs")
)

write_feather(me, snakemake@output[["data"]])

gg_me <- (
    ggplot(me, aes(y = level, x = AME, col = dimension))
    + geom_errorbar(aes(xmin = lower, xmax = upper),
                    width = .4, linewidth = .6,
                    position = position_dodge(0.6))
    + geom_point(stat = "identity", position = position_dodge(0.6), size = 1.5)
    + geom_vline(xintercept = 0, colour = "darkred", linetype = 2, linewidth = 0.75)
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
    + scale_colour_manual(values = snakemake@params[["colours"]])
)

ggsave(snakemake@output[["plot"]], gg_me, dpi = 300, width = 8, height = 10, units = "in")

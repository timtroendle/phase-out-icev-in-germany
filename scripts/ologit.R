# Fit all possible four binary logits to data and plot marginal means.
install.packages("pixiedust", repos = "https://stat.ethz.ch/CRAN/") # cannot be installed using conda at this point
install.packages("brant", repos = "https://stat.ethz.ch/CRAN/") # cannot be installed using conda at this point

library(arrow)
library(dplyr)
library(pixiedust)
library(MASS)
library(marginaleffects)
library(dplyr)
library(brant)
library(ggplot2)

imputed_data <- read_feather(snakemake@input[["imputed_data"]])

preds <- "age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + lib + pger + emat + proba + wac + wev"

olr_model <- polr(formula = paste("app ~ ", preds), data = imputed_data)

basetable <- dust(olr_model) %>%
    sprinkle(col = 2:4, round = 2) %>%
    sprinkle_print_method("console")

capture.output(print(basetable), file = snakemake@output[["coefficients"]])

parameter_significance <- as.data.frame(drop1(olr_model, test = "Chisq"))
write.csv(parameter_significance, snakemake@output[["significance"]])

# Perform Brant test for parallel regression assumptions
brant_coefs <- brant(olr_model, by.var = FALSE)
write.csv(brant_coefs, snakemake@output[["brant"]])

# Average marginal effects
ame <- avg_slopes(olr_model) %>%
    arrange(group, term)

# Prepare plotting

ame$model_name <- ordered(
    ame$group,
    c("Strongly disapprove", "Rather disapprove", "Neither approve nor disapprove",
      "Rather approve", "Strongly approve")
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

ame$level <- rep(level_names, 5)
ame$dimension <- rep(dimensions, 5)
ame$variable <- rep(variable_names, 5)

ame$variable <- factor(ame$variable, levels = c(
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

ame$dimension <- factor(
    ame$dimension,
    levels = c("Socio-demographics", "Car-related factors", "Values and beliefs")
)

write_feather(ame, snakemake@output[["ame"]])

gg_me <- (
    ggplot(ame, aes(y = level, x = estimate, col = dimension))
    + geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                    width = .4, linewidth = .6,
                    position = position_dodge(0.6))
    + geom_point(stat = "identity", position = position_dodge(0.6), size = 1.5)
    + geom_vline(xintercept = 0, colour = "darkred", linetype = 2, linewidth = 0.75)
    + labs(y = "", x = "AME")
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

ggsave(snakemake@output[["plot"]], gg_me, dpi = 300, width = 9, height = 11, units = "in")

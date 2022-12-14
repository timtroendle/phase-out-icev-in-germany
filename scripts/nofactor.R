library(arrow)
library(margins)
library(ggplot2)
library(tidyverse)

data <- read_feather(snakemake@input[["data"]])

data$v_43 <- as.numeric(data$v_43)
data$v_44 <- as.numeric(data$v_44)
data$v_45 <- as.numeric(data$v_45)
data$v_46 <- as.numeric(data$v_46)
data$v_47 <- as.numeric(data$v_47)
data$v_48 <- as.numeric(data$v_48)
data$v_49 <- as.numeric(data$v_49)
data$v_51 <- as.numeric(data$v_51)
data$v_52 <- as.numeric(data$v_52)
data$v_53 <- as.numeric(data$v_53)
data$v_91 <- as.numeric(data$v_91)

data$y1 <- 0
data$y2 <- 0
data$y3 <- 0
data$y4 <- 0

data$y1[data$app >= "Rather disapprove"] <- 1
data$y2[data$app >= "Neither approve nor disapprove"] <- 1
data$y3[data$app >= "Rather approve"] <- 1
data$y4[data$app >= "Strongly approve"] <- 1

preds <- "age + loc + chld + inc + ppref + gen + dlic + job + dfreq + npice + v_45 + v_46 + v_47 + 
          v_51 + v_52 + v_53 + v_49 + v_91 + v_48 + v_43 + v_44 + wac + wev"
factor_variables <- c("v_45", "v_46", "v_47", "v_51", "v_52", "v_53", "v_49", "v_91", "v_48", "v_43", "v_44")

fit1 <- glm(formula = paste("y1 ~ ", preds), data = data, family = "binomial" (link = "logit"))
fit2 <- glm(formula = paste("y2 ~ ", preds), data = data, family = "binomial" (link = "logit"))
fit3 <- glm(formula = paste("y3 ~ ", preds), data = data, family = "binomial" (link = "logit"))
fit4 <- glm(formula = paste("y4 ~ ", preds), data = data, family = "binomial" (link = "logit"))

me1 <- margins_summary(fit1, variables = factor_variables)
me2 <- margins_summary(fit2, variables = factor_variables)
me3 <- margins_summary(fit3, variables = factor_variables)
me4 <- margins_summary(fit4, variables = factor_variables)

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

dimensions <- c(
    "Liberalism", "Liberalism", 
    "Cultural significance", "Cultural significance", "Cultural significance", 
    "Problem attribution", "Problem attribution", 
    "Emotional attachment", "Emotional attachment", "Emotional attachment",
    "Problem attribution"
)

me$dimension <- rep(dimensions, 4)

me$dimension <- factor(
    me$dimension,
    levels = c("Problem attribution", "Cultural significance", "Liberalism", "Emotional attachment")
)

write_feather(me, snakemake@output[["data"]])

gg_me <- (
    ggplot(me, aes(y = factor, x = AME))
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
        rows = vars(dimension),
        cols = vars(model_name),
        scales = "free_y",
        space = "free",
        labeller = label_wrap_gen()
    )
    + theme(strip.text.y.right = element_text(angle = 0), strip.text = element_text(colour = "black"))
    + theme(strip.background = element_rect(colour = "grey", fill = "grey90"))
)

ggsave(snakemake@output[["plot"]], gg_me, dpi = 300, width = 8, height = 4, units = "in")

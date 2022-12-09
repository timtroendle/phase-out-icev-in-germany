# load packages
library(arrow)
library(tidyverse)
library(lavaan)


data <- read_feather(snakemake@input[["imputed_data"]])

model <- "
    proba =~ v_49 + v_91 + v_48
    pger  =~ v_45 + v_46 + v_47
    emat  =~ v_51 + v_52 + v_53
    lib   =~ v_43 + v_44
"

fit <- cfa(model, data = data, orthogonal = FALSE)

capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE), file = snakemake@output[["summary"]])

factors <- lavPredict(fit)
data <- data %>% bind_cols(factors)

write_feather(data, snakemake@output[["imputed_data_with_factors"]])

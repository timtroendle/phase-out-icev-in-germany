# load packages
library(arrow)
library(tidyverse)
library(randomForest)


# load data
data <- read_feather(snakemake@input[["data"]])

set.seed(12345)

# select all relevant variables, to be included in the model
data_rf <- data %>%
  dplyr::select(c(lfdn, acc, app, age, loc, chld, inc, ppref, gen, dlic, job, dfreq, npice,
                  wac, wev, v_43, v_44, v_48, v_49, v_91, v_45, v_46, v_47, v_51, v_52, v_53))

### impute missing values for NAs (there are very few, except for WEV variable)
data_imputed <- rfImpute(acc ~ ., data = data_rf, iter = 6, ntree = 1000)

write_feather(data_imputed, snakemake@output[["imputed_data"]])

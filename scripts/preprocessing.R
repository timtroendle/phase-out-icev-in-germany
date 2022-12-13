# load packages
library(arrow)
library(tidyverse)    # basic functions
library(writexl)      # export to excel file


# load data
data <- read.csv(snakemake@input[["data"]], header = TRUE, sep = ";")

# select relevant variables
# see pdf document (download from Unipark) for associated variable names and response option)
data <- data %>%
  dplyr::select(c(lfdn, p_0001, v_17, v_16, v_114, v_2, v_4, v_5, v_6, v_3, v_7, v_8, v_9, v_102,
                  v_104, v_106, v_108, v_18, v_19, v_20, v_21, v_22, v_23, v_24, v_72, v_73, v_74,
                  v_75, v_76, v_77, v_80, v_79, v_82, v_83, v_84, v_85, v_86, v_88, v_90, v_43, v_44,
                  v_45, v_46, v_47, v_48, v_49, v_91, v_51, v_52, v_53, c_0001, v_92, v_96, v_100))

# download dataset with only relevant set of variables, for figshare
write_xlsx(data, snakemake@output[["figshare_data"]])


############################################# clean data ##############################################################

# recode missing data (-77 was default of Unipark)
data[data == "-77"] <- NA

inverted_likert_item <- function(column) {
    recode_factor(column, `5` = "Strongly disapprove", `4` = "Rather disapprove",
                  `3` = "Neither approve nor disapprove", `2` = "Rather approve",
                  `1` = "Strongly approve", .ordered = TRUE)
}

data <- data %>% mutate(
    acc = recode_factor(v_16, `4` = 0, `5` = 0, `1` = 1, `2` = 1, `3` = 1),
    app = inverted_likert_item(v_16)
)

#################################################### explanatory variables ############################################

############################################### socio-demographic variables ###########################################

# following data cleaning entails: renaming variables, replacing zeros that stand for NAs by NAs,
# and setting variables as factors

data <- data %>% mutate(
    age = recode_factor(v_114, `1` = "<30", `2` = "30-39", `3` = "40-49", `4` = "50-59", `5` = "60+",
                        .ordered = TRUE),
    loc = recode_factor(v_2, `1` = "urban", `2` = "semi-urban", `3` = "rural", .ordered = FALSE),
    chldn = recode(v_4, `1` = 0, `2` = 1, `3` = 2, `4` = 3, `5` = 5, `6` = 5),
    chld = recode_factor(v_4, `1` = "no", `2` = "yes", `3` = "yes", `4` = "yes", `5` = "yes", `6` = "yes"),
    inc = recode_factor(v_5, `1` = "<10'000", `2` = "10-30'000", `3` = "30-50'000", `4` = "50-70'000",
                        `5` = "70-100'000", `6` = "100-150'000", `7` = "150'000+", .ordered = TRUE),
    ppref = recode_factor(v_6, `2` = "SPD", `1` = "CDU/CSU", `3` = "Die Grünen", `4` = "Die Linke",
                          `5` = "FDP", `6` = "AfD", `7` = "none/other", .ordered = FALSE),
    gen = recode_factor(v_3, `2` = "female", `3` = "female", `1` = "male", , .ordered = FALSE)
)

############################# car related factors #####################################################################

data <- data %>% mutate(
    job = recode_factor(v_8, `2` = "no", `1` = "yes", .ordered = FALSE),
    dfreq = recode_factor(v_9, `1` = "0", `2` = "1-2", `3` = "3-4", `4` = "5-6", `5` = "7", .ordered = TRUE),
    dlic = recode_factor(v_7, `2` = "no", `1` = "yes", .ordered = FALSE),
    ndie = recode(v_102, `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5),
    npet = recode(v_104, `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5),
    nhyb = recode(v_106, `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5),
    nele = recode(v_108, `0` = 0, `1` = 0, `2` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5),
    ncar = ndie + npet + nhyb + nele,
    nice = ndie + npet + nhyb,
    npice = ndie + npet,
    wac_str  = case_when(v_79 == "1" ~ "low", v_79 == "0" ~ "medium", v_72 == "2" ~ "high"),
    wev_str  = case_when(v_90 == "1" ~ "low", v_90 == "0" ~ "medium", v_82 == "2" ~ "high"),
    wac = recode_factor(wac_str, "low" = "low", "medium" = "medium", "high" = "high", .ordered = TRUE),
    wev = recode_factor(wev_str, "low" = "low", "medium" = "medium", "high" = "high", .ordered = TRUE),
)

###################################### beliefs ########################################################################

data <- data %>% mutate(
    v_43 = inverted_likert_item(v_43),
    v_44 = inverted_likert_item(v_44),
    v_45 = inverted_likert_item(v_45),
    v_46 = inverted_likert_item(v_46),
    v_47 = inverted_likert_item(v_47),
    v_48 = inverted_likert_item(v_48),
    v_49 = inverted_likert_item(v_49),
    v_51 = inverted_likert_item(v_51),
    v_52 = inverted_likert_item(v_52),
    v_53 = inverted_likert_item(v_53),
    v_91 = inverted_likert_item(v_91),
)

write_feather(data, snakemake@output[["preprocessed_data"]])

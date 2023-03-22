library(arrow)
library(dplyr)


test_policy_instruments <- function(data, path_to_output_accept, path_to_output_approve) {
    df_policy <- data %>%
        select(c(v_54, v_56, v_57, v_58, v_59, v_60, v_112, v_62))
    df_accept <- df_policy %>%
        mutate_all(accept)
    df_approve <- df_policy %>%
        mutate_all(approve)
    p_accept <- test_differences(df_accept)
    p_approve <- test_differences(df_approve)
    write.csv(p_accept, path_to_output_accept)
    write.csv(p_approve, path_to_output_approve)

}

accept <- function(x) (x >= 3)
approve <- function(x) (x > 3)

test_differences <- function(df) {
    policy_instruments <- c("v_54", "v_56", "v_57", "v_58", "v_59", "v_60", "v_112", "v_62")
    n_policy_instruments <- length(policy_instruments)
    p_values <- matrix( # empty matrix
        numeric(n_policy_instruments * n_policy_instruments),
        nrow = n_policy_instruments,
        ncol = n_policy_instruments)
    for (i in seq_along(policy_instruments)) {
        for (j in seq_along(policy_instruments)) {
            instrument_i <- df[, policy_instruments[i]]
            instrument_j <- df[, policy_instruments[j]]
            p_values[i, j] <- mcnemar.test(instrument_i, instrument_j)$p.value
        }
    }
    p_values <- data.frame(p_values, row.names = policy_instruments)
    colnames(p_values) <- policy_instruments
    p_values
}

test_policy_instruments(
        data = read_feather(snakemake@input[["data"]]),
        path_to_output_accept = snakemake@output[["accept"]],
        path_to_output_approve = snakemake@output[["approve"]]
    )

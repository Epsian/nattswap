#' nattswap_means
#'
#' @param nattswap_long Output of \code{nattswap_transform}
#'
#' @return A table of means used for Z score tests.
#' @export
#'
#' @examples
nattswap_means = function(nattswap_long) {
  # make df for output
  means_table = data.frame(
    "sim_num" = 1:length(unique(nattswap_long$simulation_num)),
    "degree_mean_true" = NA,
    "degree_sd_true" = NA,
    "evc_mean_true" = NA,
    "evc_sd_true" = NA,
    "n_bet_mean_true" = NA,
    "n_bet_sd_true" = NA,
    "closeness_mean_true" = NA,
    "closeness_sd_true" = NA,
    "degree_mean_false" = NA,
    "degree_sd_false" = NA,
    "evc_mean_false" = NA,
    "evc_sd_false" = NA,
    "n_bet_mean_false" = NA,
    "n_bet_sd_false" = NA,
    "closeness_mean_false" = NA,
    "closeness_sd_false" = NA,
    stringsAsFactors = FALSE
  )

  for (sim in unique(nattswap_long$simulation_num)) {
    # subset by one simulation at a time
    .working = nattswap_long[nattswap_long$simulation_num == sim,]

    ## degree

    # get degree mean for only swapped_att == TRUE
    means_table[means_table$sim_num == sim, "degree_mean_true"] = mean(.working[.working[, ncol(.working)] == TRUE, "degree"])

    # get degree mean for only swapped_att == FALSE
    means_table[means_table$sim_num == sim, "degree_mean_false"] = mean(.working[.working[, ncol(.working)] == FALSE, "degree"])

    # get degree sd for only swapped_att == TRUE
    means_table[means_table$sim_num == sim, "degree_sd_true"] = sd(.working[.working[, ncol(.working)] == TRUE, "degree"])

    # get degree sd for only swapped_att == FALSE
    means_table[means_table$sim_num == sim, "degree_sd_false"] = sd(.working[.working[, ncol(.working)] == FALSE, "degree"])

    ## evc

    # get evc mean for only swapped_att == TRUE
    means_table[means_table$sim_num == sim, "evc_mean_true"] = mean(.working[.working[, ncol(.working)] == TRUE, "evc"])

    # get evc mean for only swapped_att == FALSE
    means_table[means_table$sim_num == sim, "evc_mean_false"] = mean(.working[.working[, ncol(.working)] == FALSE, "evc"])

    # get evc sd for only swapped_att == TRUE
    means_table[means_table$sim_num == sim, "evc_sd_true"] = sd(.working[.working[, ncol(.working)] == TRUE, "evc"])

    # get evc sd for only swapped_att == FALSE
    means_table[means_table$sim_num == sim, "evc_sd_false"] = sd(.working[.working[, ncol(.working)] == FALSE, "evc"])

    ## norm_betweenness

    # get norm_betweenness mean for only swapped_att == TRUE
    means_table[means_table$sim_num == sim, "n_bet_mean_true"] = mean(.working[.working[, ncol(.working)] == TRUE, "norm_betweenness"])

    # get norm_betweenness mean for only swapped_att == FALSE
    means_table[means_table$sim_num == sim, "n_bet_mean_false"] = mean(.working[.working[, ncol(.working)] == FALSE, "norm_betweenness"])

    # get norm_betweenness sd for only swapped_att == TRUE
    means_table[means_table$sim_num == sim, "n_bet_sd_true"] = sd(.working[.working[, ncol(.working)] == TRUE, "norm_betweenness"])

    # get norm_betweenness sd for only swapped_att == FALSE
    means_table[means_table$sim_num == sim, "n_bet_sd_false"] = sd(.working[.working[, ncol(.working)] == FALSE, "norm_betweenness"])

    ## closeness

    # get closeness mean for only swapped_att == TRUE
    means_table[means_table$sim_num == sim, "closeness_mean_true"] = mean(.working[.working[, ncol(.working)] == TRUE, "closeness"])

    # get closeness mean for only swapped_att == FALSE
    means_table[means_table$sim_num == sim, "closeness_mean_false"] = mean(.working[.working[, ncol(.working)] == FALSE, "closeness"])

    # get closeness sd for only swapped_att == TRUE
    means_table[means_table$sim_num == sim, "closeness_sd_true"] = sd(.working[.working[, ncol(.working)] == TRUE, "closeness"])

    # get closeness sd for only swapped_att == FALSE
    means_table[means_table$sim_num == sim, "closeness_sd_false"] = sd(.working[.working[, ncol(.working)] == FALSE, "closeness"])

  }
  return(means_table)
}

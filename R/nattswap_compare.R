#' nattswap_compare
#'
#' @param get_att_swap_sig_results Result of \code{get_att_swap_sig}
#' @param metric_name Character. Either "degree", "evc", "n_bet", or "closeness"
#' @param swapped_att Character. Name of swapped attribute.
#'
#' @return A ggplot2 plot showing real vs simulated means
#' @export
#'
#' @examples
nattswap_compare = function(get_att_swap_sig_results, metric_name, swapped_att){

  # parse what metric is being plotted
  if(metric_name == "degree"){x_int = sim[["sig"]][["z-report"]][["r_degree"]]; p_x = "Mean Degree"; m_title = "Simulated vs. Actual Mean Degree"; metric_colname = "degree_mean_true"}
  if(metric_name == "evc"){x_int = sim[["sig"]][["z-report"]][["r_evc"]]; p_x = "Mean Eigenvector Centrality"; m_title = "Simulated vs. Actual Mean EVC"; metric_colname = "evc_mean_true"}
  if(metric_name == "n_bet"){x_int = sim[["sig"]][["z-report"]][["r_n_bet"]]; p_x = "Mean Normalized Betweenness"; m_title = "Simulated vs. Actual Mean Norm. Betweenness"; metric_colname = "n_bet_mean_true"}
  if(metric_name == "closeness"){x_int = sim[["sig"]][["z-report"]][["r_closeness"]]; p_x = "Mean Closeness"; m_title = "Simulated vs. Actual Mean Closeness"; metric_colname = "closeness_mean_true"}

  # plot
  ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["means"]][, metric_colname])) + geom_histogram(color="black", fill="white", bins = 50) + geom_vline((aes(xintercept=x_int)), color="black", linetype="dashed", size=1) + labs (x = p_x, y = "Frequency") + ggtitle(paste0(m_title, " for ", swapped_att, " == TRUE"))

}

#' nattswap_compare
#'
#' @param get_att_swap_sig_results Result of \code{get_att_swap_sig}
#' @param metric_name Character. Either "degree", "evc", "n_bet", or "closeness". Defualts to "all" which will display all metrics.
#' @param swapped_att Character. Name of swapped attribute.
#' @param all_nrow Only if \code{metric_name == "all}. How many rows do you want the plot to have?
#' @param all_ncol Only if \code{metric_name == "all}. How many columns do you want the plot to have?
#' @param font_family Font family to use in plots.
#'
#' @return A ggplot2 plot showing real vs simulated means
#' @export
#'
#' @examples
nattswap_compare = function(get_att_swap_sig_results, metric_name = "all", swapped_att, all_nrow = 1, all_ncol = NULL, font_family = NULL){

  # if one specific metric is requested
  if(metric_name != "all"){
    # parse what metric is being plotted
    if(metric_name == "degree"){x_int = get_att_swap_sig_results[["sig"]][["z-report"]][["r_degree"]]; p_x = "Mean Degree"; m_title = "Simulated vs. Actual Mean Degree"; metric_colname = "degree_mean_true"; zscore = get_att_swap_sig_results[["sig"]][["z-report"]][["z_degree"]]}
    if(metric_name == "evc"){x_int = get_att_swap_sig_results[["sig"]][["z-report"]][["r_evc"]]; p_x = "Mean Eigenvector Centrality"; m_title = "Simulated vs. Actual Mean EVC"; metric_colname = "evc_mean_true"; zscore = get_att_swap_sig_results[["sig"]][["z-report"]][["z_evc"]]}
    if(metric_name == "n_bet"){x_int = get_att_swap_sig_results[["sig"]][["z-report"]][["r_n_bet"]]; p_x = "Mean Normalized Betweenness"; m_title = "Simulated vs. Actual Mean Norm. Betweenness"; metric_colname = "n_bet_mean_true"; zscore = get_att_swap_sig_results[["sig"]][["z-report"]][["z_n_bet"]]}
    if(metric_name == "closeness"){x_int = get_att_swap_sig_results[["sig"]][["z-report"]][["r_closeness"]]; p_x = "Mean Closeness"; m_title = "Simulated vs. Actual Mean Closeness"; metric_colname = "closeness_mean_true"; zscore = get_att_swap_sig_results[["sig"]][["z-report"]][["z_closeness"]]}

    # plot
    out_plot = ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["sig"]][["means"]][, metric_colname])) + geom_histogram(color="black", fill="white", bins = 50) + geom_vline((aes(xintercept=x_int)), color="black", linetype="dashed", size=1) + labs (x = p_x, y = "Frequency") + ggtitle(paste0(m_title, " for ", swapped_att, " == TRUE")) + ggplot2::labs(caption = paste0("Z-Score = ", round(zscore, digits = 3))) + ggplot2::theme(text=element_text(family= font_family))

    return(out_plot)
  }

  out_plot = gridExtra::grid.arrange(

    ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["sig"]][["means"]][, "degree_mean_true"])) + geom_histogram(color="black", fill="white", bins = 50) + geom_vline((aes(xintercept=get_att_swap_sig_results[["sig"]][["z-report"]][["r_degree"]])), color="black", linetype="dashed", size=1) + labs (x = "Mean Degree", y = "Frequency") + ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["sig"]][["z-report"]][["z_degree"]], digits = 3))) + ggplot2::theme(text=element_text(family= font_family)),

    ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["sig"]][["means"]][, "evc_mean_true"])) + geom_histogram(color="black", fill="white", bins = 50) + geom_vline((aes(xintercept=get_att_swap_sig_results[["sig"]][["z-report"]][["r_evc"]])), color="black", linetype="dashed", size=1) + labs (x = "Mean Eigenvector Centrality", y = "Frequency") + ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["sig"]][["z-report"]][["z_evc"]], digits = 3))) + ggplot2::theme(text=element_text(family= font_family)),

    ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["sig"]][["means"]][, "n_bet_mean_true"])) + geom_histogram(color="black", fill="white", bins = 50) + geom_vline((aes(xintercept=get_att_swap_sig_results[["sig"]][["z-report"]][["r_n_bet"]])), color="black", linetype="dashed", size=1) + labs (x = "Mean Normalized Betweenness", y = "Frequency") + ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["sig"]][["z-report"]][["z_n_bet"]], digits = 3))) + ggplot2::theme(text=element_text(family= font_family)),

    ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["sig"]][["means"]][, "closeness_mean_true"])) + geom_histogram(color="black", fill="white", bins = 50) + geom_vline((aes(xintercept=get_att_swap_sig_results[["sig"]][["z-report"]][["r_closeness"]])), color="black", linetype="dashed", size=1) + labs (x = "Mean Closeness", y = "Frequency") + ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["sig"]][["z-report"]][["z_closeness"]], digits = 3))) + ggplot2::theme(text=element_text(family= font_family)),

    nrow = all_nrow, ncol = all_ncol, top = ggpubr::text_grob(paste0("Simulated vs. Observed for ", swapped_att), family = font_family)

  )

  return(out_plot)
}

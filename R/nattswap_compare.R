#' nattswap_compare
#'
#' @param get_att_swap_sig_results Result of \code{get_att_swap_sig}
#' @param metric_name Character. A vector of "degree", "evc", "bet", "n_bet", or "closeness". Defualts to "all" which will display all metrics.
#' @param swapped_att Character. Name of swapped attribute.
#' @param all_nrow How many rows do you want the plot to have?
#' @param all_ncol How many columns do you want the plot to have?
#' @param font_family Font family to use in plots.
#' @param yaxis Vector of 2 integers. Lower and upper limit of y axis.
#'
#' @return A ggplot2 plot showing real vs simulated means
#' @export
#'
#' @examples
nattswap_compare = function(get_att_swap_sig_results, metric_name = "all", swapped_att, all_nrow = 1, all_ncol = NULL, font_family = NULL, yaxis = c(NA, NA)){

  if(length(yaxis) != 2){stop("yaxis need to be vector with length 2!")}

  plot_list = list()

  if("degree" %in% metric_name | "all" %in% metric_name){
    plot_list[["degree"]] = ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["means"]][, "degree_mean_true"])) +
      geom_histogram(color="black", fill="white", bins = 50) +
      geom_vline((aes(xintercept=get_att_swap_sig_results[["z-report"]][["r_degree"]])), color="black", linetype="dashed", size=1) +
      labs (x = "Mean Degree", y = "Frequency") +
      ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["z-report"]][["z_degree"]], digits = 3))) +
      ggplot2::theme(text=element_text(family= font_family))
    }

  if("evc" %in% metric_name | "all" %in% metric_name){
    plot_list[["evc"]] = ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["means"]][, "evc_mean_true"])) +
      geom_histogram(color="black", fill="white", bins = 50) +
      geom_vline((aes(xintercept=get_att_swap_sig_results[["z-report"]][["r_evc"]])), color="black", linetype="dashed", size=1) +
      labs (x = "Mean Eigenvector Centrality", y = "Frequency") +
      ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["z-report"]][["z_evc"]], digits = 3))) +
      ggplot2::theme(text=element_text(family= font_family))
    }

  if("closeness" %in% metric_name | "all" %in% metric_name){
    plot_list[["closeness"]] = ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["means"]][, "closeness_mean_true"])) +
      geom_histogram(color="black", fill="white", bins = 50) +
      geom_vline((aes(xintercept=get_att_swap_sig_results[["z-report"]][["r_closeness"]])), color="black", linetype="dashed", size=1) +
      labs (x = "Mean Closeness", y = "Frequency") +
      ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["z-report"]][["z_closeness"]], digits = 3))) +
      ggplot2::theme(text=element_text(family= font_family))
    }

  if("bet" %in% metric_name | "all" %in% metric_name){
    plot_list[["bet"]] = ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["means"]][, "bet_mean_true"])) +
      geom_histogram(color="black", fill="white", bins = 50) +
      geom_vline((aes(xintercept=get_att_swap_sig_results[["z-report"]][["r_bet"]])), color="black", linetype="dashed", size=1) +
      labs (x = "Mean Betweenness", y = "Frequency") +
      ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["z-report"]][["z_bet"]], digits = 3))) +
      ggplot2::theme(text=element_text(family= font_family))
    }

  if("n_bet" %in% metric_name | "all" %in% metric_name){
    plot_list[["n_bet"]] = ggplot(get_att_swap_sig_results[["means"]], aes(x=get_att_swap_sig_results[["means"]][, "n_bet_mean_true"])) +
      geom_histogram(color="black", fill="white", bins = 50) +
      geom_vline((aes(xintercept=get_att_swap_sig_results[["z-report"]][["r_n_bet"]])), color="black", linetype="dashed", size=1) +
      labs (x = "Mean Normalized Betweenness", y = "Frequency") +
      ggplot2::labs(caption = paste0("Z-Score = ", round(get_att_swap_sig_results[["z-report"]][["z_n_bet"]], digits = 3))) +
      ggplot2::theme(text=element_text(family= font_family))
  }

  if(!all(is.na(yaxis))){
    plot_list = lapply(plot_list, function(iplot){
      iplot = iplot + ggplot2::coord_cartesian(ylim = yaxis)

      return(iplot)
    })
  }

  out_plot = gridExtra::grid.arrange(grobs = plot_list, nrow = all_nrow, ncol = all_ncol, top = ggpubr::text_grob(paste0("Simulated vs. Observed for ", swapped_att), family = font_family))

  return(out_plot)
}

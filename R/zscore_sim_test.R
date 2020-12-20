#' nattswap_z
#'
#' To test for differance between real and simulated networks.
#'
#' @param combined_net_att_tableined_net_att_table Result of \code{combined_net_att_tableine_net_att_sims}
#' @param means_table Result of \code{nattswap_means}
#' @param swapped_att Character. Name of attribute swapped in \code{run_att_swap_sims}.
#'
#' @return A printout of results.
#' @export
#'
#' @examples
nattswap_z = function(combined_att_swaps, means_table, swapped_att) {
  out_list = list()
  out_list["swapped_att"] = swapped_att

  # Real
  out_list["r_degree"] = mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "degree"])
  out_list["r_evc"] = mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "evc"])
  out_list["r_bet"] = mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "betweenness"])
  out_list["r_n_bet"] = mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "norm_betweenness"])
  out_list["r_closeness"] = mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "closeness"])
  out_list["r_max_cohesion"] = mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "max_cohesion"])
  out_list["r_nestedness"] = mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "nestedness"])

  # Simulated
  out_list["s_degree"] = mean(means_table$degree_mean_true)
  out_list["s_evc"] = mean(means_table$evc_mean_true)
  out_list["s_bet"] = mean(means_table$bet_mean_true)
  out_list["s_n_bet"] = mean(means_table$n_bet_mean_true)
  out_list["s_closeness"] = mean(means_table$closeness_mean_true)
  out_list["s_max_cohesion"] = mean(means_table$max_cohesion_mean_true)
  out_list["s_nestedness"] = mean(means_table$nestedness_mean_true)

  # Z = (Observed Mean - Mean of of random networks) /  standard deviation of random networks
  # Answer will be positive or negative and within +/-  Standard Deviations above or below the mean

  # Z Scores
  out_list["z_degree"] = (mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "degree"]) - mean(means_table$degree_mean_true)) / sd(means_table$degree_mean_true)
  out_list["z_evc"] = (mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "evc"]) - mean(means_table$evc_mean_true)) / sd(means_table$evc_mean_true)
  out_list["z_bet"] = (mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "betweenness"]) - mean(means_table$bet_mean_true)) / sd(means_table$bet_mean_true)
  out_list["z_n_bet"] = (mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "norm_betweenness"]) - mean(means_table$n_bet_mean_true)) / sd(means_table$n_bet_mean_true)
  out_list["z_closeness"] = (mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "closeness"]) - mean(means_table$closeness_mean_true)) / sd(means_table$closeness_mean_true)
  out_list["z_max_cohesion"] = (mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "max_cohesion"]) - mean(means_table$max_cohesion_mean_true)) / sd(means_table$max_cohesion_mean_true)
  out_list["z_nestedness"] = (mean(combined_att_swaps[combined_att_swaps[, swapped_att] == TRUE, "nestedness"]) - mean(means_table$nestedness_mean_true)) / sd(means_table$nestedness_mean_true)

  return(new("nattswap_z", out_list))
}

setClass("nattswap_z", representation("list"))

setMethod("show", "nattswap_z", function(object) {
  cat("nattswap significance results\n\n")

  cat("Means for", object$swapped_att, "== TRUE in real network\n")
  cat("\tdegree\t\t\teigenvector\t\tbetweenness\t\tnormalized betweenness\tcloseness\tmax_cohesion\tnestedness\n")
  cat(
    "\t",
    object$r_degree,
    "\t\t",
    object$r_evc,
    "\t\t",
    object$r_bet,
    "\t\t",
    object$r_n_bet,
    "\t\t",
    object$r_closeness,
    "\t\t",
    object$r_max_cohesion,
    "\t\t",
    object$r_nestedness,
    "\n"
  )
  cat("\n")

  cat("Means for",
      object$swapped_att,
      "== TRUE in simulated network\n")
  cat("\tdegree\t\t\teigenvector\t\tbetweenness\t\tnormalized betweenness\tcloseness\n")
  cat(
    "\t",
    object$s_degree,
    "\t\t",
    object$s_evc,
    "\t\t",
    object$s_bet,
    "\t\t",
    object$s_n_bet,
    "\t\t",
    object$s_closeness,
    "\t\t",
    object$s_max_cohesion,
    "\t\t",
    object$s_nestedness,
    "\n"
  )
  cat("\n")

  cat("Z scores for",
      object$swapped_att,
      "between real and simulated means\n")
  cat("\tdegree\t\t\teigenvector\t\tbetweenness\t\tnormalized betweenness\tcloseness\n")
  cat(
    "\t",
    object$z_degree,
    "\t\t",
    object$z_evc,
    "\t\t",
    object$z_bet,
    "\t\t",
    object$z_n_bet,
    "\t\t",
    object$z_closeness,
    "\t\t",
    object$z_max_cohesion,
    "\t\t",
    object$z_nestedness,
    "\n"
  )
  cat("\n")
})

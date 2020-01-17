#' nattswap_z
#'
#' To test for differance between real and simulated networks.
#'
#' @param means_table Result of \code{nattswap_means}
#' @param swapped_att Character. Name of attribute swapped in \code{run_att_swap_sims}.
#'
#' @return A printout of results.
#' @export
#'
#' @examples
nattswap_z = function(means_table, swapped_att) {
  out_list = list()
  out_list["swapped_att"] = swapped_att

  # Real
  out_list["r_degree"] = mean(comb[comb[, swapped_att] == TRUE, "degree"])
  out_list["r_evc"] = mean(comb[comb[, swapped_att] == TRUE, "evc"])
  out_list["r_n_bet"] = mean(comb[comb[, swapped_att] == TRUE, "norm_betweenness"])
  out_list["r_closeness"] = mean(comb[comb[, swapped_att] == TRUE, "closeness"])

  # Simulated
  out_list["s_degree"] = mean(means_table$degree_mean_true)
  out_list["s_evc"] = mean(means_table$evc_mean_true)
  out_list["s_n_bet"] = mean(means_table$n_bet_mean_true)
  out_list["s_closeness"] = mean(means_table$closeness_mean_true)

  # Z = (Observed Mean - Mean of of random networks) /  standard deviation of random networks
  # Answer will be positive or negative and within +/-  Standard Deviations above or below the mean

  # Z Scores
  out_list["z_degree"] = (mean(comb[comb[, swapped_att] == TRUE, "degree"]) - mean(means_table$degree_mean_true)) / sd(means_table$degree_mean_true)
  out_list["z_evc"] = (mean(comb[comb[, swapped_att] == TRUE, "evc"]) - mean(means_table$evc_mean_true)) / sd(means_table$evc_mean_true)
  out_list["z_n_bet"] = (mean(comb[comb[, swapped_att] == TRUE, "norm_betweenness"]) - mean(means_table$n_bet_mean_true)) / sd(means_table$n_bet_mean_true)
  out_list["z_closeness"] = (mean(comb[comb[, swapped_att] == TRUE, "closeness"]) - mean(means_table$closeness_mean_true)) / sd(means_table$closeness_mean_true)

  return(new("nattswap_z", out_list))
}

setClass("nattswap_z", representation("list"))

setMethod("show", "nattswap_z", function(object) {
  cat("nattswap significance results\n\n")

  cat("Means for", object$swapped_att, "== TRUE in real network\n")
  cat("\tdegree\t\teigenvector\t\tnormalized betweenness\t\tcloseness\n")
  cat(
    "\t",
    object$r_degree,
    "\t\t",
    object$r_evc,
    "\t\t",
    object$r_n_bet,
    "\t\t",
    object$r_closeness,
    "\n"
  )
  cat("\n")

  cat("Means for",
      object$swapped_att,
      "== TRUE in simulated network\n")
  cat("\tdegree\t\teigenvector\t\tnormalized betweenness\t\tcloseness\n")
  cat(
    "\t",
    object$s_degree,
    "\t\t",
    object$s_evc,
    "\t\t",
    object$s_n_bet,
    "\t\t",
    object$s_closeness,
    "\n"
  )
  cat("\n")

  cat("Z scores for",
      object$swapped_att,
      "between real and simulated means\n")
  cat("\tdegree\t\teigenvector\t\tnormalized betweenness\t\tcloseness\n")
  cat(
    "\t",
    object$z_degree,
    "\t\t",
    object$z_evc,
    "\t\t",
    object$z_n_bet,
    "\t\t",
    object$z_closeness,
    "\n"
  )
  cat("\n")
})

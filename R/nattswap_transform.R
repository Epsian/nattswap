#' nattswap_transform
#'
#' To turn the combined nattswap from \code{combine_net_att_sims} from wide to long
#'
#' @param comb_nattswaps The object returned from \code{combine_net_att_sims}.
#'
#' @return A long format dataframe to be used for Z score testing.
#' @export
#'
#' @examples
nattswap_transform = function(comb_nattswaps) {
  # what columns are we interested in comparing?
  .int_col = colnames(comb_nattswaps)[!grepl("sim_\\d+_", colnames(comb_nattswaps), perl = TRUE)]

  # reshape from wide to long
  comb_test = stats::reshape(
    comb_nattswaps,
    direction = "long",
    varying = list(names(comb_nattswaps)[(length(.int_col) + 1):ncol(comb_nattswaps)]),
    idvar = "id",
    timevar = "simulation_num"
  )

  colnames(comb_test)[ncol(comb_test)] = paste0("sim_", gsub("sim_\\d+_", "", colnames(comb_nattswaps))[ncol(comb_test)])

  return(comb_test)
}

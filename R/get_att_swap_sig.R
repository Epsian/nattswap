#' get_att_swap_sig
#'
#' Transform data and report differance between real and simulated networks.
#'
#' @param combined_att_swaps The result of \code{combine_net_att_sims}.
#' @param swapped_att Character. The name of the attribute that was swapped.
#'
#' @return A report of differances.
#' @export
#'
#' @examples
get_att_swap_sig = function(combined_att_swaps, swapped_att){

  long = nattswap_transform(combined_att_swaps)
  means = nattswap_means(long)
  nattswap_z(combined_att_swaps, means, swapped_att)

  out_final = list("means" = means, "z-report" = nattswap_z(combined_att_swaps, means, swapped_att))
  return(out_final)
}

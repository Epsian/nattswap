#' combine_net_att_sims
#'
#' to combine the various network simulaions into one dataframe for averaging
#'
#' @param ... Multiple returns of \code{run_att_swap}.
#' @param swapped_attribute Character string. Name of \code{attribute} given to \code{run_att_swap}.
#'
#' @return A datafarme with the network metrics and simulations results of all swap simulations.
#' @export
#'
#' @examples
combine_net_att_sims = function(..., swapped_attribute){

  # get the objects passed in
  dot_list = list(...)

  # test they are a simulation return
  if(all(sapply(dot_list, attr, "att_swap_sims_result"))){stop("Input objects are not outputs of run_att_swap!")}

  # get the original networks and test for equivilance
  orig = lapply(dot_list, FUN = function(siml){siml[["original_net"]]})
  if(all(sapply(orig, identical, orig[[1]]))){stop("Not all 'original_net' networks are identical!")}

  # save one original
  orig = orig[[1]]

  # combine simulation networks
  tsims = unlist(lapply(dot_list, FUN = function(x){x[["simulations"]]}), recursive = FALSE)

  # make output df
  sim_df = orig

  # make matrix of attributes that were swapped
  att_matrix = sapply(tsims, FUN = function(sim) {
    sim[, swapped_attribute]
  })

  # add column names
  colnames(att_matrix) = paste0("sim_", 1:length(tsims))

  # combine with original stats
  sim_df = cbind(sim_df, att_matrix)

  return(sim_df)
}














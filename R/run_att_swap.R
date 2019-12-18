#' run_att_swap_sims
#'
#' @param net_to_swap An object of type \code{network}. Passed to \code{net_att_swap}.
#' @param attribute A pre-defined attribute within \code{net_to_swap}. Passed to \code{net_attr_swap}.
#' @param swaps Integer. Passed to net_attr_swap. Defaults to 10000.
#' @param sims How many simulated networks to generate. Each will be saved as an .rda file within save_dir. Defaults to 50.
#' @param cores Integer. How many cores to use. Defaults to 1.
#' @param save_preface Character. What to preface the above simulations with when saved. Defaults to sim.
#' @param save_dir A directory filepath. Where the individual network simulations and cumulative dataframe will be saved. Defaults to ./network_sims. Only used if save_preface is not NA.
#' @param verbose Logical. Should the code print more verbose progress messages. Defaults to FALSE.
#'
#' @return A list containing the original network with network metrics and all simulations.
#' @export
#'
#' @examples
run_att_swap_sims = function(net_to_swap, attribute, swaps = 10000, sims = 50, cores = 1L, save_preface = NA, save_dir = "./network_sims", verbose = FALSE){

  # make dir to save into. showWrnings == FALSE so that it won't complain if directroy already exists.
  print("---------- Creating save directory.")
  if(!is.na(save_preface)){dir.create(save_dir, showWarnings = FALSE)}

  # get base network stats
  print("---------- Creating Network stats dataframe.")

  # copy current network attributes to df
  net_df = data.frame(id = 1:length(net_to_swap%v%"vertex.names"), stringsAsFactors = FALSE)
  for(coln in network::list.vertex.attributes(net_to_swap)){net_df[coln] = net_to_swap%v%coln}

  # get basic network stats
  net_df$degree = sna::degree(net_to_swap, gmode="graph", diag=FALSE, ignore.eval=TRUE)
  net_df$evc = sna::evcent(net_to_swap, gmode="graph", diag=FALSE, ignore.eval=TRUE)
  net_df$betweenness = sna::betweenness(net_to_swap, gmode="graph", diag=FALSE, cmode="undirected", ignore.eval=TRUE)
  net_df$norm_betweenness = net_df$betweenness / ((length(net_to_swap%v%"vertex.names") - 1) * (length(net_to_swap%v%"vertex.names") - 2) / 2)
  net_df$closeness = sna::closeness(net_to_swap, gmode="graph", diag=FALSE, cmode="undirected", ignore.eval=TRUE)

  # run the simultions
  print("---------- Starting simulations.")
  sim_dfs = parallel::mclapply(1:sims, mc.cores = cores, FUN = function(sim){

    # Run one simulation
    single_sim = net_att_swap(net_to_swap = net_to_swap, attribute = attribute, swaps = swaps, verbose = verbose)

    # make save name (WEEKDAY, MONTH, DAY, YEAR, HOUR, MIN, SEC)
    save_name = paste0(save_preface, "_", sim, "_", format(Sys.time(), "%a_%B_%e_%y_%H_%M_%S"), ".rda")

    # save sim if not NA
    if(!is.na(save_preface)){saveRDS(single_sim, paste0(save_dir, "/", save_name))}

    # create df for later analysis
    sim_df = data.frame("id" = 1:length(single_sim%v%"vertex.names"), stringsAsFactors = FALSE)
    for(coln in network::list.vertex.attributes(single_sim)){sim_df[coln] = single_sim%v%coln}

    # status message
    print(paste0("---------- Simulation #", sim, " done!"))

    return(sim_df)
  })

  # make output list
  out = list(net_df, sim_dfs)
  names(out) = c("original_net", "simulations")
  attributes(out) = list("att_swap_sims_result" = TRUE)
  print("-------------------- Finished!")

  return(out)
}

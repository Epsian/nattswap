#' Edge Swap
#'
#' @param net_to_swap An object of type `network`. This is the network you want the swaps to be conducted on. All characteristics of this network, aside from the attribute you want to swap, will be the same in the result.
#' @param swaps Integer. The number of edge swaps you want to conduct. Defaults to 500.
#' @param verbose Logical. Would you like the function to display status messages? Defaults to `TRUE`.
#' @param edge_color Color as string or R color ID. Defaults to `NULL`.
#'
#' @return A network with edges randomly swapped `swaps` times with optional color assignments for the edges.
#' @export
#'
#' @examples
edge_swap = function(net_to_swap, swaps = 500, verbose = FALSE, edge_color = NULL){

  # make copy of network to prevent edits in place to origonal by network package code
  edgeswap_network = net_to_swap

  # swap loop (must be loop as swaps are contingent on previous swaps)
  if(verbose == TRUE){print("Starting edge swaps!")}
  # set up loop
  i = 1
  # set up fail counter
  fails = 0
  # start loop
  while(i < (swaps + 1)){

    # status messages
    if((i %% 100 == 0) & verbose == TRUE){print(paste0(".......... Working on swap #", i, "."))}

    # select two random edge ids from the network's edgelist and get nodes for those edges
    edges_to_swap = network::as.edgelist(edgeswap_network)[sample(x = (length(as.edgelist(edgeswap_network))/2), size = 2, replace = FALSE), ]

    # check exclusivity
    if(sum(edges_to_swap %in% edges_to_swap[1]) > 1){fails = fails + 1; next}
    if(sum(edges_to_swap %in% edges_to_swap[2]) > 1){fails = fails + 1; next}
    if(sum(edges_to_swap %in% edges_to_swap[3]) > 1){fails = fails + 1; next}
    if(sum(edges_to_swap %in% edges_to_swap[4]) > 1){fails = fails + 1; next}

    # check if potential new edges ealready exist, if so skip
    if(length(get.edgeIDs(x = edgeswap_network, v = edges_to_swap[1,1], alter = edges_to_swap[2,2])) != 0){fails = fails + 1; next}
    if(length(get.edgeIDs(x = edgeswap_network, v = edges_to_swap[2,1], alter = edges_to_swap[1,2])) != 0){fails = fails + 1; next}

    # get internal IDs for those edges
    edges_id_remove = c(network::get.edgeIDs(x = edgeswap_network, v = edges_to_swap[1,1], alter = edges_to_swap[1,2]),
                        network::get.edgeIDs(x = edgeswap_network, v = edges_to_swap[2,1], alter = edges_to_swap[2,2]))

    # delete old edges
    network::delete.edges(x = edgeswap_network, eid = edges_id_remove)

    # create new swapped edges
    network::add.edge(x = edgeswap_network, tail = edges_to_swap[1, 1], head = edges_to_swap[2, 2], edge.check = TRUE, names.eval = list("color"), vals.eval = list("color" = edge_color))
    network::add.edge(x = edgeswap_network, tail = edges_to_swap[2, 1], head = edges_to_swap[1, 2], edge.check = TRUE, names.eval = list("color"), vals.eval = list("color" = edge_color))

    # advance
    i = i + 1
  }
  if(verbose == TRUE){print(paste0("Edges swapped. There were ", fails, " aborted swap(s)."))}
  # output
  return(edgeswap_network)
}

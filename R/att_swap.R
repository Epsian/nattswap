#' net_att_swap
#'
#' To swap the attributes of a single network object
#'
#' @param net_to_swap An object of type network. This is the network you want the swaps to be conducted on. All characteristics of this network, aside from the attribute you want to swap, will be the same in the result.
#' @param attribute A pre-defined attribute within net_to_swap. This is the attribute that already exists within the network object you want to randomize within the network. You must be able to call this attribute using net_to_swap\%v\%\\\"attribute\\\".
#' @param swaps Integer. The number of swaps between nodes you want to conduct.
#' @param verbose Logical. Would you like the function to display status messages? Defaults to FALSE.
#'
#' @return A network object with the attribute swapped a number of times equal to swaps.
#' @export
#'
#' @examples
net_att_swap = function(net_to_swap, attribute, swaps, verbose = FALSE) {

  # create internal function for the sake of message handling
  net_att_swap_int = function(net_to_swap, attribute) {
    # select two random nodes from the net_to_swap, without replacement
    random_nodes = sample(1:length(net_to_swap$oel), 2, replace = FALSE)

    # are attributes different? If so swap, else skip
    if (network::get.vertex.attribute(x = net_to_swap, attrname = attribute)[random_nodes[1]] != network::get.vertex.attribute(x = net_to_swap, attrname = attribute)[random_nodes[2]]) {
      # print a message (if verbose == TRUE) that nodes are different
      message(
        paste0(
          "---------- Attribute (",
          attribute,
          ") of nodes #",
          random_nodes[1],
          " and #",
          random_nodes[2],
          " are different, swapping!"
        )
      )

      # Save current attributes of random nodes
      old_r1_attr = network::get.vertex.attribute(x = net_to_swap, attrname = attribute)[random_nodes[1]]
      old_r2_attr = network::get.vertex.attribute(x = net_to_swap, attrname = attribute)[random_nodes[2]]

      # Swap the attributes!
      network::set.vertex.attribute(net_to_swap,
                           attrname = attribute,
                           value = old_r2_attr,
                           v = random_nodes[1])
      network::set.vertex.attribute(net_to_swap,
                           attrname = attribute,
                           value = old_r1_attr,
                           v = random_nodes[2])

    } else {
      # Print a message (if verbose == TRUE) saying nodes are the same
      message(
        paste0(
          "---------- Attribute (",
          attribute,
          ") of nodes #",
          random_nodes[1],
          " and #",
          random_nodes[2],
          " are identical, skipping!"
        )
      )
    }

    # Return the swapped network to the larger function
    return(net_to_swap)
  }

  # --------------------------------------------------------------------------------------

  # Run the code defined in net_att_swap_int(), and handle messages based on verbose
  if (verbose == TRUE) {
    for (swap in 1:swaps) {
      # Run the swaps a number of times == swaps. This is the verbose version.
      net_to_swap = net_att_swap_int(net_to_swap, attribute)
    }
  } else {
    for (swap in 1:swaps) {
      # Run the swaps a number of times == swaps. This is the silent version, due to suppressMessages().
      net_to_swap = suppressMessages(net_att_swap_int(net_to_swap, attribute))
    }
  }

  # Return the swapped network.
  return(net_to_swap)
}

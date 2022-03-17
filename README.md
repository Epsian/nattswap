# nattswap

[![DOI](https://zenodo.org/badge/228912484.svg)](https://zenodo.org/badge/latestdoi/228912484)

## Overview

*nattswap* is a small package designed to create network simulations in R which randomly swap attributes or edges in a network. It is based on the `statenet` family of network packages. If you are using `igraph` networks, I suggest you translate them using the `intergraph` package.

*nattswap* has two main groups of functions, one for attributes and the other for edges. You can see a full example application below.

### Attributes

* `run_att_swap_sims()` will take in a `network` object, and run the simulations. It can save the simulations it creates if you want them.
* `combine_net_att_sims()` will reformat the results of `run_att_swap_sims()`, and combine the results of multiple if you did not run all simulations at once. Because `swapped_attribute` comes after ... in this function, **it must be explicitly defined**.
* `get_att_swap_sig()` will report the difference between the real and simulated networks.
* `nattswap_compare()` will plot the results and show how the actual means of network metrics compare to simulations.

### Edges

* `edge_swap()` will take a `network` object, and randomly "re-wire" the edges, swapping edge pairs.

## Installation

To install this package, please use:

```
# install.packages("devtools")
devtools::install_github("epsian/nattswap")
```

## Example Network

Here is an example of the package in action. We'll use the *flomarriage* data from the *ergm* package (part of statnet).

```
# Load required libraries 
library(statnet)
library(nattswap)

# Load in example data from the *ergm* package
data(florentine, package = "ergm")

# Make a categorical variable to swap
# We will split the number of priorates (seats on the civic council) into high and low groups
flomarriage%v%"power_cat" = flomarriage%v%"priorates" > 40

# Run the simulations
# Here I ask for 500 simulations of with 500 swaps each, on the newly created "power_cat" variable.
flo_sims = run_att_swap_sims(net_to_swap = flomarriage, attribute = "power_cat", swaps = 500, sims = 500, verbose = FALSE)

# Combine simulations
# This is useful if you ran multiple simulations batches using `run_att_swap_sims`
flo_combined_sims = combine_net_att_sims(flo_sims, swapped_attribute = "power_cat")

# Get the significance of the differences between groups of "power_cat"
flo_sim_sig = get_att_swap_sig(flo_combined_sims, "power_cat")

# Make histograms to show the above differences
nattswap_compare(flo_sim_sig, metric_name = c("degree", "evc"), swapped_att = "power_cat")
```

## Known limitations

* Currently only works on networks with one component
* Only works for undirected networks
* Will always only return values for degree, eigenvector centrality, betweenness, normalized betweenness, closeness, max cohesion, and nestedness

## To Do

* add examples to help files
* fix code to remove warnings about network's "%V%"

## Attribution

Based on the process shown in: Yang, Yang, Omar Lizardo, Dong Wang, Yuxiao Dong, Aaron D. Striegel, David Hachen, and Nitesh V. Chawla. 2016. “Gender Differences in Communication Behaviors, Spatial Proximity Patterns, and Mobility Habits.” ArXiv:1607.06740 [Physics].

Based on 'statnet' project software (http://statnetproject.org). For license and citation information see http://statnetproject.org/attribution

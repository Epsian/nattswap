# nattswap

[![DOI](https://zenodo.org/badge/228912484.svg)](https://zenodo.org/badge/latestdoi/228912484)

## Overview

nattswap is a small package designed to create network simulations in R which randomly swap an attribute of the network, and test for significance. It has three main functions.

* `run_att_swap_sims()` will take in a `network` object, and run the simulations. It can save the simulations it creates if you want them.
* `combine_net_att_sims()` will reformat the results of `run_att_swap_sims()`, and combine the results of multiple if you did not run all simulations at once. Because `swapped_attribute` comes after ... in this function, **it must be explicitly defined**.
* `get_att_swap_sig()` will report the difference between the real and simulated networks.
* `nattswap_compare()` will plot the results and show how the actual means of network metrics compare to simulations.
* `edge_swap()` will take a `network` object, and randomly "re-wire" the edges, swapping edge pairs.

## Installation

To install this package, please use:

```
# install.packages("devtools")
devtools::install_github("epsian/nattswap")
```

## Known limitations

* Currently only works on networks with one component
* Only works for undirected networks

## To Do

* add examples to help files
* fix code to remove warnings about network's "%V%"

## Attribution

Based on the process shown in: Yang, Yang, Omar Lizardo, Dong Wang, Yuxiao Dong, Aaron D. Striegel, David Hachen, and Nitesh V. Chawla. 2016. “Gender Differences in Communication Behaviors, Spatial Proximity Patterns, and Mobility Habits.” ArXiv:1607.06740 [Physics].

Based on 'statnet' project software (http://statnetproject.org). For license and citation information see http://statnetproject.org/attribution

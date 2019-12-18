# nattswap

## Overview

nattswap is a small package designed to create network simulations in R which randomly swap an attribute of the network, and test for significance. It has one main function, and a helper function.

* `run_att_swap_sims()` will take in a `network` object, and run the simulations. It can save the simulations it creates if you want them.
* `combine_net_att_sims()` can be used if you created you networks in multiple backes for speed or time purposes.

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

* Add Z score code significance tests code
* add examples to help files
* fix code to remove warings about network's "%V%"

## Attribution

Based on the process shown in: Yang, Yang, Omar Lizardo, Dong Wang, Yuxiao Dong, Aaron D. Striegel, David Hachen, and Nitesh V. Chawla. 2016. “Gender Differences in Communication Behaviors, Spatial Proximity Patterns, and Mobility Habits.” ArXiv:1607.06740 [Physics].

Based on 'statnet' project software (http://statnetproject.org). For license and citation information see http://statnetproject.org/attribution

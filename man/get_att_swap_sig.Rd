% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_att_swap_sig.R
\name{get_att_swap_sig}
\alias{get_att_swap_sig}
\title{get_att_swap_sig}
\usage{
get_att_swap_sig(combined_att_swaps, swapped_att)
}
\arguments{
\item{combined_att_swaps}{The result of \code{combine_net_att_sims}.}

\item{swapped_att}{Character. The name of the attribute that was swapped.}
}
\value{
A report of differances.
}
\description{
Transform data and report differance between real and simulated networks.
}

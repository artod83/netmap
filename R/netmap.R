#' netmap: Plot \code{network} and \code{igraph} objects on a \code{sf} map
#' using \code{ggplot2}
#'
#' The netmap package extends the \code{ggnetwork} package by providing functions to
#' plot networks, with vertices usually representing objects with a spatial
#' attribute (cities, regions, countries, users with location data attached etc.)
#' on a map, provided by a \code{sf} object (which in turn is able to represent
#' more or less all spatial data available). Networks and maps need not have the
#' same set of elements: if they don't, only the intersection will be represented.
#'
#' @section netmap functions:
#' The main function is \code{\link{ggnetmap}}, which produces a \code{data.frame}
#' that is then used as \code{data} within \code{ggplot2} calls. For those wishing
#' to use the \code{\link[network]{plot.network}} or the
#' \code{\link[igraph]{plot.igraph}} function to plot the network
#' (without overlaying it on an \code{sf} object), both a custom layout function,
#' \code{\link{network.layout.extract_coordinates}}, and a wrapper that provides
#' convenient manipulation of \code{network} and \code{sf} objects,
#' \code{\link{netmap_plot}}, are available.
#'
#' @name netmap
NULL
#> NULL

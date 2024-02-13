#' Reduces network to vertices present on the map
#'
#' Removes vertices from a \code{network} or \code{igraph} object which are not
#' present in the link vector produced by \code{\link{link_network_map}} or
#' \code{\link{link_network_map2}}.
#'
#' @param n A \code{network} or \code{igraph} object.
#' @param link A vector with the identifiers of the vertices to keep.
#' @param n_name Name of the vertex attribute to filter on.
#'
#' @return A \code{network} or \code{igraph} object with only the vertices
#' listed in \code{link}.
#'
reduce_to_map <- function(n, link, n_name){
  if(!rlang::is_installed("network") && !rlang::is_installed("igraph") ) {
    stop(
      "Either package \"network\" or package \"igraph\" must be installed to
      use this function.",
      call. = FALSE
    )
  }
  if(!is_network(n)) stop("The network is not a network or igraph object")

  if(inherits(n, "network")) {
    n_vertices=get("n", pos=get("gal", pos=n))
    if(length(link)>n_vertices) stop("Link vector exceeds network size")

    if (nchar(n_name)<1) {
      stop(paste0(n_name, " is not a valid attribute name"))
    }

    if (is.null(network::get.vertex.attribute(n, n_name, null.na=FALSE))) {
      stop(paste0("Vertex attribute ", n_name, " doesn't exist in network object"))
    }

    n2=n #delete.vertices operates in place, so we create a new network to return it later
    ids=(1:n_vertices)[!(network::get.vertex.attribute(n, n_name) %in% link)]
    network::delete.vertices(n2, ids)
    return(n2)
  } else if(inherits(n, "igraph")) {
    n_vertices=igraph::gorder(n)
    if(length(link)>n_vertices) stop("Link vector exceeds network size")

    if (nchar(n_name)<1) {
      stop(paste0(n_name, " is not a valid attribute name"))
    }

    if (is.null(igraph::vertex_attr(n, n_name))) {
      stop(paste0("Vertex attribute ", n_name, " doesn't exist in igraph object"))
    }


    ids=(1:n_vertices)[!(igraph::vertex_attr(n, n_name) %in% link)]
    n2=igraph::delete.vertices(n, ids)
    return(n2)
  } else {
    stop("Incorrect input class")
  }
}

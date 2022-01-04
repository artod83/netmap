#' Plot a network object with a layout based on an \code{sf} object
#'
#' Wrapper for \code{\link[network]{plot.network}} using a custom network layout that extracts
#' coordinates of centroids from a \code{sf} object. Only vertices with a
#' corresponding feature are plotted.
#'
#' @param n A \code{network} object.
#' @param m A \code{sf} object.
#' @param lkp An optional lookup table.
#' @param m_name Optional \code{character}, name of field in \code{m} and of
#' column in \code{lkp}.
#' @param n_name Optional \code{character}, name of vertex attribute in \code{n}
#' and of column in \code{lkp}.
#' @param ... Additional parameters passed to \code{\link[network]{plot.network}}.
#'
#' @return A plot of the network.
#' @export
#'
#' @examples
#' # net=network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
#' # network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
#' # wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
#' # "01010000204071000000000000801A084100000000AC5C1441",
#' # "01010000204071000000000000801A044100000000AC5C1241",
#' # "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
#' # map=st_sf(id=c("a1", "b2", "c3", "d4"), st_as_sfc(wkb, EWKB=TRUE))
#' # lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
#' # netmap_plot(net, map, lkptbl, "id", "name")
netmap_plot <- function(
  n,
  m,
  lkp=NULL,
  m_name=NULL,
  n_name="vertex.names",
  ...
){
  if(!rlang::is_installed(c("network", "sf"))) {
    stop(
      "Packages \"network\", and \"sf\" must be installed to use this function.",
      call. = FALSE
    )
  }
  #check whether network and sf objects can be linked
  if(is.null(lkp) && !is.null(m_name)) {
    linked=link_network_map(m, n, m_name, n_name)
  } else if (!is.null(lkp))
  {
    linked=link_network_map2(m, n, lkp, m_name, n_name)
  } else
  {
    stop("Either a lookup table or a field name for the sf object must be supplied")
  }

  if(length(linked)==1 && linked==-1) {
    stop("Incorrect inputs, see messages above")
  }

  #prune network to include only objects that can be linked
  n2=reduce_to_map(n, linked$n, n_name)

  #obtain coordinates
  m2=m[get(m_name, pos=m) %in% linked$m,]
  features_ordered=get(m_name, pos=lkp)[match(linked$n, get(n_name, lkp))]
  m2=m2[match(features_ordered, get(m_name, pos=m2)),]

  #plot
  network::plot.network(n2, mode="extract_coordinates", layout.par=list(sf=m2), ...)
}

#' Layout of a network based on a \code{sf} object
#'
#' Custom layout for \code{\link[network]{plot.network}}, extracting coordinates
#' of vertices from a \code{sf} object.
#'
#' @param n A \code{network} object.
#' @param layout.par A \code{list} of layout parameters (the only one implemented is
#' \code{layout.par$sf}, an \code{sf} object whose rows match the order of
#' vertices in \code{n}).
#'
#' @return A matrix whose rows contain the x,y coordinates of the vertices of \code{n}.
#' @export
#'
#' @examples
#' # net=network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
#' # network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
#' # wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
#' # "01010000204071000000000000801A084100000000AC5C1441",
#' # "01010000204071000000000000801A044100000000AC5C1241",
#' # "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
#' # map=st_sf(id=c("a1", "b2", "c3", "d4"), st_as_sfc(wkb, EWKB=TRUE))
#' # lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
#' # ggnetmap(net, map, lkptbl, "id", "name")
network.layout.extract_coordinates <- function(
  n,
  layout.par){
  if(is.null(layout.par$sf) || !is_sf(layout.par$sf)) {
    stop("No valid sf object supplied")
  } else {
    m=layout.par$sf
  }
  if(!is_network(n)) stop("The network is not a network object")
  coords=do.call(rbind, sf::st_geometry(suppressWarnings(sf::st_centroid(m))))
  return(coords)
}

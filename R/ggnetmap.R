#this is to test with plot.network
network.layout.extract_coordinates <- function(n, layout.par){
  if(is.null(layout.par$sf) || !is_sf(layout.par$sf)) {
    stop("No valid sf object supplied")
  } else {
    m=layout.par$sf
  }
  if(!is_network(n)) stop("The network is not a network object")
  coords=do.call(rbind, st_geometry(st_centroid(m)))
  return(coords)
}

#the same, but for ggnetwork - NO NEED, JUST SET THE COORD PARAMETER IN GPLOT

#' The main function of the netmap package, takes a network object and a sf object and prepares them for the subsequent call to fortify.network().
#'
#' @param n a network object
#' @param m a sf object
#' @param lkp an optional lookup table
#'
#' @return
#' @export
#'
#' @examples
ggnetmap <- function(n, m, lkp=NULL, m_name=NULL, n_name="vertex.names"){
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

  #obtain coordinates

  #(optional) plot linked features only (plot of whole sf object to be done outside of ggnetmap)
  #call ggnetwork
}

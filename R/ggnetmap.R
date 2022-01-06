#change ggnetmap to ggnetmap.network, then add ggnetmap.igraph (same, but for igraph objects) and change ggnetmap to a wrapper for both

#' Fortify a network over a map
#'
#' Link a \code{network} or \code{igraph} and a \code{sf} object in a
#' \code{data.frame} for subsequent representation on a plot using \code{ggplot2}.
#'
#' Using a \code{network} or \code{igraph} and a \code{sf} object as inputs,
#' with an optional lookup table (a \code{data.frame}) in case the IDs don't
#' match, produces a \code{data.frame} that can be used with \code{ggnetwork}'s
#' \code{\link[ggnetwork]{geom_edges}} and \code{\link[ggnetwork]{geom_nodes}}
#' functions to represent the network as overlayed on a \code{sf} object in a
#' \code{ggplot2} graph. Only vertices with a corresponding feature in the
#' \code{sf} object are included.
#'
#' @param n A \code{network} or \code{igraph} object.
#' @param m A \code{sf} object.
#' @param lkp An optional lookup table.
#' @param m_name Optional \code{character}, name of field in \code{m} and of
#' column in \code{lkp}.
#' @param n_name Optional \code{character}, name of vertex attribute in \code{n}
#' and of column in \code{lkp}.
#' @param scale Whether coordinates should be scaled (defaults to FALSE since the
#' network should be overlayed with the non-scaled \code{sf} object).
#' @param ... Additional parameters passed to \code{\link[ggnetwork]{fortify}}.
#'
#' @return A data frame, produced by \code{\link[ggnetwork]{fortify}}, which can be
#' used as data source in \code{ggplot2} graphs.
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
ggnetmap <- function(
  n,
  m,
  lkp=NULL,
  m_name=NULL,
  n_name="vertex.names",
  scale=FALSE,
  ...){
  res=check_network_sf(n, m, lkp, m_name, n_name)
  n2=res[[1]]
  linked=res[[2]]

  #obtain coordinates
  coords=data.frame(do.call(rbind, sf::st_geometry(suppressWarnings(sf::st_centroid(m)))),
                    id=get(m_name, pos=m))[get(m_name, pos=m) %in% linked$m,]
  if(is.null(lkp)) { #in this case m_name and n_name have the same keys, so both
    #vectors in linked have the same elements, just ordered differently
    features_ordered=coords$id[match(linked$n, linked$m)]
  }
  else {
    features_ordered=get(m_name, pos=lkp)[match(linked$n, get(n_name, lkp))]
  }
  coords=as.matrix(coords[match(features_ordered, coords$id),1:2])


  #call fortify (it will automatically call either fortify.network or
  # fortify.igraph)
  fortified_df=ggnetwork::fortify(n2, layout=coords, scale=scale, ...)

  #add m_name identifiers as well
  features_ordered2=data.frame(m=features_ordered, n=linked$n)
  names(features_ordered2)[1]=m_name
  fortified_df=merge(fortified_df, features_ordered2, by.x=n_name, by.y="n",
                     all.x=TRUE)
  return(fortified_df)
}

#' Internal checks before ggnetmap and ggcentrality
#'
#' Checks whether the proper packages are installed, whether the parameters are
#' of the proper classes, whether the network-map link is possible, then
#' performs the link.
#'
#' @inheritParams ggnetmap
#'
#' @return A list with a \code{network} or \code{igraph} object with only the
#' vertices present in the \code{sf} object as th first element and a list
#' with two vectors, one of features in \code{m} present
#' both in the lookup table and in \code{n}, the other of nodes in \code{n}
#' present both in the lookup table and in \code{m}
#'
#' @examples
#' #only used from within ggnetmap and ggcentrality
check_network_sf <- function(
  n,
  m,
  lkp=NULL,
  m_name=NULL,
  n_name="vertex.names"
) {
  if(!rlang::is_installed(c("ggnetwork", "sf"))) {
    stop(
      "Packages \"ggnetwork\" and \"sf\" must be installed to use
      this function.",
      call. = FALSE
    )
  } else if(!rlang::is_installed("network") && !rlang::is_installed("igraph")) {
    stop(
      "Either package \"network\" or package \"igraph\" must be installed to use
      this function.",
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

  return(list(n2=n2, linked=linked))
}

#' Calculate centrality indices for vertices linked to a sf object
#'
#' Given a \code{sf} object with features that can be linked to a \code{network}
#' or \code{igraph} object, obtain centrality indices for linked features.
#'
#' @inheritParams ggnetmap
#' @param par.deg List with additional optional parameters to functions
#' \code{\link[sna]{degree}} or \code{\link[igraph]{degree}}.
#' @param par.bet List with additional optional parameters to functions
#' \code{\link[sna]{betweenness}} or \code{\link[igraph]{betweenness}}.
#' @param par.clo List with additional optional parameters to functions
#' \code{\link[sna]{closeness}} or \code{\link[igraph]{closeness}}.
#'
#' @return An \code{sf} object, input \code{m} with added columns for centrality
#' indices (degree, betweenness, closeness; existing columns with the same name
#' will be overwritten) and with only the features linked to vertices in input
#' \code{n}.
#' @export
#'
#' @examples
#' net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
#' network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
#' wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
#' "01010000204071000000000000801A084100000000AC5C1441",
#' "01010000204071000000000000801A044100000000AC5C1241",
#' "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
#' map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
#' lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
#' netmap::ggcentrality(net, map, lkptbl, "id", "name")
ggcentrality <- function(
  n,
  m,
  lkp=NULL,
  m_name=NULL,
  n_name="vertex.names",
  par.deg=NULL,
  par.bet=NULL,
  par.clo=NULL
) {
  res=check_network_sf(n, m, lkp, m_name, n_name)
  n2=res[[1]] #reduced network, with only nodes present in m
  linked=res[[2]]

  #pick only linked features
  m2=m[get(m_name, m) %in% linked$m,]
  if(is.null(lkp)) {
    m2=m2[match(linked$n, linked$m),]
  }
  else {
    m2=m2[match(get(m_name, pos=lkp)[match(linked$n, get(n_name, lkp))], get(m_name, pos=m2)),]
  }

  #calculate centrality measures
  if(inherits(n2, "network")){
    if(!rlang::is_installed("sna")) {
      stop(
        "Package \"sna\" must be installed to use this function.",
        call. = FALSE
      )
    }
    m2$degree=do.call(sna::degree, c(list(n2), par.deg))
    m2$betweenness=do.call(sna::betweenness, c(list(n2), par.bet))
    m2$closeness=do.call(sna::closeness, c(list(n2), par.clo))
  } else if (inherits(n2, "igraph")) {
    m2$degree=do.call(igraph::degree, c(list(n2), par.deg))
    m2$betweenness=do.call(igraph::betweenness, c(list(n2), par.bet))
    m2$closeness=do.call(igraph::closeness, c(list(n2), par.clo))
  }
  return(m2)
}

#' Calculate connectedness to a specific vertex for vertices linked to a sf
#' object
#'
#' Given a \code{sf} object with features that can be linked to a \code{network}
#' or \code{igraph} object and given a node with id \code{id} in said graph that
#' can be linked to the \code{sf} object, obtain an indicator variable denoting,
#' for each node, a connection to \code{id}.
#'
#' @inheritParams ggnetmap
#' @param id The identifier (as vertex attribute \code{n_name} of object \code{n})
#' of the feature that needs to be checked for connections.
#'
#' @return An \code{sf} object, input \code{m} with an added column
#' \code{conn_area} with an indicator variable set to 1 if the feature is
#' connected to the feature with vertex id \code{id}, 0 otherwise.
#' In directed graphs, only outgoing links are considered a connection.
#' Any existing column with the same name will be overwritten, the result will
#' contain only the features linked to vertices in input. If the vertex
#' \code{id} is not present in object \code{n}, \code{conn_area} will be set to
#' 0 for all vertices.
#' @export
#'
#' @examples
#' net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
#' network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
#' wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
#' "01010000204071000000000000801A084100000000AC5C1441",
#' "01010000204071000000000000801A044100000000AC5C1241",
#' "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
#' map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
#' lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
#' ggconn_area(net, map, "b", lkptbl, "id", "name")
ggconn_area <- function(
  n,
  m,
  id,
  lkp=NULL,
  m_name=NULL,
  n_name="vertex.names"
) {
  res=check_network_sf(n, m, lkp, m_name, n_name)
  n2=res[[1]] #reduced network, with only nodes present in m
  linked=res[[2]]

  #pick only linked features
  m2=m[get(m_name, m) %in% linked$m,]
  if(is.null(lkp)) {
    m2=m2[match(linked$n, linked$m),]
  }
  else {
    m2=m2[match(get(m_name, pos=lkp)[match(linked$n, get(n_name, lkp))], get(m_name, pos=m2)),]
  }

  #get id of vertex with n_name == id
  if(inherits(n2, "network")){
    if(!rlang::is_installed("sna")) {
      stop(
        "Package \"sna\" must be installed to use this function.",
        call. = FALSE
      )
    }
    idv=which(network::get.vertex.attribute(n2, n_name)==id)
    neighb_list=rep(0, network::get.network.attribute(n2, "n"))
    if(length(idv)>0) {#vertex "id" found in graph
      neighb_list[network::get.neighborhood(n2, idv)]=1
    }

  } else if (inherits(n2, "igraph")) {
    idv=which(igraph::vertex_attr(n2, n_name)==id)
    neighb_list=rep(0, igraph::vcount(n2))
    if(length(idv)>0) {#vertex "id" found in graph
      neighb_list[as.integer(igraph::neighbors(n2, idv))]=1
    }
  }
  m2$conn_area=neighb_list
  return(m2)
}

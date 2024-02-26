#' Link a network and a map
#'
#' Checks which vertices of a \code{network} object can be represented with
#' features of a \code{sf} object.
#'
#' @param m Object of class \code{sf}.
#' @param n Object of class \code{network} or \code{igraph}.
#' @param n_name Name of the vertex attribute to use for the link, defaults to
#' \code{vertex.names}.
#' @param m_name Name of the map field to use for the link.
#'
#' @return On success a list with two vectors, one of features in \code{m}
#' present in \code{n}, the other of nodes in \code{n} present in \code{m},
#' \code{-1} on error.
#'
link_network_map <- function(m, n, m_name, n_name="vertex.names"){
  if(!rlang::is_installed("network") && !rlang::is_installed("igraph")) {
    stop(
      "Either package \"network\" or package \"igraph\" must be installed to use this function.",
      call. = FALSE
    )
  }
  #classes
  if(!is_network(n)) return(-1)
  if(!is_sf(m)) return(-1)

  #do attributes/fields exist?
  if (!exists(m_name, m)) {
    message(paste0("Field ", m_name, " doesn't exist in sf object"))
    return(-1)
  }
  if(inherits(n, "network")) {
    if (is.null(network::get.vertex.attribute(n, n_name, null.na=FALSE))) {
      message(paste0("Vertex attribute ", n_name, " doesn't exist in network object"))
      return(-1)
    }
  } else if (inherits(n, "igraph")) {
    if (is.null(igraph::vertex_attr(n, n_name))) {
      message(paste0("Vertex attribute ", n_name, " doesn't exist in igraph object"))
      return(-1)
    }
  } else {
    return(-1)
  }

  #main check
  if(inherits(n, "network")) {
    res1=network::get.vertex.attribute(n, n_name)[network::get.vertex.attribute(n, n_name) %in% get(m_name, pos=m)]
    res2=get(m_name, pos=m)[get(m_name, pos=m) %in% (network::get.vertex.attribute(n, n_name))]
  } else if (inherits(n, "igraph")) {
    res1=igraph::vertex_attr(n, n_name)[igraph::vertex_attr(n, n_name) %in% get(m_name, pos=m)]
    res2=get(m_name, pos=m)[get(m_name, pos=m) %in% (igraph::vertex_attr(n, n_name))]
  }

  return(list(m=res2, n=res1))
}

#' Link a network and a map with a lookup table
#'
#' Checks which vertices of a network object can be represented with features of
#' a sf object with a lookup table.
#'
#' @param lkp Lookup table, a \code{data.frame}.
#' @param m_name Optional character, name of field in \code{m} and of column in
#' \code{lkp} (first column of \code{lkp} is used if \code{NULL}).
#' @param n_name Optional character, name of vertex attribute in \code{n} and of
#' column in \code{lkp}  (second column of \code{lkp} is used if \code{NULL}).
#' @inheritParams link_network_map
#'
#' @return On success a list with two vectors, one of features in \code{m} present
#' both in the lookup table and in \code{n}, the other of nodes in \code{n} present
#' both in the lookup table and in \code{m}, \code{-1} on error.
#'
link_network_map2 <- function(m, n, lkp, m_name=NULL, n_name=NULL){
  if(!rlang::is_installed("network")) {
    stop(
      "Package \"network\" must be installed to use this function.",
      call. = FALSE
    )
  }
  #classes
  if(!is_network(n)) return(-1)
  if(!is_sf(m)) return(-1)
  lkp_check=is_lookup_table(lkp, m_name, n_name)
  if(length(lkp_check) == 1 && lkp_check == FALSE) {
    message("Lookup table is not valid")
    return(-1)
  } else {
    m_name=lkp_check[1]
    n_name=lkp_check[2]
  }

  #do attributes/fields exist?
  if (!exists(m_name, m)) {
    message(paste0("Field ", m_name, ", either set explicitly or inherited from
                   lookup table, doesn't exist in sf object"))
    return(-1)
  }

  if(inherits(n, "network")) {
    if (is.null(network::get.vertex.attribute(n, n_name, null.na=FALSE))) {
      message(paste0("Vertex attribute ", n_name, ", either set explicitly or
                   inherited from lookup table, doesn't exist in network object"))
      return(-1)
    }
  } else if (inherits(n, "igraph")) {
    if (is.null(igraph::vertex_attr(n, n_name))) {
      message(paste0("Vertex attribute ", n_name, ", either set explicitly or
                   inherited from lookup table, doesn't exist in igraph object"))
      return(-1)
    }
  } else {
    return(-1)
  }

  #number of features present in the lookup table and in the network
  if(inherits(n, "network")) {
    res=get(m_name, pos=m)[get(m_name, pos=m)
                           %in% get(m_name, pos=lkp)[get(n_name, pos=lkp)
                           %in% network::get.vertex.attribute(n, n_name)]]
    res2=network::get.vertex.attribute(n,
                         n_name)[network::get.vertex.attribute(n, n_name) %in% get(n_name,
                         pos=lkp)[get(m_name, pos=lkp) %in% get(m_name, pos=m)]]
  } else if (inherits(n, "igraph")) {
    res=get(m_name, pos=m)[get(m_name, pos=m)
                           %in% get(m_name, pos=lkp)[get(n_name, pos=lkp)
                           %in% igraph::vertex_attr(n, n_name)]]
    res2=igraph::vertex_attr(n,
                        n_name)[igraph::vertex_attr(n, n_name) %in% get(n_name,
                         pos=lkp)[get(m_name, pos=lkp) %in% get(m_name, pos=m)]]
  }

  return(list(m=res, n=res2))
}


#' Is object a network?
#'
#' Checks whether an object is a \code{network} object or an \code{igraph} object,
#' returns message if it's not
#'
#' @inheritParams link_network_map
#'
#' @return \code{TRUE} if object of class \code{network}, \code{FALSE} otherwise.
#'
is_network <- function(n){
  if(!network::is.network(n) && !igraph::is.igraph(n)) {
    message(paste0("Invalid network/igraph object supplied"))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Is object a map?
#'
#' Checks whether an object is an \code{sf} object, returns message if it's not
#'
#' @inheritParams link_network_map
#'
#' @return \code{TRUE} if object of classes \code{sf} and \code{data.frame}, \code{FALSE} otherwise.
#'
is_sf <- function(m){
  clsf=class(m)
  if(!all(c("sf", "data.frame") %in% clsf)) {
    message(paste0("Invalid sf object supplied"))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Is data frame a lookup table?
#'
#' Checks whether a \code{data.frame} is a valid lookup table.
#'
#' @param lkp A \code{data.frame}.
#' @param m_name Optional, a \code{character} string with the name of the column
#' in \code{lkp} to check against \code{m}.
#' @param n_name Optional, a \code{character} string with the name of the column
#' in \code{lkp} to check against \code{n}.
#'
#' @return \code{FALSE} on error, a vector with \code{m_name} and \code{n_name}
#' if the lookup table is valid.
#'
is_lookup_table <- function(lkp, m_name=NULL, n_name=NULL) {
  #are table and variable names valid?
  if(!is.data.frame(lkp) || ncol(lkp)<2) {
    message("Lookup table is not a data.frame or doesn't have 2 columns")
    return(FALSE)
  }
  m_name <- if (is.null(m_name)) names(lkp)[1] else m_name
  n_name <- if (is.null(n_name)) names(lkp)[2] else n_name
  if (nchar(m_name)<1 || nchar(n_name)<1) {
    message(paste0(m_name, ", ", n_name, " are not valid column names"))
    return(FALSE)
  }
  if (!exists(m_name, lkp) || !exists(n_name, lkp)) {
    message(paste0(m_name, " or ", n_name, " not found in lookup table"))
    return(FALSE)
  }

  #are there empty cells? (NA, NULL or empty strings)
  check=sum(is.na(get(m_name, lkp))) +
    sum(is.null(get(m_name, lkp))) +
    sum(nchar(as.character(get(m_name, lkp)), allowNA=TRUE)<1, na.rm=TRUE) +
    sum(is.na(get(n_name, lkp))) +
    sum(is.null(get(n_name, lkp))) +
    sum(nchar(as.character(get(n_name, lkp)), allowNA=TRUE)<1, na.rm=TRUE)
  if (check>0) {
    message("Empty cells in lookup table")
    return(FALSE)
  }

  #are there duplicate entries?
  check2=(sort(table(get(m_name, lkp)), decreasing=TRUE)[1]>1) +
    (sort(table(get(n_name, lkp)), decreasing=TRUE)[1]>1)
  if (check2>0) {
    message("Lookup table is ambiguous (duplicate entries in at least one column)")
    return(FALSE)
  }

  return(c(m_name, n_name))
}

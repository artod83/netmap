#' Checks which vertices of a network object can be represented with features of a sf object
#'
#' @param m object of class sf
#' @param n object of class network
#' @param n_name name of the vertex attribute to use for the link, defaults to vertex.names
#' @param m_name name of the map field to use for the link
#'
#' @return on success a list with two vectors, one of features in m present in n, the other of nodes in n present in m, -1 on error
#' @export
#'
#' @examples
link_network_map <- function(m, n, m_name, n_name="vertex.names"){
  #classes
  if(!is_network(n)) return(-1)
  if(!is_sf(m)) return(-1)

  #do attributes/fields exist?
  if (!exists(m_name, m)) {
    message(paste0("Field ", m_name, " doesn't exist in sf object"))
    return(-1)
  }
  if (is.null(network::get.vertex.attribute(n, n_name, null.na=FALSE))) {
    message(paste0("Vertex attribute ", n_name, " doesn't exist in network object"))
    return(-1)
  }
  #main check
  res1=n %v% n_name %in% get(m_name, pos=m)
  res2=get(m_name, pos=m)[get(m_name, pos=m) %in% (n %v% n_name)]
  return(list(m=res2, n=res1))
}

#same as link_network_map, but with a lookup table that maps m_name to n_name. by default m_name and
# n_name first and second column of lkp, respectively
#' Checks which vertices of a network object can be represented with features of a sf object with a lookup table
#'
#' @param m object of class sf
#' @param n object of class network
#' @param lkp look-up table, a data frame
#' @param m_name optional character, name of field in m and of column in lkp
#' @param n_name optional character, name of vertex attribute in n and of column in lkp
#'
#' @return on success a list with two vectors, one of features in m present both in the lookup table and in n, the other of nodes in n present both in the lookup table and in m, -1 on error
#' @export
#'
#' @examples
link_network_map2 <- function(m, n, lkp, m_name=NULL, n_name=NULL){
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
    message(paste0("Field ", m_name, ", either set explicitly or inherited from lookup table, doesn't exist in sf object"))
    return(-1)
  }
  if (is.null(network::get.vertex.attribute(n, n_name, null.na=FALSE))) {
    message(paste0("Vertex attribute ", n_name, ", either set explicitly or inherited from lookup table, doesn't exist in network object"))
    return(-1)
  }

  #number of features present in the lookup table and in the network
  res=get(m_name, pos=m)[get(m_name, pos=m) %in% get(m_name, pos=lkp)[get(n_name, pos=lkp) %in% (n %v% n_name)]]
  res2=(n %v% n_name)[(n %v% n_name) %in% get(n_name, pos=lkp)[get(m_name, pos=lkp) %in% get(m_name, pos=m)]]
  return(list(m=res, n=res2))
}



#' Checks whether an object is a network object, returns message if it's not
#'
#' @param n object of class network
#'
#' @return TRUE if object of class network, FALSE otherwise
#'
#' @examples is_network(network(1)) #TRUE
is_network <- function(n){
  if(!network::is.network(n)) {
    message(paste0("Invalid network object supplied"))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Checks whether an object is an sf object, returns message if it's not
#'
#' @param m object of class sf
#'
#' @return TRUE if object of classes sf and data.frame, FALSE otherwise
#'
#' @examples
is_sf <- function(m){
  clsf=class(m)
  if(!(length(clsf)==2 && clsf[1] == "sf" && clsf[2] == "data.frame")) {
    message(paste0("Invalid sf object supplied"))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Checks whether a data frame is a valid lookup table
#'
#' @param lkp a data frame
#' @param m_name optional, a character string with the name of the column in lkp to check against m
#' @param n_name optional, a character string with the name of the column in lkp to check against n
#'
#' @return FALSE on error, a vector with m_name and n_name if the lookup table is valid
#' @export
#'
#' @examples
is_lookup_table <- function(lkp, m_name=NULL, n_name=NULL) {
  #are table and variable names valid?
  if(!is.data.frame(lkp) || ncol(lkp)<2) return(FALSE)
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
    sum(nchar(as.character(get(m_name, lkp)))<1) +
    sum(is.na(get(n_name, lkp))) +
    sum(is.null(get(n_name, lkp))) +
    sum(nchar(as.character(get(n_name, lkp)))<1)
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

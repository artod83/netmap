% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggcentrality.R
\name{ggcentrality}
\alias{ggcentrality}
\title{Calculate centrality indices for vertices linked to a sf object}
\usage{
ggcentrality(
  n,
  m,
  lkp = NULL,
  m_name = NULL,
  n_name = "vertex.names",
  par.deg = NULL,
  par.bet = NULL,
  par.clo = NULL
)
}
\arguments{
\item{n}{A \code{network} or \code{igraph} object.}

\item{m}{A \code{sf} object.}

\item{lkp}{An optional lookup table.}

\item{m_name}{Optional \code{character}, name of field in \code{m} and of
column in \code{lkp}.}

\item{n_name}{Optional \code{character}, name of vertex attribute in \code{n}
and of column in \code{lkp}.}

\item{par.deg}{List with additional optional parameters to functions
\code{\link[sna]{degree}} or \code{\link[igraph]{degree}}.}

\item{par.bet}{List with additional optional parameters to functions
\code{\link[sna]{betweenness}} or \code{\link[igraph]{betweenness}}.}

\item{par.clo}{List with additional optional parameters to functions
\code{\link[sna]{closeness}} or \code{\link[igraph]{closeness}}.}
}
\value{
An \code{sf} object, input \code{m} with added columns for centrality
indices (degree, betweenness, closeness; existing columns with the same name
will be overwritten) and with only the features linked to vertices in input
\code{n}.
}
\description{
Given a \code{sf} object with features that can be linked to a \code{network}
or \code{igraph} object, obtain centrality indices for linked features.
}
\examples{
net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
"01010000204071000000000000801A084100000000AC5C1441",
"01010000204071000000000000801A044100000000AC5C1241",
"01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
netmap::ggcentrality(net, map, lkptbl, "id", "name")
}

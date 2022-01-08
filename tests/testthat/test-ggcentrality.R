test_that("ggcentrality returns a sf object with 3 more columns with centrality measures", {
  net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
  network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
                       "01010000204071000000000000801A084100000000AC5C1441",
                       "01010000204071000000000000801A044100000000AC5C1241",
                       "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
  map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
  lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
  res=ggcentrality(net, map, lkptbl, "id", "name")
  expect_equal(res$degree, c(4, 5, 3, 2))
  expect_equal(res$betweenness, c(2, 4, 0, 0))
  expect_equal(res$closeness, c(1, 0.75, 0.5, 0.6))
  res=ggcentrality(net, map, lkptbl, "id", "name", par.deg=list(cmod="outdegree"))
  expect_equal(res$degree, c(3, 2, 1, 1))
})

test_that("ggconn_area returns the correct connected areas for a network object", {
  net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
  network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
                       "01010000204071000000000000801A084100000000AC5C1441",
                       "01010000204071000000000000801A044100000000AC5C1241",
                       "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
  map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
  lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
  res=ggconn_area(net, map, "b", lkptbl, "id", "name")
  expect_equal(res$conn_area, c(1, 0, 1, 0))
  res=ggconn_area(net, map, "d", lkptbl, "id", "name")
  expect_equal(res$conn_area, c(0, 1, 0, 0))
})

test_that("ggconn_area returns the correct connected areas for am igraph object", {
  net2=igraph::graph_from_adjacency_matrix(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
  igraph::vertex_attr(net2, "name") <- c("a", "b", "c", "d")
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
                       "01010000204071000000000000801A084100000000AC5C1441",
                       "01010000204071000000000000801A044100000000AC5C1241",
                       "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
  map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
  lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
  res=ggconn_area(net2, map, "b", lkptbl, "id", "name")
  expect_equal(res$conn_area, c(1, 0, 1, 0))
  res=ggconn_area(net2, map, "d", lkptbl, "id", "name")
  expect_equal(res$conn_area, c(0, 1, 0, 0))
})

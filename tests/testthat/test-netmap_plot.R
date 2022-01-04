test_that("netmap_plot draws a plot", {
  net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4,
                     byrow=TRUE))
  network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
                       "01010000204071000000000000801A084100000000AC5C1441",
                       "01010000204071000000000000801A044100000000AC5C1241",
                       "01010000204071000000000000801A024100000000AC5C1841"),
                  class = "WKB")
  map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
  lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
  expect_silent(netmap_plot(net, map, lkptbl, "id", "name"))
})

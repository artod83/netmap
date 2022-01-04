test_that("link_network_map works without lookup table", {
  net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
  network::set.vertex.attribute(net, "name", value=c("a", "e", "c", "d"))
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
                       "01010000204071000000000000801A084100000000AC5C1441",
                       "01010000204071000000000000801A044100000000AC5C1241",
                       "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
  map=sf::st_sf(id=c("a", "b", "c", "e"), sf::st_as_sfc(wkb, EWKB=TRUE))
  res=link_network_map(map, net, "id", "name")
  expect_equal(res, list(m=c("a", "c", "e"), n=c("a", "e", "c")))
})

test_that("link_network_map works with a lookup table", {
  net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
  network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
                       "01010000204071000000000000801A084100000000AC5C1441",
                       "01010000204071000000000000801A044100000000AC5C1241",
                       "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
  map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
  lkptbl=data.frame(id=c("a1", "b2", "c4", "d4"), name=c("a", "b", "c", "d"))
  res=link_network_map2(map, net, lkptbl, "id", "name")
  expect_equal(res, list(m=c("a1", "b2", "d4"), n=c("a", "b", "d")))
})

test_that("is_* functions recognize objects of the class they test for", {
  expect_true(is_network(network::network(matrix(1))), TRUE)
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
    "01010000204071000000000000801A084100000000AC5C1441",
    "01010000204071000000000000801A044100000000AC5C1241",
    "01010000204071000000000000801A024100000000AC5C1841"), class = "WKB")
    map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
  expect_true(is_sf(map))
})

test_that("is_lookup recognizes a complete data frame with two columns", {
  expect_equal(is_lookup_table(data.frame(id=c("a1", "b2", "c3", "d4"),
                                         name=c("a", "b", "c", "d"))),
               c("id", "name"))
})

test_that("is_lookup fails with duplicates or missing/empty values", {
  expect_false(is_lookup_table(data.frame(id=c("a1", "b2", "c3", "d4"),
                                         name=c("a", "b", "b", "d"))))
  expect_false(is_lookup_table(data.frame(id=c("a1", "", "c3", "d4"),
                                          name=c("a", "b", "c", "d"))))
  expect_false(is_lookup_table(data.frame(id=c("a1", "b2", NA, "d4"),
                                          name=c("a", "b", "b", "d"))))
})

test_that("is_lookup fails with column names not found in df", {
  expect_false(is_lookup_table(data.frame(id=c("a1", "b2", "c3", "d4"),
                                          name=c("a", "b", "c", "d")),
                               m_name="id",
                               n_name="vertex.names"))
})

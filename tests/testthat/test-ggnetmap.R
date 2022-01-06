test_that("ggnetmap produces the expected fortified data frame with lookup table", {
  net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
  network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
  wkb = structure(list("01010000204071000000000000801A064100000000AC5C1641",
                       "01010000204071000000000000801A084100000000AC5C1441",
                       "01010000204071000000000000801A044100000000AC5C1241",
                       "01010000204071000000000000801A024100000000AC5C1841"),
                  class = "WKB")
  map=sf::st_sf(id=c("a1", "b2", "c3", "d4"), sf::st_as_sfc(wkb, EWKB=TRUE))
  lkptbl=data.frame(id=c("a1", "b2", "c3", "d4"), name=c("a", "b", "c", "d"))
  res=ggnetmap(net, map, lkptbl, "id", "name")
  expect_equal(
    res,
    data.frame(name=c("a", "a", "a", "a", "b", "b", "b", "c", "c", "d", "d"),
               x=c(181072, 181072, 181072, 181072,
                   197456, 197456, 197456,
                   164688, 164688,
                   148304, 148304),
               y=c(366379, 366379, 366379, 366379,
                   333611, 333611, 333611,
                   300843, 300843,
                   399147, 399147),
               vertex.names=c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4),
               xend=c(164688, 148304, 197456, 181072,
                      197456, 181072, 164688,
                      197456, 164688,
                      197456, 148304),
               yend=c(300843, 399147, 333611, 366379,
                      333611, 366379, 300843,
                      333611, 300843,
                      333611, 399147),
               id=c("a1", "a1", "a1", "a1",
                    "b2", "b2", "b2",
                    "c3", "c3",
                    "d4", "d4")),
    tolerance=0.03
    )
})

test_that("ggnetmap produces the expected fortified data frame without lookup table", {
  net=network::network(matrix(c(0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0), nrow=4, byrow=TRUE))
  network::set.vertex.attribute(net, "name", value=c("a", "b", "c", "d"))
  wkb = structure(list("01010000204071000000000000801A084100000000AC5C1441",
                       "01010000204071000000000000801A044100000000AC5C1241",
                       "01010000204071000000000000801A024100000000AC5C1841",
                       "01010000204071000000000000801A064100000000AC5C1641"),
                  class = "WKB")
  map=sf::st_sf(id=c("b", "c", "d", "a"), sf::st_as_sfc(wkb, EWKB=TRUE))
  res=ggnetmap(net, map, NULL, "id", "name")
  expect_equal(
    res,
    data.frame(name=c("a", "a", "a", "a", "b", "b", "b", "c", "c", "d", "d"),
               x=c(181072, 181072, 181072, 181072,
                   197456, 197456, 197456,
                   164688, 164688,
                   148304, 148304),
               y=c(366379, 366379, 366379, 366379,
                   333611, 333611, 333611,
                   300843, 300843,
                   399147, 399147),
               vertex.names=c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4),
               xend=c(164688, 148304, 197456, 181072,
                      197456, 181072, 164688,
                      197456, 164688,
                      197456, 148304),
               yend=c(300843, 399147, 333611, 366379,
                      333611, 366379, 300843,
                      333611, 300843,
                      333611, 399147),
               id=c("a", "a", "a", "a",
                    "b", "b", "b",
                    "c", "c",
                    "d", "d")),
    tolerance=0.03
  )
})

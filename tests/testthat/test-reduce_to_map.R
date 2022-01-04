test_that("non-network object gives error", {
  expect_error(reduce_to_map(c(1,2,3,4), c(1,2), "vertex.names"))
})

test_that("simple network reduced from 4 to 2 vertices", {
  net=reduce_to_map(network::network(matrix(c(0,1,1,1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0)), nrow=4, ncol=4, byrow=TRUE), c(1,3), "vertex.names")
  expect_true(is_network(net) && network::get.network.attribute(net, "n") == 2)
})

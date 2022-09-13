
test_that("GDC is calculated correctly.", {
  expect_equal(euclidean(123612, 13892347912), 4)
  expect_equal(euclidean(100, 1000), 100)
  expect_equal(euclidean(-100, 1000), 100)
})


wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
test_that("Wrong input throws an error.", {
  expect_error(euclidean("100", 1000))  
  expect_error(euclidean(100, "1000"))
  expect_error(euclidean(TRUE, "1000"))  
  expect_error(euclidean(c(100,200), 300))  
  expect_error(euclidean(wiki_graph, 1000))  
})
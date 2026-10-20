wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

# The edges of an undirected tree as sorted "a-b" strings (order and direction ignored)
edge_set <- function(edges) sort(paste(pmin(edges$v1, edges$v2), pmax(edges$v1, edges$v2), sep = "-"))

test_that("prim_mst() returns a data.frame with the correct structure.", {
  res <- prim_mst(wiki_graph, 1)
  expect_true(is.data.frame(res))
  expect_true(setequal(colnames(res), c("v1", "v2", "w")))
  expect_equal(nrow(res), 5)
})

test_that("outputs are correct in Prim's algorithm.", {
  mst_edges <- c("1-2", "1-3", "3-6", "4-5", "5-6")
  for(init_node in c(1, 3, 6)){
    res <- prim_mst(wiki_graph, init_node)
    expect_equal(sum(res$w), 33)
    expect_equal(edge_set(res), mst_edges)
    # Every new edge must connect the tree built so far to a new node
    expect_true(res$v1[1] == init_node || res$v2[1] == init_node)
    expect_equal(length(unique(c(res$v1, res$v2))), 6)
  }
})

test_that("Error messages are returned for erronous input in Prim's algorithm.", {
  wiki_wrong_graph <- wiki_graph
  names(wiki_wrong_graph) <- c("v1", "v3", "w")
  expect_error(prim_mst(wiki_wrong_graph, 3))
  wiki_wrong_graph <- wiki_graph[1:2]
  expect_error(prim_mst(wiki_wrong_graph, 3))
  expect_error(prim_mst(wiki_graph, 7))
  expect_error(prim_mst(wiki_graph, c(1, 2)))
  expect_error(prim_mst(as.matrix(wiki_graph), 3))
})

data(gr_gc1)
data(gr_fig3)

## reduce to 6 <-> 8 graph
gr_gc1_68 <- reduce_graph(gr_gc1, c(6,8))
nv_68 <- length(dis(gr_gc1_68, 6))

## reduce to 1 -> 6 graph
gr_gc1_6_1 <- reduce_graph(gr_gc1, 6, 1)
nv_6_1 <- length(dis(gr_gc1_6_1, 6))

test_that("graphs collapse as expected", {
  expect_equal(nedge(gr_gc1_68, edges = "directed"), nv_68-2)
  expect_equal(nedge(gr_gc1_68, edges = "bidirected"), nv_68-1)

  expect_equal(nedge(gr_gc1_6_1, edges = "directed"), nv_6_1)
  expect_equal(nedge(gr_gc1_6_1, edges = "bidirected"), nv_6_1-1)
})

gr_fig3_r <- reduce_graph2(gr_fig3, 6, 7)

test_that("graphs collapse as expected 2", {
  expect_equal(nedge(gr_fig3_r, edges = "directed"), 1)
  expect_equal(nedge(gr_fig3_r, edges = "bidirected"), 0)
})

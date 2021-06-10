set.seed(123)
dat <- rNormDAG(5e3, grGadget, rho=0.5, hide=TRUE)
res <- MVN::mvn(dat)

test_that("distribution is truly Gaussian", {
  expect_equal(res$multivariateNormality$Result, rep("YES", 3))
  expect_equal(as.character(res$univariateNormality$Normality), rep("   YES   ", 4))
})

set.seed(124)
gr_gc1a <- reduce_graph2(reduce_graph(gr_gc1, c(6, 8)), 6, 8)
dat <- rNormDAG(5e3, gr_gc1a, rho=0.5, hide=TRUE)
res2 <- MVN::mvn(dat)

test_that("distribution is truly Gaussian 2", {
  expect_equal(res2$multivariateNormality$Result, rep("YES", 3))
  expect_true(all(as.numeric(res2$univariateNormality$`p value`) > 0.01))
})

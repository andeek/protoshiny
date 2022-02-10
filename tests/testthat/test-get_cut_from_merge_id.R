test_that("get_cut_from_merge_id works", {
  x <- c(1, 2, 7, 10, 12, 17)
  names(x) <- LETTERS[1:length(x)]
  hc <- protoclust::protoclust(dist(x))
  # plot(hc)
  expect_equal(get_cut_from_merge_id(hc, length(x) - 1),
               stats::cutree(hc, k = 1))
  expect_equal(get_cut_from_merge_id(hc, c(1, 4)), 
               stats::cutree(hc, k = 2))
  expect_equal(get_cut_from_merge_id(hc, c(4, 1)), 
               stats::cutree(hc, k = 2))
  expect_equal(
    as.numeric(get_cut_from_merge_id(hc, c(-1, -2, 4))),
    c(1, 2, rep(3, 4)))
  expect_equal(
    as.numeric(get_cut_from_merge_id(hc, c(1, -6, 3))),
    c(1, 1, 2, 2, 2, 3)
    )
  expect_error(get_cut_from_merge_id(hc, 1:2), "leaves do not descend")
  expect_error(get_cut_from_merge_id(hc, c(1, 2, 4)), "descend from multiple")
})

test_that("get_paths_from_cut works", {
  x <- c(1, 2, 7, 10, 12, 17)
  names(x) <- LETTERS[1:length(x)]
  hc <- protoclust::protoclust(dist(x))
  # plot(hc)
  cl <- stats::cutree(hc, k = 2)
  expect_equal(get_paths_from_cut(hc, cl), list(0, 1))
  cl <- stats::cutree(hc, k = 3)
  expect_equal(get_paths_from_cut(hc, cl), list(c(1, 0), 0, c(1, 1), 1))
  cl <- stats::cutree(hc, k = 4)
  expect_equal(get_paths_from_cut(hc, cl), list(c(1, 1, 0),
                                                c(1, 0),
                                                0,
                                                c(1, 1, 1),
                                                c(1, 1),
                                                1))
  cl[2] <- 5
  expect_equal(get_paths_from_cut(hc, cl), list(c(0, 0),
                                                c(1, 1, 0),
                                                c(1, 0),
                                                0,
                                                c(0, 1),
                                                c(1, 1, 1),
                                                c(1, 1),
                                                1))
  cl <- stats::cutree(hc, k = 5)
  expect_equal(get_paths_from_cut(hc, cl), list(c(1, 1, 1, 0),
                                                c(1, 1, 0),
                                                c(1, 0),
                                                0,
                                                c(1, 1, 1, 1),
                                                c(1, 1, 1),
                                                c(1, 1),
                                                1))
  cl <- stats::cutree(hc, k = 6)
  expect_equal(get_paths_from_cut(hc, cl), list(c(0, 0),
                                                c(1, 1, 1, 0),
                                                c(1, 1, 0),
                                                c(1, 0),
                                                0,
                                                c(0, 1),
                                                c(1, 1, 1, 1),
                                                c(1, 1, 1),
                                                c(1, 1),
                                                1))
})

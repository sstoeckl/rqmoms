test_that("packaged datasets are available and usable", {
  expect_true(exists("qmoms_surface"))
  expect_true(exists("qmoms_zerocd"))
  expect_gt(nrow(qmoms_surface), 10)
  expect_gt(nrow(qmoms_zerocd), 10)


  one <- subset(qmoms_surface, id == qmoms_surface$id[1] & days == qmoms_surface$days[1])
  r30 <- get_rate_for_maturity(qmoms_zerocd, date = one$date[1], days = one$days[1])
  res <- qmoms_compute(one$mnes, one$impl_volatility, one$days[1], r30, rq_default_params(), output = "list")
  expect_true(is.list(res))
  expect_true(all(c("smfiv","mfiv_bkm","mfiv_bjn") %in% names(res)))
})

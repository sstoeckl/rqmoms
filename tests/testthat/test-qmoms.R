test_that("qmoms core metrics (pure R) compute without errors", {
  params <- rq_default_params()

  # pick a representative (id,date,days)
  one <- subset(qmoms_surface,
                id == qmoms_surface$id[1] & days == qmoms_surface$days[1])

  # rate from packaged zero curve (decimal)
  r30 <- get_rate_for_maturity(qmoms_zerocd, date = one$date[1], days = one$days[1])

  r <- qmoms_compute(one$mnes, one$impl_volatility, one$days[1], r30,
                     params = params, output = "list")

  expect_true(is.list(r))
  expect_true(all(c("smfiv","mfiv_bkm","mfiv_bjn") %in% names(r)))
  expect_true(all(is.finite(unlist(r[c("smfiv","mfiv_bkm","mfiv_bjn")]))))
  # slopes included here; no separate slope test needed
  expect_true(all(c("slopedn","slopeup") %in% names(r)))
})

test_that("qmoms core metrics match Python on the same dataset (if Python available)", {
  if (!rq_has_python()) testthat::skip("Local Python env not available; skipping Python parity.")

  params <- rq_default_params()
  one <- subset(qmoms_surface,
                id == qmoms_surface$id[1] & days == qmoms_surface$days[1])
  r30 <- get_rate_for_maturity(qmoms_zerocd, date = one$date[1], days = one$days[1])

  py <- rq_pyref_compute(one$mnes, one$impl_volatility, one$days[1], r30, params)
  r  <- qmoms_compute(one$mnes, one$impl_volatility, one$days[1], r30,
                      params = params, output = "list")

  # Core metrics
  expect_equal(r$smfiv,    py$smfiv,    tolerance = 5e-4)
  expect_equal(r$mfiv_bkm, py$mfiv_bkm, tolerance = 5e-4)
  expect_equal(r$mfiv_bjn, py$mfiv_bjn, tolerance = 5e-4)

  # Optional blocks (present in our defaults)
  if (!is.null(py$smfivd))    expect_equal(r$smfivd,    py$smfivd,    tolerance = 5e-4)
  if (!is.null(py$mfivd_bkm)) expect_equal(r$mfivd_bkm, py$mfivd_bkm, tolerance = 5e-4)
  if (!is.null(py$mfivd_bjn)) expect_equal(r$mfivd_bjn, py$mfivd_bjn, tolerance = 5e-4)

  if (!is.null(py$mfis))      expect_equal(r$mfis,      py$mfis,      tolerance = 5e-3)
  if (!is.null(py$mfik))      expect_equal(r$mfik,      py$mfik,      tolerance = 5e-2)

  if (!is.null(py$slopedn))   expect_equal(r$slopedn,   py$slopedn,   tolerance = 5e-3)
  if (!is.null(py$slopeup))   expect_equal(r$slopeup,   py$slopeup,   tolerance = 5e-3)

  if (!is.null(py$rix))       expect_equal(r$rix,       py$rix,       tolerance = 5e-4)
  if (!is.null(py$rixnorm))   expect_equal(r$rixnorm,   py$rixnorm,   tolerance = 5e-4)

  # dynamic CVIX/TLM keys
  cv_keys <- intersect(names(r)[startsWith(names(r), "cvix_")], names(py))
  for (k in cv_keys) expect_equal(r[[k]], py[[k]], tolerance = 5e-4)

  tlm_keys <- intersect(names(r)[startsWith(names(r), "tlm_")], names(py))
  for (k in tlm_keys) if (!is.na(py[[k]]) && !is.na(r[[k]])) expect_equal(r[[k]], py[[k]], tolerance = 5e-2)
})

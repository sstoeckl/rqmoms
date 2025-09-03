test_that("qmoms_compute_bygroup equals manual vector interface", {
  params <- rq_default_params()

  # merge rate into surface (decimal)
  surf_r <- get_rate_for_maturity(qmoms_zerocd, df_surf = qmoms_surface)

  groups <- split(surf_r, list(surf_r$id, surf_r$date, surf_r$days), drop = TRUE)

  # A) wrapper
  out_bygroup <- vctrs::vec_rbind(!!!lapply(groups, function(g)
    qmoms_compute_bygroup(list(g, params))
  ))

  # B) manual vector interface
  out_manual <- vctrs::vec_rbind(!!!lapply(groups, function(g)
    tibble::tibble(
      id = g$id[1], date = g$date[1], days = g$days[1],
      !!!qmoms_compute(g$mnes, g$impl_volatility, g$days[1], g$rate[1], params, output = "list")
    )
  ))

  cols <- intersect(names(out_bygroup), names(out_manual))
  out_bygroup <- dplyr::arrange(out_bygroup[cols], id, date, days)
  out_manual  <- dplyr::arrange(out_manual[cols],  id, date, days)
  expect_equal(out_bygroup, out_manual, tolerance = 1e-12)
})

test_that("bygroup respects column mapping", {
  params <- rq_default_params()
  surf_r <- get_rate_for_maturity(qmoms_zerocd, df_surf = qmoms_surface)

  df_alt <- surf_r |>
    dplyr::rename(ID = id, Date = date, Days = days, Rate = rate, M = mnes, IV = impl_volatility)

  map <- list(id = "ID", date = "Date", days = "Days", rate = "Rate", mnes = "M", impl_volatility = "IV")

  groups_alt <- split(df_alt, list(df_alt$ID, df_alt$Date, df_alt$Days), drop = TRUE)

  out_mapped <- vctrs::vec_rbind(!!!lapply(groups_alt, function(g)
    qmoms_compute_bygroup(list(g, params), cols_map = map)
  ))

  # Compare to the wrapper on the original data
  groups <- split(surf_r, list(surf_r$id, surf_r$date, surf_r$days), drop = TRUE)
  out_bygroup <- vctrs::vec_rbind(!!!lapply(groups, function(g)
    qmoms_compute_bygroup(list(g, params))
  ))

  cols <- intersect(names(out_bygroup), names(out_mapped))
  out_bygroup <- dplyr::arrange(out_bygroup[cols], id, date, days)
  out_mapped  <- dplyr::arrange(out_mapped[cols],  id, date, days)
  expect_equal(out_mapped, out_bygroup, tolerance = 1e-12)
})

test_that("bygroup spot-check matches Python for a few groups (if Python available)", {
  if (!rq_has_python()) testthat::skip("Local Python env not available; skipping Python parity.")

  params <- rq_default_params()
  surf_r <- get_rate_for_maturity(qmoms_zerocd, df_surf = qmoms_surface)
  groups <- split(surf_r, list(surf_r$id, surf_r$date, surf_r$days), drop = TRUE)
  pick <- utils::head(names(groups), 3)

  r_sub <- vctrs::vec_rbind(!!!lapply(groups[pick], function(g)
    qmoms_compute_bygroup(list(g, params))
  ))

  py_sub <- vctrs::vec_rbind(!!!lapply(groups[pick], function(g)
  {
    py <- rq_pyref_compute(g$mnes, g$impl_volatility, g$days[1], g$rate[1], params)
    tibble::tibble(id = g$id[1], date = g$date[1], days = g$days[1], !!!py)
  }
  ))

  key <- c("smfiv","mfiv_bkm","mfiv_bjn","smfivd","mfivd_bkm","mfivd_bjn",
           "mfis","mfik","slopedn","slopeup","rix","rixnorm")
  present <- intersect(key, intersect(names(r_sub), names(py_sub)))
  for (k in present) {
    expect_equal(r_sub[[k]], py_sub[[k]], tolerance = 5e-3)
  }
})

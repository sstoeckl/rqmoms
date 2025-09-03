#' Default parameters (mirrors Python qmoms_params.default_params)
#' @export
rq_default_params <- function() {
  list(
    atmfwd = FALSE,
    grid = list(number_points = 500L, grid_limit = 2),
    filter = list(
      mnes_lim = c(0, 1000),
      delta_put_lim = c(-0.5 + 1e-3, 0),
      delta_call_lim = c(0, 0.5),
      best_bid_zero = -1,
      open_int_zero = -1,
      min_price = 0
    ),
    semivars = list(compute = TRUE),
    mfismfik = list(compute = TRUE),
    cvix = list(compute = TRUE, abs_dev = list(0.2), vol_dev = list(2)),
    rix = list(compute = TRUE),
    tlm = list(compute = TRUE, delta_lim = list(20), vol_lim = list(2)),
    slope = list(compute = TRUE, deltaP_lim = c(-0.5, -0.05), deltaC_lim = c(0.05, 0.5))
  )
}

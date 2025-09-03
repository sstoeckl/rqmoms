# PCHIP-like monotone interpolation and grid construction
#' @keywords internal
rq_grid_params <- function(params) {
  gp <- params$grid %||% list()
  m  <- gp$number_points %||% 500L
  k  <- gp$grid_limit   %||% 2
  c(m, k)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' PCHIP-Interpolation + Clamping (Python-kompatibel)
#' @keywords internal
interpolate_iv_by_moneyness <- function(mnes, vol, grid) {
  # grid: c(points, limit)
  m <- as.integer(grid[[1]])
  k <- as.numeric(grid[[2]])
  stopifnot(m >= 1, k > 0)

  # identisches moneyness-Grid wie Python
  u  <- (1 + k)^(1 / m)
  mi <- -m:m
  ki <- u^mi

  # Eingaben sortieren + einzigartig
  o <- order(mnes); mnes <- as.numeric(mnes[o]); vol <- as.numeric(vol[o])
  if (length(unique(mnes)) != length(mnes)) stop("duplicate mnes not allowed for PCHIP")

  # PCHIP (monoton, wie SciPy PchipInterpolator), nur im Datenbereich
  iv <- rep(NA_real_, length(ki))

  k_s_max <- max(mnes)  # OTM calls (größere mnes)
  k_s_min <- min(mnes)  # OTM puts  (kleinere mnes)

  iv_max <- vol[1]              # für noch weiter links (mehr OTM puts)
  iv_min <- vol[length(vol)]    # für noch weiter rechts (mehr OTM calls)

  ks_larger_ind  <- ki >  k_s_max   # rechts außerhalb
  ks_smaller_ind <- ki <  k_s_min   # links außerhalb
  ks_between_ind <- !ks_larger_ind & !ks_smaller_ind

  if (any(ks_larger_ind))  iv[ks_larger_ind]  <- iv_min
  if (any(ks_smaller_ind)) iv[ks_smaller_ind] <- iv_max

  if (any(ks_between_ind)) {
    iv[ks_between_ind] <- pracma::pchip(mnes, vol, ki[ks_between_ind])
  }

  list(iv = iv, ki = ki)
}

#' @keywords internal
rq_grid_params <- function(params) {
  gp <- params$grid %||% list()
  m <- gp$number_points %||% 500L
  k <- gp$grid_limit %||% 2
  c(m, k)
}


`%||%` <- function(a, b) if (is.null(a)) b else a

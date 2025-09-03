#' Compute moments for one (id,date,days) combo (pure R)
#' @param mnes numeric K/S (sorted internally)
#' @param vol numeric IV
#' @param days integer days to maturity
#' @param rate numeric risk-free (per year)
#' @param params list from rq_default_params()
#' @param output 'list' (default) or 'pandas' (1-row tibble)
#' @export
qmoms_compute <- function(mnes, vol, days, rate, params = rq_default_params(), output = c("list","pandas")) {
  output <- match.arg(output)
  mnes <- as.numeric(mnes); vol <- as.numeric(vol)
  o <- order(mnes); mnes <- mnes[o]; vol <- vol[o]


  nopt <- length(mnes)
  out <- list(nopt = nopt)
  if (nopt < 4 || length(unique(mnes)) != nopt) return(.maybe_tibble(out, output))


  grid <- rq_grid_params(params)
  mat <- as.numeric(days) / 365
  er <- exp(mat * rate)

  # Interpolate IV on symmetric moneyness grid
  inter <- interpolate_iv_by_moneyness(mnes, vol, grid)
  iv <- as.numeric(inter$iv); ki <- as.numeric(inter$ki)
  otmcalls <- ki >= 1
  otmputs <- !otmcalls
  kicalls <- ki[otmcalls]
  kiputs <- ki[otmputs]


  # Price OTM options on grid
  if (isTRUE(params$atmfwd)) {
    P_call <- Black(1, kicalls, rate, iv[otmcalls], mat)$c
    P_put <- Black(1, kiputs, rate, iv[otmputs], mat)$p
  } else {
    P_call <- BlackScholes(1, kicalls, rate, iv[otmcalls], mat)$c
    P_put <- BlackScholes(1, kiputs, rate, iv[otmputs], mat)$p
  }


  # Integration weights dKi (trapezoid‑like, Python style)
  Q <- c(P_put, P_call)
  dKi <- rep(NA_real_, length(ki))
  dKi[1] <- ki[2] - ki[1]
  dKi[length(ki)] <- ki[length(ki)] - ki[length(ki)-1]
  dKi[2:(length(ki)-1)] <- (ki[3:length(ki)] - ki[1:(length(ki)-2)]) / 2
  dKi <- abs(dKi)


  # SMFIV (Ian Martin)
  K0sq <- 1
  svar_ingredients <- (dKi * Q) / K0sq
  svar_multiplier <- 2 * er
  smfivu <- svar_multiplier * sum(svar_ingredients[otmcalls]) / mat
  smfivd <- svar_multiplier * sum(svar_ingredients[otmputs]) / mat
  smfiv <- smfivu + smfivd


  # MFIV_BJN
  Ksq <- ki^2
  bjn_ingredients <- (dKi * Q) / Ksq
  bjn_multiplier <- 2 * er
  mfivu_bjn <- bjn_multiplier * sum(bjn_ingredients[otmcalls]) / mat
  mfivd_bjn <- bjn_multiplier * sum(bjn_ingredients[otmputs]) / mat
  mfiv_bjn <- mfivu_bjn + mfivd_bjn


  # MFIV_BKM
  bkm_ingredients <- (dKi * (1 - log(ki)) * Q) / Ksq
  bkm_multiplier <- 2 * er
  mfivu_bkm <- bkm_multiplier * sum(bkm_ingredients[otmcalls]) / mat
  mfivd_bkm <- bkm_multiplier * sum(bkm_ingredients[otmputs]) / mat
  mfiv_bkm <- mfivu_bkm + mfivd_bkm


  out <- c(out, list(smfiv = smfiv, mfiv_bkm = mfiv_bkm, mfiv_bjn = mfiv_bjn))
  if (isTRUE(params$semivars$compute)) out <- c(out, list(smfivd = smfivd, mfivd_bkm = mfivd_bkm, mfivd_bjn = mfivd_bjn))


  # Inputs for skew/kurt (BKM)
  m <- as.integer(grid[[1]]); k <- as.numeric(grid[[2]]); u <- (1 + k)^(1/m)
  mi <- -m:m; ic <- mi[otmcalls]; ip <- mi[otmputs]
  a <- 2 * (u - 1)
  b1 <- (1 - (log(1 + k) / m) * ic) * P_call / (u^ic)
  b2 <- (1 - (log(1 + k) / m) * ip) * P_put / (u^ip)
  b_all <- c(b2, b1) # puts first


  # MFIS / MFIK
  if (isTRUE(params$mfismfik$compute)) {
    V <- mfiv_bkm * mat / er
    mf <- compute_skew_kurtosis(m, k, u, ic, ip, P_call, P_put, er, V)
    out <- c(out, mf)
  }

  # CVIX
  if (isTRUE(params$cvix$compute)) {
    cv <- cvix_func(b_all, params$cvix, ki, mfiv_bkm, mat, a)
    out <- c(out, cv)
  }


  # RIX
  if (isTRUE(params$rix$compute)) {
    rix <- mfivd_bkm - mfivd_bjn
    rixn <- rix / mfivd_bkm
    out <- c(out, list(rix = rix, rixnorm = rixn))
  }


  # TLM
  if (isTRUE(params$tlm$compute)) {
    if (isTRUE(params$atmfwd)) {
      del <- Black_delta(1, kiputs, rate, iv[otmputs], mat)$delta_p
    } else {
      del <- BlackScholes_delta(1, kiputs, rate, iv[otmputs], mat)$delta_p
    }
    tlm <- tlm_func(P_put, kiputs, del, params$tlm, mfiv_bkm, mat)
    out <- c(out, tlm)
  }


  # Slopes
  # nach iv/ki/otmputs/otmcalls …
  if (isTRUE(params$slope$compute)) {
    if (isTRUE(params$atmfwd)) {
      dput  <- Black_delta(1, kiputs,  rate, iv[otmputs],  mat)$delta_p
      dcall <- Black_delta(1, kicalls, rate, iv[otmcalls], mat)$delta_c
    } else {
      dput  <- BlackScholes_delta(1, kiputs,  rate, iv[otmputs],  mat)$delta_p
      dcall <- BlackScholes_delta(1, kicalls, rate, iv[otmcalls], mat)$delta_c
    }
    limP <- params$slope$deltaP_lim
    selP <- is.finite(dput) & is.finite(iv[otmputs]) & dput >= min(limP) & dput <= max(limP)
    slopedn <- if (sum(selP) > 3) stats::coef(stats::lm(iv[otmputs][selP] ~ dput[selP]))[2] else NA_real_

    limC <- params$slope$deltaC_lim
    selC <- is.finite(dcall) & is.finite(iv[otmcalls]) & dcall >= min(limC) & dcall <= max(limC)
    slopeup <- if (sum(selC) > 3) -stats::coef(stats::lm(iv[otmcalls][selC] ~ dcall[selC]))[2] else NA_real_

    out <- c(out, list(slopedn = as.numeric(slopedn), slopeup = as.numeric(slopeup)))
  }

  .maybe_tibble(out, output)
}


#' Compute qmoms for a single grouped surface (convenience wrapper)
#'
#' Accepts one (id, date, days) surface and returns a one-row tibble with
#' the keys plus all qmoms metrics. Column names can be remapped via `cols_map`.
#'
#' @param x A data.frame/tibble containing a *single* surface. Must include
#'   columns for moneyness and IV (default: `mnes`, `impl_volatility`);
#'   optionally `rate` if you don’t pass `rate` explicitly.
#' @param groupparams A list of the group to be treated as well as the parameters as returned by [rq_default_params()].
#' @param cols_map Optional named list mapping required columns in `x` to the
#'   expected names, e.g. `list(id="ID", date="Date", days="Days", rate="Rate",
#'   mnes="M", impl_volatility="IV")`.
#' @param id,date,days,rate Optional scalar overrides for the group keys and rate.
#'   If `rate` is `NULL`, the function will look for a `rate` column in `x`.
#'
#' @return A one-row tibble with `id`, `date`, `days` and all qmoms metrics.
#' @examples
#' g <- subset(qmoms_surface, id == qmoms_surface$id[1] & days == qmoms_surface$days[1])
#' qmoms_compute_bygroup(list(g, rq_default_params()))
#' @export
qmoms_compute_bygroup <- function(groupparams, id = NULL, rate = NULL, days = NULL, date = NULL,
                                  cols_map = list(id = "id", date = "date", days = "days", rate = "rate", mnes = "mnes", impl_volatility = "impl_volatility")) {
  cols_map <- modifyList(list(id = "id", date = "date", days = "days", rate = "rate", mnes = "mnes", impl_volatility = "impl_volatility"), cols_map)
  if ((is.list(groupparams) || is.vector(groupparams)) && length(groupparams) == 2) {
    group <- groupparams[[1]]
    params <- groupparams[[2]]
  } else {
    stop("groupparams must be list(list(data), params)")
  }
  id <- id %||% group[[cols_map$id]][1]
  date <- date %||% group[[cols_map$date]][1]
  days <- days %||% group[[cols_map$days]][1]
  rate <- rate %||% group[[cols_map$rate]][1]


  group <- group[order(group[[cols_map$mnes]]), ]
  mnes <- group[[cols_map$mnes]]
  vol <- group[[cols_map$impl_volatility]]
  goods <- !duplicated(mnes)
  mnes <- mnes[goods]; vol <- vol[goods]


  res <- list(id = id, date = date, days = days)
  res <- c(res, qmoms_compute(mnes, vol, days, rate, params, output = "list"))
  tibble::as_tibble(res)
}


# helpers
.maybe_tibble <- function(x, output) {
  if (identical(output, "pandas")) tibble::as_tibble_row(x) else x
}

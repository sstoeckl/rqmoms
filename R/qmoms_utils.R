# parallel helpers (API mirrors Python)
#' @keywords internal
applyParallel <- function(dfGrouped, func_main, params, CPUUsed = parallel::detectCores()) {
  # dfGrouped: list of data.frames; func_main expects list(group, params)
  cl <- parallel::makeCluster(min(CPUUsed, parallel::detectCores()))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl, varlist = c("func_main", "params"), envir = environment())
  ret_list <- parallel::parLapply(cl, dfGrouped, function(g) func_main(list(g, params)))
  out <- Filter(Negate(is.null), ret_list)
  if (!length(out)) return(data.frame())
  vctrs::vec_rbind(!!!out)
}


#' @keywords internal
applySerial <- function(dfGrouped, func_main, params) {
  ret_list <- lapply(dfGrouped, function(g) func_main(list(g, params)))
  out <- Filter(Negate(is.null), ret_list)
  if (!length(out)) return(data.frame())
  vctrs::vec_rbind(!!!out)
}

#' Filter option rows by moneyness, delta windows and quality flags
#'
#' Mirrors the Python helper: removes in-the-money quotes and rows
#' outside delta windows; optionally enforces open interest / bid / min price.
#'
#' @param optdata A data frame with columns `mnes`, optional `delta`,
#'   and optionally `open_interest`, `best_bid`, `best_offer`, `prem`.
#' @param filter A list with bounds (see [rq_default_params()]$filter),
#'   e.g. `mnes_lim`, `delta_put_lim`, `delta_call_lim`, `open_int_zero`,
#'   `best_bid_zero`, `min_price`.
#' @return A filtered data frame (sorted by `id`, `date`, `mnes` if present).
#' @examples
#' # keep only OTM & within delta windows
#' head(filter_options(qmoms_surface, rq_default_params()$filter))
#' @export
filter_options <- function(optdata, filter) {
  g1 <- (optdata$mnes >= filter$mnes_lim[1]) & (optdata$mnes <= filter$mnes_lim[2])
  g2 <- (!is.null(optdata$delta)) & (optdata$delta >= filter$delta_call_lim[1]) & (optdata$delta <= filter$delta_call_lim[2])
  g3 <- (!is.null(optdata$delta)) & (optdata$delta >= filter$delta_put_lim[1]) & (optdata$delta <= filter$delta_put_lim[2])
  g1 <- g1 & (g2 | g3)
  if ("open_interest" %in% names(optdata)) g1 <- g1 & (optdata$open_interest >= filter$open_int_zero)
  if ("best_bid" %in% names(optdata)) {
    g1 <- g1 & (optdata$best_bid >= filter$best_bid_zero)
    if ("best_offer" %in% names(optdata)) g1 <- g1 & ((optdata$best_bid + optdata$best_offer)/2 >= filter$min_price)
  }
  optdata[g1, , drop = FALSE][order(optdata$id[g1], optdata$date[g1], optdata$mnes[g1]), ]
}


#' Interpolate a zero rate for a given maturity (or merge rates into a surface)
#'
#' Interpolates the zero curve by `date` and `days`, returning either a single
#' scalar rate (when `date` and `days` are given) or merging a `rate` column
#' into a surface data frame (`df_surf`).
#'
#' @param df_rate Data frame with `date`, `days`, `rate` (decimal p.a.).
#' @param df_surf Optional surface with `date`, `days`. If supplied, returns
#'   `df_surf` with a `rate` column merged by date/maturity.
#' @param date Optional single `Date` for scalar interpolation.
#' @param days Optional single integer days-to-maturity for scalar interpolation.
#' @return A numeric rate (if `date` & `days` are given) or a data frame with
#'   a `rate` column (if `df_surf` is given).
#' @examples
#' # scalar interpolation
#' one <- subset(qmoms_surface, id == qmoms_surface$id[1] & days == qmoms_surface$days[1])
#' get_rate_for_maturity(qmoms_zerocd, date = one$date[1], days = one$days[1])
#'
#' # merge into a surface
#' head(get_rate_for_maturity(qmoms_zerocd, df_surf = qmoms_surface))
#'
#' @importFrom utils tail
#'
#' @export
get_rate_for_maturity <- function(df_rate, df_surf = NULL, date = NULL, days = NULL) {
  if (is.null(df_surf) && (is.null(date) || is.null(days))) {
    message("Error: Not enough inputs, provide date and days"); return(NA_real_)
  }
  if (!is.null(date) && !is.null(days)) {
    Z <- df_rate[df_rate$date == date, , drop = FALSE]
    if (!nrow(Z)) { # fallback to last available date <= date
      Z <- df_rate[df_rate$date <= date, , drop = FALSE]
      if (!nrow(Z)) return(NA_real_)
      last_date <- tail(Z$date, 1)
      Z <- df_rate[df_rate$date == last_date, , drop = FALSE]
    }
    Z <- Z[order(Z$date, Z$days), ]
    return(stats::approxfun(Z$days, Z$rate)(days))
  }
  if (is.data.frame(df_surf)) {
    df <- unique(df_surf[c("date", "days")])
    merged <- rbind(df_rate[, c("date","days","rate")], transform(df, rate = NA_real_))
    merged <- merged[order(merged$date, merged$days), ]
    fill_one <- function(sub) {
      bad <- is.na(sub$rate)
      if (all(!bad)) return(sub)
      sub$rate[bad] <- stats::approx(sub$days[!bad], sub$rate[!bad], xout = sub$days[bad], rule = 2)$y
      sub
    }
    filled <- do.call(rbind, by(merged, merged$date, fill_one))
    out <- merge(df_surf, filled, by = c("date","days"), all.x = TRUE, sort = FALSE)
    return(out)
  }
  NA_real_
}

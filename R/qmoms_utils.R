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


# filter options (mirrors Python signature)
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


# get zero rate for maturity (mirrors Python get_rate_for_maturity)
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

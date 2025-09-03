#' Example implied-vol surface (from vilkovgr/qmoms)
#'
#' A tibble of implied vol surfaces for several `id`/`date`/`days` snapshots.
#' Column names and units mirror the Python examples.
#'
#' @format A tibble with columns:
#' \describe{
#' \item{id}{integer — underlying identifier}
#' \item{date}{Date — observation date}
#' \item{days}{integer — calendar days to maturity}
#' \item{mnes}{numeric — moneyness K/S}
#' \item{impl_volatility}{numeric — Black(–Scholes) implied volatility}
#' \item{delta}{numeric (optional) — quote delta if available}
#' \item{best_bid, best_offer, open_interest, prem}{optional numeric fields, if present}
#' }
#' @source https://github.com/vilkovgr/qmoms (data/surface.csv)
"qmoms_surface"


#' Example zero curve (from vilkovgr/qmoms)
#'
#' Zero rates by `date` and `days` used to value the surface. Rates are in
#' **decimal** (e.g. 0.02 = 2% per annum). If the original CSV is in percent,
#' the data-raw script converts it.
#'
#' @format A tibble with columns:
#' \describe{
#' \item{date}{Date}
#' \item{days}{integer — calendar days to maturity}
#' \item{rate}{numeric — zero rate (decimal p.a.)}
#' }
#' @source https://github.com/vilkovgr/qmoms (data/zerocd.csv)
"qmoms_zerocd"

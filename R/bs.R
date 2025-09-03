#' @keywords internal
BlackScholes <- function(S0, K, r, sigma, T) {
  d1 <- (log(S0 / K) + (r + (sigma^2)/2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  c <- S0 * stats::pnorm(d1) - K * exp(-r * T) * stats::pnorm(d2)
  p <- K * exp(-r * T) * stats::pnorm(-d2) - S0 * stats::pnorm(-d1)
  list(c = c, p = p)
}


#' @keywords internal
Black <- function(F, K, r, sigma, T) {
  d1 <- (log(F / K) + (sigma^2)/2 * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  c <- (F * stats::pnorm(d1) - K * stats::pnorm(d2)) * exp(-r * T)
  p <- (K * stats::pnorm(-d2) - F * stats::pnorm(-d1)) * exp(-r * T)
  list(c = c, p = p)
}


#' @keywords internal
BlackScholes_delta <- function(S0, K, r, sigma, T) {
  d1 <- (log(S0 / K) + (r + (sigma^2)/2) * T) / (sigma * sqrt(T))
  delta_c <- stats::pnorm(d1)
  delta_p <- delta_c - 1
  list(delta_c = delta_c, delta_p = delta_p)
}


#' @keywords internal
Black_delta <- function(F, K, r, sigma, T) {
  d1 <- (log(F / K) + (sigma^2)/2 * T) / (sigma * sqrt(T))
  delta_c <- stats::pnorm(d1) * exp(-r * T)
  delta_p <- -stats::pnorm(-d1) * exp(-r * T)
  list(delta_c = delta_c, delta_p = delta_p)
}

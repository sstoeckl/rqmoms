#' @keywords internal
compute_skew_kurtosis <- function(m, k, u, ic, ip, currcalls, currputs, er, V) {
  u <- as.numeric(u); ic <- as.numeric(ic); ip <- as.numeric(ip)
  cc <- as.numeric(currcalls); pp <- as.numeric(currputs)


  a <- 3 * (u - 1) * log(1 + k) / m
  b1 <- sum(ic * (2 - (log(1 + k)/m) * ic) * cc / (u^ic))
  b2 <- sum(ip * (2 - (log(1 + k)/m) * ip) * pp / (u^ip))
  W <- a * (b1 + b2)


  a2 <- 4 * (u - 1) * (log(1 + k)/m)^2
  b1 <- sum(ic^2 * (3 - (log(1 + k)/m) * ic) * cc / (u^ic))
  b2 <- sum(ip^2 * (3 - (log(1 + k)/m) * ip) * pp / (u^ip))
  X <- a2 * (b1 + b2)


  mu <- er - 1 - er/2 * V - er/6 * W - er/24 * X
  c <- (er * V - mu^2)


  mfis <- (er * W - 3 * mu * er * V + 2 * mu^3) / (c^(3/2))
  mfik <- (er * X - 4 * mu * er * W + 6 * er * mu^2 * V - 3 * mu^4) / (c^2)
  list(mfis = as.numeric(mfis), mfik = as.numeric(mfik))
}


#' @keywords internal
cvix_func <- function(b_all, cvix_params, ki, iv, mat, a) {
  a <- a / mat
  sd_ls <- cvix_params$vol_dev %||% list()
  abs_ls <- cvix_params$abs_dev %||% list()
  out <- list()
  iv_m <- sqrt(iv * mat)
  b_all <- as.numeric(b_all); ki <- as.numeric(ki)


  for (r in sd_ls) {
    kl <- 1 - r * iv_m; ku <- 1 + r * iv_m
    sel <- ki >= kl & ki <= ku
    out[[paste0("cvix_sigma", r)]] <- a * sum(b_all[sel])
  }
  for (r in abs_ls) {
    kl <- 1 - r; ku <- 1 + r
    sel <- ki >= kl & ki <= ku
    out[[paste0("cvix_mnes", as.integer(r * 100))]] <- a * sum(b_all[sel])
  }
  out
}


#' @keywords internal
tlm_func <- function(currputs, kiputs, currputd, tlm_params, iv, mat) {
  currputs <- as.numeric(currputs); kiputs <- as.numeric(kiputs); currputd <- as.numeric(currputd)
  out <- list()
  sd_ls <- tlm_params$vol_lim %||% list()
  abs_ls <- tlm_params$delta_lim %||% list()


  iv_m <- sqrt(iv * mat)
  for (r in sd_ls) {
    tlm <- NA_real_
    k0 <- 1.0 - r * iv_m
    goods <- kiputs <= k0
    if (sum(goods) > 1) {
      Pt <- currputs[goods]; K <- kiputs[goods]
      P0 <- max(Pt); ind <- which.max(Pt); K0 <- K[ind]
      res <- try(stats::optim(c(2, 0.01), f_tlm, P0 = P0, K0 = K0, Pt = Pt, K = K, method = "L-BFGS-B"), silent = TRUE)
      if (!inherits(res, "try-error")) tlm <- res$par[2] / (1 - res$par[1])
    }
    out[[paste0("tlm_sigma", r)]] <- tlm
  }
  for (r in abs_ls) {
    tlm <- NA_real_
    rr <- if (abs(r) > 1) r/100 else r
    goods <- abs(currputd) <= abs(rr)
    if (sum(goods) > 1) {
      Pt <- currputs[goods]; K <- kiputs[goods]
      P0 <- max(Pt); ind <- which.max(Pt); K0 <- K[ind]
      res <- try(stats::optim(c(2, 0.01), f_tlm, P0 = P0, K0 = K0, Pt = Pt, K = K, method = "L-BFGS-B"), silent = TRUE)
      if (!inherits(res, "try-error")) tlm <- res$par[2] / (1 - res$par[1])
    }
    out[[paste0("tlm_delta", as.integer(abs(rr*100)))]] <- tlm
  }
  out
}


#' @keywords internal
f_tlm <- function(X, P0, K0, Pt, K) {
  xi <- X[1]; beta <- X[2]
  checkterms <- 1 + (xi / beta) * (K0 - K)
  term2 <- P0 * (checkterms^(1 - 1/xi)) / Pt
  sum((1 - term2)^2)
}

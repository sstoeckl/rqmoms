# Internal helper to import Python qmoms when present
.qmoms_py <- local({ ref <- NULL; function() {
  if (is.null(ref)) ref <<- reticulate::import("qmoms", delay_load = TRUE)
  ref
}})

#' DEV: compute via Python reference (not exported)
#' @keywords internal
pyref_compute <- function(mnes, vol, days, rate, params = rq_default_params()) {
  m <- .qmoms_py()
  out <- m$qmoms_compute(
    mnes = as.numeric(mnes), vol = as.numeric(vol),
    days = as.integer(days), rate = as.numeric(rate),
    params = reticulate::r_to_py(params, convert = TRUE), output = "pandas"
  )
  r <- reticulate::py_to_r(out)
  # normalize to named list
  if (is.atomic(r)) {
    nm <- names(r); r <- as.list(r); if (!is.null(nm)) names(r) <- nm
  } else if (is.data.frame(r)) {
    r <- as.list(r[1, , drop = TRUE])
  }
  # unify slope names
  nms <- names(r)
  nms <- sub("^slopedn$", "Slopedn", nms)
  nms <- sub("^slopeup$", "Slopeup", nms)
  names(r) <- nms
  r
}


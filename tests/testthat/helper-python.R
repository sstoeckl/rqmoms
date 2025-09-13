# tests/testthat/helper-python.R
# Robust Python detection for CI (toolcache) and local (venv).

has_py <- FALSE

if (requireNamespace("reticulate", quietly = TRUE)) {
  # 1) Prefer a pre-selected interpreter (CI sets RETICULATE_PYTHON)
  rp <- Sys.getenv("RETICULATE_PYTHON")

  # 2) Otherwise allow a project venv path
  venv <- Sys.getenv("RQMOMS_VENV", "python/.venv")

  try({
    if (nzchar(rp)) {
      # Bind to the exact interpreter our workflow installed into
      reticulate::use_python(rp, required = TRUE)
    } else if (nzchar(venv)) {
      # Local dev: use the project venv if it exists
      reticulate::use_virtualenv(venv, required = TRUE)
    }
    # Initialize and verify
    has_py <- reticulate::py_available(initialize = TRUE)

    if (has_py) {
      # numpy 2.x shim for legacy code expecting np.NAN
      reticulate::py_run_string("import numpy as np\nif not hasattr(np,'NAN'): np.NAN = np.nan")
      # ensure qmoms import works
      reticulate::import("qmoms")
    }
  }, silent = TRUE)
}

options(rqmoms.has_py = has_py)

rq_has_python <- function() isTRUE(getOption("rqmoms.has_py", FALSE))

# small helper to call Python reference (used in tests)
rq_pyref_compute <- function(mnes, vol, days, rate, params) {
  stopifnot(rq_has_python())
  m <- reticulate::import("qmoms")
  out <- m$qmoms_compute(
    mnes   = as.numeric(mnes),
    vol    = as.numeric(vol),
    days   = as.integer(days),
    rate   = as.numeric(rate),
    params = reticulate::r_to_py(params, convert = TRUE),
    output = "pandas"
  )
  r <- reticulate::py_to_r(out)
  if (is.atomic(r)) {
    nm <- names(r); r <- as.list(r); if (!is.null(nm)) names(r) <- nm
  } else if (is.data.frame(r)) {
    r <- as.list(r[1, , drop = TRUE])
  }
  r
}

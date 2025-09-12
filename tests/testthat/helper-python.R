Sys.unsetenv("RETICULATE_PYTHON")               # falls CI irgendwas gesetzt hat
venv <- Sys.getenv("RQMOMS_VENV", "python/.venv")
has_py <- FALSE
if (requireNamespace("reticulate", quietly = TRUE)) {
  try({
    reticulate::use_virtualenv(venv, required = TRUE)
    reticulate::py_run_string("import numpy as np\nif not hasattr(np,'NAN'): np.NAN = np.nan")
    reticulate::import("qmoms")
    has_py <- TRUE
  }, silent = TRUE)
}
options(rqmoms.has_py = has_py)
# ... (rest wie bei dir)

if (requireNamespace("reticulate", quietly = TRUE)) {
  try({
    reticulate::use_virtualenv(venv, required = TRUE)

    # numpy 2.x shim
    reticulate::py_run_string("import numpy as np\nif not hasattr(np,'NAN'): np.NAN = np.nan")

    # Prefer local src if present (not required in CI parity workflow)
    srcs <- c(file.path(pkg_root, "python", "qmoms_src"),
              file.path(pkg_root, "python", "qmoms"))
    srcs <- srcs[dir.exists(srcs)]
    if (length(srcs)) {
      reticulate::py_run_string(sprintf(
        "import sys; sys.path.insert(0, r'%s')",
        normalizePath(srcs[[1]], winslash="/", mustWork=TRUE)
      ))
    }

    reticulate::import("qmoms")
    has_py <- TRUE
  }, silent = TRUE)
}

options(rqmoms.has_py = has_py)

rq_has_python <- function() isTRUE(getOption("rqmoms.has_py", FALSE))

rq_pyref_compute <- function(mnes, vol, days, rate, params) {
  stopifnot(rq_has_python())
  m <- reticulate::import("qmoms")
  out <- m$qmoms_compute(
    mnes = as.numeric(mnes),
    vol  = as.numeric(vol),
    days = as.integer(days),
    rate = as.numeric(rate),
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

# tests/testthat/helper-python.R
# Runs before all tests in a clean session

pkg_root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                          winslash = "/", mustWork = TRUE)

# Allow override via env var if you want: RQMOMS_VENV="path/to/.venv"
venv <- Sys.getenv("RQMOMS_VENV", file.path(pkg_root, "python", ".venv"))

ok <- FALSE
if (dir.exists(venv)) {
  # try to initialize reticulate with this venv
  suppressWarnings({
    try({
      reticulate::use_virtualenv(venv, required = TRUE)

      # numpy 2.x shim
      reticulate::py_run_string("import numpy as np\nif not hasattr(np,'NAN'): np.NAN = np.nan")

      # prefer submodule
      srcs <- c(file.path(pkg_root, "python", "qmoms_src"),
                file.path(pkg_root, "python", "qmoms"))
      srcs <- normalizePath(srcs[dir.exists(srcs)], winslash = "/", mustWork = FALSE)
      if (length(srcs)) {
        reticulate::py_run_string(sprintf("import sys; sys.path.insert(0, r'%s')", srcs[1]))
      }

      # test import
      reticulate::import("qmoms")
      ok <- TRUE
    }, silent = TRUE)
  })
}

# Make availability visible across files
options(rqmoms.has_py = ok)

# Small helpers for tests ----------------------------------------------

rq_has_python <- function() isTRUE(getOption("rqmoms.has_py", FALSE))

rq_pyref_compute <- function(mnes, vol, days, rate, params) {
  stopifnot(rq_has_python())
  m <- reticulate::import("qmoms")
  out <- m$qmoms_compute(
    mnes = as.numeric(mnes), vol = as.numeric(vol),
    days = as.integer(days), rate = as.numeric(rate),
    params = reticulate::r_to_py(params, convert = TRUE),
    output = "pandas"
  )
  r <- reticulate::py_to_r(out)
  if (is.atomic(r)) { nm <- names(r); r <- as.list(r); if (!is.null(nm)) names(r) <- nm
  } else if (is.data.frame(r)) { r <- as.list(r[1, , drop = TRUE]) }
  r
}

.onLoad <- function(libname, pkgname) {
  # Prefer our local dev venv if present
  venv <- file.path(getwd(), "python", ".venv")
  if (dir.exists(venv)) {
    try(reticulate::use_virtualenv(venv, required = FALSE), silent = TRUE)
  }
}

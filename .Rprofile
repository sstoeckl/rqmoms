local({
  venv <- file.path("python", ".venv")
  py <- if (.Platform$OS.type == "windows")
    file.path(venv, "Scripts", "python.exe") else file.path(venv, "bin", "python")
  # Only set RETICULATE_PYTHON if not already provided (e.g. by CI)
  if (!nzchar(Sys.getenv("RETICULATE_PYTHON")))
    Sys.setenv(RETICULATE_PYTHON = normalizePath(py, winslash = "/", mustWork = FALSE))

  # Optional: falls du mit qmoms_src (Submodule) arbeitest, sys.path ergänzen
  src <- file.path("python", "qmoms_src")
  if (dir.exists(src)) {
    src <- normalizePath(src, winslash = "/", mustWork = TRUE)
    try(reticulate::py_run_string(sprintf("import sys; sys.path.insert(0, r'%s')", src)),
        silent = TRUE)
  }
})

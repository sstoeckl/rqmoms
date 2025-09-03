# dev/01-setup-python.R
stop_on_error <- function(expr) tryCatch(expr, error = function(e) stop(e))

venv_path <- file.path("python", ".venv")
if (!dir.exists("python")) dir.create("python")

# Create virtualenv
reticulate::virtualenv_create(envname = venv_path, python = NULL)
reticulate::use_virtualenv(venv_path, required = TRUE)


# Ensure pip is fresh
reticulate::py_install("pip", envname = venv_path)


# Install qmoms from local path in editable mode
qmoms_src <- normalizePath(file.path("python","qmoms"), winslash = "/", mustWork = TRUE)

reticulate::py_install(
  c("pip>=24.0","setuptools>=68","wheel","build","setuptools_scm"),
  envname = venv_path, pip = TRUE, upgrade = TRUE
)

# *** WICHTIG: -e und Pfad als ZWEI getrennte Argumente übergeben ***
reticulate::py_install(
  packages = c("-e", qmoms_src),
  envname = venv_path, pip = TRUE
)

# Test: Modul importieren
qm <- reticulate::import("qmoms")
str(qm)
qm$`__file__`

# Show config
print(reticulate::py_config())
cli::cli_alert_success("Python venv ready at {venv_path} and 'qmoms' installed (editable).")

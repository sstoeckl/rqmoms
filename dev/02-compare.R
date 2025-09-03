# dev/02-compare.R — run after dev/01-setup-python.R


# Load package code in dev mode
devtools::load_all()

# Example data from the Python repo (if present)
cand <- c(
  file.path("python","qmoms_src","data"),
  file.path("python","qmoms_src","qmoms","data")
)
ex_dir <- cand[dir.exists(cand)][1]
stopifnot(length(ex_dir) == 1L)

surf_path <- file.path(ex_dir, "surface.csv")
rate_path <- file.path(ex_dir, "zerocd.csv")
stopifnot(file.exists(surf_path), file.exists(rate_path))

surf <- readr::read_csv(surf_path, show_col_types = FALSE)
# pick a single (id, date, days) grouping as smoke test
one <- surf |>
  dplyr::filter(id == dplyr::first(id), days == dplyr::first(days))


# --- Python reference ---
py_res <- pyref_compute(
  mnes = one$mnes,
  vol = one$impl_volatility,
  days = dplyr::first(one$days),
  rate = 0.02,
  params = rq_default_params()
)

# --- Pure R implementation (to be completed) ---
# r_res <- qmoms_compute(surface = one, params = rq_default_params())
print(py_res)
cli::cli_alert_info("Python reference produced {length(py_res)} metrics.")

# Next comparison
r_res <- qmoms_compute(
  mnes = one$mnes,
  vol = one$impl_volatility,
  days = dplyr::first(one$days),
  rate = 0.02,
  params = rq_default_params(),
  output = "list"
)
cli::cli_alert_success(sprintf("Slopedn diff = %.6g", r_res[["slopedn"]] - py_res[["slopedn"]]))
cli::cli_alert_success(sprintf("Slopeup diff = %.6g", r_res[["slopeup"]] - py_res[["slopeup"]]))


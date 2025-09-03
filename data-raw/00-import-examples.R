# data-raw/00-import-examples.R
# Build package datasets from the Python repo's example CSVs.
# Run from the PACKAGE ROOT: source("data-raw/00-import-examples.R")


# deps used only at build time
stopifnot(file.exists("DESCRIPTION"))
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(usethis); library(tibble)
})


# Locate the examples directory. Prefer the submodule/clone under python/.
# You may also set an env var RQMOMS_EXAMPLES_DIR to point to a local folder.
find_examples <- function() {
  cand <- c(
    file.path("python","qmoms_src","data"),
    file.path("python","qmoms_src","qmoms","data"),
    file.path("python","qmoms","data"),
    file.path("python","qmoms","qmoms","data"),
    Sys.getenv("RQMOMS_EXAMPLES_DIR", unset = NA_character_)
  )
  cand <- cand[!is.na(cand)]
  ex <- cand[dir.exists(cand)]
  if (length(ex)) ex[[1]] else NA_character_
}


ex_dir <- find_examples()
if (is.na(ex_dir)) {
  stop("Could not find examples directory. Either add the Python repo under python/qmoms_src or set RQMOMS_EXAMPLES_DIR to the folder containing surface.csv and zerocd.csv.")
}


surf_path <- file.path(ex_dir, "surface.csv")
rate_path <- file.path(ex_dir, "zerocd.csv")
stopifnot(file.exists(surf_path), file.exists(rate_path))


surf_raw <- readr::read_csv(surf_path, show_col_types = FALSE)
rate_raw <- readr::read_csv(rate_path, show_col_types = FALSE)

# Convert rate to decimal if the file is in percent
if (max(rate$rate, na.rm = TRUE) > 1) rate$rate <- rate$rate / 100

x <- c("04JAN1996", "15FEB1997")

old <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
# Standardize types
surf <- surf_raw |>
  mutate(
    id = as.integer(id),
    date = as.Date(date, format="%d%b%Y"),
    days = as.integer(days),
    mnes = as.numeric(mnes),
    impl_volatility = as.numeric(impl_volatility)
  ) |>
  arrange(id, date, days, mnes) |>
  as_tibble()

rate <- rate_raw |>
  mutate(
    date = as.Date(date, format="%d%b%Y"),
    days = as.integer(days),
    rate = as.numeric(rate)
  ) |>
  arrange(date, days) |>
  as_tibble()

Sys.setlocale("LC_TIME", old)

# Assign final dataset objects
qmoms_surface <- surf
qmoms_zerocd <- rate


# Save to data/ (xz for small footprint)
usethis::use_data(qmoms_surface, qmoms_zerocd, overwrite = TRUE, compress = "xz")

message("Saved data/ {qmoms_surface, qmoms_zerocd}. Access after library(rqmoms).")

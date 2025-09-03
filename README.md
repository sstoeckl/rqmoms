rqmoms
================

# rqmoms <img src="man/figures/logo.png" align="right" height="120" alt="logo"/>

**Option-Implied Moments & Diagnostics in Pure R** — a faithful port of
the Python package
**[vilkovgr/qmoms](https://github.com/vilkovgr/qmoms)** with identical
function signatures, output keys, and defaults.

<!-- badges: start -->

[![pkgdown](https://github.com/sstoeckl/rqmoms/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/sstoeckl/rqmoms/actions/workflows/pkgdown.yaml)[![parity](https://github.com/sstoeckl/rqmoms/actions/workflows/parity.yaml/badge.svg)](https://github.com/sstoeckl/rqmoms/actions/workflows/parity.yaml)[![check](https://github.com/sstoeckl/rqmoms/actions/workflows/check.yaml/badge.svg)](https://github.com/sstoeckl/rqmoms/actions/workflows/check.yaml)

<!-- badges: end -->

> ⚖️ *Scientific intent:* `rqmoms` aims for **numerical identity** with
> the reference Python implementation for a given IV surface and
> parameter set. A dev-only Python reference workflow (ignored in
> builds) can be used locally to double-check results.

## Installation

Soon, installation should be possible from CRAN

``` r
# From source (development)
install.packages("rqmoms")
```

util then it can be installed from github with:

``` r
# install.packages("devtools")
devtools::install_github("sstoeckl/rqmoms")
```

`rqmoms` imports: `dplyr`, `tibble`, `purrr`, `cli`, `stats`, `vctrs`,
`pracma`.

------------------------------------------------------------------------

## Packaged example data

This package ships two datasets (loaded automatically with
`library(rqmoms)`):

- `qmoms_surface` — IV surface snapshots (columns: `id`, `date`, `days`,
  `mnes`, `impl_volatility`, optional: `delta`, `best_bid`,
  `best_offer`, `open_interest`, `prem`)
- `qmoms_zerocd` — zero curve by `date` and `days` (in **decimal**;
  e.g. `0.02` = 2% p.a.)

They are derived from the Python repo’s `data/surface.csv` and
`data/zerocd.csv`.

``` r
library(rqmoms)
str(qmoms_surface)
> Classes 'tbl_df', 'tbl' and 'data.frame': 234 obs. of  7 variables:
>  $ id             : int  10078 10078 10078 10078 10078 10078 10078 10078 10078 10078 ...
>  $ date           : Date, format: "1996-01-04" "1996-01-04" ...
>  $ days           : int  30 30 30 30 30 30 30 30 30 30 ...
>  $ mnes           : num  0.843 0.853 0.879 0.886 0.912 ...
>  $ prem           : num  0.191 0.0277 0.1657 0.0372 0.1442 ...
>  $ impl_volatility: num  0.847 0.778 0.84 0.777 0.834 ...
>  $ delta          : num  80 -20 75 -25 70 -30 65 -35 -40 60 ...
head(qmoms_zerocd)
>         date days     rate
> 1 1996-01-02    9 5.763067
> 2 1996-01-02   15 5.745902
> 3 1996-01-02   50 5.673317
> 4 1996-01-02   78 5.608884
> 5 1996-01-02  169 5.473762
> 6 1996-01-02  260 5.352667
```

------------------------------------------------------------------------

## Quick start — single surface (mirrors Python `qmoms_compute()`)

``` r
params <- rq_default_params()

# pick one (id, date, days)
one <- subset(qmoms_surface,
              id == qmoms_surface$id[1] & days == qmoms_surface$days[1])

# rate from the zero curve (interpolated at this date/maturity)
r30  <- get_rate_for_maturity(qmoms_zerocd,
                              date = one$date[1],
                              days = one$days[1])

res <- qmoms_compute(
  mnes   = one$mnes,
  vol    = one$impl_volatility,
  days   = one$days[1],
  rate   = r30,
  params = params,
  output = "list"
)
str(res)
> List of 17
>  $ nopt       : int 234
>  $ smfiv      : num 6.39
>  $ mfiv_bkm   : num 3.1
>  $ mfiv_bjn   : num 3.92
>  $ smfivd     : num 0.0076
>  $ mfivd_bkm  : num 0.0115
>  $ mfivd_bjn  : num 0.01
>  $ mfis       : num -0.0476
>  $ mfik       : num 3.28
>  $ cvix_sigma2: num 1.94
>  $ cvix_mnes20: num 1.2
>  $ rix        : num 0.00151
>  $ rixnorm    : num 0.131
>  $ tlm_sigma2 : num NA
>  $ tlm_delta20: num NA
>  $ slopedn    : num NA
>  $ slopeup    : num -2.29e-15
```

------------------------------------------------------------------------

## Whole-dataframe — grouped compute (mirrors Python README)

First, merge/interpolate rates into the surface:

``` r
library(dplyr)
> 
> Attache Paket: 'dplyr'
> Die folgenden Objekte sind maskiert von 'package:stats':
> 
>     filter, lag
> Die folgenden Objekte sind maskiert von 'package:base':
> 
>     intersect, setdiff, setequal, union

# exact-maturity rate interpolation by date
surf_r <- get_rate_for_maturity(qmoms_zerocd, df_surf = qmoms_surface)
```

### A) dplyr `group_map()`

``` r
res_bygroup <- surf_r |>
  dplyr::group_by(id, date, days) |>
  dplyr::group_map(~{
    # .x: rows of the group (no keys); .y: one-row tibble of keys
    tibble::as_tibble(
      c(
        list(id = .y$id[[1]], date = .y$date[[1]], days = .y$days[[1]]),
        qmoms_compute(
          mnes   = .x$mnes,
          vol    = .x$impl_volatility,
          days   = .y$days[[1]],
          rate   = .x$rate[1],
          params = params,
          output = "list"
        )
      )
    )
  }) |>
  dplyr::bind_rows()

dplyr::glimpse(res_bygroup)
> Rows: 9
> Columns: 20
> $ id          <int> 10078, 10078, 10078, 10078, 10078, 10078, 10078, 10078, 10…
> $ date        <date> 1996-01-04, 1996-01-05, 1996-01-08, 1996-01-09, 1996-01-1…
> $ days        <int> 30, 30, 30, 30, 30, 30, 30, 30, 30
> $ nopt        <int> 26, 26, 26, 26, 26, 26, 26, 26, 26
> $ smfiv       <dbl> 6.079506, 6.118586, 6.155363, 6.197620, 6.356862, 6.012891…
> $ mfiv_bkm    <dbl> 3.039818, 3.041774, 3.046623, 3.066407, 3.084455, 3.030018…
> $ mfiv_bjn    <dbl> 3.809570, 3.818995, 3.830234, 3.855023, 3.901564, 3.788134…
> $ smfivd      <dbl> 0.003832652, 0.003139576, 0.003262268, 0.006018649, 0.0040…
> $ mfivd_bkm   <dbl> 0.004951385, 0.003880698, 0.004068641, 0.007911383, 0.0051…
> $ mfivd_bjn   <dbl> 0.004538752, 0.003611914, 0.003775383, 0.007208670, 0.0047…
> $ mfis        <dbl> -0.090644976, -0.024247430, -0.015743917, -0.109163284, 0.…
> $ mfik        <dbl> 3.262624, 3.129021, 3.101145, 3.287541, 3.001450, 3.364392…
> $ cvix_sigma2 <dbl> 1.899215, 1.900327, 1.903911, 1.915603, 1.926735, 1.893552…
> $ cvix_mnes20 <dbl> 1.197292, 1.194654, 1.194702, 1.202204, 1.198877, 1.198188…
> $ rix         <dbl> 0.0004126325, 0.0002687845, 0.0002932578, 0.0007027131, 0.…
> $ rixnorm     <dbl> 0.08333678, 0.06926188, 0.07207760, 0.08882305, 0.07299096…
> $ tlm_sigma2  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA
> $ tlm_delta20 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA
> $ slopedn     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA
> $ slopeup     <dbl> -1.637620e-15, -3.249709e-15, 1.608695e-15, 2.615529e-15, …
```

### B) Convenience wrapper `qmoms_compute_bygroup()`

If you prefer, you can call the wrapper on each group.

``` r
res_bygroup2 <- surf_r |>
  dplyr::group_by(id, date, days) |>
  dplyr::group_map(~{
    qmoms_compute_bygroup(
      list(.x, params),
      id   = .y$id[[1]],
      date = .y$date[[1]],
      days = .y$days[[1]],
      rate = .x$rate[1]
    )
  }) |>
  dplyr::bind_rows()

# Same result (up to column order)
dplyr::all_equal(
  dplyr::arrange(res_bygroup,  id, date, days),
  dplyr::arrange(res_bygroup2, id, date, days)
)
> Warning: `all_equal()` was deprecated in dplyr 1.1.0.
> ℹ Please use `all.equal()` instead.
> ℹ And manually order the rows/cols as needed
> This warning is displayed once every 8 hours.
> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
> generated.
> [1] TRUE
```

### C) Column mapping example (mirrors Python column/dtype mapping)

``` r
# rename columns
df_alt <- surf_r |>
  dplyr::rename(ID = id, Date = date, Days = days, Rate = rate, M = mnes, IV = impl_volatility)

# mapping for the wrapper
map <- list(id = "ID", date = "Date", days = "Days", rate = "Rate", mnes = "M", impl_volatility = "IV")

# one group through the wrapper
gs <- df_alt |>
  dplyr::group_by(ID, Date, Days) |>
  dplyr::group_split()

ex_mapped <- qmoms_compute_bygroup(
  list(gs[[1]], params),
  id   = dplyr::first(gs[[1]]$ID),
  date = dplyr::first(gs[[1]]$Date),
  days = dplyr::first(gs[[1]]$Days),
  rate = dplyr::first(gs[[1]]$Rate),
  cols_map = map
)
ex_mapped
> # A tibble: 1 × 20
>      id date        days  nopt smfiv mfiv_bkm mfiv_bjn  smfivd mfivd_bkm
>   <int> <date>     <int> <int> <dbl>    <dbl>    <dbl>   <dbl>     <dbl>
> 1 10078 1996-01-04    30    26  6.08     3.04     3.81 0.00383   0.00495
> # ℹ 11 more variables: mfivd_bjn <dbl>, mfis <dbl>, mfik <dbl>,
> #   cvix_sigma2 <dbl>, cvix_mnes20 <dbl>, rix <dbl>, rixnorm <dbl>,
> #   tlm_sigma2 <dbl>, tlm_delta20 <dbl>, slopedn <dbl>, slopeup <dbl>
```

------------------------------------------------------------------------

## Parameters — defaults & customization

Defaults mirror Python (grid, filters, CVIX/TLM windows, slope windows):

``` r
rq_default_params()
> $atmfwd
> [1] FALSE
> 
> $grid
> $grid$number_points
> [1] 500
> 
> $grid$grid_limit
> [1] 2
> 
> 
> $filter
> $filter$mnes_lim
> [1]    0 1000
> 
> $filter$delta_put_lim
> [1] -0.499  0.000
> 
> $filter$delta_call_lim
> [1] 0.0 0.5
> 
> $filter$best_bid_zero
> [1] -1
> 
> $filter$open_int_zero
> [1] -1
> 
> $filter$min_price
> [1] 0
> 
> 
> $semivars
> $semivars$compute
> [1] TRUE
> 
> 
> $mfismfik
> $mfismfik$compute
> [1] TRUE
> 
> 
> $cvix
> $cvix$compute
> [1] TRUE
> 
> $cvix$abs_dev
> $cvix$abs_dev[[1]]
> [1] 0.2
> 
> 
> $cvix$vol_dev
> $cvix$vol_dev[[1]]
> [1] 2
> 
> 
> 
> $rix
> $rix$compute
> [1] TRUE
> 
> 
> $tlm
> $tlm$compute
> [1] TRUE
> 
> $tlm$delta_lim
> $tlm$delta_lim[[1]]
> [1] 20
> 
> 
> $tlm$vol_lim
> $tlm$vol_lim[[1]]
> [1] 2
> 
> 
> 
> $slope
> $slope$compute
> [1] TRUE
> 
> $slope$deltaP_lim
> [1] -0.50 -0.05
> 
> $slope$deltaC_lim
> [1] 0.05 0.50
```

### Change what’s computed and tweak windows

``` r
params2 <- rq_default_params()

# Example: turn off TLM, adjust slope & CVIX windows
params2 <- utils::modifyList(params2, list(
  tlm   = list(compute = FALSE),
  slope = list(compute = TRUE, deltaP_lim = c(-0.4, -0.1), deltaC_lim = c(0.05, 0.5)),
  cvix  = list(compute = TRUE, abs_dev = list(0.1, 0.2, 0.3), vol_dev = list(1.5, 2))
))

one <- subset(qmoms_surface, id == qmoms_surface$id[1] & days == qmoms_surface$days[1])
r30 <- get_rate_for_maturity(qmoms_zerocd, date = one$date[1], days = one$days[1])
res_custom <- qmoms_compute(one$mnes, one$impl_volatility, one$days[1], r30, params2, output = "list")

str(res_custom[names(res_custom)[startsWith(names(res_custom), "cvix_")]])
> List of 2
>  $ cvix_sigma2: num 1.94
>  $ cvix_mnes20: num 1.2
```

### Slopes (sign convention)

`slopedn` is the **OLS slope** of IV on Δ on the put side; `slopeup` is
**minus** the OLS slope on the call side (larger `slopeup` ⇒ steeper
right tail). This matches the Python implementation.

------------------------------------------------------------------------

## Dev: Python parity vignette

A vignette **“Python parity: R vs Python outputs”** reproduces the
grouped examples and prints side-by-side diffs for all metrics.

- **Coming soon:** `vignettes/python-parity.Rmd`
- It will appear on the pkgdown site under **Articles → Python parity**.

*(The vignette is optional and only for developers who keep a local
clone of the Python repo in `python/qmoms_src` with a private
virtualenv. The R package itself is pure R.)*

------------------------------------------------------------------------

## What does `qmoms_compute()` return?

Keys match the Python package exactly:

| Key | Meaning (brief) |
|----|----|
| `nopt` | Number of options used |
| `smfiv` | Martin’s model-free implied variance |
| `mfiv_bkm` | BKM (Bakshi–Kapadia–Madan) variance kernel |
| `mfiv_bjn` | Alternative variance kernel |
| `smfivd` | Semi-variance (downside) |
| `mfivd_bkm` | Semi-variance (BKM kernel) |
| `mfivd_bjn` | Semi-variance (BJN kernel) |
| `mfis` | Implied skewness (BKM) |
| `mfik` | Implied kurtosis (BKM) |
| `cvix_*` | Curvature window integrals (by vol window `sigmaX` or abs moneyness window `mnesY`) |
| `rix`, `rixnorm` | Difference & normalized difference between BKM and BJN downside integrals |
| `tlm_*` | Tail loss measure for delta/vol windows |
| `slopedn`/`slopeup` | OLS slope of IV on Δ in put/call windows (call side reported with a negative sign) |

------------------------------------------------------------------------

## Citation & Credits

This package is a **pure-R re-implementation** of
**[vilkovgr/qmoms](https://github.com/vilkovgr/qmoms)**. If you use
`rqmoms`, please also cite the original Python package and related
research by the authors.

## License

MIT. See `LICENSE`.

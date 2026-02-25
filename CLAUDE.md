# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this package does

`rqmoms` is a pure-R port of the Python package [`vilkovgr/qmoms`](https://github.com/vilkovgr/qmoms). It computes **option-implied moments and characteristics** (variance, skewness, kurtosis, semi-variances, CVIX, TLM, RIX, slopes) from implied-volatility surface data. The primary goal is **numerical identity** with the Python reference for a given IV surface and parameter set.

## Common development commands

All R development uses `devtools`:

```r
devtools::load_all()        # reload package during development
devtools::document()        # regenerate roxygen2 docs (NAMESPACE, man/)
devtools::test()            # run all tests
devtools::check()           # full R CMD check
```

Run a single test file:
```r
devtools::test(filter = "qmoms")   # matches test-qmoms.R
devtools::test(filter = "bygroup") # matches test-bygroup.R
```

Python-parity tests are skipped automatically when Python is unavailable (guarded by `rq_has_python()`).

## Python dev environment (for parity testing)

The Python reference implementation is a git submodule at `python/qmoms_src`. Set up the local venv once:

```bash
git submodule update --init
python -m venv python/.venv
python/.venv/Scripts/pip install numpy pandas scipy tqdm
python/.venv/Scripts/pip install -e python/qmoms_src
```

`.Rprofile` auto-sets `RETICULATE_PYTHON` to the venv Python. `R/zzz.R` activates it on load. The parity CI workflow (`parity.yaml`) runs weekly and is triggered manually; it installs qmoms from GitHub into the CI toolcache Python.

## Architecture

### Computation pipeline (`R/qmoms.R`)

`qmoms_compute(mnes, vol, days, rate, params, output)` is the core function:
1. Sorts and deduplicates moneyness/vol inputs
2. Calls `interpolate_iv_by_moneyness()` to place IV on a symmetric geometric grid
3. Prices OTM calls and puts using Black-Scholes (`atmfwd=FALSE`) or Black/forward (`atmfwd=TRUE`)
4. Computes trapezoid integration weights (Python-style)
5. Computes SMFIV, MFIV_BJN, MFIV_BKM integrals
6. Conditionally computes: semi-variances, MFIS/MFIK (skew/kurtosis), CVIX, RIX, TLM, slopes

`qmoms_compute_bygroup(list(group_df, params), ...)` is a convenience wrapper that accepts a single-surface data frame, applies `filter_options()`, and returns a one-row tibble with keys + all metrics.

### Key internal modules

| File | Role |
|---|---|
| `R/interp.R` | PCHIP interpolation + moneyness grid construction (matches SciPy `PchipInterpolator`; clamps at observed boundaries) |
| `R/bs.R` | `BlackScholes()` / `Black()` pricing and delta functions (internal only) |
| `R/moments_extra.R` | `compute_skew_kurtosis()`, `cvix_func()`, `tlm_func()` |
| `R/qmoms_params.R` | `rq_default_params()` — single source of truth for all defaults |
| `R/qmoms_utils.R` | `filter_options()`, `get_rate_for_maturity()`, `applySerial()` / `applyParallel()` |
| `R/py_ref.R` | `pyref_compute()` — dev-only bridge to Python reference via `reticulate` |
| `R/zzz.R` | `.onLoad` activates local venv if present |

### Grid and interpolation convention

The moneyness grid is a symmetric geometric sequence: `ki = u^(-m:m)` where `u = (1+k)^(1/m)`, `m = 500` points (default), `k = 2` limit. IV is interpolated inside the observed moneyness range with PCHIP; outside the range the boundary IV value is used (flat extrapolation). OTM is `mnes >= 1` for calls, `< 1` for puts.

### Parameters

All behaviour is controlled via the list from `rq_default_params()`. Modify with `utils::modifyList()`. Boolean `compute` flags in nested lists (`semivars`, `mfismfik`, `cvix`, `rix`, `tlm`, `slope`) turn feature blocks on/off. CVIX and TLM accept variable-length window lists.

### Datasets

`qmoms_surface` and `qmoms_zerocd` are bundled lazy datasets (sourced from the Python repo's CSVs). Rates are in **decimal** (e.g. `0.02` = 2% p.a.). Use `get_rate_for_maturity()` to interpolate/merge rates before calling `qmoms_compute`.

### Test structure

- `test-qmoms.R` — pure-R smoke tests + optional Python parity for core metrics
- `test-bygroup.R` — consistency between `qmoms_compute_bygroup()` and the manual vector interface + optional Python spot-check
- `test-data.R` — dataset sanity checks
- `helper-python.R` — Python detection logic; sets `options(rqmoms.has_py = ...)` used by `rq_has_python()`

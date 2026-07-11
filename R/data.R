#' Preset spectral indices definitions
#'
#' @format ## `spectral_indices`
#' A tibble with 25 rows and 9 columns:
#' \describe{
#'   \item{proxy_name}{Character. Unique proxy identifier.}
#'   \item{proxy_type}{Character. One of `"rabd"`, `"raba"`, `"ratio"`, `"difference"`, `"remp"`.}
#'   \item{continuum_edges}{List. Numeric vector of length 2, continuum anchor wavelengths in nm. `NA` where not applicable.}
#'   \item{absorption_band}{List. Numeric vector, trough wavelength(s) in nm. Single value for `"strict"`, length-2 range for `"max"` and `"mid"`. `NA` where not applicable.}
#'   \item{index_type}{Character. Calculation variant passed to the corresponding function. `NA` where not applicable.}
#'   \item{bands}{List. Numeric vector of length 2, band wavelengths in nm. `NA` where not applicable.}
#'   \item{search_range}{List. Numeric vector of length 2, search window in nm. `NA` defers to function default. `NA` where not applicable.}
#'   \item{interpretation}{Character. Typical sedimentological or biogeochemical interpretation.}
#'   \item{reference}{Character. Bibliographic reference. `NA` where not sourced from a specific publication.}
#' }
#'
#' @details
#' Each row maps directly to a single function call. Columns correspond to
#' argument names in the relevant `hsi_calc_*` function, so a row can be
#' passed to a function without renaming. Columns that are not used by a given
#' `proxy_type` carry `NA`.
#'
#' Entries marked with `index_type = "x"` (RABA rows) use a placeholder
#' pending finalisation of the RABA calculation variants.
#'
#' The SWIR entry `rabd16601690` carries `NA` for all spectral parameters
#' pending verification.
#'
#' @source Selected from available literature. See the `reference` column for
#'   per-entry citations.
"spectral_indices"

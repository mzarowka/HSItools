#' Apply Minimum Noise Fraction transformation to a SpatRaster
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param fit An object of class \code{mnf} as returned by
#'   [`hsi_calc_mnf()`][hsi_calc_mnf].
#' @param n Positive integer. Number of signal-rich MNF components to retain.
#'   Inspect \code{fit$values} to choose. Default \code{NULL}.
#' @param filename Character. Output filename. Default `""` keeps result in memory.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#' @param ... Additional arguments passed to [`terra::writeRaster()`].
#'
#' @returns A [`SpatRaster`][terra::SpatRaster-class] with `n` MNF component layers.
#'
#' @details
#' MNF components are ordered by decreasing noise fraction: the first component
#' carries the most noise and the last carries the most signal. `n` selects the
#' signal-rich tail, so the output layer `MNF_1` always corresponds to the most
#' signal-rich component regardless of the total number of components computed.
#' The internal index reversal is hidden from the user.
#'
#' `x` is used only as a spatial template (extent, CRS, resolution). Its band
#' count is irrelevant — only cell count must match `nrow(fit$x)`.
#'
#' Separating [`hsi_calc_mnf()`] from `hsi_apply_mnf()` means the expensive
#' eigen decomposition runs once; `hsi_apply_mnf()` can be called repeatedly
#' with different values of `n` without recomputing the transform.
#'
#' @seealso [`hsi_calc_mnf()`]
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#' fit <- hsi_calc_mnf(x)
#' fit$values
#'
#' x_mnf <- hsi_apply_mnf(x, fit = fit, n = 10L)
#'
#' x_mnf <- hsi_apply_mnf(
#'   x,
#'   fit = fit,
#'   n = 10L,
#'   filename = "output_mnf.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
hsi_apply_mnf <- function(
  x,
  fit,
  n = NULL,
  filename = "",
  overwrite = FALSE,
  ...
) {
  # Validate inputs
  check_spatraster(x)

  if (!inherits(fit, "mnf")) {
    cli::cli_abort(
      c(
        "{.arg fit} must be of class {.cls mnf}, not {.cls {class(fit)[[1]]}}.",
        "i" = "{.arg fit} should come either from {hsi_calc_mnf} or {spacetime::mnf}."
      ),
      class = "hsitools_error"
    )
  }

  if (is.null(n)) {
    cli::cli_abort(
      c(
        "{.arg n} must be specified.",
        "i" = "Inspect {.code fit$values} to choose the number of signal-rich components, then pass it as {.arg n}."
      ),
      class = "hsitools_error"
    )
  }

  check_numeric(n, len = 1, positive = TRUE)

  if (n > ncol(fit$x)) {
    cli::cli_abort(
      "{.arg n} is > than number of available components.",
      class = "hsitools_error"
    )
  }

  if (terra::ncell(x) != nrow(fit$x)) {
    cli::cli_abort(
      c(
        "Number of cells in {.arg x} does not match number of pixels in {.arg fit}.",
        "i" = "{.val {terra::ncell(x)}} cells vs {.val {nrow(fit$x)}} pixels."
      ),
      class = "hsitools_error"
    )
  }

  # Retained indices
  # Keeping the last n components with the lowest eigenvalues
  # Rather than first like expected in PCA
  indices <- rev((ncol(fit$x) - n + 1L):ncol(fit$x))

  # Build write options
  wopt_user <- rlang::list2(...)

  wopt <- purrr::list_modify(
    list(names = paste0("MNF_", 1:n)),
    !!!wopt_user
  )

  # Set values to SpatRaster
  result <- terra::rast(x, nlyr = n) |>
    terra::setValues(fit$x[, indices])

  # Write to file
  if (filename != "") {
    result <- terra::writeRaster(
      result,
      filename = filename,
      overwrite = overwrite,
      wopt = wopt
    )
  }

  # Return result
  result
}

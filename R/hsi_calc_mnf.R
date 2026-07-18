#' Compute Minimum Noise Fraction transform
#'
#' @family HSI Transformations
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class] with hyperspectral data.
#' @param trim Non-negative integer. Number of bands to drop symmetrically from
#'   both spectral edges before computing the transform. Default `0L` applies
#'   no trimming.
#' @param ... Additional arguments passed to [`spacetime::mnf()`], notably
#'   `Sigma.Noise` for a custom noise covariance matrix and `use` for NA
#'   handling. Default noise estimate uses `0.5 * cov(diff(x))` (MAF).
#'
#' @returns An object of class `c("mnf", "prcomp")` with components:
#'   \item{values}{Numeric vector of eigenvalues (noise fractions), one per band.}
#'   \item{rotation}{Numeric matrix of eigenvectors (loadings).}
#'   \item{x}{Numeric matrix of MNF scores (pixels x bands).}
#'
#' @details
#' MNF components are ordered by **decreasing noise fraction**: MNF1 carries
#' the most noise, the final components carry the most signal. Eigenvalues
#' approximate the noise fraction under the proportional covariance model and
#' are non-negative, but may exceed 1 with real data. `1 - eigenvalue` is the
#' lag-1 autocorrelation of that component. Inspect `$values` to identify the
#' signal-rich tail before passing the result to `hsi_apply_mnf()`.
#'
#' When `x` has been pre-processed with [`hsi_smooth_savgol()`], spectral edge
#' bands become nearly collinear due to the polynomial fitting, which can make
#' the noise covariance matrix singular. Set `trim` to `ceiling(n / 2)` where
#' `n` is the window size passed to [`hsi_smooth_savgol()`] to drop the
#' affected bands from both ends before computing the transform.
#'
#' Wraps [`spacetime::mnf()`] by Edzer Pebesma, implementing the algorithm of
#' Green et al. (1988) with noise estimation following Switzer & Green (1984).
#'
#' Green, A.A., Berman, M., Switzer, P. and Craig, M.D. (1988). A
#' transformation for ordering multispectral data in terms of image quality
#' with implications for noise removal. *IEEE Transactions on Geoscience and
#' Remote Sensing*, 26(1), 65–74.
#'
#' Switzer, P. and Green, A. (1984). Min/max autocorrelation factors for
#' multivariate spatial imagery. Technical Report, Department of Statistics,
#' Stanford University.
#'
#' @seealso [`hsi_apply_mnf()`], [`spacetime::mnf()`]
#'
#' @examples
#' \dontrun{
#' x <- terra::rast("REFLECTANCE_testdata.tif")
#' x_mnf <- hsi_calc_mnf(x)
#' x_mnf$values
#'
#' # After hsi_smooth_savgol() with window = 15
#' x_mnf <- hsi_calc_mnf(x, trim = 8L)
#' }
#'
#' @export
hsi_calc_mnf <- function(x, trim = 0L, ...) {
  # Validate inputs
  check_spatraster(x)

  check_numeric(trim, len = 1)

  # Validate required packages
  rlang::check_installed("spacetime")

  # Calculate number of effective lyrs
  lyrs <- terra::nlyr(x) - (trim * 2)

  # Check if there are any bands left
  if (lyrs <= 0) {
    cli::cli_abort(
      c(
        "There are no layers left in {.arg x}.",
        "i" = "{.arg x} now has {.val {lyrs}} bands."
      ),
      class = "hsitools_error"
    )
  }

  # Check relation between the cell number and band number
  if (terra::ncell(x) <= lyrs) {
    cli::cli_abort(
      c(
        "There are not enough pixels in {.arg x}.",
        "i" = "{.arg x} now has {.val {terra::ncell(x)}} cells which is <= {.val {lyrs}} bands."
      ),
      class = "hsitools_error"
    )
  }

  # Drop trimmed bands before materializing
  if (trim != 0L) {
    x <- terra::subset(x, (trim + 1):(terra::nlyr(x) - trim))
  }

  # Materialize once; the guard below and spacetime::mnf() share this matrix
  mat <- terra::as.matrix(x)

  # Masked or cropped input can clear the cell count check above while holding
  # too few valid pixels to estimate a non-singular covariance. That count is
  # only knowable after the read, so the guard sits here rather than with the
  # other input checks.
  n_valid <- sum(stats::complete.cases(mat))

  if (n_valid <= lyrs) {
    cli::cli_abort(
      c(
        "There are not enough valid pixels in {.arg x}.",
        "i" = "{.arg x} has {.val {n_valid}} pixel{?s} without {.val {NA}} values, which is <= {.val {lyrs}} bands.",
        "i" = "Masked or cropped input? Check mask coverage before computing MNF."
      ),
      class = "hsitools_error"
    )
  }

  # Calculate minimum noise fraction
  result <- spacetime::mnf(mat, ...)

  # Return result
  result
}

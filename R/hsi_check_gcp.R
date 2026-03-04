#' Assess affine transformation quality from matched GCPs
#'
#' @family HSI Co-registration
#'
#' @param x A [data.frame] or [tibble][tibble::tibble] of matched GCPs from
#'   [`hsi_match_gcp()`]. Must contain columns `source_x`, `source_y`,
#'   `target_x`, `target_y`.
#' @param verbose Logical. Print GCP count and RMSE to console. Default `FALSE`.
#'
#' @returns A named list containing:
#'   \item{residuals}{A [tibble][tibble::tibble] with all input columns plus
#'     `residual_x`, `residual_y`, and `residual_total` in target pixels.}
#'   \item{rmse}{Numeric. Root mean square error in target pixels.}
#'   \item{n_gcps}{Integer. Number of GCPs used.}
#'
#' @description
#' Fit a first-order affine transformation from matched GCPs using least
#' squares and return per-GCP residuals. Use this to identify poorly
#' digitized points before the expensive warp step.
#'
#' @details
#' The affine model (6 parameters) handles translation, rotation, independent
#' X/Y scaling, and shear. With N GCPs, residual assessment has N - 3 degrees
#' of freedom. Residuals are in target pixel units. An RMSE above 5 pixels
#' triggers a warning.
#'
#' @seealso
#' [`hsi_match_gcp()`] for preparing input,
#' [`hsi_coregister()`] for applying the warp.
#'
#' @examples
#' \dontrun{
#' matched <- hsi_match_gcp(swir_gcps, vnir_gcps)
#' x_check <- hsi_check_gcp(matched)
#'
#' # Inspect worst GCPs
#' x_check$residuals |>
#'   dplyr::arrange(dplyr::desc(residual_total))
#'
#' # Remove outliers and re-check
#' cleaned <- matched |> dplyr::filter(!gcp_id %in% c(5, 12))
#' hsi_check_gcp(cleaned)
#' }
#'
#' @export
hsi_check_gcp <- function(
  x,
  verbose = FALSE
) {
  # Validate inputs
  if (!inherits(x, "data.frame")) {
    cli::cli_abort(
      "{.arg x} must be a data frame."
    )
  }

  # Validate columns
  check_has_cols(x, cols = c("source_x", "source_y", "target_x", "target_y"))

  # Check minimum of 3 GCP
  if (nrow(x) < 3) {
    cli::cli_abort(
      "Need at least 3 matched GCPs, found {nrow(x)}."
    )
  }

  # Check for collinear GCPs
  if (qr(cbind(1, x$source_x, x$source_y))$rank < 3) {
    cli::cli_abort(
      "GCPs are collinear — affine transform cannot be fit."
    )
  }

  # Design matrix: [1, source_x, source_y] to perform fit
  design <- cbind(1, x$source_x, x$source_y)

  # Solve for X and Y coefficients via least squares
  coeffs_x <- stats::lm.fit(design, x$target_x)$coefficients
  coeffs_y <- stats::lm.fit(design, x$target_y)$coefficients

  # Predicted values (where would GCP land)
  predicted_x <- as.vector(design %*% coeffs_x)
  predicted_y <- as.vector(design %*% coeffs_y)

  # Residuals tibble (compare where point was and where it was predicted to be)
  residuals <- x |>
    dplyr::mutate(
      residual_x = target_x - predicted_x,
      residual_y = target_y - predicted_y,
      residual_total = sqrt(residual_x^2 + residual_y^2)
    )

  # RMSE (root mean square error)
  rmse <- sqrt(mean(residuals$residual_total^2))

  # Report
  if (verbose) {
    cli::cli_alert_info("GCPs: {nrow(x)}, RMSE: {format(rmse, digits = 3)} px")
  }

  if (rmse > 5) {
    cli::cli_warn(
      "RMSE > 5 pixels. Check GCP digitization quality."
    )
  }

  # Return
  list(
    residuals = residuals,
    rmse = rmse,
    n_gcps = nrow(x)
  )
}

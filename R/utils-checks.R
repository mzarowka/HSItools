#' Check if input is a SpatRaster
#'
#' @param x Object to check
#' @param arg Argument name for error message (auto-detected)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Invisible x if valid, otherwise aborts
#' @noRd
check_spatraster <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls SpatRaster}, not {.cls {class(x)[[1]]}}.",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if input is numeric with optional constraints
#'
#' @param x Object to check
#' @param len Expected length (NULL to skip check)
#' @param positive Must all values be positive? (FALSE to skip check)
#' @param odd Must value be odd? For window sizes (FALSE to skip check)
#' @param arg Argument name for error message (auto-detected)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Invisible x if valid, otherwise aborts
#' @noRd
check_numeric <- function(
  x,
  len = NULL,
  positive = FALSE,
  odd = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  # Check if numeric
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be numeric, not {.cls {class(x)[[1]]}}.",
      call = call
    )
  }

  # Check length if specified
  if (!is.null(len) && length(x) != len) {
    cli::cli_abort(
      "{.arg {arg}} must be length {len}, not {length(x)}.",
      call = call
    )
  }

  # Check positive if requested
  if (positive && any(x <= 0)) {
    cli::cli_abort(
      "{.arg {arg}} must contain only positive values.",
      call = call
    )
  }

  # Check odd if requested (for window sizes)
  if (odd && (length(x) != 1 || x %% 2 == 0)) {
    cli::cli_abort(
      "{.arg {arg}} must be an odd number.",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check and extract wavelengths from SpatRaster band names
#'
#' Validates that band names can be converted to numeric wavelengths
#' and returns them. This both validates AND provides the wavelengths
#' for further use.
#'
#' @param x A SpatRaster (assumed already validated)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Numeric vector of wavelengths if valid, otherwise aborts
#' @noRd
check_wavelengths <- function(
  x,
  call = rlang::caller_env()
) {
  wavelengths <- suppressWarnings(as.numeric(terra::names(x)))

  if (all(is.na(wavelengths))) {
    cli::cli_abort(
      c(
        "Band names cannot be converted to numeric wavelengths.",
        "i" = "Band names are: {.val {head(terra::names(x), 5)}}..."
      ),
      call = call
    )
  }

  # Return
  wavelengths
}

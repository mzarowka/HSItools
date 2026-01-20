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

#' Check if input is a SpatVector
#'
#' @param x Object to check
#' @param arg Argument name for error message (auto-detected)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Invisible x if valid, otherwise aborts
#' @noRd
check_spatvector <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!inherits(x, "SpatVector")) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls SpatVector}, not {.cls {class(x)[[1]]}}.",
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

#' Check if SpatVector has allowed geometry type
#'
#' @param x SpatVector to check (assumed already validated as SpatVector)
#' @param allowed Character vector of allowed geometry types
#'   (e.g., c("points", "lines"))
#' @param arg Argument name for error message (auto-detected)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Invisible x if valid, otherwise aborts
#' @noRd
check_geom_type <- function(
  x,
  allowed,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  geom_type <- terra::geomtype(x)

  if (!geom_type %in% allowed) {
    cli::cli_abort(
      "{.arg {arg}} geometry must be {.or {.val {allowed}}}, not {.val {geom_type}}.",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if value is one of allowed choices
#'
#' @param x Value to check
#' @param choices Character vector of allowed values
#' @param arg Argument name for error message (auto-detected)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Invisible x if valid, otherwise aborts
#' @noRd
check_one_of <- function(
  x,
  choices,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!x %in% choices) {
    cli::cli_abort(
      "{.arg {arg}} must be one of {.or {.val {choices}}}, not {.val {x}}.",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if object has required columns/attributes
#'
#' @param x Object to check (SpatVector, data.frame, or tibble)
#' @param cols Character vector of required column names
#' @param arg Argument name for error message (auto-detected)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Invisible x if valid, otherwise aborts
#' @noRd
check_has_cols <- function(
  x,
  cols,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  # Get column names depending on object type
  if (inherits(x, "SpatVector")) {
    x_cols <- terra::names(x)
  } else if (inherits(x, "data.frame")) {
    x_cols <- names(x)
  } else {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls SpatVector} or {.cls data.frame}.",
      call = call
    )
  }

  # Find missing columns
  missing <- cols[!cols %in% x_cols]

  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {arg}} is missing required column{?s}: {.val {missing}}.",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if SpatVector or SpatRaster has no CRS (pixel coordinates)
#'
#' @param x SpatVector or SpatRaster to check
#' @param arg Argument name for error message (auto-detected)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Invisible x if valid, otherwise aborts
#' @noRd
check_crs_null <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  x_crs <- terra::crs(x)

  if (!is.na(x_crs) && x_crs != "") {
    cli::cli_abort(
      c(
        "{.arg {arg}} must have no CRS for pixel-based calculations.",
        "i" = "Use {.code terra::crs({arg}) <- NULL} to remove CRS."
      ),
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if list has required elements
#'
#' @param x List to check
#' @param elements Character vector of required element names
#' @param arg Argument name for error message (auto-detected)
#' @param call Environment for error reporting (auto-detected)
#'
#' @return Invisible x if valid, otherwise aborts
#' @noRd
check_list_has <- function(
  x,
  elements,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  # Check if list
  if (!is.list(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a list, not {.cls {class(x)[[1]]}}.",
      call = call
    )
  }

  # Find missing elements
  missing <- elements[!elements %in% names(x)]

  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {arg}} is missing required element{?s}: {.val {missing}}.",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

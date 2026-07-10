#' Check if input is a SpatRaster
#'
#' @param x Object to check.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_spatraster <- function(
  x,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  if (!inherits(x, "SpatRaster")) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls SpatRaster}, not {.cls {class(x)[[1]]}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if input is a SpatVector
#'
#' @param x Object to check.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_spatvector <- function(
  x,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  if (!inherits(x, "SpatVector")) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls SpatVector}, not {.cls {class(x)[[1]]}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if input is numeric with optional constraints
#'
#' @param x Object to check.
#' @param len Integer. Expected length. `NULL` skips the check.
#' @param positive Logical. Must all values be positive. Default `FALSE`.
#' @param odd Logical. Must value be odd (for window sizes). Default `FALSE`.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_numeric <- function(
  x,
  len = NULL,
  positive = FALSE,
  odd = FALSE,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  # Check if numeric
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be numeric, not {.cls {class(x)[[1]]}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Check length if specified
  if (!is.null(len) && length(x) != len) {
    cli::cli_abort(
      "{.arg {arg}} must be length {len}, not {length(x)}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Check positive if requested
  if (positive && any(x <= 0)) {
    cli::cli_abort(
      "{.arg {arg}} must contain only positive values.",
      class = "hsitools_error",
      call = call
    )
  }

  # Check odd if requested (for window sizes)
  if (odd && (length(x) != 1 || x %% 2 == 0)) {
    cli::cli_abort(
      "{.arg {arg}} must be an odd number.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check and extract wavelengths from SpatRaster band names
#'
#' @param x A [`SpatRaster`][terra::SpatRaster-class], assumed already validated.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Numeric vector of wavelengths if valid, otherwise aborts.
#' @noRd
check_wavelengths <- function(
  x,
  allow_null = FALSE,
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  wavelengths <- suppressWarnings(as.numeric(terra::names(x)))

  if (all(is.na(wavelengths))) {
    cli::cli_abort(
      c(
        "Band names cannot be converted to numeric wavelengths.",
        "i" = "Band names are: {.val {head(terra::names(x), 5)}}..."
      ),
      class = "hsitools_error",
      call = call
    )
  }

  # Return
  wavelengths
}

#' Check if SpatVector has an allowed geometry type
#'
#' @param x A [`SpatVector`][terra::SpatVector-class], assumed already validated.
#' @param allowed Character vector. Allowed geometry types.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_geom_type <- function(
  x,
  allowed,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  geom_type <- terra::geomtype(x)

  if (!geom_type %in% allowed) {
    cli::cli_abort(
      "{.arg {arg}} geometry must be {.or {.val {allowed}}}, not {.val {geom_type}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if value is one of allowed choices
#'
#' @param x Value to check.
#' @param choices Character vector. Allowed values.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_one_of <- function(
  x,
  choices,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  if (!x %in% choices) {
    cli::cli_abort(
      "{.arg {arg}} must be one of {.or {.val {choices}}}, not {.val {x}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if object has required columns or attributes
#'
#' @param x Object to check. A [`SpatVector`][terra::SpatVector-class],
#'   `data.frame`, or tibble.
#' @param cols Character vector. Required column names.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_has_cols <- function(
  x,
  cols,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  # Get column names depending on object type
  if (inherits(x, "SpatVector")) {
    x_cols <- terra::names(x)
  } else if (inherits(x, "data.frame")) {
    x_cols <- names(x)
  } else {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls SpatVector} or {.cls data.frame}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Find missing columns
  missing <- cols[!cols %in% x_cols]

  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {arg}} is missing required column{?s}: {.val {missing}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if SpatVector or SpatRaster has no CRS
#'
#' @param x A [`SpatVector`][terra::SpatVector-class] or
#'   [`SpatRaster`][terra::SpatRaster-class] to check.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_crs_null <- function(
  x,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  x_crs <- terra::crs(x)

  if (!is.na(x_crs) && x_crs != "") {
    cli::cli_abort(
      c(
        "{.arg {arg}} must have no CRS for pixel-based calculations.",
        "i" = "Use {.code terra::crs({arg}) <- NULL} to remove CRS."
      ),
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if list has required elements
#'
#' @param x List to check.
#' @param elements Character vector. Required element names.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_list_has <- function(
  x,
  elements,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  # Check if list
  if (!is.list(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a list, not {.cls {class(x)[[1]]}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Find missing elements
  missing <- elements[!elements %in% names(x)]

  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {arg}} is missing required element{?s}: {.val {missing}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if input is a data.frame with optional constraints
#'
#' @param x Object to check.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_data_frame <- function(
  x,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  if (!inherits(x, "data.frame")) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls data.frame}, not {.cls {class(x)[[1]]}}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

#' Check if input is a list of SpatRasters
#'
#' @param x Object to check.
#' @param allow_null Logical. Allow `NULL` values. Default `FALSE`.
#' @param arg Argument name for error messages. Auto-detected via
#'   [rlang::caller_arg()].
#' @param call Environment for error reporting. Auto-detected via
#'   [rlang::caller_env()].
#'
#' @returns Invisible `x` if valid, otherwise aborts.
#' @noRd
check_spatraster_list <- function(
  x,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  if (!is.list(x)) {
    cli::cli_abort(
      "{.arg {arg} must be a list, not {.obj_type_friendly {x}.",
      class = "hsitools_error",
      call = call
    )
  }

  # Wrong items
  wrong <- which(!purrr::map_lgl(x, \(i) inherits(i, "SpatRaster")))

  if (length(wrong) > 0) {
    cli::cli_abort(
      "{.arg {arg}} must contain only {.cls SpatRaster} objects; {cli::qty(length(offenders))}element{?s} {.val {offenders}} {?is/are} not.",
      class = "hsitools_error",
      call = call
    )
  }

  # Return invisibly
  invisible(x)
}

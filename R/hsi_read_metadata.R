#' Read hsi_metadata from file
#'
#' @family HSI Metadata
#' @param filename Character. Path to an `hsi_metadata` YAML sidecar.
#'
#' @returns An object of class `hsi_metadata`.
#'
#' @examples
#' \dontrun{
#' x <- hsi_create_metadata(name = "test", wavelengths = 400:900, fwhm = rep(2, 501))
#' hsi_write_metadata(x, filename = "metadata.yaml")
#'
#' x_metadata <- hsi_read_metadata("metadata.yaml")
#' }
#' @export
hsi_read_metadata <- function(filename) {
  # Required string
  rlang::check_string(
    filename,
    allow_empty = FALSE,
    arg = "filename"
  )

  # Check if file exists
  if (!file.exists(filename)) {
    cli::cli_abort(
      c(
        "File {.file {filename}} does not exist.",
        "i" = "Check the file path and try again."
      ),
      class = "hsitools_error"
    )
  }

  # Read raw metadata
  raw <- yaml::read_yaml(filename)

  # Check schema
  if (is.null(raw$schema_version) || !identical(raw$schema_version, "1.1.0")) {
    cli::cli_abort(
      c(
        "{.file {filename}} does not carry a supported {.field schema_version}.",
        "i" = "Supported: {.val 1.1.0}. Found: {.val {raw$schema_version %||% 'none'}}."
      ),
      class = "hsitools_error"
    )
  }

  # Assemble class
  result <- structure(raw, class = "hsi_metadata")

  # Validate metadata
  result <- validate_hsi_metadata(result)

  # Return result
  result
}

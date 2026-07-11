#' Write hsi_metadata to file
#'
#' @family HSI Metadata
#' @param x An `hsi_metadata` object to write.
#' @param filename Character. Output filename.
#' @param overwrite Logical. Overwrite existing file. Default `FALSE`.
#'
#' @returns An `hsi_metadata` object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' x <- hsi_create_metadata(name = "test", wavelengths = 400:900, fwhm = rep(2, 501))
#' hsi_write_metadata(x, filename = "metadata.yaml")
#' x_written <- hsi_write_metadata(x, filename = "metadata.yaml", overwrite = TRUE)
#' }
#' @export
hsi_write_metadata <- function(x, filename, overwrite = FALSE) {
  # Validate inputs
  if (!inherits(x, "hsi_metadata")) {
    cli::cli_abort(
      "{.arg x} must be an {.cls hsi_metadata} object, not {.cls {class(x)[[1]]}}.",
      class = "hsitools_error"
    )
  }
  validate_hsi_metadata(x)

  # Write to file
  if (file.exists(filename) && !overwrite) {
    cli::cli_abort(
      c(
        "File {.file {filename}} already exists.",
        "i" = "Use {.arg overwrite = TRUE} to overwrite."
      ),
      class = "hsitools_error"
    )
  }
  yaml::write_yaml(x, file = filename)

  # Return result
  invisible(x)
}

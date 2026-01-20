#' Show expected geopackage schema for spatial calibration
#'
#' @family HSI Calibration
#'
#' @param layer Character. Which layer schema to show. One of "scale", "ends",
#'   or "all" (default).
#' @param print Logical. Print formatted output to console (default TRUE).
#'
#' @description
#' Display the expected schema for geopackage layers used in spatial calibration.
#' Useful as a reference when digitizing calibration geometries.
#'
#' @return Invisibly returns a list with schema information for each layer.
#'
#' @examples
#' # Show all schemas
#' hsi_calibration_schema()
#'
#' # Show only ends schema
#' hsi_calibration_schema("ends")
#'
#' # Get schema as list without printing
#' schema <- hsi_calibration_schema(print = FALSE)
#'
#' @export
hsi_calibration_schema <- function(
  layer = "all",
  print = TRUE
) {
  # Validate layer
  check_one_of(layer, choices = c("all", "scale", "ends"))

  # Define schemas
  schema <- list(
    scale = list(
      layer_name = "scale",
      geometry = "LINE or 2 POINTS",
      crs = "NULL (pixel coordinates)",
      attributes = dplyr::tibble(
        column = character(),
        type = character(),
        required = logical(),
        description = character()
      ),
      notes = "No attributes required. Geometry defines pixel distance for ratio calculation."
    ),
    ends = list(
      layer_name = "ends",
      geometry = "2 POINTS",
      crs = "NULL (pixel coordinates)",
      attributes = dplyr::tibble(
        column = c(
          "label",
          "physical_position",
          "sample_position",
          "splice_position"
        ),
        type = c(
          "character",
          "numeric",
          "numeric",
          "numeric"
        ),
        required = c(
          TRUE,
          TRUE,
          FALSE,
          FALSE
        ),
        description = c(
          "\"start\" or \"end\"",
          "Scale/tape reading at this point",
          "Position in sample coordinates (e.g., 0 at top)",
          "Position in spliced/composite sequence"
        )
      ),
      notes = "Geometry provides pixel coordinates. Labels identify which point is which."
    )
  )

  # Print if requested
  if (print) {
    layers_to_show <- if (layer == "all") c("scale", "ends") else layer

    cli::cli_h1("Geopackage Schema for HSI Calibration")
    cli::cli_text("CRS must be NULL (pixel coordinates) for all layers.")
    cli::cli_text("")

    purrr::walk(layers_to_show, \(lyr) {
      s <- schema[[lyr]]

      cli::cli_h2("Layer: {.val {s$layer_name}}")
      cli::cli_text("Geometry: {.val {s$geometry}}")

      if (nrow(s$attributes) > 0) {
        cli::cli_text("")
        cli::cli_h3("Attributes:")

        purrr::pwalk(s$attributes, \(column, type, required, description) {
          req_label <- if (required) {
            cli::col_red("required")
          } else {
            cli::col_grey("optional")
          }
          cli::cli_text(
            "
          {.field {column}} [{.cls {type}}] ({req_label}): {description}"
          )
        })
      }

      cli::cli_text("")
      cli::cli_text("{.emph Note: {s$notes}}")
      cli::cli_text("")
    })
  }

  # Return invisibly
  invisible(schema)
}

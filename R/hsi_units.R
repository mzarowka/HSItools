#' Read HSI unit metadata
#'
#' @noRd
hsi_get_units <- function(x) {
  units <- NULL

  if (inherits(x, "SpatRaster")) {
    tags <- terra::metags(x, name = "hsi_units")

    if (inherits(tags, "data.frame") && nrow(tags) > 0) {
      if ("value" %in% names(tags)) {
        units <- tags$value[[1]]
      } else if (ncol(tags) >= 2) {
        units <- tags[[2]][1]
      }
    }
  }

  if (is.null(units) || length(units) == 0 || is.na(units[[1]]) || units[[1]] == "") {
    return(NULL)
  }

  as.character(units[[1]])
}

#' Create a unit-aware tick label function
#'
#' @noRd
hsi_unit_label_fun <- function(units = NULL, digits = 2) {
  if (is.null(units)) {
    return(ggplot2::waiver())
  }

  function(i) {
    paste0(
      formatC(abs(i), format = "f", digits = digits, drop0trailing = TRUE),
      " ",
      units
    )
  }
}

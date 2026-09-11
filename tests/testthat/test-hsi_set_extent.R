# Test hsi_set_extent ----
# hsi_set_extent() replaces a raster's pixel-space extent with a physically
# calibrated one. Cell values, dimensions and layer names are untouched; only
# the extent and the hsi_units metadata tag change.
#
# Key contracts, each of which has a regression test below:
#   - `reference` carries a SPATIAL coordinate, y increasing upward, converted
#     internally to a fractional row position. It is not a row index.
#   - The extent is expressed in cell EDGES, so the calibrated resolution equals
#     `um_per_pixel` exactly on both axes.
#   - The row holding `reference` is placed at `origin`.
#   - An anchor outside the raster extrapolates and raises a hsitools_warning.

## Setup ----
# The fixture is 9 x 9 x 101 with extent 1000-1009 / 2000-2009 and unit
# resolution. The non-zero spatial origin is load-bearing: it catches any
# assumption that the input raster starts at zero.
test_nrow <- terra::nrow(test_reflectance)
test_ncol <- terra::ncol(test_reflectance)

# A single-point anchor at a spatial y coordinate, as it would arrive from a
# digitised geopackage.
anchor <- function(y) {
  terra::vect(
    cbind(terra::xmin(test_reflectance) + 0.5, y),
    type = "points"
  )
}

# Spatial y of the centre of a row, counting from 1 at the top.
row_centre <- function(row) {
  terra::ymax(test_reflectance) - (row - 0.5) * terra::yres(test_reflectance)
}

# Physical centre of a row in a calibrated result.
centre_of <- function(x, row) {
  terra::ymax(x) - (row - 0.5) * terra::yres(x)
}

# ── Output type ──────────────────────────────────────────────────────────────

test_that("hsi_set_extent returns the input grid unchanged apart from extent", {
  result <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1)),
    um_per_pixel = 1000,
    units = "mm"
  )

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nrow(result), test_nrow)
  expect_equal(terra::ncol(result), test_ncol)
  expect_equal(terra::nlyr(result), terra::nlyr(test_reflectance))
  expect_equal(names(result), names(test_reflectance))
  expect_equal(terra::values(result), terra::values(test_reflectance))
})

# ── Output geometry ──────────────────────────────────────────────────────────

test_that("hsi_set_extent gives the result a resolution of exactly um_per_pixel", {
  # Regression: the extent was previously built from cell centres rather than
  # edges, which shrank the grid by (n - 1) / n on each axis.
  result <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1)),
    um_per_pixel = 1000,
    units = "mm"
  )

  expect_equal(terra::xres(result), 1)
  expect_equal(terra::yres(result), 1)
})

test_that("hsi_set_extent spans the whole grid in physical units", {
  result <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1)),
    um_per_pixel = 1000,
    units = "mm"
  )

  expect_equal(terra::xmin(result), 0)
  expect_equal(terra::xmax(result), test_ncol * 1)
  expect_equal(terra::ymax(result) - terra::ymin(result), test_nrow * 1)
})

# ── Anchor placement ─────────────────────────────────────────────────────────

test_that("hsi_set_extent places the referenced row at origin", {
  purrr::walk(c(1, 5, test_nrow), \(row) {
    result <- hsi_set_extent(
      test_reflectance,
      anchor(row_centre(row)),
      um_per_pixel = 1000,
      origin = 0,
      units = "mm"
    )

    expect_lt(abs(centre_of(result, row)), 1e-9)
  })
})

test_that("hsi_set_extent reads reference as a spatial coordinate, not a row index", {
  # Regression: the raw y value used to be fed straight into row arithmetic, so
  # an anchor on the top row produced an axis offset by the full raster height.
  result <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1)),
    um_per_pixel = 1000,
    units = "mm"
  )

  # Top row anchored at zero means the axis runs from +0.5 down to -8.5, not
  # somewhere near the fixture's own y coordinates of 2000-2009.
  expect_equal(terra::ymax(result), 0.5)
  expect_equal(terra::ymin(result), -(test_nrow - 0.5))
})

test_that("hsi_set_extent preserves a sub-pixel anchor position", {
  on_centre <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1)),
    um_per_pixel = 1000,
    units = "mm"
  )

  quarter_below <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1) - 0.25),
    um_per_pixel = 1000,
    units = "mm"
  )

  # Moving the anchor a quarter of a pixel shifts the axis by a quarter of a
  # pixel — rounding to the nearest row would collapse this to zero.
  expect_equal(terra::ymax(quarter_below) - terra::ymax(on_centre), 0.25)
})

test_that("hsi_set_extent shifts the axis by origin", {
  # `origin` is given in micrometres regardless of `units`, so 3000 um is a
  # 3 mm shift and the anchored row moves from 0 to -3.
  at_zero <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1)),
    um_per_pixel = 1000,
    origin = 0,
    units = "mm"
  )

  shifted <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1)),
    um_per_pixel = 1000,
    origin = 3000,
    units = "mm"
  )

  expect_equal(terra::ymax(shifted), terra::ymax(at_zero) - 3)
  expect_equal(centre_of(shifted, 1), -3)
})

test_that("hsi_set_extent reads origin in micrometres whatever the units", {
  # Regression: `origin` was previously added in the output units, so the same
  # number meant a different physical shift for each choice of `units`.
  expected <- c(um = 3000, mm = 3, cm = 0.3)

  purrr::iwalk(expected, \(shift, unit) {
    result <- hsi_set_extent(
      test_reflectance,
      anchor(row_centre(1)),
      um_per_pixel = 1000,
      origin = 3000,
      units = unit
    )

    expect_equal(centre_of(result, 1), -shift)
  })
})

# ── Units ────────────────────────────────────────────────────────────────────

test_that("hsi_set_extent converts the pixel size to the requested units", {
  expected <- c(um = 1000, mm = 1, cm = 0.1)

  purrr::iwalk(expected, \(size, unit) {
    result <- hsi_set_extent(
      test_reflectance,
      anchor(row_centre(1)),
      um_per_pixel = 1000,
      units = unit
    )

    expect_equal(terra::yres(result), size)
    expect_equal(terra::xmax(result), test_ncol * size)
  })
})

test_that("hsi_set_extent tags the result with the output units", {
  result <- hsi_set_extent(
    test_reflectance,
    anchor(row_centre(1)),
    um_per_pixel = 1000,
    units = "cm"
  )

  tags <- terra::metags(result)

  expect_equal(tags[["value"]][tags[["name"]] == "hsi_units"], "cm")
})

# ── Extrapolation ────────────────────────────────────────────────────────────

test_that("hsi_set_extent extrapolates an anchor above the raster", {
  # An anchor digitised before a crop legitimately sits outside the raster.
  above <- row_centre(1) + 3 * terra::yres(test_reflectance)

  expect_warning(
    result <- hsi_set_extent(
      test_reflectance,
      anchor(above),
      um_per_pixel = 1000,
      units = "mm"
    ),
    class = "hsitools_warning"
  )

  # Three rows above the top row puts the top row three pixels down.
  expect_equal(centre_of(result, 1), -3)
})

test_that("hsi_set_extent extrapolates an anchor below the raster", {
  below <- row_centre(test_nrow) - 2 * terra::yres(test_reflectance)

  expect_warning(
    result <- hsi_set_extent(
      test_reflectance,
      anchor(below),
      um_per_pixel = 1000,
      units = "mm"
    ),
    class = "hsitools_warning"
  )

  # Below the core, everything sits above the anchor and so reads positive.
  expect_equal(centre_of(result, test_nrow), 2)
})

test_that("hsi_set_extent accepts an anchor on the raster edge without warning", {
  purrr::walk(
    c(terra::ymax(test_reflectance), terra::ymin(test_reflectance)),
    \(edge) {
      expect_no_warning(
        hsi_set_extent(
          test_reflectance,
          anchor(edge),
          um_per_pixel = 1000,
          units = "mm"
        )
      )
    }
  )
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("hsi_set_extent rejects a reference that is not a single point", {
  two_points <- terra::vect(
    cbind(c(1000.5, 1000.5), c(2008.5, 2000.5)),
    type = "points"
  )

  expect_error(
    hsi_set_extent(test_reflectance, two_points, um_per_pixel = 1000),
    class = "hsitools_error"
  )

  polygon <- terra::as.polygons(terra::ext(test_reflectance))

  expect_error(
    hsi_set_extent(test_reflectance, polygon, um_per_pixel = 1000),
    class = "hsitools_error"
  )
})

test_that("hsi_set_extent rejects inputs carrying a CRS", {
  crs_raster <- terra::deepcopy(test_reflectance)
  terra::crs(crs_raster) <- "EPSG:4326"

  expect_error(
    hsi_set_extent(crs_raster, anchor(row_centre(1)), um_per_pixel = 1000),
    class = "hsitools_error"
  )

  crs_point <- anchor(row_centre(1))
  terra::crs(crs_point) <- "EPSG:4326"

  expect_error(
    hsi_set_extent(test_reflectance, crs_point, um_per_pixel = 1000),
    class = "hsitools_error"
  )
})

test_that("hsi_set_extent rejects a non-finite pixel size", {
  # Regression: NA and NaN made the positivity comparison NA and crashed the
  # `if ()` with a bare simpleError, while Inf passed the check entirely and
  # produced a raster with an infinite extent.
  purrr::walk(c(NaN, NA_real_, Inf), \(bad) {
    expect_error(
      hsi_set_extent(
        test_reflectance,
        anchor(row_centre(1)),
        um_per_pixel = bad
      ),
      "finite",
      class = "hsitools_error"
    )
  })
})

test_that("hsi_set_extent rejects an invalid pixel size or units", {
  expect_error(
    hsi_set_extent(
      test_reflectance,
      anchor(row_centre(1)),
      um_per_pixel = -1
    ),
    class = "hsitools_error"
  )

  expect_error(
    hsi_set_extent(
      test_reflectance,
      anchor(row_centre(1)),
      um_per_pixel = 1000,
      units = "inch"
    ),
    class = "hsitools_error"
  )
})

# ── Message quality ──────────────────────────────────────────────────────────

test_that("hsi_set_extent error and warning messages are informative", {
  expect_snapshot(
    {
      hsi_set_extent(
        test_reflectance,
        terra::vect(
          cbind(c(1000.5, 1000.5), c(2008.5, 2000.5)),
          type = "points"
        ),
        um_per_pixel = 1000
      )
    },
    error = TRUE
  )

  expect_snapshot(
    hsi_set_extent(
      test_reflectance,
      anchor(row_centre(1)),
      um_per_pixel = 1000,
      units = "inch"
    ),
    error = TRUE
  )

  expect_snapshot(
    hsi_set_extent(
      test_reflectance,
      anchor(row_centre(1) + 3 * terra::yres(test_reflectance)),
      um_per_pixel = 1000,
      units = "mm"
    )
  )
})

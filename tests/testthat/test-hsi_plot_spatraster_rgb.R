# Test hsi_plot_spatraster_rgb ----
# Produces a ggplot object from a three-layer SpatRaster, optionally
# replacing the pixel-space extent with a physically calibrated extent
# derived from a coordinate raster (y).
#
# Key contracts:
# - always returns an object inheriting "gg" (compatible with S3 and S7 ggplot2)
# - caller's raster extent is never mutated (deepcopy guarantee)
# - when y is supplied, the plot coordinate ranges reflect physical extent
#   with negated row dimension so depth reads top-to-bottom
# - without y, pixel extent is passed through unchanged
# - invalid inputs abort with informative errors

## Setup ----
test_rgb <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/SAVGOL_testdata.tif"
  )
) |>
  terra::subset(1:3)

test_single <- terra::rast(
  system.file(
    package = "HSItools",
    "testdata/products/SAVGOL_testdata.tif"
  )
) |>
  terra::subset(1)

test_coords <- hsi_calc_coords(
  test_rgb,
  um_per_pixel = hsi_calibration_direct(60)
)

# ── Output type ───────────────────────────────────────────────────────────────

test_that("hsi_plot_spatraster_rgb returns a ggplot object without coords", {
  result <- hsi_plot_spatraster_rgb(test_rgb)

  # inherits() works regardless of whether ggplot2 uses S3 or S7 internally,
  # since ggplot2 guarantees "gg" stays in the class vector for compatibility
  expect_true(inherits(result, "gg"))
})

test_that("hsi_plot_spatraster_rgb returns a ggplot object with coords", {
  result <- hsi_plot_spatraster_rgb(test_rgb, y = test_coords)

  expect_true(inherits(result, "gg"))
})

# ── Caller mutation guard ─────────────────────────────────────────────────────

test_that("hsi_plot_spatraster_rgb does not mutate the caller's raster extent", {
  # Convert to numeric vector before calling the function — comparing
  # SpatExtent S7 objects via expect_equal checks C++ pointers, which gives
  # false negatives even when the values are identical
  ext_before <- as.vector(terra::ext(test_rgb))

  hsi_plot_spatraster_rgb(test_rgb, y = test_coords)

  expect_equal(as.vector(terra::ext(test_rgb)), ext_before)
})

# ── Extent contracts ──────────────────────────────────────────────────────────

# tidyterra fortifies the SpatRaster to a tibble eagerly when building the
# layer, so result$layers[[1]]$data is a tbl_df, not a SpatRaster — terra::ext()
# cannot be called on it. ggplot2::ggplot_build() fully renders the plot and
# exposes panel_params with the actual coordinate ranges after all scale
# transformations. This is the correct and stable introspection point.

test_that("hsi_plot_spatraster_rgb with y sets correct xmin in mm", {
  mm <- terra::minmax(test_coords)
  expected <- mm["min", "col_um"] * 0.001

  result <- hsi_plot_spatraster_rgb(test_rgb, y = test_coords, units = "mm")
  built <- ggplot2::ggplot_build(result)

  expect_equal(
    built$layout$panel_params[[1]]$x.range[[1]],
    expected,
    tolerance = 1e-6
  )
})

test_that("hsi_plot_spatraster_rgb with y sets correct xmax in mm", {
  mm <- terra::minmax(test_coords)
  expected <- mm["max", "col_um"] * 0.001

  result <- hsi_plot_spatraster_rgb(test_rgb, y = test_coords, units = "mm")
  built <- ggplot2::ggplot_build(result)

  expect_equal(
    built$layout$panel_params[[1]]$x.range[[2]],
    expected,
    tolerance = 1e-6
  )
})

test_that("hsi_plot_spatraster_rgb with y sets correct ymin from negated row_um maximum", {
  # Row coordinates increase downward; negation satisfies terra's ymin < ymax
  # requirement while placing shallow positions at the top of the plot
  mm <- terra::minmax(test_coords)
  expected <- -mm["max", "row_um"] * 0.001

  result <- hsi_plot_spatraster_rgb(test_rgb, y = test_coords, units = "mm")
  built <- ggplot2::ggplot_build(result)

  expect_equal(
    built$layout$panel_params[[1]]$y.range[[1]],
    expected,
    tolerance = 1e-6
  )
})

test_that("hsi_plot_spatraster_rgb with y sets correct ymax from negated row_um minimum", {
  mm <- terra::minmax(test_coords)
  expected <- -mm["min", "row_um"] * 0.001

  result <- hsi_plot_spatraster_rgb(test_rgb, y = test_coords, units = "mm")
  built <- ggplot2::ggplot_build(result)

  expect_equal(
    built$layout$panel_params[[1]]$y.range[[2]],
    expected,
    tolerance = 1e-6
  )
})

test_that("hsi_plot_spatraster_rgb units = 'um' sets extent in micrometres", {
  mm <- terra::minmax(test_coords)
  expected <- mm["max", "col_um"]

  result <- hsi_plot_spatraster_rgb(test_rgb, y = test_coords, units = "um")
  built <- ggplot2::ggplot_build(result)

  expect_equal(
    built$layout$panel_params[[1]]$x.range[[2]],
    expected,
    tolerance = 1e-6
  )
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("hsi_plot_spatraster_rgb errors with non-SpatRaster x", {
  expect_error(hsi_plot_spatraster_rgb(x = "not a raster"))
})

test_that("hsi_plot_spatraster_rgb errors when x does not have exactly three layers", {
  expect_error(
    hsi_plot_spatraster_rgb(x = test_single),
    "layers"
  )
})

test_that("hsi_plot_spatraster_rgb errors with invalid units", {
  expect_error(
    hsi_plot_spatraster_rgb(test_rgb, units = "furlongs")
  )
})

test_that("hsi_plot_spatraster_rgb errors when y is missing required coord layers", {
  bad_y <- c(test_single, test_single)
  names(bad_y) <- c("wrong_name_a", "wrong_name_b")

  expect_error(hsi_plot_spatraster_rgb(test_rgb, y = bad_y))
})

test_that("hsi_plot_spatraster_rgb errors when x and y have different dimensions", {
  # Aggregate doubles pixel size, halving row and col counts
  small_x <- terra::aggregate(test_rgb, fact = 2)

  expect_error(
    hsi_plot_spatraster_rgb(small_x, y = test_coords),
    "dimensions"
  )
})

# Test RABA calculation ----

test_that("hsi_calc_raba integrates the signed absorption area", {
  wavelengths <- seq(650, 710, by = 10)
  reflectance <- c(1, 0.8, 0.5, 0.8, 1, 1.1, 1)

  x <- terra::rast(nrows = 1, ncols = 1, nlyrs = length(wavelengths))
  terra::values(x) <- matrix(reflectance, nrow = 1)
  names(x) <- as.character(wavelengths)

  result <- hsi_calc_raba(x, continuum_edges = c(650, 710), cores = 1)
  result_parallel <- hsi_calc_raba(
    x,
    continuum_edges = c(650, 710),
    cores = 2
  )

  # With a continuum of 1, trapezoidal areas are:
  # 1 + 3.5 + 3.5 + 1 - 0.5 - 0.5 = 8 reflectance-nm.
  expect_equal(unname(terra::values(result)[1, 1]), 8)
  expect_equal(terra::values(result_parallel), terra::values(result))

  # Optional visual check:
  # continuum <- rep(1, length(wavelengths))
  # plot(wavelengths, reflectance, type = "o", ylim = c(0.4, 1.2))
  # polygon(
  #   c(wavelengths, rev(wavelengths)),
  #   c(continuum, rev(reflectance)),
  #   col = grDevices::adjustcolor("steelblue", alpha.f = 0.3),
  #   border = NA
  # )
  # lines(wavelengths, continuum, lty = 2)
  # lines(wavelengths, reflectance, type = "o")
})

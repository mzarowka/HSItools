# hsi_set_extent error and warning messages are informative

    Code
      hsi_set_extent(test_reflectance, terra::vect(cbind(c(1000.5, 1000.5), c(2008.5,
        2000.5)), type = "points"), um_per_pixel = 1000)
    Condition
      Error in `hsi_set_extent()`:
      ! `reference` must contain exactly one point.

---

    Code
      hsi_set_extent(test_reflectance, anchor(row_centre(1)), um_per_pixel = 1000,
      units = "inch")
    Condition
      Error in `hsi_set_extent()`:
      ! `units` must be one of "um", "mm", or "cm", not "inch".

---

    Code
      hsi_set_extent(test_reflectance, anchor(row_centre(1) + 3 * terra::yres(
        test_reflectance)), um_per_pixel = 1000, units = "mm")
    Condition
      Warning:
      `reference` falls outside `raster`; extrapolating.
      i Anchor is 2.5 rows beyond the edge (2.5 mm).
    Output
      class       : SpatRaster
      size        : 9, 9, 101  (nrow, ncol, nlyr)
      resolution  : 1, 1  (x, y)
      extent      : 0, 9, -11.5, -2.5  (xmin, xmax, ymin, ymax)
      coord. ref. : 
      source      : REFLECTANCE_testdata.tif
      names       :   517.58,   520.06,   522.54,   525.02,   527.50,   529.98, ...
      min values  : 0.119117, 0.124191, 0.123117, 0.127552, 0.129594,  0.12893, ...
      max values  : 0.175443, 0.183574, 0.187908, 0.186543, 0.193428, 0.198677, ...


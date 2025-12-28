# # Test derivative ----
# # Some tolerance is needed
# test_that("Savitzky-Golay smoothed reflectance is calculated as expected", {
#   expect_equal(
#     terra::values(hsi_smooth_savgol(
#       x = terra::rast(
#         system.file(
#           package = "HSItools",
#           "testdata/products/REFLECTANCE_testdata.tif"
#         )
#       )
#     )),
#     terra::values(terra::rast(
#       system.file(
#         package = "HSItools",
#         "testdata/products/SAVGOL_testdata.tif"
#       )
#     )),
#     tolerance = 1e-6
#   )
# })

# # Test type
# test_that("Calculated Savitzky-Golay data is a SpatRaster", {
#   expect_s4_class(
#     hsi_smooth_savgol(
#       x = terra::rast(
#         system.file(
#           package = "HSItools",
#           "testdata/products/REFLECTANCE_testdata.tif"
#         )
#       )
#     ),
#     class = "SpatRaster"
#   )
# })
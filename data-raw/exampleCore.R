library(terra)
#terra 1.5.23
a1 <- terra::rast("data-raw/CORE_XYZ.tif")
saveRDS(a1, "data/CORE_XYZ.rds")

a1 <- terra::rast("data-raw/DARKREF_CORE_XYZ.tif")
saveRDS(a1, "data/DARKREF_CORE_XYZ.rds")


a1 <- terra::rast("data-raw/WHITEREF_CORE_XYZ.tif")
saveRDS(a1, "data/WHITEREF_CORE_XYZ.rds")



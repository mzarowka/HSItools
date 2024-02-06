#Read the tif files into R and store them as rda

library(terra)

a1 <- terra::rast("data-raw/CORE_XYZ.tif")
save("a1", file = "data/CORE_XYZ.rda")

a1 <- terra::rast("data-raw/DARKREF_CORE_XYZ.tif")
save("a1", file = "data/DARKREF_CORE_XYZ.rda")

a1 <- terra::rast("data-raw/WHITEREF_CORE_XYZ.tif")
save("a1", file = "data/WHITEREF_CORE_XYZ.rda")


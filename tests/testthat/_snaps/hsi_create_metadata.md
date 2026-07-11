# hsi_create_metadata error messages match snapshot

    Code
      hsi_create_metadata(name = "capture_01", nlyr = 3, wavelengths = c(450, 550))
    Condition
      Error in `hsi_create_metadata()`:
      ! `wavelengths` must have one value per layer.
      i `nlyr` is 3, but `wavelengths` has length 2.

---

    Code
      hsi_create_metadata(name = "capture_01", nrow = c(9, 9))
    Condition
      Error in `hsi_create_metadata()`:
      ! `nrow` must be length 1, not 2.


# hsi_write_metadata error messages match snapshot

    Code
      hsi_write_metadata(list(name = "capture_01"), filename = temp_file)
    Condition
      Error in `hsi_write_metadata()`:
      ! `x` must be an <hsi_metadata> object, not <list>.

---

    Code
      hsi_write_metadata(valid_metadata, filename = temp_file)
    Condition
      Error in `hsi_write_metadata()`:
      ! File '<temp_file>' already exists.
      i Use `overwrite = TRUE` to overwrite.


# hsi_read_metadata error messages match snapshot

    Code
      hsi_read_metadata(nonexistent_file)
    Condition
      Error in `hsi_read_metadata()`:
      ! File '<temp_file>' does not exist.
      i Check the file path and try again.

---

    Code
      hsi_read_metadata(temp_file)
    Condition
      Error in `hsi_read_metadata()`:
      ! '<temp_file>' does not carry a supported schema_version.
      i Supported: "1.1.0". Found: "0.9.0".


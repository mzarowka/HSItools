# hsi_calc_reflectance error messages are informative

    Code
      hsi_calc_reflectance(x = test_x, whiteref = whiteref_short, darkref = test_darkref)
    Condition
      Error in `hsi_calc_reflectance()`:
      ! All inputs must have the same number of bands.
      x Sample: 101 bands
      x White reference: 10 bands
      x Dark reference: 101 bands

---

    Code
      hsi_calc_reflectance(x = test_x, whiteref = test_whiteref, darkref = test_darkref,
        darkspec = darkspec_short)
    Condition
      Error in `hsi_calc_reflectance()`:
      ! Specimen dark reference must have the same number of bands as `x`.
      x Sample: 101 bands
      x Specimen dark reference: 10 bands

---

    Code
      hsi_calc_reflectance(x = x_bad_names, whiteref = test_whiteref, darkref = test_darkref)
    Condition
      Error in `hsi_calc_reflectance()`:
      ! Band names cannot be converted to numeric wavelengths.
      i Band names are: "band_1", "band_2", "band_3", "band_4", and "band_5"...


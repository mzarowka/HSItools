# Prepare a list of lists with spectral indices presets
spectral_indices_raw <- list(
  # ── RABD ────────────────────────────────────────────────────────────────────

  list(
    proxy_name = "rabd510",
    proxy_type = "rabd",
    continuum_edges = c(590, 730),
    absorption_band = 510,
    index_type = "strict",
    bands = NA,
    search_range = NA,
    interpretation = "carotenoids",
    reference = NA
  ),

  list(
    proxy_name = "rabd615",
    proxy_type = "rabd",
    continuum_edges = c(590, 730),
    absorption_band = 615,
    index_type = "strict",
    bands = NA,
    search_range = NA,
    interpretation = "phycocyanin",
    reference = NA
  ),

  # Same trough as rabd615 but narrower continuum window
  list(
    proxy_name = "rabd615_narrow",
    proxy_type = "rabd",
    continuum_edges = c(590, 640),
    absorption_band = 615,
    index_type = "strict",
    bands = NA,
    search_range = NA,
    interpretation = "albite",
    reference = "von Gunten et al. (2012) https://doi.org/10.1007/s10933-012-9582-9"
  ),

  list(
    proxy_name = "rabd640655",
    proxy_type = "rabd",
    continuum_edges = c(590, 730),
    absorption_band = c(640, 655),
    index_type = "max",
    bands = NA,
    search_range = NA,
    interpretation = "total chlorophyll-a",
    reference = NA
  ),

  list(
    proxy_name = "rabd660",
    proxy_type = "rabd",
    continuum_edges = c(590, 730),
    absorption_band = 660,
    index_type = "strict",
    bands = NA,
    search_range = NA,
    interpretation = "total chlorophyll-a",
    reference = NA
  ),

  list(
    proxy_name = "rabd660670",
    proxy_type = "rabd",
    continuum_edges = c(590, 730),
    absorption_band = c(660, 670),
    index_type = "max",
    bands = NA,
    search_range = NA,
    interpretation = "total chlorophyll-a",
    reference = NA
  ),

  list(
    proxy_name = "rabd845",
    proxy_type = "rabd",
    continuum_edges = c(790, 900),
    absorption_band = 845,
    index_type = "strict",
    bands = NA,
    search_range = NA,
    interpretation = "bacteriophaeophytin-a",
    reference = NA
  ),

  # SWIR — spectral values need verification
  list(
    proxy_name = "rabd16601690",
    proxy_type = "rabd",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = NA,
    search_range = NA,
    interpretation = "terrestrial aromatic matter",
    reference = NA
  ),

  # ── RABA ────────────────────────────────────────────────────────────────────
  # index_type vocabulary TBD — placeholder "x" until confirmed with specialist

  list(
    proxy_name = "raba650700",
    proxy_type = "raba",
    continuum_edges = c(650, 700),
    absorption_band = NA,
    index_type = "x",
    bands = NA,
    search_range = NA,
    interpretation = "total chlorophyll-a",
    reference = NA
  ),

  list(
    proxy_name = "raba600760",
    proxy_type = "raba",
    continuum_edges = c(600, 760),
    absorption_band = NA,
    index_type = "x",
    bands = NA,
    search_range = NA,
    interpretation = "total chlorophyll-a",
    reference = NA
  ),

  list(
    proxy_name = "raba590730",
    proxy_type = "raba",
    continuum_edges = c(590, 730),
    absorption_band = NA,
    index_type = "x",
    bands = NA,
    search_range = NA,
    interpretation = "total chlorophyll-a",
    reference = NA
  ),

  list(
    proxy_name = "raba650750",
    proxy_type = "raba",
    continuum_edges = c(650, 750),
    absorption_band = NA,
    index_type = "x",
    bands = NA,
    search_range = NA,
    interpretation = "total chlorophyll-a",
    reference = NA
  ),

  # ── Band ratio ───────────────────────────────────────────────────────────────

  list(
    proxy_name = "ratio570630",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(570, 630),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  list(
    proxy_name = "ratio590690",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(590, 690),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  list(
    proxy_name = "ratio590640",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(590, 640),
    search_range = NA,
    interpretation = "lithogenic content (illite, chlorite, mica)",
    reference = "von Gunten et al. (2012) https://doi.org/10.1007/s10933-012-9582-9"
  ),

  list(
    proxy_name = "ratio645675",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(645, 675),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  list(
    proxy_name = "ratio660670",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(660, 670),
    search_range = NA,
    interpretation = "degree of photopigment diagenesis",
    reference = "von Gunten et al. (2012) https://doi.org/10.1007/s10933-012-9582-9"
  ),

  list(
    proxy_name = "ratio675750",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(675, 750),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  list(
    proxy_name = "ratio650675",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(650, 675),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  list(
    proxy_name = "ratio850900",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(850, 900),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  list(
    proxy_name = "ratio950970",
    proxy_type = "ratio",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(950, 970),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  # ── Band difference ──────────────────────────────────────────────────────────

  list(
    proxy_name = "diff675750",
    proxy_type = "difference",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(675, 750),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  list(
    proxy_name = "diff650675",
    proxy_type = "difference",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(650, 675),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  list(
    proxy_name = "diff660690",
    proxy_type = "difference",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = c(660, 690),
    search_range = NA,
    interpretation = "clay minerals, dust",
    reference = NA
  ),

  # ── Red-edge minimum point ───────────────────────────────────────────────────
  # search_range NA defers to hsi_calc_remp() default c(660, 680)

  list(
    proxy_name = "remp",
    proxy_type = "remp",
    continuum_edges = NA,
    absorption_band = NA,
    index_type = NA,
    bands = NA,
    search_range = NA,
    interpretation = "total chlorophyll-a",
    reference = "Ghanbari et al. (2023) https://doi.org/10.1002/lom3.10576"
  )
)

# Bind to tibble — list columns wrap multi-value fields
spectral_indices <- spectral_indices_raw |>
  purrr::map(
    \(entry) {
      tibble::tibble(
        proxy_name = entry$proxy_name,
        proxy_type = entry$proxy_type,
        continuum_edges = list(entry$continuum_edges),
        absorption_band = list(entry$absorption_band),
        index_type = entry$index_type,
        bands = list(entry$bands),
        search_range = list(entry$search_range),
        interpretation = entry$interpretation,
        reference = entry$reference
      )
    }
  ) |>
  purrr::list_rbind()

usethis::use_data(spectral_indices, overwrite = TRUE)

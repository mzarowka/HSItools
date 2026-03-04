# Test hsi_match_gcp ----
# Matches ground control points between two SpatVectors by a shared identifier.
# Key contracts: CRS is stripped before matching; unmatched points are dropped
# with a warning; fewer than 3 matched GCPs is an error; duplicated IDs abort.

## Setup ----
make_gcp_vect <- function(ids, xs, ys) {
  terra::vect(
    data.frame(gcp_id = ids, x = xs, y = ys),
    geom = c("x", "y")
  )
}

source_gcp <- make_gcp_vect(
  ids = 1:5,
  xs = c(10, 20, 30, 40, 50),
  ys = c(100, 200, 300, 400, 500)
)

target_gcp <- make_gcp_vect(
  ids = 1:5,
  xs = c(15, 25, 35, 45, 55),
  ys = c(110, 210, 310, 410, 510)
)

# ── Output structure ──────────────────────────────────────────────────────────

test_that("hsi_match_gcp returns a tibble", {
  result <- hsi_match_gcp(source_gcp, target_gcp)

  expect_s3_class(result, "tbl_df")
})

test_that("hsi_match_gcp returns exactly five columns", {
  result <- hsi_match_gcp(source_gcp, target_gcp)

  expect_equal(ncol(result), 5L)
})

test_that("hsi_match_gcp returns expected column names", {
  result <- hsi_match_gcp(source_gcp, target_gcp)

  expect_named(
    result,
    c("gcp_id", "source_x", "source_y", "target_x", "target_y")
  )
})

# ── Column contracts ──────────────────────────────────────────────────────────

test_that("hsi_match_gcp id column is preserved verbatim", {
  result <- hsi_match_gcp(source_gcp, target_gcp)

  expect_equal(sort(result$gcp_id), 1:5)
})

test_that("hsi_match_gcp respects custom id_col", {
  source_custom <- make_gcp_vect(ids = letters[1:4], xs = 1:4, ys = 1:4)
  terra::values(source_custom) <- data.frame(marker = letters[1:4])
  target_custom <- make_gcp_vect(ids = letters[1:4], xs = 5:8, ys = 5:8)
  terra::values(target_custom) <- data.frame(marker = letters[1:4])

  result <- hsi_match_gcp(source_custom, target_custom, id_col = "marker")

  expect_true("marker" %in% names(result))
})

test_that("hsi_match_gcp coordinate columns are numeric", {
  result <- hsi_match_gcp(source_gcp, target_gcp)

  expect_true(is.numeric(result$source_x))
  expect_true(is.numeric(result$source_y))
  expect_true(is.numeric(result$target_x))
  expect_true(is.numeric(result$target_y))
})

# ── Value sanity ──────────────────────────────────────────────────────────────

test_that("hsi_match_gcp coordinate values are finite", {
  result <- hsi_match_gcp(source_gcp, target_gcp)

  expect_true(all(is.finite(result$source_x)))
  expect_true(all(is.finite(result$source_y)))
  expect_true(all(is.finite(result$target_x)))
  expect_true(all(is.finite(result$target_y)))
})

test_that("hsi_match_gcp source coordinates match input vector coordinates", {
  result <- hsi_match_gcp(source_gcp, target_gcp)

  expect_equal(sort(result$source_x), c(10, 20, 30, 40, 50))
  expect_equal(sort(result$source_y), c(100, 200, 300, 400, 500))
})

test_that("hsi_match_gcp strips CRS before matching", {
  # QGIS assigns WGS84 to pixel-space vectors by default; stripping is required
  # for coordinates to be meaningful as pixel values
  terra::crs(source_gcp) <- "EPSG:4326"
  terra::crs(target_gcp) <- "EPSG:4326"

  expect_no_error(hsi_match_gcp(source_gcp, target_gcp))

  terra::crs(source_gcp) <- ""
  terra::crs(target_gcp) <- ""
})

# ── Unmatched GCPs ────────────────────────────────────────────────────────────

test_that("hsi_match_gcp warns when GCPs are present in only one input", {
  target_partial <- make_gcp_vect(ids = 1:4, xs = 15:18, ys = 115:118)

  expect_warning(
    hsi_match_gcp(source_gcp, target_partial),
    "Dropped unmatched GCPs"
  )
})

test_that("hsi_match_gcp returns only matched rows when GCPs are unmatched", {
  target_partial <- make_gcp_vect(ids = 1:4, xs = 15:18, ys = 115:118)

  result <- suppressWarnings(hsi_match_gcp(source_gcp, target_partial))

  expect_equal(nrow(result), 4L)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("hsi_match_gcp errors with non-SpatVector source", {
  expect_error(hsi_match_gcp(source = "not a vector", target = target_gcp))
})

test_that("hsi_match_gcp errors with non-SpatVector target", {
  expect_error(hsi_match_gcp(source = source_gcp, target = "not a vector"))
})

test_that("hsi_match_gcp errors with non-point geometry", {
  # Lines are not valid GCP geometry
  line_vect <- terra::vect(
    "LINESTRING (0 0, 1 1)",
    crs = ""
  )

  expect_error(hsi_match_gcp(source = line_vect, target = target_gcp))
})

test_that("hsi_match_gcp errors when id_col is missing from source", {
  expect_error(
    hsi_match_gcp(source_gcp, target_gcp, id_col = "nonexistent")
  )
})

test_that("hsi_match_gcp errors on duplicate IDs in source", {
  source_dup <- make_gcp_vect(ids = c(1, 1, 2, 3, 4), xs = 1:5, ys = 1:5)

  expect_error(
    hsi_match_gcp(source_dup, target_gcp),
    "Duplicate"
  )
})

test_that("hsi_match_gcp errors on duplicate IDs in target", {
  target_dup <- make_gcp_vect(ids = c(1, 1, 2, 3, 4), xs = 1:5, ys = 1:5)

  expect_error(
    hsi_match_gcp(source_gcp, target_dup),
    "Duplicate"
  )
})

test_that("hsi_match_gcp errors when fewer than 3 GCPs match", {
  # Only IDs 1 and 2 overlap
  target_two <- make_gcp_vect(ids = c(1, 2), xs = c(15, 25), ys = c(115, 215))

  expect_error(
    hsi_match_gcp(source_gcp, target_two),
    "at least 3"
  )
})

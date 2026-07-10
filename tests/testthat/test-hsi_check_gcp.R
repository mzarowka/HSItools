# Test hsi_check_gcp ----
# Fits a first-order affine transformation from matched GCPs and returns
# per-GCP residuals and RMSE.
# Key contracts: output is always a named list with residuals tibble, numeric
# RMSE, and GCP count; perfect affine input produces near-zero residuals;
# RMSE > 5 always warns; collinear GCPs abort.

## Setup ----

# Perfect affine transform: target = source + c(5, 10) — zero residuals expected
# source_y is intentionally non-uniform to avoid collinear design matrix
perfect_gcps <- tibble::tibble(
  gcp_id = 1:5,
  source_x = c(10, 20, 30, 40, 50),
  source_y = c(105, 210, 285, 395, 480),
  target_x = c(15, 25, 35, 45, 55),
  target_y = c(110, 210, 310, 410, 510)
)

# Noisy GCPs: large offsets to push RMSE well above 5
# source_y is intentionally non-uniform to avoid collinear design matrix
noisy_gcps <- tibble::tibble(
  gcp_id = 1:6,
  source_x = c(10, 20, 30, 40, 50, 60),
  source_y = c(105, 195, 285, 410, 490, 620),
  target_x = c(15, 90, 35, 120, 55, 200),
  target_y = c(110, 50, 310, 80, 510, 20)
)

# ── Output structure ──────────────────────────────────────────────────────────

test_that("hsi_check_gcp returns a list", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_type(result, "list")
})

test_that("hsi_check_gcp returns exactly three named elements", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_named(result, c("residuals", "rmse", "n_gcps"))
})

test_that("hsi_check_gcp residuals element is a tibble", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_s3_class(result$residuals, "tbl_df")
})

# ── Column contracts ──────────────────────────────────────────────────────────

test_that("hsi_check_gcp residuals tibble contains all input columns", {
  result <- hsi_check_gcp(perfect_gcps)

  purrr::walk(
    names(perfect_gcps),
    \(col) expect_true(col %in% names(result$residuals))
  )
})

test_that("hsi_check_gcp residuals tibble contains residual columns", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_true("residual_x" %in% names(result$residuals))
  expect_true("residual_y" %in% names(result$residuals))
  expect_true("residual_total" %in% names(result$residuals))
})

test_that("hsi_check_gcp residual columns are numeric", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_true(is.numeric(result$residuals$residual_x))
  expect_true(is.numeric(result$residuals$residual_y))
  expect_true(is.numeric(result$residuals$residual_total))
})

# ── Value sanity ──────────────────────────────────────────────────────────────

test_that("hsi_check_gcp rmse is a single finite non-negative number", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_length(result$rmse, 1L)
  expect_true(is.finite(result$rmse))
  expect_gte(result$rmse, 0)
})

test_that("hsi_check_gcp n_gcps matches input row count", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_equal(result$n_gcps, nrow(perfect_gcps))
})

test_that("hsi_check_gcp residual_total values are non-negative", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_true(all(result$residuals$residual_total >= 0))
})

test_that("hsi_check_gcp residual_total values are finite", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_true(all(is.finite(result$residuals$residual_total)))
})

test_that("hsi_check_gcp perfect affine input produces near-zero residuals", {
  # A pure translation is a valid affine transform — residuals must be
  # negligible (floating-point only)
  result <- hsi_check_gcp(perfect_gcps)

  expect_lt(result$rmse, 1e-6)
  expect_true(all(result$residuals$residual_total < 1e-6))
})

test_that("hsi_check_gcp residuals tibble has same row count as input", {
  result <- hsi_check_gcp(perfect_gcps)

  expect_equal(nrow(result$residuals), nrow(perfect_gcps))
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("hsi_check_gcp warns when RMSE exceeds 5 pixels", {
  expect_warning(
    hsi_check_gcp(noisy_gcps),
    "RMSE > 5 pixels",
    class = "hsitools_warning"
  )
})

test_that("hsi_check_gcp errors with non-data-frame input", {
  expect_error(hsi_check_gcp("not a data frame"))
})

test_that("hsi_check_gcp errors when required columns are missing", {
  bad_input <- tibble::tibble(source_x = 1:4, source_y = 1:4)

  expect_error(hsi_check_gcp(bad_input))
})

test_that("hsi_check_gcp errors when fewer than 3 GCPs provided", {
  two_gcps <- perfect_gcps |> dplyr::slice(1:2)

  expect_error(
    hsi_check_gcp(two_gcps),
    "at least 3",
    class = "hsitools_error"
  )
})

test_that("hsi_check_gcp errors when source GCPs are collinear", {
  # Perfectly collinear source coordinates produce a rank-deficient design
  # matrix — lm.fit would return NA coefficients without this guard
  collinear_gcps <- tibble::tibble(
    gcp_id = 1:4,
    source_x = c(10, 20, 30, 40),
    source_y = c(100, 200, 300, 400),
    target_x = c(15, 25, 35, 45),
    target_y = c(110, 210, 310, 410)
  )

  expect_error(
    hsi_check_gcp(collinear_gcps),
    "collinear",
    class = "hsitools_error"
  )
})

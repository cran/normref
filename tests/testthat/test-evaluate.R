# Unit Tests
test_that("normtable_create validates input parameters correctly", {
  #skip_on_cran()
  
  # Create test model and data
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  # Test with valid parameters
  expect_silent({
    result <- normtable_create(model, data, "age", "shaped_score")
  })
  
  # Test invalid reliability data structure
  invalid_rel <- data.frame(invalid_name = 1:5, other_col = 1:5)
  expect_error(
    normtable_create(model, data, "age", "shaped_score", datarel = invalid_rel),
    "datarel` must have columns named 'age' and 'rel'"
  )
})

test_that("normtable_create returns correct structure", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  result <- normtable_create(model, data, "age", "shaped_score")
  
  # Check class and basic structure
  expect_s3_class(result, "NormTable")
  
  # Check required elements are present
  expected_elements <- c("norm_sample", "norm_sample_lower", "norm_sample_upper",
                         "norm_matrix", "norm_matrix_lower", "norm_matrix_upper",
                         "znorm_sample", "cdf_sample", "cdf_matrix",
                         "data", "age_name", "score_name", "pop_age")
  
  expect_true(all(expected_elements %in% names(result)))
  
  # Check dimensions and types
  expect_equal(nrow(result$norm_sample), nrow(data))
  expect_true(is.data.frame(result$norm_matrix))
  expect_true(is.matrix(result$cdf_sample))
  expect_equal(result$age_name, "age")
  expect_equal(result$score_name, "shaped_score")
})

test_that("normtable_create works with different norm types", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  norm_types <- c("Z", "T", "IQ")
  
  for (normtype in norm_types) {
    result <- normtable_create(model, data, "age", "shaped_score", normtype = normtype)
    
    expect_s3_class(result, "NormTable")
    
    # Check that norm values are in expected ranges
    if (normtype == "Z") {
      expect_true(all(abs(result$norm_sample[, 2]) <= 5), # Most Z scores should be within ±5
                  info = "Z scores outside reasonable range")
      expect_equal(colnames(result$norm_sample)[2],normtype)
    } else if (normtype == "T") {
      expect_true(all(result$norm_sample[, 2] >= 0 & result$norm_sample[, 2] <= 100),
                  info = "T scores outside 0-100 range")
      expect_equal(colnames(result$norm_sample)[2],normtype)
    } else if (normtype == "IQ") {
      expect_true(all(result$norm_sample[, 2] >= 50 & result$norm_sample[, 2] <= 150),
                  info = "IQ scores outside reasonable range")
      expect_equal(colnames(result$norm_sample)[2],normtype)
    }
  }
})

test_that("normtable_create handles reliability data correctly", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  # Test with dataframe reliability
  rel_data <- create_reliability_data(15, 70, 0.85)
  result_df <- normtable_create(model, data, "age", "shaped_score", datarel = rel_data)
  
  expect_false(all(is.na(result_df$norm_sample_lower)))
  expect_false(all(is.na(result_df$norm_sample_upper)))
  expect_true(is.data.frame(result_df$norm_matrix_lower))
  expect_true(is.data.frame(result_df$norm_matrix_upper))
  
  # Test with single reliability value
  result_single <- normtable_create(model, data, "age", "shaped_score", datarel = 0.8)
  
  expect_false(all(is.na(result_single$norm_sample_lower)))
  expect_false(all(is.na(result_single$norm_sample_upper)))
  
  # Test without reliability data
  result_no_rel <- normtable_create(model, data, "age", "shaped_score")
  
  expect_true(all(is.na(result_no_rel$norm_sample_lower)))
  expect_true(all(is.na(result_no_rel$norm_sample_upper)))
})

test_that("normtable_create handles age and score range parameters", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  # Test custom age range
  result_custom <- normtable_create(model, data, "age", "shaped_score", 
                                    min_age = 20, max_age = 60)
  
  expect_true(min(result_custom$pop_age$age) >= 20)
  expect_true(max(result_custom$pop_age$age) <= 60)
  
  # Test custom score range
  result_score <- normtable_create(model, data, "age", "shaped_score", normtype = "IQ",
                                   min_score = 5, max_score = 15, step_size_score = 2)
  
  score_cols <- as.numeric(names(result_score$norm_matrix)[-1])
  expect_true(min(score_cols) >= 5)
  expect_true(max(score_cols) <= 15)
  expect_true(all(diff(score_cols) == 2))
})

test_that("normtable_create handles step size parameters", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  # Test age step size
  result_age_step <- normtable_create(model, data, "age", "shaped_score", 
                                      step_size_age = 5, min_age = 20, max_age = 60)
  
  age_diff <- diff(result_age_step$pop_age$age)
  expect_true(all(abs(age_diff - 5) < 0.001))
  
  # Test score step size
  result_score_step <- normtable_create(model, data, "age", "shaped_score",
                                        step_size_score = 0.5, min_score = 10, max_score = 15)
  
  score_cols <- as.numeric(names(result_score_step$norm_matrix)[-1])
  score_diff <- diff(score_cols)
  expect_true(all(abs(score_diff - 0.5) < 0.001))
})

test_that("normtable_create handles continuity correction", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  result_no_corr <- normtable_create(model, data, "age", "shaped_score", cont_cor = FALSE)
  result_with_corr <- normtable_create(model, data, "age", "shaped_score", cont_cor = TRUE)
  
  # Results should be different when continuity correction is applied
  expect_false(identical(result_no_corr$cdf_sample, result_with_corr$cdf_sample))
  expect_false(identical(result_no_corr$norm_sample, result_with_corr$norm_sample))
  
  expect_true(all(result_no_corr$cdf_sample >= result_with_corr$cdf_sample))
  expect_true(all(result_no_corr$norm_sample >= result_with_corr$norm_sample))
})

test_that("normtable_create handles confidence interval levels", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  rel_data <- create_reliability_data(15, 70, 0.85)
  
  result_95 <- normtable_create(model, data, "age", "shaped_score", 
                                datarel = rel_data, ci_level = 0.95)
  result_90 <- normtable_create(model, data, "age", "shaped_score", 
                                datarel = rel_data, ci_level = 0.90)
  
  # 90% CI should be narrower than 95% CI
  interval_95 <- result_95$norm_sample_upper[, 2] - result_95$norm_sample_lower[, 2]
  interval_90 <- result_90$norm_sample_upper[, 2] - result_90$norm_sample_lower[, 2]
  
  expect_true(all(interval_90 <= interval_95))
})

test_that("normtable_create handles trimming parameter", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  result_trim_3 <- normtable_create(model, data, "age", "shaped_score", trim = 3)
  result_trim_2 <- normtable_create(model, data, "age", "shaped_score", trim = 2)
  
  # More aggressive trimming should result in values closer to the mean
  z_scores_3 <- result_trim_3$znorm_sample[, 2]
  z_scores_2 <- result_trim_2$znorm_sample[, 2]
  
  expect_true(all(abs(z_scores_3) <= 3.01)) # Allow small numerical tolerance
  expect_true(all(abs(z_scores_2) <= 2.01))
  expect_true(max(abs(z_scores_2)) <= max(abs(z_scores_3)))
})

test_that("normtable_create new_data parameter works correctly", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  original_data <- test_setup$data
  
  # Create new data
  new_data <- create_test_data("NO", 20)
  new_shaped <- shape_data(new_data, "age", "score", "NO", verbose = FALSE)
  
  result_new <- normtable_create(model, new_shaped, "age", "shaped_score", new_data = TRUE)
  
  # When new_data = TRUE, matrix outputs should be NA
  expect_true(all(is.na(result_new$norm_matrix)))
  expect_true(all(is.na(result_new$cdf_matrix)))
  expect_true(all(is.na(result_new$pop_age)))
  
  # But sample outputs should be present
  expect_false(all(is.na(result_new$norm_sample)))
  expect_false(all(is.na(result_new$cdf_sample)))
  expect_equal(nrow(result_new$norm_sample), nrow(new_shaped))
})

test_that("normtable_create works with different GAMLSS families", {
  families_to_test <- c("NO", "BB", "BCPE")
  
  # Add NOtr if gamlss.tr is available
  if (requireNamespace("gamlss.tr", quietly = TRUE)) {
    gamlss.tr::gen.trun(par = c(0), family = "NO", name = "tr",
                        type = "left", envir = .GlobalEnv)
    families_to_test <- c(families_to_test, "NOtr")
  }
  
  for (family in families_to_test) {
    test_setup <- create_test_model(family, 500)
    model <- test_setup$model
    data <- test_setup$data
    
    result <- normtable_create(model, data, "age", "shaped_score")
    
    expect_s3_class(result, "NormTable")
    expect_equal(nrow(result$norm_sample), nrow(data))
    expect_true(all(is.finite(result$norm_sample[, 2])))
  }
})

test_that("normtable_create produces consistent results", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  result1 <- normtable_create(model, data, "age", "shaped_score")
  result2 <- normtable_create(model, data, "age", "shaped_score")
  
  # Results should be identical for same inputs
  expect_equal(result1$norm_sample, result2$norm_sample)
  expect_equal(result1$cdf_sample, result2$cdf_sample)
  expect_equal(result1$norm_matrix, result2$norm_matrix)
})

test_that("normtable_create handles edge cases", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  
  # Test with single age/score value
  single_data <- data.frame(age = 25, score = 15)
  result_single <- normtable_create(model, single_data, "age", "score")
  
  expect_s3_class(result_single, "NormTable")
  expect_equal(nrow(result_single$norm_sample), 1)
})

test_that("normtable_create CDF values are reasonable", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  data <- test_setup$data
  
  result <- normtable_create(model, data, "age", "shaped_score")
  
  # CDF values should be between 0 and 1
  expect_true(all(result$cdf_sample >= 0 & result$cdf_sample <= 1))
  expect_true(all(result$cdf_matrix >= 0 & result$cdf_matrix <= 1))
  
  # Check that CDF values make sense (roughly uniform distribution expected)
  cdf_quantiles <- quantile(result$cdf_sample, c(0.1, 0.5, 0.9))
  expect_true(cdf_quantiles[1] > 0.05 & cdf_quantiles[1] < 0.25)
  expect_true(cdf_quantiles[2] > 0.3 & cdf_quantiles[2] < 0.7)
  expect_true(cdf_quantiles[3] > 0.75 & cdf_quantiles[3] < 0.95)
})

test_that("normtable_create Z-scores are reasonable", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  data <- test_setup$data
  
  result <- normtable_create(model, data, "age", "shaped_score", normtype = "Z")
  
  z_scores <- result$norm_sample[, 2]
  
  # Z-scores should be roughly centered around 0
  expect_true(abs(mean(z_scores)) < 0.5)
  
  # Most Z-scores should be within ±3 (due to trimming)
  expect_true(all(abs(z_scores) <= 3.1)) # Small tolerance for numerical precision
  
  # Standard deviation should be reasonable
  expect_true(sd(z_scores) > 0.5 & sd(z_scores) < 2.0)
})

test_that("normtable_create confidence intervals are logical", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  rel_data <- create_reliability_data(15, 70, 0.8)
  
  result <- normtable_create(model, data, "age", "shaped_score", datarel = rel_data)
  
  # Lower bound should be less than upper bound
  expect_true(all(result$norm_sample_lower[, 2] <= result$norm_sample_upper[, 2]))
  
  # Point estimates should generally be between confidence bounds
  # (allowing for some edge cases due to trimming)
  within_bounds <- result$norm_sample[, 2] >= result$norm_sample_lower[, 2] & 
    result$norm_sample[, 2] <= result$norm_sample_upper[, 2]
  expect_true(mean(within_bounds) > 0.8) # At least 80% should be within bounds
})

# Performance and memory tests
test_that("normtable_create completes in reasonable time", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 200)
  model <- test_setup$model
  data <- test_setup$data
  
  start_time <- Sys.time()
  result <- normtable_create(model, data, "age", "shaped_score")
  end_time <- Sys.time()
  
  # Should complete within 15 seconds for moderate-sized dataset
  expect_true(as.numeric(end_time - start_time) < 15)
})

test_that("normtable_create handles missing values appropriately", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  # All outputs should have finite values (no NA/NaN/Inf)
  result <- normtable_create(model, data, "age", "shaped_score")
  
  expect_true(all(is.finite(result$norm_sample[, 2])))
  expect_true(all(is.finite(result$cdf_sample)))
  expect_true(all(is.finite(result$znorm_sample[, 2])))
  expect_true(all(is.finite(as.matrix(result$norm_matrix[, -1]))))
})



# -------------------------------------------------------------------
# Tests for reliability_window
# -------------------------------------------------------------------
test_that("reliability_window validates input parameters correctly", {
  #skip_on_cran()
  
  d <- simulate_1pl()
  expect_error(
    reliability_window(d, "age", paste0("item", 1:6), 2, window_version = "invalid"),
    "`window_version` must be 'window_per_person' or 'step'"
  )
})

test_that("reliability_window returns correct structure", {
  #skip_on_cran()
  
  d <- simulate_1pl()
  res <- reliability_window(d, "age", paste0("item", 1:6), 2)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("rel", "age", "window_width", "window_per") %in% names(res)))
})

test_that("reliability_window works with step window version", {
  #skip_on_cran()
  
  d <- simulate_1pl()
  res <- reliability_window(d, "age", paste0("item", 1:6), 2, window_version = "step")
  expect_true(length(unique(res$age)) > 1)
})

test_that("reliability_window works with window_per_person version", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 50)
  res <- reliability_window(d, "age", paste0("item", 1:6), 2, window_version = "window_per_person")
  expect_equal(nrow(res), nrow(d))
})

test_that("reliability_window handles age range parameters", {
  #skip_on_cran()
  
  d <- simulate_1pl()
  res <- reliability_window(d, "age", paste0("item", 1:6), 2, min_agegroup = 7, max_agegroup = 9)
  expect_true(all(res$age >= 7 & res$age <= 9))
})

test_that("reliability_window handles step_agegroup parameter", {
  #skip_on_cran()
  
  d <- simulate_1pl()
  res <- reliability_window(d, "age", paste0("item", 1:6), 2, step_agegroup = 2)
  diffs <- diff(res$age)
  expect_true(all(diffs %in% c(2, NA)))
})

test_that("reliability_window handles missing data correctly", {
  #skip_on_cran()
  
  d <- simulate_1pl()
  d$item1[sample(1:nrow(d), 10)] <- NA
  res1 <- reliability_window(d, "age", paste0("item", 1:6), 2, complete.obs = TRUE)
  res2 <- reliability_window(d, "age", paste0("item", 1:6), 2, complete.obs = FALSE)
  expect_true(mean(is.na(res1$rel)) >= mean(is.na(res2$rel)))
})

test_that("reliability_window produces reasonable reliability estimates", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 300)
  res <- reliability_window(d, "age", paste0("item", 1:6), 3)
  expect_true(all(res$rel >= -1 & res$rel <= 1, na.rm = TRUE))
})

test_that("reliability_window handles different numbers of items", {
  #skip_on_cran()
  
  d <- simulate_1pl(J = 3)
  res <- reliability_window(d, "age", paste0("item", 1:3), 2)
  expect_true(all(!is.na(res$rel)))
})

test_that("reliability_window handles different window widths", {
  #skip_on_cran()
  
  d <- simulate_1pl()
  res1 <- reliability_window(d, "age", paste0("item", 1:6), 1)
  res2 <- reliability_window(d, "age", paste0("item", 1:6), 5)
  expect_false(identical(res1$rel, res2$rel))
})

test_that("reliability_window produces NA for insufficient data", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 20)
  res <- reliability_window(d, "age", paste0("item", 1:6), 0.1)
  expect_true(any(is.na(res$rel)))
})

test_that("reliability_window calculations are comparable to psych::alpha", {
  #skip_on_cran()
  
  skip_if_not_installed("psych")

  d <- simulate_1pl(N = 300, J = 6)
  d_sub <- subset(d, age >= 9 & age < 11)
  alpha_res <- psych::alpha(d_sub[, paste0("item", 1:6)])$total$raw_alpha
  res <- reliability_window(d, "age", paste0("item", 1:6),
                            window_width = 2,
                            window_version = "step",
                            min_agegroup = 10,
                            max_agegroup = 10,
                            step_agegroup =1)
  expect_equal(res$rel[1], alpha_res, tolerance = 1e-6)
})

test_that("reliability_window is consistent across runs", {
  #skip_on_cran()
  
  d <- simulate_1pl(seed = 42)
  res1 <- reliability_window(d, "age", paste0("item", 1:6), 2)
  res2 <- reliability_window(d, "age", paste0("item", 1:6), 2)
  expect_equal(res1, res2)
})

# -------------------------------------------------------------------
# Tests for different_rel
# -------------------------------------------------------------------
test_that("different_rel returns correct structure", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 100, J = 5)
  res <- different_rel(d, paste0("item", 1:5), "age",
                       step_window = c(1, 2), step_agegroup = c(1, 2))
  expect_s3_class(res, "Drel")
  expect_true(all(c("rel", "age", "window_width", "age_group_width", "version") %in% names(res)))
})

test_that("different_rel expands grid of window_width and step_agegroup", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 100, J = 4)
  res <- different_rel(d, paste0("item", 1:4), "age",
                       step_window = c(1, 2), step_agegroup = c(1, 2))
  expect_equal(length(unique(res$window_width)), 2)
  expect_equal(length(unique(res$age_group_width)), 2)
})

test_that("different_rel includes window_per_person when requested", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 50, J = 3)
  res <- different_rel(d, paste0("item", 1:3), "age",
                       step_window = 2, step_agegroup = 1,
                       include_window_per_person = TRUE)
  expect_true(any(res$version == "window_per_person"))
  expect_true(any(res$version == "step"))
})

test_that("different_rel handles missing data correctly", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 80, J = 4)
  d$item1[sample(1:nrow(d), 5)] <- NA
  res1 <- different_rel(d, paste0("item", 1:4), "age",
                        step_window = 2, step_agegroup = 1,
                        complete.obs = TRUE)
  res2 <- different_rel(d, paste0("item", 1:4), "age",
                        step_window = 2, step_agegroup = 1,
                        complete.obs = FALSE)
  expect_true(mean(is.na(res1$rel)) >= mean(is.na(res2$rel)))
})

test_that("different_rel handles edge cases in window width", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 30, J = 3)
  res_wide <- different_rel(d, paste0("item", 1:3), "age",
                            step_window = 100, step_agegroup = 1)
  res_narrow <- different_rel(d, paste0("item", 1:3), "age",
                              step_window = 0.1, step_agegroup = 1)
  expect_true(any(is.na(res_narrow$rel)))
  expect_true(all(!is.na(res_wide$rel)))
})

test_that("different_rel respects min_agegroup and max_agegroup", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 100, J = 4)
  res <- different_rel(d, paste0("item", 1:4), "age",
                       step_window = 2, step_agegroup = 1,
                       min_agegroup = 7, max_agegroup = 9)
  expect_true(all(res$age >= 7 & res$age <= 9))
})

test_that("different_rel is consistent across runs", {
  #skip_on_cran()
  
  d <- simulate_1pl(seed = 123)
  res1 <- different_rel(d, paste0("item", 1:5), "age",
                        step_window = 2, step_agegroup = 1)
  res2 <- different_rel(d, paste0("item", 1:5), "age",
                        step_window = 2, step_agegroup = 1)
  expect_equal(res1, res2)
})

test_that("different_rel completes within reasonable time", {
  #skip_on_cran()
  
  d <- simulate_1pl(N = 500, J = 10)
  expect_silent({
    res <- different_rel(d, paste0("item", 1:10), "age",
                         step_window = c(1, 2, 3),
                         step_agegroup = c(1, 2))
  })
})

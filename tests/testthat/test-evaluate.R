# Unit Tests
test_that("normtable_create validates input parameters correctly", {
  #skip_on_cran()
  
  # Create test model and data
  test_setup <- create_test_model("NO", 50)
  model <- test_setup$model
  data <- test_setup$data
  
  # Test with valid parameters
  expect_silent({
    result <- normtable_create(model, data = data, age_name = "age", score_name = "shaped_score")
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
  
  test_setup <- create_test_model("BB", 50)
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

test_that("normtable_create works with different GAMLSS families", {
  skip_on_cran()
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

test_that("normtable_create maintains population norm_matrix while predicting on newdata", {
  #skip_on_cran()
  
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  fit_data <- test_setup$data
  
  # Cohort of 10 people restricted to age 0
  test_newdata <- data.frame(age = rep(25, 10), shaped_score = seq(-2, 2, length.out = 10))
  
  res <- normtable_create(
    model = model,
    data = fit_data,
    newdata = test_newdata,
    age_name = "age",
    score_name = "shaped_score",
    normtype = "Z"
  )
  
  # 1. Sample norms evaluate the 10 individuals in newdata
  expect_equal(nrow(res$norm_sample), 10)
  expect_equal(res$norm_sample[, "age"], rep(25, 10))
  
  # 2. Population norm_matrix remains full population grid from fit_data
  expect_true(nrow(res$norm_matrix) > 10)
  expect_equal(min(res$pop_age$age), min(fit_data$age))
  expect_equal(max(res$pop_age$age), max(fit_data$age))
  expect_true(is.matrix(res$cdf_matrix) || is.data.frame(res$cdf_matrix))
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
  single_data <- data.frame(age = 25, score = 0)
  result_single <- normtable_create(model, age_name = "age", score_name = "score", newdata = single_data)
  
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
  expect_true(all(result$cdf_matrix[,-1] >= 0 & result$cdf_matrix[,-1] <= 1))
  
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
  skip_on_cran()
  
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

test_that("normtable_create produces identical results whether data is explicit or NULL", {
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  fit_data <- test_setup$data
  
  res_data <- normtable_create(
    model = model,
    data = fit_data,
    age_name = "age",
    score_name = "shaped_score",
    normtype = "Z"
  )
  
  res_null_data <- normtable_create(
    model = model,
    data = NULL,
    age_name = "age",
    score_name = "shaped_score",
    normtype = "Z"
  )
  
  expect_equal(res_null_data$norm_sample, res_data$norm_sample)
  expect_equal(res_null_data$znorm_sample, res_data$znorm_sample)
  expect_equal(res_null_data$norm_matrix, res_data$norm_matrix)
})

test_that("normtable_create evaluates norm scores correctly on newdata within normative sample range", {
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  fit_data <- test_setup$data
  
  # Generate distinct test data kept strictly within the calibration range
  test_n <- 10
  age_rng <- range(fit_data$age)
  score_rng <- range(fit_data$shaped_score)
  
  test_data <- data.frame(
    age = seq(age_rng[1] + 0.1, age_rng[2] - 0.1, length.out = test_n),
    score = seq(score_rng[1] + 0.1, score_rng[2] - 0.1, length.out = test_n)
  )
  test_shaped <- shape_data(test_data, "age", "score", "NO", verbose = FALSE)
  
  res_data <- normtable_create(
    model = model,
    data = fit_data,
    age_name = "age",
    score_name = "shaped_score",
    normtype = "Z"
  )
  
  expect_silent(
    res_newdata <- normtable_create(
      model = model,
      newdata = test_shaped,
      age_name = "age",
      score_name = "shaped_score",
      normtype = "Z"
    )
  )
  
  # Check that population norm matrix is unchanged
  expect_equal(res_newdata$norm_matrix, res_data$norm_matrix)
  
  # Check that evaluation is done on newdata rows
  expect_equal(res_newdata$znorm_sample[, "age"], test_shaped$age)
  expect_equal(nrow(res_newdata$znorm_sample), test_n)
  expect_false(identical(res_newdata$znorm_sample[, "age"], res_data$znorm_sample[1:test_n, "age"]))
  expect_false(identical(res_newdata$znorm_sample[, "Z"], res_data$znorm_sample[1:test_n, "Z"]))
})

test_that("normtable_create throws an error when supplied data does not correspond to model$data", {
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  
  test_data <- create_test_data("NO", 10)
  test_shaped <- shape_data(test_data, "age", "score", "NO", verbose = FALSE)
  
  # Supplying mismatched data via `data` must throw an error
  expect_error(
    normtable_create(
      model = model,
      data = test_shaped,
      age_name = "age",
      score_name = "shaped_score",
      normtype = "Z"
    ),
    regexp = "does not correspond to the data found in `model\\$data`"
  )
})

test_that("normtable_create warns when newdata extrapolates outside age or score range", {
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  fit_data <- test_setup$data
  
  # Create newdata where age and score exceed the calibration range
  out_of_bounds_data <- data.frame(
    age = c(min(fit_data$age) - 5, max(fit_data$age) + 5),
    shaped_score = c(min(fit_data$shaped_score) - 5, max(fit_data$shaped_score) + 5)
  )
  
  expect_warning(
    normtable_create(
      model = model,
      newdata = out_of_bounds_data,
      age_name = "age",
      score_name = "shaped_score"
    ),
    regexp = "fall outside the range of the normative sample"
  )
})

test_that("normtable_create predictions on a subset via newdata match full data predictions", {
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  fit_data <- test_setup$data
  
  sub_indices <- 1:5
  sub_data <- fit_data[sub_indices, ]
  
  res_data <- normtable_create(
    model = model,
    data = fit_data,
    age_name = "age",
    score_name = "shaped_score",
    normtype = "Z"
  )
  
  res_subset_newdata <- normtable_create(
    model = model,
    newdata = sub_data,
    age_name = "age",
    score_name = "shaped_score",
    normtype = "Z"
  )
  
  expect_equal(
    res_subset_newdata$norm_sample[, 2],
    res_data$norm_sample[sub_indices, 2],
    tolerance = 1e-6,
    info = "Norm predictions on newdata must match predictions on original fitting data."
  )
  expect_equal(
    as.vector(res_subset_newdata$cdf_sample),
    as.vector(res_data$cdf_sample[sub_indices, ]),
    tolerance = 1e-6,
    info = "CDF predictions on newdata must match predictions on original fitting data."
  )
})

test_that("normtable_create computes valid confidence intervals for newdata", {
  test_setup <- create_test_model("NO", 100)
  model <- test_setup$model
  fit_data <- test_setup$data
  
  # Create newdata within range
  test_n <- 10
  age_rng <- range(fit_data$age)
  score_rng <- range(fit_data$shaped_score)
  test_data <- data.frame(
    age = seq(age_rng[1] + 0.1, age_rng[2] - 0.1, length.out = test_n),
    score = seq(score_rng[1] + 0.1, score_rng[2] - 0.1, length.out = test_n)
  )
  test_shaped <- shape_data(test_data, "age", "score", "NO", verbose = FALSE)
  
  res_newdata_rel <- normtable_create(
    model = model,
    newdata = test_shaped,
    age_name = "age",
    score_name = "shaped_score",
    datarel = 0.85
  )
  
  expect_equal(nrow(res_newdata_rel$norm_sample_lower), test_n)
  expect_equal(nrow(res_newdata_rel$norm_sample_upper), test_n)
  expect_true(all(res_newdata_rel$norm_sample_lower[, 2] <= res_newdata_rel$norm_sample_upper[, 2]))
  expect_true(all(res_newdata_rel$norm_sample_lower[, 2] <= res_newdata_rel$norm_sample[, 2]))
  expect_true(all(res_newdata_rel$norm_sample[, 2] <= res_newdata_rel$norm_sample_upper[, 2]))
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
  skip_on_cran()
  
  d <- simulate_1pl(N = 500, J = 10)
  expect_silent({
    res <- different_rel(d, paste0("item", 1:10), "age",
                         step_window = c(1, 2, 3),
                         step_agegroup = c(1, 2))
  })
})


test_that("boot_subtest works with standard continuous model and respects seed replicability", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 42)
  
  # Run twice with the exact same seed
  res1 <- boot_subtest(
    model = fitted$model,
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 42,
    step_size_age = 10,
    parallel = FALSE
  )
  
  res2 <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 42,
    step_size_age = 10,
    parallel = FALSE
  )
  
  # Check validity and reproducibility
  expect_valid_boot_subtest(res1, expected_boot_type = "nonparametric")
  expect_equal(res1$normtable$boot_se, res2$normtable$boot_se)
  expect_equal(res1$sample$boot_se, res2$sample$boot_se)
})

test_that("boot_subtest works with standard continuous model and respects seed replicability when parallel is true", {
  skip_on_cran()
  skip_if_not_installed("doParallel")
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 42)
  
  # Run twice with the exact same seed
  res1 <- boot_subtest(
    model = fitted$model,
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 42,
    step_size_age = 10,
    parallel = TRUE
  )
  
  res2 <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 42,
    step_size_age = 10,
    parallel = TRUE
  )
  
  # Check validity and reproducibility
  expect_valid_boot_subtest(res1, expected_boot_type = "nonparametric")
  expect_equal(res1$normtable$boot_se, res2$normtable$boot_se)
  expect_equal(res1$sample$boot_se, res2$sample$boot_se)
})

test_that("boot_subtest works with spline models defined via gamlss::pb()", {
  skip_on_cran()
  
  fitted_spline <- fit_pb_no(n = N_BOOT, seed = 101)
  
  res_spline <- boot_subtest(
    model = fitted_spline$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 102,
    step_size_age = 20,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res_spline, expected_boot_type = "nonparametric")
})


test_that("boot_subtest works with spline models defined via gamlss::pb() when parallel is true", {
  skip_on_cran()
  skip_if_not_installed("doParallel")
  
  fitted_spline <- fit_pb_no(n = N_BOOT, seed = 101)
  
  res_spline <- boot_subtest(
    model = fitted_spline$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 102,
    step_size_age = 20,
    parallel = TRUE
  )
  
  expect_valid_boot_subtest(res_spline, expected_boot_type = "nonparametric")
})


test_that("boot_subtest works with truncated distributions (gamlss.tr)", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.tr")
  
  fitted_tr <- fit_fb_select_truncated_no(n = N_BOOT, seed = 123)
  
  res_tr <- boot_subtest(
    model = fitted_tr$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 777,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res_tr, expected_boot_type = "nonparametric")
})

test_that("boot_subtest works with truncated distributions (gamlss.tr) when parallel is true", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.tr")
  skip_if_not_installed("doParallel")
  
  fitted_tr <- fit_fb_select_truncated_no(n = N_BOOT, seed = 123)
  
  res_tr <- boot_subtest(
    model = fitted_tr$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 777,
    step_size_age = 15,
    parallel = TRUE
  )
  
  expect_valid_boot_subtest(res_tr, expected_boot_type = "nonparametric")
})

test_that("boot_subtest parametric mode respects n_sim and age_sampling_frame", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 55)
  custom_age_gen <- function(n) runif(n, 20, 60)
  
  res_param <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "parametric",
    n_sim = 40L,
    age_sampling_frame = custom_age_gen,
    B = B_BOOT,
    seed = 56,
    step_size_age = 25,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res_param, expected_boot_type = "parametric")
  expect_equal(res_param$meta$boot_type, "parametric")
  expect_equal(res_param$meta$n_sim, 40L)
})

# -----------------------------------------------------------------------------
# Input Validation
# -----------------------------------------------------------------------------

test_that("boot_subtest validates input model type", {
  skip_on_cran()
  
  lm_mod <- stats::lm(mpg ~ hp, data = mtcars)
  
  expect_error(
    boot_subtest(
      model = lm_mod,
      age_name = "hp",
      B = B_BOOT
    ),
    regexp = "'model' must be a fitted gamlss object"
  )
})

test_that("boot_subtest validates independent variable and response existence", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 201)
  
  # Non-existent age_name
  expect_error(
    boot_subtest(
      model = fitted$model,
      age_name = "non_existent_age",
      score_name = "shaped_score",
      B = B_BOOT
    ),
    regexp = "Independent variable 'non_existent_age' not found"
  )
})

test_that("boot_subtest validates boot_type and normtype options", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 202)
  
  expect_error(
    boot_subtest(
      model = fitted$model,
      age_name = "age",
      boot_type = "invalid_type",
      B = B_BOOT
    )
  )
  
  expect_error(
    boot_subtest(
      model = fitted$model,
      age_name = "age",
      normtype = "INVALID_SCALE",
      B = B_BOOT
    )
  )
})

# -----------------------------------------------------------------------------
# Model Reselection (reselect = TRUE) and Selection History
# -----------------------------------------------------------------------------

test_that("boot_subtest records model selection history and frequency table when reselect = TRUE", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 210)
  
  res_reselect <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    reselect = TRUE,
    B = B_BOOT+10,
    seed = 211,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res_reselect, expected_boot_type = "nonparametric")
  expect_true(is.list(res_reselect$model_selection_history))
  expect_equal(length(res_reselect$model_selection_history), res_reselect$trueB)
  
  # Frequency table must be present and sum to trueB
  expect_true(class(res_reselect$model_freq_table) %in% c("integer", "table"))
  expect_equal(sum(res_reselect$model_freq_table), res_reselect$trueB)
  
  # Each history element should contain numeric polynomial degrees
  for (deg in res_reselect$model_selection_history) {
    expect_true(is.numeric(deg))
  }
})

test_that("boot_subtest does not compute model_freq_table when reselect = FALSE", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 212)
  
  res_noreselect <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    reselect = FALSE,
    B = B_BOOT,
    seed = 213,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_null(res_noreselect$model_freq_table)
  expect_equal(length(res_noreselect$model_selection_history), res_noreselect$trueB)
})

# -----------------------------------------------------------------------------
# fb_control Parameter Constraints (min_poly == max_poly)
# -----------------------------------------------------------------------------

test_that("boot_subtest respects fb_control constraints when reselect = TRUE", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 210)
  
  target_poly <- c(1, 0)
  names(target_poly) <- c("mu", "sigma")
  
  res_constrained <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "parametric",
    reselect = TRUE,
    fb_control = list(
      min_poly = target_poly ,
      max_poly = target_poly ,
      start_poly = target_poly ,
      method = "RS(100)"
    ),
    B = B_BOOT + 10,
    seed = 221,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res_constrained, expected_boot_type = "parametric")
  
  # Check that every replicate selected the exact fixed degrees
  expect_true(length(res_constrained$model_selection_history) > 0)
  for (deg in res_constrained$model_selection_history) {
    expect_equal(deg, target_poly)
  }
  
  # Ensure the frequency table has only one unique model configuration
  expect_equal(length(res_constrained$model_freq_table), 1L)
  expect_equal(names(res_constrained$model_freq_table)[1], paste(target_poly, collapse = "-"))
  expect_equal(as.integer(res_constrained$model_freq_table[1]), res_constrained$trueB)
})

# -----------------------------------------------------------------------------
# Trimming
# -----------------------------------------------------------------------------

test_that("boot_subtest sets normtable boot_se to NA outside trimmed Z scores", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 301)
  
  res <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    trim = 0.50,
    min_age = 20,
    max_age = 60,
    step_size_age = 10,
    B = B_BOOT,
    seed = 302,
    parallel = FALSE
  )
  
  # For normtype = "Z", $original is the original z-score estimate.
  outside <- !is.na(res$normtable$original) &
    abs(res$normtable$original) >= 0.50
  inside <- !is.na(res$normtable$original) &
    abs(res$normtable$original) < 0.50
  
  expect_true(any(outside))
  expect_true(any(inside))
  expect_true(all(is.na(res$normtable$boot_se[outside])))
  expect_true(all(!is.na(res$normtable$boot_se[inside])))
})

test_that("boot_subtest sets sample boot_se to NA outside trimmed Z scores", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 303)
  
  res <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    trim = 0.50,
    min_age = 20,
    max_age = 60,
    step_size_age = 10,
    B = B_BOOT,
    seed = 304,
    parallel = FALSE
  )
  
  outside <- !is.na(res$sample$original) &
    abs(res$sample$original) >= 0.50
  inside <- !is.na(res$sample$original) &
    abs(res$sample$original) < 0.50
  
  expect_true(any(outside))
  expect_true(any(inside))
  expect_true(all(is.na(res$sample$boot_se[outside])))
  expect_true(all(!is.na(res$sample$boot_se[inside])))
})

test_that("trimming is applied on the requested norm scale", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 305)
  
  res <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    normtype = "T",
    boot_type = "nonparametric",
    trim = 0.50,
    min_age = 20,
    max_age = 60,
    step_size_age = 10,
    B = B_BOOT,
    seed = 306,
    parallel = FALSE
  )
  
  # On the T scale, z = +/-0.50 corresponds to T = 45 and T = 55.
  outside <- !is.na(res$normtable$original) &
    (res$normtable$original <= 45 | res$normtable$original >= 55)
  inside <- !is.na(res$normtable$original) &
    res$normtable$original > 45 & res$normtable$original < 55
  
  expect_true(any(outside))
  expect_true(any(inside))
  expect_true(all(is.na(res$normtable$boot_se[outside])))
  expect_true(all(!is.na(res$normtable$boot_se[inside])))
})

# -----------------------------------------------------------------------------
# Parametric bootstrap: fb_select polynomial, truncated, and pb() spline
# -----------------------------------------------------------------------------

test_that("parametric boot_subtest works with an fb_select polynomial model", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 401)
  res <- run_small_parametric_boot(fitted, seed = 402)
  
  expect_valid_boot_subtest(res, expected_boot_type = "parametric")
  expect_equal(res$meta$n_sim, 50L)
  expect_true(any(is.finite(res$normtable$boot_se)))
})

test_that("parametric boot_subtest works with an fb_select truncated model", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.tr")
  
  fitted <- fit_fb_select_truncated_no(n = N_BOOT, seed = 401)
  res <- run_small_parametric_boot(fitted, seed = 404)
  
  expect_valid_boot_subtest(res, expected_boot_type = "parametric")
  expect_equal(res$meta$n_sim, 50L)
  expect_true(any(is.finite(res$normtable$boot_se)))
})

test_that("parametric boot_subtest works with a gamlss pb() spline model", {
  skip_on_cran()
  
  fitted <- fit_pb_no(n = N_BOOT, seed = 405)
  res <- run_small_parametric_boot(fitted, seed = 406)
  
  expect_valid_boot_subtest(res, expected_boot_type = "parametric")
  expect_equal(res$meta$n_sim, 50L)
  expect_true(any(is.finite(res$normtable$boot_se)))
})

test_that("parametric boot_subtest uses a numeric age sampling frame", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 601)
  age_values <- seq(25, 55, by = 5)
  
  res <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "parametric",
    n_sim = 40L,
    age_sampling_frame = age_values,
    min_age = 25,
    max_age = 55,
    step_size_age = 10,
    B = B_BOOT,
    seed = 602,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res, expected_boot_type = "parametric")
  expect_equal(res$meta$n_sim, 40L)
})

test_that("parametric boot_subtest is reproducible with a fixed seed", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 603)
  
  args <- list(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "parametric",
    n_sim = 40L,
    age_sampling_frame = function(n) runif(n, 20, 60),
    min_age = 20,
    max_age = 60,
    step_size_age = 10,
    B = B_BOOT,
    seed = 604,
    parallel = FALSE
  )
  
  res1 <- do.call(boot_subtest, args)
  res2 <- do.call(boot_subtest, args)
  
  expect_equal(res1$normtable$boot_se, res2$normtable$boot_se)
  expect_equal(res1$sample$boot_se, res2$sample$boot_se)
})

# -----------------------------------------------------------------------------
# keep_boot checks
# -----------------------------------------------------------------------------

test_that("boot_subtest retains raw bootstrap replicates when keep_boot = TRUE and discards when FALSE", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 111)
  
  res_keep <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    keep_boot = TRUE,
    B = B_BOOT,
    seed = 112,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_true(!is.null(res_keep$boot))
  expect_equal(length(res_keep$boot), res_keep$trueB)
  expect_named(res_keep$boot[[1]], c("table", "sample", "poly"))
  
  res_drop <- boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    keep_boot = FALSE,
    B = B_BOOT,
    seed = 112,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_null(res_drop$boot)
})

test_that("parametric boot_subtest works with bounded BB model", {
  skip_on_cran()
  
  fitted_bb <- fit_fb_select_bb(n = N_BOOT)
  
  res_bb_param <- boot_subtest(
    model = fitted_bb$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "parametric",
    n_sim = 40L,
    age_sampling_frame = function(n) runif(n, 20, 60),
    B = B_BOOT,
    seed = 116,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res_bb_param, expected_boot_type = "parametric")
  expect_equal(res_bb_param$meta$n_sim, 40L)
})

# -----------------------------------------------------------------------------
# Reselection (reselect = TRUE) across all model types
# -----------------------------------------------------------------------------

test_that("boot_subtest reselection works with truncated model (NOtr)", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  fitted_tr <- fit_fb_select_truncated_no(n = N_BOOT, seed = 120)
  
  res_tr_reselect <- boot_subtest(
    model = fitted_tr$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    reselect = TRUE,
    B = B_BOOT,
    seed = 121,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res_tr_reselect, expected_boot_type = "nonparametric")
  expect_true(is.list(res_tr_reselect$model_selection_history))
  expect_equal(length(res_tr_reselect$model_selection_history), res_tr_reselect$trueB)
  expect_true(class(res_tr_reselect$model_freq_table) %in% c("integer", "table"))
})

test_that("boot_subtest reselection works with bounded BB model", {
  skip_on_cran()
  
  fitted_bb <- fit_fb_select_bb(n = N_BOOT)
  
  res_bb_reselect <- boot_subtest(
    model = fitted_bb$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    reselect = TRUE,
    B = B_BOOT,
    seed = 126,
    step_size_age = 15,
    parallel = FALSE
  )
  
  expect_valid_boot_subtest(res_bb_reselect, expected_boot_type = "nonparametric")
  expect_true(is.list(res_bb_reselect$model_selection_history))
  expect_equal(length(res_bb_reselect$model_selection_history), res_bb_reselect$trueB)
  expect_true(class(res_bb_reselect$model_freq_table) %in% c("integer", "table"))
})


# ============================================================================
# boot_composite Tests
# ============================================================================

# -----------------------------------------------------------------------------
# Input Validation for boot_composite
# -----------------------------------------------------------------------------

test_that("boot_composite validates inputs and subtest consistency", {
  skip_on_cran()
  
  fix <- create_composite_fixture(n = N_BOOT)
  
  # 1. subtest_models must be a named list
  expect_error(
    boot_composite(
      subtest_models = list(fix$subtest_models[[1]], fix$subtest_models[[2]]),
      composite_model = fix$composite_model,
      age_name = "age",
      B = B_BOOT
    ),
    regexp = "'subtest_models' must be a named list"
  )
  
  # 2. composite_model must be a gamlss object
  expect_error(
    boot_composite(
      subtest_models = fix$subtest_models,
      composite_model = stats::lm(mpg ~ hp, data = mtcars),
      age_name = "age",
      B = B_BOOT
    ),
    regexp = "'composite_model' must be a fitted gamlss object"
  )
  
  # 3. Independent variable not found in one of the subtests
  expect_error(
    boot_composite(
      subtest_models = fix$subtest_models,
      composite_model = fix$composite_model,
      age_name = "wrong_age_col",
      B = B_BOOT
    ),
    regexp = "Independent variable 'wrong_age_col' not found"
  )
  
  # 4. Mismatched row counts across subtests
  mismatch_dat <- create_test_data("NO", n = N_BOOT + 10)
  mismatch_shaped <- shape_data(mismatch_dat, "age", "score", "NO", verbose = FALSE)
  mismatch_mod <- fb_select(
    data = mismatch_shaped,
    age_name = "age",
    score_name = "shaped_score",
    family = "NO",
    start_poly = c(1, 0),
    max_poly = c(1, 0),
    trace = FALSE
  )
  
  expect_error(
    boot_composite(
      subtest_models = list(sub_no = fix$subtest_models$sub_no, sub_bad = mismatch_mod),
      composite_model = fix$composite_model,
      age_name = "age",
      B = B_BOOT
    ),
    regexp = "All subtest models must be fitted on the exact same complete observations"
  )
  
  # 5. Invalid weights
  expect_error(
    boot_composite(
      subtest_models = fix$subtest_models,
      composite_model = fix$composite_model,
      age_name = "age",
      weights = c(1, 2, 3), # length mismatch
      B = B_BOOT
    ),
    regexp = "'weights' must be NULL or a numeric vector with length equal to subtest_models"
  )
})

# -----------------------------------------------------------------------------
# Basic Execution and Reproducibility
# -----------------------------------------------------------------------------

test_that("boot_composite runs with mixed continuous/bounded subtests and replicates with seed", {
  skip_on_cran()
  
  fix <- create_composite_fixture(n = N_BOOT)
  
  args <- list(
    subtest_models = fix$subtest_models,
    composite_model = fix$composite_model,
    age_name = "age",
    composite_score_name = "z_sum",
    subtest_score_names = list(sub_no = "score_no", sub_bb = "score_bb"),
    min_age = 20,
    max_age = 60,
    step_size_age = 15,
    min_score = -4,
    max_score = 4,
    step_size_score = 1,
    B = B_BOOT,
    seed = 711,
    parallel = FALSE
  )
  
  res1 <- do.call(boot_composite, args)
  res2 <- do.call(boot_composite, args)
  
  # Class & metadata checks
  expect_s3_class(res1, "normref_bootstrap")
  expect_equal(res1$meta$type, "composite")
  expect_equal(res1$B, B_BOOT)
  expect_gt(res1$trueB, 0)
  
  # Structure checks
  expect_true(is.data.frame(res1$normtable))
  expect_true(is.data.frame(res1$sample))
  expect_true("boot_se" %in% names(res1$normtable))
  expect_true("boot_se" %in% names(res1$sample))
  expect_true("score_no" %in% names(res1$sample))
  expect_true("score_bb" %in% names(res1$sample))
  expect_true("z_sum" %in% names(res1$sample))
  
  # Seed reproducibility
  expect_equal(res1$normtable$boot_se, res2$normtable$boot_se)
  expect_equal(res1$sample$boot_se, res2$sample$boot_se)
})

# -----------------------------------------------------------------------------
# Trimming in boot_composite
# -----------------------------------------------------------------------------

test_that("boot_composite applies trimming on the composite norm scale", {
  skip_on_cran()
  
  fix <- create_composite_fixture(n = N_BOOT)
  
  res_trim <- boot_composite(
    subtest_models = fix$subtest_models,
    composite_model = fix$composite_model,
    age_name = "age",
    trim = 0.50,
    min_age = 20,
    max_age = 60,
    step_size_age = 15,
    B = B_BOOT,
    seed = 721,
    parallel = FALSE
  )
  
  outside <- !is.na(res_trim$normtable$original) &
    abs(res_trim$normtable$original) >= 0.50
  inside <- !is.na(res_trim$normtable$original) &
    abs(res_trim$normtable$original) < 0.50
  
  expect_true(any(outside))
  expect_true(any(inside))
  expect_true(all(is.na(res_trim$normtable$boot_se[outside])))
  expect_true(all(!is.na(res_trim$normtable$boot_se[inside])))
})

# -----------------------------------------------------------------------------
# Reselection and fb_control in boot_composite
# -----------------------------------------------------------------------------

test_that("boot_composite supports reselect and inspects per-model selection history", {
  skip_on_cran()
  
  fix <- create_composite_fixture(n = N_BOOT)
  
  res_reselect <- boot_composite(
    subtest_models = fix$subtest_models,
    composite_model = fix$composite_model,
    age_name = "age",
    reselect = TRUE,
    min_age = 20,
    max_age = 60,
    step_size_age = 15,
    B = B_BOOT,
    seed = 731,
    parallel = FALSE
  )
  
  model_names <- c("sub_no", "sub_bb", "composite")
  expect_named(res_reselect$model_selection_history, model_names)
  expect_named(res_reselect$model_freq_table, model_names)
  
  for (nm in model_names) {
    expect_equal(length(res_reselect$model_selection_history[[nm]]), res_reselect$trueB)
    expect_true(class(res_reselect$model_freq_table[[nm]]) %in% c("integer", "table"))
    expect_equal(sum(res_reselect$model_freq_table[[nm]]), res_reselect$trueB)
  }
})

test_that("boot_composite respects fb_control degree constraints across subtests", {
  skip_on_cran()
  
  fix <- create_composite_fixture(n = N_BOOT)
  
  target_poly <- c(1, 0)
  names(target_poly) <- c("mu", "sigma")
  
  fb_ctrl <- list(
    sub_no = list(min_poly = target_poly, max_poly = target_poly, start_poly = target_poly, trace = FALSE),
    sub_bb = list(min_poly = target_poly, max_poly = target_poly, start_poly = target_poly, trace = FALSE),
    composite = list(min_poly = target_poly, max_poly = target_poly, start_poly = target_poly, trace = FALSE)
  )
  
  res_constrained <- boot_composite(
    subtest_models = fix$subtest_models,
    composite_model = fix$composite_model,
    age_name = "age",
    reselect = TRUE,
    fb_control = fb_ctrl,
    min_age = 20,
    max_age = 60,
    step_size_age = 15,
    B = B_BOOT,
    seed = 741,
    parallel = FALSE
  )
  
  for (nm in c("sub_no", "sub_bb", "composite")) {
    for (deg in res_constrained$model_selection_history[[nm]]) {
      expect_equal(deg, target_poly)
    }
    expect_equal(length(res_constrained$model_freq_table[[nm]]), 1L)
    expect_equal(as.integer(res_constrained$model_freq_table[[nm]][1]), res_constrained$trueB)
  }
})

# -----------------------------------------------------------------------------
# keep_boot in boot_composite
# -----------------------------------------------------------------------------

test_that("boot_composite retains replicate outputs when keep_boot = TRUE and discards when FALSE", {
  skip_on_cran()
  
  fix <- create_composite_fixture(n = N_BOOT)
  
  res_keep <- boot_composite(
    subtest_models = fix$subtest_models,
    composite_model = fix$composite_model,
    age_name = "age",
    keep_boot = TRUE,
    min_age = 20,
    max_age = 60,
    step_size_age = 15,
    B = B_BOOT,
    seed = 751,
    parallel = FALSE
  )
  
  expect_true(!is.null(res_keep$boot))
  expect_equal(length(res_keep$boot), res_keep$trueB)
  expect_named(res_keep$boot[[1]], c("table", "sample", "poly"))
  
  res_drop <- boot_composite(
    subtest_models = fix$subtest_models,
    composite_model = fix$composite_model,
    age_name = "age",
    keep_boot = FALSE,
    min_age = 20,
    max_age = 60,
    step_size_age = 15,
    B = B_BOOT,
    seed = 751,
    parallel = FALSE
  )
  
  expect_null(res_drop$boot)
})


# ============================================================================
# Convergence Check Tests (trueB = 0 on non-convergence)
# ============================================================================

test_that("boot_subtest aborts when non-converged with RS(1)", {
  skip_on_cran()
  
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 801)
  
  # method = 'RS(1)' forces non-convergence; ignore intermediate warnings and check error
  expect_error(
    suppressWarnings(
      boot_subtest(
        model = fitted$model,
        age_name = "age",
        score_name = "shaped_score",
        boot_type = "nonparametric",
        reselect = TRUE,
        fb_control = list(
          method = "RS(1)"
        ),
        B = B_BOOT,
        seed = 802,
        parallel = FALSE
      )
    ),
    regexp = "All bootstrap replicates failed"
  )
  
  fitted$model$call$method <- "RS(1)"
  expect_error(
    suppressWarnings(
      boot_subtest(
        model = fitted$model,
        age_name = "age",
        score_name = "shaped_score",
        boot_type = "nonparametric",
        reselect = FALSE,
        B = B_BOOT,
        seed = 802,
        parallel = FALSE
      )
    ),
    regexp = "All bootstrap replicates failed"
  )
})

# ============================================================================
# Full Model Grid Tests: boot_subtest
# Grid: {NO, Spline pb(), Truncated, BB} x {nonparametric, parametric} x {reselect: TRUE, FALSE}
# ============================================================================

test_that("boot_subtest runs across entire model grid, boot_types, and reselection modes", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  # 1. Fit models representing each architecture
  mod_no     <- fit_fb_select_no(n = N_BOOT, seed = 810)$model
  mod_spline <- fit_pb_no(n = N_BOOT, seed = 811)$model
  mod_tr     <- fit_fb_select_truncated_no(n = N_BOOT, seed = 812)$model
  mod_bb     <- fit_fb_select_bb(n = N_BOOT)$model
  
  models_grid <- list(
    "NO"        = mod_no,
    "spline"    = mod_spline,
    "truncated" = mod_tr,
    "BB"        = mod_bb
  )
  
  boot_types <- c("nonparametric", "parametric")
  reselect_opts <- c(FALSE, TRUE)
  
  for (m_name in names(models_grid)) {
    curr_mod <- models_grid[[m_name]]
    is_spline <- grepl("pb\\(", deparse(curr_mod$mu.formula))
    
    for (b_type in boot_types) {
      for (res_opt in reselect_opts) {
        
        # Splines fitted with pb() do not reselect polynomial degrees via fb_select
        if (is_spline && res_opt) next
        
        desc <- sprintf("boot_subtest grid: model=%s | boot_type=%s | reselect=%s", m_name, b_type, res_opt)
        
        res <- boot_subtest(
          model = curr_mod,
          age_name = "age",
          score_name = "shaped_score",
          boot_type = b_type,
          n_sim = if (b_type == "parametric") 40L else NULL,
          age_sampling_frame = if (b_type == "parametric") function(n) runif(n, 20, 60) else NULL,
          reselect = res_opt,
          min_age = 20,
          max_age = 60,
          step_size_age = 20,
          B = B_BOOT+10,
          seed = 820,
          parallel = FALSE
        )
        
        expect_s3_class(res, "normref_bootstrap")
        expect_equal(res$meta$boot_type, b_type)
        expect_gt(res$trueB, 0)
        expect_true(is.data.frame(res$normtable))
        expect_true(is.data.frame(res$sample))
        
        if (res_opt) {
          expect_true(!is.null(res$model_freq_table))
          expect_equal(sum(res$model_freq_table), res$trueB)
        } else {
          expect_null(res$model_freq_table)
        }
      }
    }
  }
})



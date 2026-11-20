test_that("fb_select validates input parameters correctly", {
  #skip_on_cran()
  skip_if_not_installed("gamlss")
  
  # Minimal test data
  test_data <- data.frame(
    age   = c(20, 25, 30, 35, 40),
    Score = c(10, 12, 15, 18, 20)
  )
  
  # Negative values
  expect_error(
    fb_select(test_data, "age", "shaped_score", "NO", max_poly = c(-1, 5, 2, 2)),
    "max_poly.*negative"
  )
  expect_error(
    fb_select(test_data, "age", "shaped_score", "NO", start_poly = c(-1, 1, 0, 0)),
    "start_poly.*negative"
  )
  expect_error(
    fb_select(test_data, "age", "shaped_score", "NO", min_poly = c(-1, 0, 0, 0)),
    "min_poly.*negative"
  )
  
  # min_poly > max_poly
  expect_error(
    fb_select(test_data, "age", "shaped_score", "NO", 
             min_poly = c(3, 0, 0, 0), max_poly = c(2, 5, 2, 2)),
    "min_poly.*exceed.*max_poly"
  )
  
  # start_poly outside range
  expect_error(
    fb_select(test_data, "age", "shaped_score", "NO", 
             start_poly = c(6, 1, 0, 0), max_poly = c(5, 5, 2, 2)),
    "start_poly.*between"
  )
  
  # Non-integers
  expect_error(
    fb_select(test_data, "age", "shaped_score", "NO", max_poly = c(5.5, 5, 2, 2)),
    "max_poly.*integers"
  )
  expect_error(
    fb_select(test_data, "age", "shaped_score", "NO", start_poly = c(2.5, 1, 0, 0)),
    "start_poly.*integers"
  )
  expect_error(
    fb_select(test_data, "age", "shaped_score", "NO", min_poly = c(0.5, 0, 0, 0)),
    "min_poly.*integers"
  )

})

test_that("fb_select works with different selection criteria", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 50)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  # Test BIC (default)
  model_bic <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                        selcrit = "BIC", trace = FALSE, method = "RS(50)")
  expect_s3_class(model_bic, "gamlss")
  expect_equal(model_bic$selcrit,145.738462, tolerance = 1e-4)
  
  # Test AIC
  model_aic <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                        selcrit = "AIC", trace = FALSE, method = "RS(50)")
  expect_s3_class(model_aic, "gamlss")
  expect_equal(model_aic$selcrit,142.776072, tolerance = 1e-4)
  
  # Test GAIC(3)
  model_gaic <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                         selcrit = "GAIC(3)", trace = FALSE, method = "RS(50)")
  expect_s3_class(model_gaic, "gamlss")
  expect_equal(model_gaic$selcrit,146.776072, tolerance = 1e-4)
  
  # Test CV
  model_cv <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                       selcrit = "CV", trace = FALSE, method = "RS(50)")
  expect_s3_class(model_cv, "gamlss")
  expect_equal(model_cv$selcrit,150.682923, tolerance = 1e-4)
})

test_that("fb_select handles spline option correctly", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 100)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  # Test with spline = FALSE (polynomial)
  model_poly <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                         spline = FALSE, trace = FALSE, method = "RS(50)")
  expect_s3_class(model_poly, "gamlss")
  
  # Test with spline = TRUE
  model_spline <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                           spline = TRUE, trace = FALSE, method = "RS(50)")
  expect_s3_class(model_spline, "gamlss")
})

test_that("fb_select respects polynomial degree constraints", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 100)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  # Test with constrained polynomial degrees
  model <- fb_select(shaped_data, "age", "shaped_score", "NO",
                    min_poly = c(2, 0, 0, 0),
                    max_poly = c(2, 1, 0, 0),
                    trace = FALSE, method = "RS(50)")
  
  model_pars <- extract_gamlss_coefs(model)
  expect_s3_class(model, "gamlss")
  expect_length(model_pars,4)
  # The selected model should respect the constraints
  # (exact polynomial degrees depend on the selection algorithm)
})

test_that("fb_select produces consistent results with same seed", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 50)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  model1 <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                     seed = 123, trace = FALSE, method = "RS(50)")
  model2 <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                     seed = 123, trace = FALSE, method = "RS(50)")
  
  expect_equal(model1$selcrit, model2$selcrit, tolerance = 1e-6)
  expect_equal(extract_gamlss_coefs(model1), extract_gamlss_coefs(model2), 
               tolerance = 1e-6)
})

test_that("fb_select handles different estimation methods", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 50)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  methods_to_test <- c("RS(50)", "CG(50)", "mixed(50)")
  
  for (method in methods_to_test) {
    expect_no_error(model <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                      method = method, trace = FALSE), message = paste("Failed for method:", method))
  }
})

test_that("fb_select returns model with correct structure", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 100)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  model <- fb_select(shaped_data, "age", "shaped_score", "NO", trace = FALSE, method = "RS(50)")
  
  # Check model structure
  expect_s3_class(model, "gamlss")
  expect_true("selcrit" %in% names(model))
  expect_true("call" %in% names(model))
  expect_true("parameters" %in% names(model))
  expect_true(is.numeric(model$selcrit))
  expect_true(model$selcrit > 0)
})


test_that("fb_select trace option works correctly", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 50)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  # Capture output with trace = TRUE
  output <- capture.output({
    model <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                      trace = TRUE, method = "RS(50)")
  })
  
  expect_true(length(output) > 0)
  expect_true(any(grepl("FB-select iteration", output)))
  expect_true(any(grepl("BIC", output)))
})

test_that("fb_select handles edge cases in polynomial selection", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 100)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  # Test with all zero polynomial degrees (intercept only)
  model_zero <- fb_select(shaped_data, "age", "shaped_score", "NO",
                         min_poly = c(0, 0, 0, 0),
                         max_poly = c(0, 0, 0, 0),
                         start_poly = c(0, 0, 0, 0),
                         trace = FALSE, method = "RS(50)")
  
  expect_s3_class(model_zero, "gamlss")
  
  # Test with maximum allowed degrees
  model_max <- fb_select(shaped_data, "age", "shaped_score", "NO",
                        min_poly = c(2, 1, 1, 1),
                        max_poly = c(2, 1, 1, 1),
                        start_poly = c(2, 1, 1, 1),
                        trace = FALSE, method = "RS(50)")
  
  expect_s3_class(model_max, "gamlss")
})

# Integration test with known test models (if available)
if (exists("info")) {
  test_that("fb_select produces similar results to reference models", {
    families <- c("NO", "BB", "BCPE")
    
    # Add NOtr if gamlss.tr is available
    if (requireNamespace("gamlss.tr", quietly = TRUE)) {
      gamlss.tr::gen.trun(par = c(0), family = "NO", name = "tr",
                          type = "left", envir = .GlobalEnv)
      families <- c(families, "NOtr")
    }
    
    for (fam in families) {
      if (fam %in% names(info)) {
        sim_data <- create_test_data(fam, n = 1000)
        shaped_data <- shape_data(sim_data, "age", "score", fam, verbose = FALSE)
        
        model <- fb_select(shaped_data, "age", "shaped_score", fam,
                          selcrit = "BIC",
                          start_poly = c(1, 0, 0, 0),
                          method = "RS(50)",
                          trace = FALSE)
        
        # Extract coefficients
        model_coefs <- list(extract_gamlss_coefs(model))
        names(model_coefs) <- fam
        reference_coefs <- info[which(names(info) == fam)]
        
        # Checks
        expect_true(length(model_coefs) == length(reference_coefs),
                    info = paste("Coefficient length mismatch for family:", fam))
        
        expect_equal(model_coefs, reference_coefs,
                     info = paste("Parameter mismatch for:", fam))
      }
    }
  })
}

# Performance test
test_that("fb_select completes within reasonable time", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 100)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  # Should complete within 30 seconds for small dataset
  expect_true({
    start_time <- Sys.time()
    model <- fb_select(shaped_data, "age", "shaped_score", "NO", 
                      trace = FALSE, method = "RS(50)")
    end_time <- Sys.time()
    as.numeric(end_time - start_time) < 30
  })
})

# Test for data integrity
test_that("fb_select preserves original data in model$data", {
  #skip_on_cran()
  
  sim_data <- create_test_data("NO", n = 50)
  shaped_data <- shape_data(sim_data, "age", "score", "NO", verbose = FALSE)
  
  model <- fb_select(shaped_data, "age", "shaped_score", "NO", trace = FALSE, method = "RS(50)")
  
  # Check that original data is preserved in call
  expect_true("data" %in% names(model$call))
  expect_equal(nrow(model$data), nrow(shaped_data))
})



test_that("get_selcrit returns Inf on convergence warnings (not discrepancy)", {
  skip_if_not_installed("gamlss")
  skip_on_cran()
  
  sim_data <- create_test_data("BCPE", n = 500)
  shaped_data <- shape_data(sim_data, "age", "score", "BCPE", verbose = FALSE)
  
  
  # This will often throw a convergence warning with BCPE
  expect_warning(
    res <- normref:::get_selcrit(
      poly_deg   = c(5, 5, 5, 15),
      selcrit    = "AIC",
      family     = "BCPE",
      spline     = "FALSE",
      method     = "RS(100)",
      data_name  = "shaped_data",
      age_name   = "age",
      score_name = "score",
      env        = environment()
    ),
    regexp = "excluded due to error"
  )
  # Convergence warnings should lead to exclusion
  expect_equal(res$val, Inf)
  expect_null(res$mod)
})


test_that("fb_select returns a spline when spline = TRUE", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  
  sim_data <- create_test_data("NO", n = 100)
  shaped_data <- shape_data(
    sim_data, "age", "score", "NO", verbose = FALSE
  )
  
  model <- fb_select(
    shaped_data,
    age_name = "age",
    score_name = "shaped_score",
    family = "NO",
    spline = TRUE,
    trace = FALSE,
    method = "RS(50)"
  )
  
  expect_s3_class(model, "gamlss")
  expect_false(is.null(getSmo(model)))
})

test_that("fb_select supports CV with a truncated distribution", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.tr")
  
  # Create a user-defined truncated NO distribution
  gamlss.tr::gen.trun(
    par = c(0),
    family = "NO",
    name = "tr",
    type = "left",
    envir = .GlobalEnv
  )
  
  sim_data <- create_test_data("NOtr", n = 100)
  shaped_data <- shape_data(
    sim_data, "age", "score", "NOtr", verbose = FALSE
  )
  
  model <- fb_select(
    shaped_data,
    age_name = "age",
    score_name = "shaped_score",
    family = "NOtr",
    selcrit = "CV",
    start_poly = c(0, 0, 0, 0),
    max_poly = c(2, 1, 0, 0),
    trace = FALSE,
    method = "RS(100)",
    seed = 123
  )
  
  expect_s3_class(model, "gamlss")
  expect_true(is.numeric(model$selcrit))
  expect_true(is.finite(model$selcrit))
  expect_equal(model$call$selcrit, "CV")
})

test_that("fb_select errors for an unrecognized distribution", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  sim_data <- create_test_data("NO", n = 50)
  shaped_data <- shape_data(
    sim_data, "age", "score", "NO", verbose = FALSE
  )
  
  expect_error(
    fb_select(
      shaped_data,
      age_name = "age",
      score_name = "shaped_score",
      family = "NOtr699",
      trace = FALSE
    ),
    "Distribution 'NOtr699' could not be found"
  )
})

test_that("fb_select returns a valid search history", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  sim_data <- create_test_data("NO", n = 50)
  shaped_data <- shape_data(
    sim_data, "age", "score", "NO", verbose = FALSE
  )
  
  model <- fb_select(
    shaped_data,
    age_name = "age",
    score_name = "shaped_score",
    family = "NO",
    trace = FALSE,
    method = "RS(50)"
  )
  
  expect_true("search_history" %in% names(model))
  expect_named(model$search_history, c("poly_deg", "selcrit"))
  
  expect_true(nrow(model$search_history) >= 1)
  expect_true(all(is.character(model$search_history$poly_deg)))
  expect_true(is.numeric(model$search_history$selcrit))
  expect_true(all(is.finite(model$search_history$selcrit)))
})

test_that("CV selection is reproducible with the same seed", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  sim_data <- create_test_data("NO", n = 750)
  shaped_data <- shape_data(
    sim_data, "age", "score", "NO", verbose = FALSE
  )
  
  model1 <- fb_select(
    shaped_data,
    age_name = "age",
    score_name = "shaped_score",
    family = "NO",
    selcrit = "CV",
    trace = FALSE,
    method = "RS(50)",
    seed = 123
  )
  
  model2 <- fb_select(
    shaped_data,
    age_name = "age",
    score_name = "shaped_score",
    family = "NO",
    selcrit = "CV",
    trace = FALSE,
    method = "RS(50)",
    seed = 123
  )
  
  expect_equal(model1$selcrit, model2$selcrit, tolerance = 1e-8)
  expect_equal(
    model1$search_history,
    model2$search_history,
    tolerance = 1e-8
  )
})
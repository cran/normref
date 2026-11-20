# -------------------------------------------------------------------
# centiles_bin tests
# -------------------------------------------------------------------
test_that("centiles_bin works silently and returns centile matrix when save=TRUE", {
  skip_on_cran()
  skip_if_not_installed("gamlss")
  
  model_info <- create_test_model(family = "BB", n = 80)
  mod_BB <- model_info$model
  data_BB <- model_info$data
  
  expect_silent({
    res <- centiles_bin(mod_BB,
                            xvar = data_BB$age,
                            cent = c(10, 50, 90),
                            plot = FALSE,
                            save = TRUE)
    expect_true(is.matrix(res))
    expect_equal(ncol(res), 2)
    expect_equal(res[, 1], c(10, 50, 90))
  })
})

test_that("centiles_bin errors with non-binomial family", {
  #skip_on_cran()
  skip_if_not_installed("gamlss")
  
  model_info <- create_test_model(family = "NO", n = 80)
  mod_NO <- model_info$model
  expect_error(
    centiles_bin(mod_NO, xvar = model_info$data$age),
    "binomial-type"
  )
})

test_that("centiles_bin snapshot test", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  get(data("ids_data"))
  
  mydata_BB <- shape_data(data = ids_data,
                          age_name = "age",
                          score_name = "y14",
                          family = "BB",
                          max_score = 34,
                          verbose = FALSE)
  mod_BB_y14 <- fb_select(data = mydata_BB,
                         age_name = "age",
                         score_name = "shaped_score",
                         family = "BB",
                         selcrit = "BIC",
                         trace = FALSE)
  
  vdiffr::expect_doppelganger(
    "centiles-BB-plot",
    function() centiles_bin(mod_BB_y14, xvar = mydata_BB$age, cent = c(10, 50, 90))
  )
})

# -------------------------------------------------------------------
# plot_normtable tests
# -------------------------------------------------------------------
test_that("plot_normtable works with fake normtable object", {
  #skip_on_cran()
  
  model_info <- create_test_model(family = "BB", n = 50)
  mod_BB <- model_info$model
  data_BB <- model_info$data
  fake_normtable <- normtable_create(mod_BB,data_BB,age_name = "age", score_name = "score")
  
  expect_silent({
    plot_normtable(fake_normtable)
  })
})

test_that("plot_normtable errors if pop_age is missing", {
  #skip_on_cran()
  
  bad_normtable <- list(
    cdf_matrix = matrix(runif(100), ncol = 5),
    cdf_sample = runif(20),
    pop_age = data.frame(age = rep(NA, 20)),
    data = data.frame(age = runif(20, 18, 65))
  )
  class(bad_normtable) <- "NormTable"
  
  expect_error(
    plot_normtable(bad_normtable),
    "normtable must be created with"
  )
})

test_that("plot_normtable works with NOtr family if gamlss.tr is installed", {
  # Skip test if gamlss.tr is not available
  skip_if_not_installed("gamlss.tr")
  
  # Generate truncated NO family functions
  gamlss.tr::gen.trun(par = c(0), family = "NO", name = "tr",
                      type = "left", envir = .GlobalEnv)
  
  # Create a test model and data for NOtr
  test_setup <- create_test_model(family = "NOtr", n = 50)
  model <- test_setup$model
  data <- test_setup$data
  
  # Create a normtable
  fake_normtable <- normtable_create(model, data, age_name = "age", score_name = "shaped_score")
  
  # Check that plotting works silently
  expect_silent({
    plot_normtable(fake_normtable)
  })
})

test_that("plot_normtable snapshot test", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  get(data("ids_data"))
  
  modNO_y14 <- fb_select(data = ids_data,
                        age_name = "age",
                        score_name = "y14",
                        family = "NO",
                        selcrit = "BIC",
                        trace = FALSE)
  
  get(data("ids_rel_data"))
  
  norm_mod_NO_y14  <- normtable_create(model= modNO_y14,
                                         data = ids_data,
                                         age_name = "age",
                                         score_name = "y14",
                                         datarel = ids_rel_data,
                                         normtype = "IQ")
  
  vdiffr::expect_doppelganger(
    "normtable-plot",
    function() plot_normtable(norm_mod_NO_y14)
  )
})

test_that("plot_drel snapshot with real ids_kn_data", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  
  data("ids_kn_data", package = "normref")
  item_variables_kn <- which(substr(colnames(ids_kn_data), 1, 2) == "KN")
  rel_est <- reliability_window(
    data = ids_kn_data,
    age_name = "age_years",
    item_variables = item_variables_kn,
    window_width = 2,
  )
  
  rel_int <- different_rel(data = ids_kn_data, 
                           item_variables = item_variables_kn, 
                           step_window = c(0.5, 1 , 2, 5, 10, 20), 
                           age_name = "age_years",
                           min_agegroup = 5,
                           max_agegroup = 20,
                           step_agegroup = c(0.5,1,1.5,2))
  
  p <- plot_drel(rel_int, ncol = 2)
  vdiffr::expect_doppelganger("Drel plot ids_kn_data", p)
})

# -----------------------------------------------------------------------------
# bootstrap plots
# -----------------------------------------------------------------------------
delayedAssign("boot_bb",   get_ids_bb_boot(B = B_BOOT))
delayedAssign("boot_bccg", get_ids_bccgtr_boot(B = B_BOOT))
delayedAssign("boot_comp", get_ids_composite_boot(B = B_BOOT))

# -----------------------------------------------------------------------------
# boot_centiles tests
# -----------------------------------------------------------------------------
test_that("boot_centiles validates input object class", {
  skip_on_cran()
  
  expect_error(
    boot_centiles(boot = list(a = 1, b = 2)),
    regexp = "'boot' must be of class 'normref_bootstrap'"
  )
})

test_that("boot_centiles returns a ggplot object and respects options", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  # Multiple centiles -> produces facet_wrap
  p_multi <- boot_centiles(
    boot_bccg,
    centiles = c(10, 50, 90),
    benchmark_n = c(200, 400),
    interpolate = TRUE
  )
  
  expect_s3_class(p_multi, "ggplot")
  expect_true(inherits(p_multi$facet, "FacetWrap"))
  
  # Single centile -> no facet wrap
  p_single <- boot_centiles(
    boot_bccg,
    centiles = 50,
    benchmark_n = 300,
    interpolate = FALSE
  )
  expect_s3_class(p_single, "ggplot")
  expect_true(inherits(p_single$facet, "FacetNull"))
})

test_that("boot_centiles respects age_range filter", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  p <- boot_centiles(boot_bccg, centiles = c(25, 75), age_range = c(8, 16))
  
  # Plotted range within [8, 16]
  plot_data <- p$data
  expect_true(all(plot_data$age >= 8 & plot_data$age <= 16))
})

test_that("boot_centiles vdiffr snapshot tests (BCCGtr34 and BB)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("gamlss.tr")
  
  # 1. Truncated continuous model (BCCGtr34) on IQ scale
  vdiffr::expect_doppelganger(
    "boot-centiles-bccgtr-iq",
    boot_centiles(
      boot = boot_bccg,
      centiles = c(5, 50, 95),
      benchmark_n = c(200, 300, 400)
    )
  )
  
  # 2. Discrete bounded count model (BB) on Z scale
  
  vdiffr::expect_doppelganger(
    "boot-centiles-bb-z",
    boot_centiles(
      boot = boot_bb,
      centiles = c(10, 50, 90),
      benchmark_n = c(200, 400)
    )
  )
})

# -----------------------------------------------------------------------------
# boot_heatmap tests
# -----------------------------------------------------------------------------

test_that("boot_heatmap validates input arguments", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  # 1. Invalid class
  expect_error(
    boot_heatmap(boot = data.frame(x = 1:5)),
    regexp = "'boot' must be of class 'normref_bootstrap'"
  )
  
  # 2. Invalid type
  expect_error(
    boot_heatmap(boot_bccg, type = "invalid_type"),
    regexp = "type must be 'se', 'difference', or 'ratio'"
  )
  
  # 3. Missing benchmark_n when type is difference or ratio
  expect_error(
    boot_heatmap(boot_bccg, type = "difference"),
    regexp = "benchmark_n must be provided when type = 'difference' or 'ratio'"
  )
  expect_error(
    boot_heatmap(boot_bccg, type = "ratio"),
    regexp = "benchmark_n must be provided when type = 'difference' or 'ratio'"
  )
})

test_that("boot_heatmap constructs valid plots for all types with overlay options", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  # Type: 'se'
  p_se <- boot_heatmap(
    boot_bccg,
    type = "se",
    add_points = TRUE,
    point_style = "hollow"
  )
  expect_s3_class(p_se, "ggplot")
  
  # Type: 'difference' with contour
  p_diff <- boot_heatmap(
    boot_bccg,
    type = "difference",
    benchmark_n = 300,
    add_contour = TRUE,
    print_pct = FALSE
  )
  expect_s3_class(p_diff, "ggplot")
  
  # Type: 'ratio' with solid points
  p_ratio <- boot_heatmap(
    boot_bccg,
    type = "ratio",
    benchmark_n = 300,
    add_points = TRUE,
    point_style = "solid"
  )
  expect_s3_class(p_ratio, "ggplot")
})

test_that("boot_heatmap respects range filtering and print_pct output", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  # Filtering check
  p_filtered <- boot_heatmap(
    boot_bccg,
    type = "se",
    age_range = c(8, 16),
    score_range = c(5, 25)
  )
  expect_true(all(p_filtered$data$age >= 8 & p_filtered$data$age <= 16))
  expect_true(all(p_filtered$data$score >= 5 & p_filtered$data$score <= 25))
  
  # Console percentage output check
  expect_output(
    boot_heatmap(
      boot_bccg,
      type = "difference",
      benchmark_n = 300,
      print_pct = TRUE
    ),
    regexp = "Percentage of age-score range below contour"
  )
})

test_that("boot_heatmap vdiffr snapshot tests (BCCGtr34 and BB)", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("gamlss.tr")
  
  # 1. Standard Error heatmap (BCCGtr34) with centiles overlay
  vdiffr::expect_doppelganger(
    "boot-heatmap-bccgtr-se",
    boot_heatmap(
      boot = boot_bccg,
      type = "se",
      add_points = TRUE,
      centiles = c(5, 25, 50, 75, 95)
    )
  )
  
  # 2. Difference heatmap with contour (BCCGtr34)
  vdiffr::expect_doppelganger(
    "boot-heatmap-bccgtr-difference",
    boot_heatmap(
      boot = boot_bccg,
      type = "difference",
      benchmark_n = 300,
      add_contour = TRUE
    )
  )
  
  # 3. Ratio heatmap with contour and points (BB model)
  vdiffr::expect_doppelganger(
    "boot-heatmap-bb-ratio",
    boot_heatmap(
      boot = boot_bb,
      type = "ratio",
      benchmark_n = 300,
      add_contour = TRUE,
      add_points = TRUE
    )
  )
})

# -----------------------------------------------------------------------------
# boot_sample tests
# -----------------------------------------------------------------------------

test_that("boot_sample validates inputs", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  # 1. Invalid class
  expect_error(
    boot_sample(boot = 1:10),
    regexp = "'boot' must be of class 'normref_bootstrap'"
  )
  
  # 2. Invalid type option
  expect_error(
    boot_sample(boot_comp, type = "not_a_type")
  )
  
  # 3. Missing $sample element
  bad_boot <- boot_comp
  bad_boot$sample <- NULL
  expect_error(
    boot_sample(bad_boot),
    regexp = "No '\\$sample' slot found"
  )
})

test_that("boot_sample returns ggplot object for all types and supports smoother", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  # Ratio with smoother
  p_ratio <- boot_sample(
    boot_comp,
    type = "ratio",
    benchmark_n = 300,
    smooth = TRUE
  )
  expect_s3_class(p_ratio, "ggplot")
  expect_true(inherits(p_ratio$facet, "FacetWrap"))
  
  # Difference without smoother
  p_diff <- boot_sample(
    boot_comp,
    type = "difference",
    benchmark_n = 300,
    smooth = FALSE
  )
  expect_s3_class(p_diff, "ggplot")
  
  # Raw SE
  p_se <- boot_sample(
    boot_comp,
    type = "se",
    benchmark_n = 300
  )
  expect_s3_class(p_se, "ggplot")
})

test_that("boot_sample respects custom z_bins and age_range", {
  skip_on_cran()
  skip_if_not_installed("gamlss.tr")
  
  custom_bins <- c(-2, 0, 2)
  
  p <- boot_sample(
    boot_comp,
    benchmark_n = 300,
    z_bins = custom_bins,
    age_range = c(8, 16)
  )
  
  expect_equal(nlevels(p$data$score_bin), length(custom_bins) - 1)
  expect_true(all(p$data$age >= 8 & p$data$age <= 16))
})

test_that("boot_sample vdiffr snapshot tests", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("gamlss.tr")
  
  # 1. Ratio plot across z-bins with smoother
  vdiffr::expect_doppelganger(
    "boot-sample-composite-ratio",
    boot_sample(
      boot = boot_comp,
      type = "ratio",
      benchmark_n = 400,
      z_bins = seq(-2, 2, by = 1),
      smooth = TRUE
    )
  )
  
  # 2. Raw SE plot across z-bins without smoother
  vdiffr::expect_doppelganger(
    "boot-sample-composite-se",
    boot_sample(
      boot = boot_comp,
      type = "se",
      benchmark_n = 400,
      z_bins = seq(-2, 2, by = 1),
      smooth = FALSE
    )
  )
})
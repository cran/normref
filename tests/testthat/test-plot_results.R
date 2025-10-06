
# -------------------------------------------------------------------
# centiles_bin tests
# -------------------------------------------------------------------
test_that("centiles_bin works silently and returns centile matrix when save=TRUE", {
  #skip_on_cran()
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
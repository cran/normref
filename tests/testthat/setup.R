# tests/testthat/setup.R
# -------------------------------------------------------------------------
# Setup file for testthat
# Runs once before the test suite.
# -------------------------------------------------------------------------
library(gamlss.dist)
library(gamlss)

# Ensure required truncated family is available
if (requireNamespace("gamlss.tr", quietly = TRUE)) {
  gamlss.tr::gen.trun(
    par   = c(0),
    family = "NO",
    name   = "tr",
    type   = "left",
    envir  = .GlobalEnv  # make functions visible globally
  )
  
  gamlss.tr::gen.trun(par = 34, family = "BCCG", name = "tr34", type = "right", envir = .GlobalEnv)
  
} else {
  warning("Package 'gamlss.tr' not installed; truncated NO family not available for tests")
}

# Load test data if available
if (file.exists("tests/modsel_testmodels.Rda")) {
  load("tests/modsel_testmodels.Rda")
}

# Helper functions for testing
create_test_data <- function(family, n = 100, age_range = c(18, 65), max_score = 20) {
  set.seed(42)
  ages <- runif(n, age_range[1], age_range[2])
  rfun <- paste0("r", family)
  scores <- eval(call(rfun, n))
  
  if (family =="BNB"){
    scores <- eval(call(rfun, n, mu = 10, sigma = 0.5, nu=2))
  }
  if (family %in% c("ZABNB", "ZIBNB")){
    scores <- eval(call(rfun, n, mu = 10, sigma = 0.5, nu=2, tau=.1))
  }
  if (family == "SI"){
    scores <- eval(call(rfun, n, mu=10, sigma=1, nu=1))
  }
  if (family == "RGE"){
    scores <- eval(call(rfun, n, mu=0,sigma=1,nu=5))
  }
  
  if (family == "NBF"){
    ages <- log(ages)
  }
  return(data.frame(age = ages, score = scores))
}

extract_gamlss_coefs <- function(model) {
  params <- model$parameters
  out_vec <- NULL
  for(p in params){
    out_vec <- c(out_vec, coef(model, what = p))
  }
  out_vec
}

create_test_model <- function(family = "NO", n = 100) {
  test_data <- create_test_data(family, n)
  shaped_data <- shape_data(test_data, "age", "score", family, verbose = FALSE)
  model <- fb_select(shaped_data, "age", "shaped_score", family, 
                    trace = FALSE, method = "RS(50)",
                    start_poly = c(2, 1, 0, 0))
  return(list(model = model, data = shaped_data))
}

create_reliability_data <- function(min_age = 18, max_age = 65, rel_value = 0.8) {
  data.frame(
    age = seq(min_age, max_age, length.out = 10),
    rel = rep(rel_value, 10)
  )
}

simulate_1pl <- function(N = 200, J = 6, seed = 123) {
  set.seed(seed)
  theta <- rnorm(N, mean = 0, sd = 1)
  age   <- round(runif(N, min = 5, max = 15), 1)
  b <- rnorm(J, mean = 0, sd = 1)
  prob <- sapply(b, function(bj) plogis(theta - bj))
  responses <- matrix(rbinom(N * J, 1, c(prob)), nrow = N, ncol = J)
  colnames(responses) <- paste0("item", 1:J)
  data.frame(age = age, responses)
}


make_fake_normtable <- function(n = 10, offset = 0) {
  norm_sample <- data.frame(age = seq(5, 5 + n - 1))
  znorm_sample <- data.frame(id = seq_len(n),
                             z = rnorm(n, mean = offset))
  list(norm_sample = norm_sample,
       znorm_sample = znorm_sample)
}

# bootstrap
# Small B keeps these tests suitable for routine package checks.
B_BOOT <- 10L
N_BOOT <- 60L

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

fit_fb_select_no <- function(n = N_BOOT, seed = 101) {
  set.seed(seed)
  dat <- create_test_data("NO", n = n)
  shaped <- shape_data(
    dat,
    age_name = "age",
    score_name = "score",
    family = "NO",
    verbose = FALSE
  )
  
  mod <- fb_select(
    data = shaped,
    age_name = "age",
    score_name = "shaped_score",
    family = "NO",
    start_poly = c(1, 0, 0, 0),
    max_poly = c(1, 0, 0, 0),
    trace = FALSE,
    method = "RS(30)"
  )
  
  list(data = shaped, model = mod)
}

fit_fb_select_truncated_no <- function(n = N_BOOT, seed = 102) {
  skip_if_not_installed("gamlss.tr")
  
  # gen.trun() creates the NOtr family in the requested environment.  The
  # guard avoids redefining it if setup.R has already created it.
  if (!exists("NOtr", mode = "function", envir = .GlobalEnv)) {
    gamlss.tr::gen.trun(
      par = c(0),
      family = "NO",
      name = "tr",
      type = "left",
      envir = .GlobalEnv
    )
  }
  
  set.seed(seed)
  dat <- create_test_data("NOtr", n = n)
  shaped <- shape_data(
    dat,
    age_name = "age",
    score_name = "score",
    family = "NOtr",
    verbose = FALSE
  )
  
  mod <- fb_select(
    data = shaped,
    age_name = "age",
    score_name = "shaped_score",
    family = "NOtr",
    start_poly = c(1, 0, 0, 0),
    max_poly = c(1, 0, 0, 0),
    trace = FALSE,
    method = "RS(100)"
  )
  
  list(data = shaped, model = mod)
}

fit_pb_no <- function(n = N_BOOT, seed = 103) {
  set.seed(seed)
  dat <- create_test_data("NO", n = n)
  shaped <- shape_data(
    dat,
    age_name = "age",
    score_name = "score",
    family = "NO",
    verbose = FALSE
  )
  
  mod <- gamlss::gamlss(
    shaped_score ~ pb(age),
    data = shaped,
    family = "NO",
    trace = FALSE
  )
  
  list(data = shaped, model = mod)
}

run_small_parametric_boot <- function(fitted, seed = 201) {
  boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "parametric",
    n_sim = 50L,
    age_sampling_frame = function(n) runif(n, min = 20, max = 60),
    min_age = 20,
    max_age = 60,
    step_size_age = 10,
    B = B_BOOT,
    seed = seed,
    parallel = FALSE
  )
}

fit_fb_select_bb <- function(n = N_BOOT) {
  dat <- create_test_data("BB", n = n)
  shaped <- shape_data(
    dat,
    age_name = "age",
    score_name = "score",
    family = "BB",
    verbose = FALSE
  )
  
  mod <- fb_select(
    data = shaped,
    age_name = "age",
    score_name = "shaped_score",
    family = "BB",
    start_poly = c(1, 0),
    max_poly = c(1, 0),
    trace = FALSE,
    method = "RS(30)"
  )
  
  list(data = shaped, model = mod)
}

expect_valid_boot_subtest <- function(x, expected_boot_type = "parametric") {
  expect_s3_class(x, "normref_bootstrap")
  expect_equal(x$meta$type, "single")
  expect_equal(x$meta$boot_type, expected_boot_type)
  expect_gt(x$B, 0)
  expect_gt(x$trueB, 0)
  expect_true(all(x$normtable$boot_se[!is.na(x$normtable$boot_se)] > 0))
  expect_true(is.data.frame(x$normtable))
  expect_true(is.data.frame(x$sample))
  expect_true("original" %in% names(x$normtable))
  expect_true("boot_se" %in% names(x$normtable))
  expect_true("original" %in% names(x$sample))
  expect_true("boot_se" %in% names(x$sample))
}

create_composite_fixture <- function(n = N_BOOT) {
  # Subtest 1: NO continuous
  dat_no <- create_test_data("NO", n = n)
  shaped_no <- shape_data(dat_no, "age", "score", "NO", verbose = FALSE)
  mod_no <- fb_select(
    data = shaped_no,
    age_name = "age",
    score_name = "shaped_score",
    family = "NO",
    start_poly = c(1, 0),
    max_poly = c(1, 0),
    trace = FALSE,
    method = "RS(30)"
  )
  
  # Subtest 2: BB bounded, aligned to the exact same age vector
  dat_bb <- create_test_data("BB", n = n)
  dat_bb$age <- dat_no$age  # Align age vector for matching observations
  shaped_bb <- shape_data(dat_bb, "age", "score", "BB", verbose = FALSE)
  mod_bb <- fb_select(
    data = shaped_bb,
    age_name = "age",
    score_name = "shaped_score",
    family = "BB",
    start_poly = c(1, 0),
    max_poly = c(1, 0),
    trace = FALSE,
    method = "RS(30)"
  )
  
  # Construct composite norm tables and composite model
  nt1 <- normtable_create(mod_no, shaped_no, age_name = "age", score_name = "shaped_score", normtype = "Z", trim= Inf)
  nt2 <- normtable_create(mod_bb, shaped_bb, age_name = "age", score_name = "shaped_score", normtype = "Z", trim = Inf)
  comp_df <- composite_shape(list(sub_no = nt1, sub_bb = nt2))
  comp_df$age <- dat_no$age
  
  comp_mod <- fb_select(
    data = comp_df,
    age_name = "age",
    score_name = "z_sum",
    family = "NO",
    start_poly = c(1, 0),
    max_poly = c(1, 0),
    trace = FALSE,
    method = "RS(30)"
  )
  
  list(
    subtest_models = list(sub_no = mod_no, sub_bb = mod_bb),
    composite_model = comp_mod,
    data_no = shaped_no,
    data_bb = shaped_bb,
    comp_df = comp_df
  )
}

get_plot_test_boot <- function() {
  fitted <- fit_fb_select_no(n = N_BOOT, seed = 801)
  boot_subtest(
    model = fitted$model,
    age_name = "age",
    score_name = "shaped_score",
    boot_type = "nonparametric",
    B = B_BOOT,
    seed = 802,
    step_size_age = 10,
    parallel = FALSE
  )
}

get_ids_bccgtr_boot <- function(B = B_BOOT) {
  data("ids_data", package = "normref", envir = environment())
  
  dat_bccg <- shape_data(
    ids_data,
    age_name = "age",
    score_name = "y7",
    family = "BCCGtr34",
    verbose = FALSE
  )
  
  mod_bccg <- fb_select(
    data = dat_bccg,
    age_name = "age",
    score_name = "shaped_score",
    family = "BCCGtr34",
    selcrit = "BIC",
    trace = FALSE
  )
  
  boot_subtest(
    model = mod_bccg,
    age_name = "age",
    normtype = "IQ",
    B = B,
    min_age = 5,
    max_age = 20,
    step_size_age = 2,
    seed = 42,
    parallel = FALSE
  )
}

get_ids_bb_boot <- function(B = B_BOOT) {
  data("ids_data", package = "normref", envir = environment())
  
  dat_bb <- shape_data(
    ids_data,
    age_name = "age",
    score_name = "y7",
    family = "BB",
    verbose = FALSE
  )
  
  mod_bb <- fb_select(
    data = dat_bb,
    age_name = "age",
    score_name = "shaped_score",
    family = "BB",
    selcrit = "BIC",
    trace = FALSE
  )
  
  boot_subtest(
    model = mod_bb,
    age_name = "age",
    normtype = "Z",
    B = B,
    min_age = 5,
    max_age = 20,
    step_size_age = 2,
    seed = 43,
    parallel = FALSE
  )
}

get_ids_composite_boot <- function(B = B_BOOT) {
  data("ids_data", package = "normref", envir = environment())
  
  d1 <- shape_data(ids_data, age_name = "age", score_name = "y7", family = "BCCGtr34", verbose = FALSE)
  d2 <- shape_data(ids_data, age_name = "age", score_name = "y14", family = "BCCGtr34", verbose = FALSE)
  
  m1 <- fb_select(d1, age_name = "age", score_name = "shaped_score", family = "BCCGtr34", trace = FALSE)
  m2 <- fb_select(d2, age_name = "age", score_name = "shaped_score", family = "BCCGtr34", trace = FALSE)
  
  nt1 <- normtable_create(m1, d1, age_name = "age", score_name = "shaped_score", normtype = "Z", trim = Inf)
  nt2 <- normtable_create(m2, d2, age_name = "age", score_name = "shaped_score", normtype = "Z", trim = Inf)
  
  c_data <- composite_shape(list(y7 = nt1, y14 = nt2))
  c_mod <- fb_select(c_data, age_name = "age", score_name = "z_sum", family = "NO", trace = FALSE)
  
  boot_composite(
    subtest_models = list(y7 = m1, y14 = m2),
    composite_model = c_mod,
    age_name = "age",
    composite_score_name = "z_sum",
    normtype = "Z",
    min_age = 5,
    max_age = 20,
    step_size_age = 2,
    min_score = -4,
    max_score = 4,
    step_size_score = 0.5,
    B = B,
    seed = 44,
    parallel = FALSE
  )
}


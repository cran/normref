# tests/testthat/setup.R
# -------------------------------------------------------------------------
# Setup file for testthat
# Runs once before the test suite.
# -------------------------------------------------------------------------
library(gamlss.dist)

# Ensure required truncated family is available
if (requireNamespace("gamlss.tr", quietly = TRUE)) {
  gamlss.tr::gen.trun(
    par   = c(0),
    family = "NO",
    name   = "tr",
    type   = "left",
    envir  = .GlobalEnv  # make functions visible globally
  )
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


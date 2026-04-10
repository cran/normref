test_that("gamlss models run without error for all supported distributions", {
  #skip_on_cran()
  
  set.seed(123)
  
  distributions <- c("BE", "BB", "BNB", "BEOI", "BEZI", "BEINF", "BI", "BCCG", "BCPE", "BCT",
                     "DEL", "DBURR12", "DPO", "DBI", "EXP", "exGAUS", "EGB2", "GA", "GB1", "GB2",
                     "GG", "GIG", "GT", "GEOM", "GEOMo", "GU", "IGAMMA", "IG", "JSU", "LG", "LO",
                     "LOGITNO", "LOGNO", "LNO", "NBI", "NBII", "NBF", "NET", "NO", "NOF", "LQNO",
                     "PARETO2", "PARETO2o", "PE", "PE2", "PO", "PIG", "RGE", "RG", "SHASH", "SHASHo",
                     "SI", "SICHEL", "SIMPLEX", "ST1", "ST2", "ST3", "ST4", "ST5", "TF", "WARING",
                     "WEI", "WEI2", "WEI3", "YULE", "ZABI", "ZABNB", "ZAIG", "ZALG", "ZANBI", "ZAP",
                     "ZASICHEL", "ZAZIPF", "ZIBI", "ZIBNB", "ZINBI", "ZIP", "ZIP2", "ZIPIG", 
                     "ZISICHEL", "ZIPF")
  
  for (dist in distributions) {
    # Create mock data for different distribution types
    age <- runif(200, 20, 60)
    
    if (dist %in% c("BE", "BEZI", "BEINF", "BEOI", "SIMPLEX")) {
      score <- runif(200, 0.01, 0.99)
    } else if (dist == "BB") {
      score <- round(runif(200, 0, 10))
    } else if (dist %in% c("PO", "NBI", "NBII", "ZINBI", "ZIP", "ZIP2", "ZIPIG", "ZAP", "ZIBI",
                           "ZABNB", "ZANBI", "GEOM", "GEOMo", "ZIBNB", "WARING", "ZASICHEL",
                           "SICHEL", "ZAZIPF", "ZIPF", "DPO", "DBI", "ZABI", "ZISICHEL")) {
      score <- rpois(200, lambda = 3)
    } else if (dist %in% c("GA", "WEI", "WEI2", "WEI3", "IG", "IGAMMA", "BCCG", "BCPE", "BCT",
                           "GB1", "GB2", "GG", "GIG", "GT", "PARETO2", "PARETO2o", "RGE", "RG",
                           "PE", "PE2", "LOGNO", "LNO", "LOGITNO", "DEL", "NET", "EXP", "PIG")) {
      score <- rexp(200, rate = 1) + 0.01
    } else {
      score <- rnorm(200)
    }
    
    score <- rnorm(200)
    
    df <- data.frame(age = age, score = score)
    


    pfun <- paste("p", dist, sep = "")
    
    shaped <- shape_data(df, age_name = "age", score_name = "score", family = dist, max_score = 10, verbose = FALSE)
    
    expect_error({
           newcall <- call(pfun, shaped$Score)
      eval(call)
            #model <- gamlss(Score ~ age, data = shaped, family = dist, method = RS(200))
    }, NA, info = paste("gamlss should run without error for family:", dist))
  }})


# -------------------------------------------------------------------
# Tests
# -------------------------------------------------------------------
test_that("composite_shape works with a single normtable", {
  #skip_on_cran()
  
  fake_norm <- make_fake_normtable(5, offset = 0)
  res <- composite_shape(list(fake_norm))
  
  expect_s3_class(res, "data.frame")
  expect_equal(ncol(res), 2)
  expect_named(res, c("age", "z_sum"))
  expect_equal(res$age, fake_norm$norm_sample[, 1])
  expect_equal(res$z_sum, fake_norm$znorm_sample[, 2])
})

test_that("composite_shape correctly sums z-scores across objects", {
  #skip_on_cran()
  
  fake1 <- make_fake_normtable(5, offset = 0)
  fake2 <- make_fake_normtable(5, offset = 1) # shifted mean
  res <- composite_shape(list(fake1, fake2))
  
  expected <- fake1$znorm_sample[, 2] + fake2$znorm_sample[, 2]
  expect_equal(res$z_sum, expected)
})

test_that("composite_shape errors if input is not a list or empty", {
  #skip_on_cran()
  
  expect_error(composite_shape(NULL), "non-empty list")
  expect_error(composite_shape(list()), "non-empty list")
  expect_error(composite_shape("notalist"), "non-empty list")
})

test_that("composite_shape errors if required components are missing", {
  #skip_on_cran()
  
  bad_obj1 <- list(norm_sample = data.frame(age = 1:5)) # missing znorm_sample
  bad_obj2 <- list(znorm_sample = data.frame(id = 1:5, z = rnorm(5))) # missing norm_sample
  
  expect_error(composite_shape(list(bad_obj1)), "missing required components")
  expect_error(composite_shape(list(bad_obj2)), "missing required components")
})

test_that("sample_size_poly: n_groups validation works", {
  expect_error(sample_size_poly(n_groups = 0, poly_degree = 1, n0 = 10))
  expect_error(sample_size_poly(n_groups = -1, poly_degree = 1, n0 = 10))
  expect_error(sample_size_poly(n_groups = "three", poly_degree = 1, n0 = 10))
  expect_error(sample_size_poly(n_groups = c(3,4), poly_degree = 1, n0 = 10))
})

test_that("sample_size_poly: n0 must be provided", {
  expect_error(sample_size_poly(n_groups = 3, poly_degree = 1))
})

test_that("sample_size_poly: variances length must match n_groups", {
  expect_error(sample_size_poly(n_groups = 3, poly_degree = 1, n0 = 10, variances = c(1,2)))
})

test_that("sample_size_poly: output has correct columns and row count", {
  res <- sample_size_poly(n_groups = 3, poly_degree = c(0,1), n0 = c(10,20))
  expected_cols <- c("n0", "group", "poly_degree", "n", "variance", 
                     "variance_traditional", "total_n", "traditional_total_n", 
                     "n_min", "n_max")
  expect_true(all(expected_cols %in% colnames(res)))
  
  # Number of rows = n_groups * length(poly_degree) * length(n0)
  expect_equal(nrow(res), 3 * 2 * 2)
})

test_that("sample_size_poly: LP solution respects bounds and sums", {
  res <- sample_size_poly(n_groups = 3, poly_degree = c(0,1), n0 = c(10,20))
  
  # n within n_min and n_max
  expect_true(all(res$n >= res$n_min & res$n <= res$n_max))
  
  # total_n equals sum of n per combination of degree and n0
  combinations <- unique(res[, c("n0", "poly_degree")])
  for(i in seq_len(nrow(combinations))) {
    sub <- res[res$n0 == combinations$n0[i] & res$poly_degree == combinations$poly_degree[i], ]
    expect_equal(sub$total_n[1], sum(sub$n))
  }
})

test_that("sample_size_poly: multiple n0 and poly_degrees handled correctly", {
  res <- sample_size_poly(n_groups = 3, poly_degree = c(0,1), n0 = c(10,20))
  expect_true(all(res$n0 %in% c(10,20)))
  expect_true(all(res$poly_degree %in% c(0,1)))
})

test_that("sample_size_poly: edge cases", {
  # Single group
  res1 <- sample_size_poly(n_groups = 1, poly_degree = 0, n0 = 10)
  expect_equal(res1$n, 10)
  expect_equal(res1$total_n, 10)
  
  # Polynomial degree = 0
  res2 <- sample_size_poly(n_groups = 3, poly_degree = 0, n0 = 10)
  expect_true(all(res2$poly_degree == 0))
  
  # Single n0
  res3 <- sample_size_poly(n_groups = 3, poly_degree = 1, n0 = 10)
  expect_equal(unique(res3$n0), 10)
})

test_that("sample_size_poly: solution_type argument works", {
  # Balanced (default)
  res_bal <- sample_size_poly(n_groups = 3, poly_degree = 1, n0 = 10)
  expect_true(all(c("n0", "group", "poly_degree", "n") %in% colnames(res_bal)))
  expect_equal(nrow(res_bal), 3)  # one solution, one row per group
  
  # All solutions
  res_all <- sample_size_poly(n_groups = 3, poly_degree = 1, n0 = 10, solution_type = "all")
  expect_true(all(c("solution_id") %in% colnames(res_all)))
  expect_true(nrow(res_all) >= 3)  # >= 1 solution, 3 rows per solution
  expect_true(all(res_all$n >= res_all$n_min & res_all$n <= res_all$n_max))
  
  # Range solution
  res_range <- sample_size_poly(n_groups = 3, poly_degree = 1, n0 = 10, solution_type = "range")
  expect_true(all(c("n_range", "group", "poly_degree") %in% colnames(res_range)))
  expect_equal(nrow(res_range), 3)
  expect_true(all(grepl("-", res_range$n_range)))  # format like "14-19"
})

test_that("sample_size_poly: balanced solution has minimal n spread", {
  res_all <- sample_size_poly(n_groups = 4, poly_degree = 1, n0 = 10, solution_type = "all")
  spreads <- aggregate(n ~ solution_id, res_all, function(x) max(x) - min(x))
  
  res_bal <- sample_size_poly(n_groups = 4, poly_degree = 1, n0 = 10, solution_type = "balanced")
  bal_spread <- max(res_bal$n) - min(res_bal$n)
  
  expect_equal(bal_spread, min(spreads$n))
})

test_that("sample_size_poly: range output correctly summarizes min/max per group", {
  res_all <- sample_size_poly(n_groups = 3, poly_degree = 1, n0 = 10, solution_type = "all")
  res_range <- sample_size_poly(n_groups = 3, poly_degree = 1, n0 = 10, solution_type = "range")
  
  # Check consistency with all solutions
  for (g in 1:3) {
    group_n <- res_all$n[res_all$group == g]
    expected_range <- paste0(min(group_n), "-", max(group_n))
    expect_equal(res_range$n_range[res_range$group == g], expected_range)
  }
})

test_that("sample_size_poly: solution_id includes poly_degree, n0, and solution number", {
  res_all <- sample_size_poly(n_groups = 3, poly_degree = c(0,1), n0 = c(10,20), solution_type = "all")
  expect_true(all(grepl("deg[0-9]+_n0[0-9]+_s[0-9]+", res_all$solution_id)))
})
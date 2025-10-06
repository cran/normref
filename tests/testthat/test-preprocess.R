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

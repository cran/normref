test_that("precomputed vignette objects match the package version", {
  skip_on_cran()
  for (f in c("intro_vignette.rds", "bootstrap_vignette.rds")) {
    path <- system.file("extdata", "precomputed", f, package = "normref")
    expect_true(nzchar(path), info = f)
    expect_equal(attr(readRDS(path), "normref_version"),
                 as.character(packageVersion("normref")), info = f)
  }
})

test_that("precomputed vignette objects stay small", {
  skip_on_cran()
  files <- list.files(system.file("extdata", "precomputed", package = "normref"),
                      full.names = TRUE)
  expect_gt(length(files), 0)
  expect_lt(sum(file.size(files)), 5 * 1024^2)
})
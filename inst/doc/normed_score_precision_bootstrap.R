## ----setup1, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE, 
  warning = FALSE, 
  message = FALSE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6,
  out.width = "100%"
)

## ----setup2, echo = FALSE, include = TRUE-------------------------------------
library(normref)
library(gamlss)
library(gamlss.tr)
library(dplyr)

## ----load-precomputed, include=FALSE------------------------------------------
pre <- readRDS(system.file("extdata", "precomputed", "bootstrap_vignette.rds",
                           package = "normref", mustWork = TRUE))
list2env(pre, envir = knitr::knit_global())

## ----inspect-data-------------------------------------------------------------
# Load data
data(ids_data)

# Visualize scores
plot(ids_data$age, ids_data$y7,
     xlab = "Age",
     ylab = "Raw test score",
     pch = 15,
     cex = 0.5,
     col = gray(0.7))

## ----model-selection----------------------------------------------------------
# BB
mydata.BB <- shape_data(
  ids_data,
  age_name = "age",
  score_name = "y7",
  family = "BB"
)

mod.BB <- fb_select(
  data = mydata.BB,
  age_name = "age",
  score_name = "shaped_score",
  family = "BB",
  selcrit = "BIC"
)

# NOtr
gamlss.tr::gen.trun(par = c(0,34), family = "NO", name = 'tr34', type = "both")

mydata.NOtr34 <- shape_data(
  ids_data,
  age_name = "age",
  score_name = "y7",
  family = "NOtr34"
)

mod.NOtr34 <- fb_select(
  data = mydata.NOtr34,
  age_name = "age",
  score_name = "shaped_score",
  family = "NOtr34",
  selcrit = "BIC"
)

# BCCGtr
gamlss.tr::gen.trun(par = c(34), family = "BCCG", name = 'tr34', type = "right")

mydata.BCCGtr34 <- shape_data(
  ids_data,
  age_name = "age",
  score_name = "y7",
  family = "BCCGtr34"
)

mod.BCCGtr34 <- fb_select(
  data = mydata.BCCGtr34,
  age_name = "age",
  score_name = "shaped_score",
  family = "BCCGtr34",
  selcrit = "BIC"
)

# Compare model fit in terms of BIC
BIC(mod.BB, mod.NOtr34, mod.BCCGtr34)%>%arrange(BIC)

## ----create-normtable---------------------------------------------------------
normtable1 <- normtable_create(
  mod.BCCGtr34,
  mydata.BCCGtr34,
  normtype = "IQ",
  min_age = 5,
  max_age = 20,
  age_name = "age",
  score_name = "shaped_score"
)

normmat <- normtable1$norm_matrix

normmat %>%
  filter(age == 10)

## ----boot-subtest, eval = FALSE-----------------------------------------------
# # Bootstrap without model reselection, IQ-scale
# boot.BCCGtr34 <- boot_subtest(
#   model = mod.BCCGtr34,
#   age_name = "age",
#   normtype = "IQ",
#   B = 1000,
#   min_age = 5,
#   max_age = 20,
#   parallel = TRUE,
#   seed = 1
# )
# 
# 
# # Bootstrap with model reselection, Z-scale
# boot.BCCGtr34.reselect <- boot_subtest(
#   model = mod.BCCGtr34,
#   age_name = "age",
#   normtype = "Z",
#   B = 2000,
#   min_age = 5,
#   max_age = 20,
#   parallel = TRUE,
#   reselect = TRUE,
#   fb_control = list(method = "RS(30)"), # limit the maximum amount of iterations per fit to speed up calculations
#   seed = 2
# )

## ----boot_output--------------------------------------------------------------
# Table-level grid results
boot.BCCGtr34$normtable %>%
  filter(age == 10) %>%
  filter(`Subtest Score` %in% c(1,20,21,22,34))

# Sample-level individual results
head(boot.BCCGtr34$sample)

## ----inspect-mcse-------------------------------------------------------------
# Inspect relative MCSE of the bootstrap SE
boxplot(boot.BCCGtr34$mcse_normtable$boot_cv_se,
     main = "Distribution of Relative MCSE",
     ylab = "Relative MCSE of Bootstrap SE",
     col = "gray85")

# Inspect largest relative mcse
head(boot.BCCGtr34$mcse_normtable%>%arrange(desc(boot_cv_se)))

## ----selection-history--------------------------------------------------------
boot.BCCGtr34.reselect$model_freq_table

## ----plot-heatmap-se----------------------------------------------------------
boot_heatmap(
  boot.BCCGtr34,
  type = "se",
  add_points = TRUE,
  centiles = c(1, 5, 25, 50, 75, 95, 99),
  centile_label_nudge = 2.0
)

## ----plot-heatmap-compare-----------------------------------------------------
# Difference map for fixed-parameter model
boot_heatmap(
  boot.BCCGtr34,
  type = "difference",
  benchmark_n = 300,
  add_points = TRUE,
  add_contour = TRUE,
  centiles = c(1, 5, 25, 50, 75, 95, 99),
  centile_label_nudge = 2.0
)

# Relative ratio map
boot_heatmap(
  boot.BCCGtr34,
  type = "ratio",
  benchmark_n = 300,
  add_points = TRUE,
  add_contour = TRUE,
  centiles = c(1, 5, 25, 50, 75, 95, 99),
  centile_label_nudge = 2.0
)

# Reselected model specification across bootstrap samples
boot_heatmap(
  boot.BCCGtr34.reselect,
  type = "difference",
  benchmark_n = 300,
  add_points = TRUE,
  add_contour = TRUE,
  centiles = c(1, 5, 25, 50, 75, 95, 99),
  centile_label_nudge = 2.0
)

## ----plot-centiles------------------------------------------------------------
boot_centiles(
  boot.BCCGtr34,
  centiles = c(5,50,95),
  benchmark_n = c(200, 300, 400)
)

boot_centiles(
  boot.BCCGtr34.reselect,
  centiles = c(5,50,95),
  benchmark_n = c(200, 300, 400)
)

## ----composite-model----------------------------------------------------------
mydata1 <- shape_data(ids_data, age_name = "age", score_name = "y7",  family = "BCCGtr34")
mod1 <- fb_select(mydata1, age_name = "age", score_name = "shaped_score", family = "BCCGtr34")
mydata2 <- shape_data(ids_data, age_name = "age", score_name = "y14", family = "BCCGtr34")
mod2 <- fb_select(mydata2, age_name = "age", score_name = "shaped_score", family = "BCCGtr34")

normtable1 <- normtable_create(mod1, mydata1, age_name = "age", score_name = "shaped_score", trim = Inf)
normtable2 <- normtable_create(mod2, mydata2, age_name = "age", score_name = "shaped_score", trim = Inf)
comp_data <- composite_shape(list(y7 = normtable1, y14 = normtable2))
comp_model <- fb_select(comp_data, age_name = "age", score_name = "z_sum", family = "NO")

## ----composite-bootstrap, eval = FALSE----------------------------------------
# boot_comp <- boot_composite(
#   subtest_models = list(y7 = mod1, y14 = mod2),
#   composite_model = comp_model,
#   age_name = "age",
#   composite_score_name = "z_sum",
#   normtype = "Z",
#   step_size_score = 0.1,
#   B = 1000,
#   parallel = TRUE,
#   seed = 3
# )

## ----composite-sample---------------------------------------------------------
head(boot_comp$sample)

## ----composite-sample-plot----------------------------------------------------
boot_sample(
  boot_comp,
  type = "ratio",
  benchmark_n = 400,
  z_bins = seq(-3, 3, by = 1)
)

boot_sample(
  boot_comp,
  type = "se",
  benchmark_n = 400,
  z_bins = seq(-3, 3, by = 1),
  smooth = TRUE
)

## ----composite-normtable------------------------------------------------------
head(boot_comp$normtable)

boot_heatmap(boot_comp, type = "se", benchmark_n = 400, add_points = TRUE)
boot_heatmap(boot_comp, type = "difference", benchmark_n = 400, add_points = TRUE)
boot_centiles(boot_comp, centiles = c(5, 50, 95))

## ----planning-frames----------------------------------------------------------
# Define candidate age sampling frames
frame_uniform <- function(n) {
  runif(n, min = 5, max = 20)
}

frame_boundary_enriched <- function(n) {
  n_mid <- round(n * 0.80)
  n_low <- round(n * 0.10)
  n_high <- n - n_mid - n_low
  c(
    runif(n_mid, min = 5, max = 20),
    runif(n_low, min = 5, max = 8),
    runif(n_high, min = 17, max = 20)
  )
}

## ----simulate planning-frames, eval = FALSE-----------------------------------
# # Simulate Plan 1: Uniform N = 500
# plan_unif_500 <- boot_subtest(
#   model = mod.BCCGtr34,
#   age_name = "age",
#   score_name = "y7",
#   boot_type = "parametric",
#   n_sim = 500,
#   age_sampling_frame = frame_uniform,
#   normtype = "Z",
#   min_age = 5,
#   max_age = 20,
#   B = 1000,
#   parallel = TRUE,
#   seed = 123
# )
# 
# # Simulate Plan 2: Uniform N = 1000
# plan_unif_1000 <- boot_subtest(
#   model = mod.BCCGtr34,
#   age_name = "age",
#   score_name = "y7",
#   boot_type = "parametric",
#   n_sim = 1000,
#   age_sampling_frame = frame_uniform,
#   normtype = "Z",
#   min_age = 5,
#   max_age = 20,
#   B = 1000,
#   parallel = TRUE,
#   seed = 123
# )
# 
# # Simulate Plan 3: Boundary-Enriched N = 1000
# plan_boundary_1000 <- boot_subtest(
#   model = mod.BCCGtr34,
#   age_name = "age",
#   score_name = "y7",
#   boot_type = "parametric",
#   n_sim = 1000,
#   age_sampling_frame = frame_boundary_enriched,
#   normtype = "Z",
#   min_age = 5,
#   max_age = 20,
#   B = 1000,
#   parallel = TRUE,
#   seed = 123
# )

## ----planning-visualization---------------------------------------------------
# Uniform N = 500: Notice the SE spikes at ages 5 and 20
boot_centiles(
  plan_unif_500,
  centiles = c(5, 50, 95)
) + ggplot2::labs(title = "Uniform, N=500")

# Uniform N = 1000: Lower standard errors across the entire continuum
boot_centiles(
  plan_unif_1000,
  centiles = c(5,50, 95)
) + ggplot2::labs(title = "Uniform, N=1000")

# Uniform N = 1000: Boundary spikes are flattened with the same total N
boot_centiles(
  plan_boundary_1000,
  centiles = c(5,50, 95)
) + ggplot2::labs(title = "Boundary-enriched, N=1,000")


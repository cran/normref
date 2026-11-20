## ----setup, include = FALSE---------------------------------------------------
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

## ----load-precomputed, include=FALSE------------------------------------------
pre <- readRDS(system.file("extdata", "precomputed", "intro_vignette.rds",
                           package = "normref", mustWork = TRUE))
list2env(pre, envir = knitr::knit_global())

## ----loadlibrary, echo=TRUE, include=TRUE-------------------------------------
library(normref)
library(gamlss.dist) # also needed for vignette

## ----getdata, echo=TRUE, results='hide'---------------------------------------
get(data("ids_data"))

## ----shapedata_BB_y14, echo=TRUE, include=TRUE--------------------------------
mydata_BB_y14 <- shape_data(data = ids_data,
                     age_name = "age",
                     score_name = "y14",
                     family = "BB",
                     max_score = 34)

## ----shapedata_BCPE_y14, echo=TRUE, include=TRUE------------------------------
mydata_BCPE_y14 <- shape_data(data = ids_data,
                     age_name = "age",
                     score_name = "y14",
                     family = "BCPE")


## ----selectmodel_BB_y14, echo=TRUE, include=TRUE------------------------------
mod_BB_y14 <- fb_select(data = mydata_BB_y14,
                       age_name = "age",
                       score_name = "shaped_score",
                       family = "BB",
                       selcrit = "BIC")

## ----selectmodel_NO_y14, echo=TRUE, include=TRUE------------------------------
mod_NO_y14 <- fb_select(data = ids_data,
                      age_name = "age",
                      score_name = "y14",
                      family = "NO",
                      selcrit = "BIC")

## ----selectmodel_BCPE_y14, echo=TRUE, include=TRUE, eval=FALSE----------------
# mod_BCPE_y14<- fb_select(data = mydata_BCPE_y14,
#                          age_name = "age",
#                          score_name = "shaped_score",
#                          family = "BCPE",
#                          selcrit = "BIC")

## ----showfit, echo=TRUE, include=TRUE-----------------------------------------
mod_BB_y14$selcrit
mod_BCPE_y14$selcrit
mod_NO_y14$selcrit

## ----show_wormplot, echo=TRUE, include=TRUE, fig.width=6, fig.height=4--------
gamlss::wp(mod_BCPE_y14, ylim.all = 2)
gamlss::wp(mod_BB_y14, ylim.all = 2) 
gamlss::wp(mod_NO_y14, ylim.all = 2) 

## ----show_wormplot_age, echo=TRUE, include=TRUE, fig.width=6, fig.height=4, warning=FALSE, message = FALSE----
gamlss::wp(mod_BCPE_y14, xvar = age, ylim.worm = 2, n.inter = 4)

## ----plot_model_BCPE, echo=TRUE, include=TRUE, fig.width=6, fig.height=4------
gamlss::centiles(mod_BCPE_y14, cent = c(5,25,50,75,95))

## ----create_normtable_BCPE_excel, echo=FALSE----------------------------------
temp_file <- tempfile(fileext = ".xlsx")

## ----create_normtable_BCPE_excel2, echo=FALSE, eval = FALSE-------------------
# norm_mod_BCPE_y14  <- normtable_create(model = mod_BCPE_y14,
#                                       data = mydata_BCPE_y14,
#                                       age_name = "age",
#                                       score_name = "shaped_score",
#                                       normtype = "IQ",
#                                       excel_name = temp_file,
#                                       excel = TRUE)

## ----create_normtable_BCPE, echo=TRUE, include=TRUE---------------------------
head(norm_mod_BCPE_y14[["norm_matrix"]][, c(1, 11)],
     n = 15)

## ----plot_normtable_y14, echo=TRUE, include=TRUE, fig.width=6, fig.height=4----
plot_normtable(normtable = norm_mod_BCPE_y14)

## ----create_normtable_y14, echo=FALSE,  eval = FALSE--------------------------
# get(data("ids_rel_data"))
# temp_file <- tempfile(fileext = ".xlsx")
# norm_mod_BCPE_y14  <- normtable_create(model = mod_BCPE_y14,
#                                       data = mydata_BCPE_y14,
#                                       age_name = "age",
#                                       score_name = "shaped_score",
#                                       datarel = ids_rel_data,
#                                       normtype = "IQ",
#                                       excel_name = temp_file,
#                                       excel = TRUE)

## ----data_y7, echo=TRUE, include=TRUE-----------------------------------------
mydata_BCPE_y7 <- shape_data(data = ids_data,
                     age_name = "age",
                     score_name = "y7",
                     family = "BCPE")

## ----model_y7, echo=TRUE, include=TRUE, eval = FALSE--------------------------
# mod_BCPE_y7 <- fb_select(data = mydata_BCPE_y7,
#                         age_name = "age",
#                         score_name = "shaped_score",
#                         family = "BCPE",
#                         selcrit = "BIC")

## ----norms_y7, echo=TRUE, include=TRUE----------------------------------------
norm_mod_BCPE_y7_untrimmed <- normtable_create(model = mod_BCPE_y7,
                                    data = mydata_BCPE_y7,
                                    age_name = "age",
                                    score_name = "shaped_score",
                                    trim = Inf)

## ----modelnormcompositescore, echo=TRUE, include=TRUE-------------------------
norm_mod_BCPE_y14_untrimmed <- normtable_create(model = mod_BCPE_y14,
                                    data = mydata_BCPE_y14,
                                    age_name = "age",
                                    score_name = "shaped_score",
                                    trim = Inf)


composite <- list(norm_mod_BCPE_y7_untrimmed,
                  norm_mod_BCPE_y14_untrimmed )

composite_data <- composite_shape(normtables = composite)

modNO_comp <- fb_select(
  data = composite_data,
  age_name = "age",
  score_name = "z_sum",
  family = "NO",
  selcrit = "BIC"
)

norm_modNO_comp <- normtable_create(modNO_comp,
                                   composite_data,
                                   age_name = "age",
                                   score_name = "z_sum",
                                   cont_cor = TRUE)

## ----newsample, echo=TRUE, include=TRUE---------------------------------------
newdata <- ids_data[1:5,c("age","y14")]

## ----newsamplenormtable, echo=TRUE, include=TRUE, eval = FALSE----------------
# norm_mod_BCPE_newdata <- normtable_create(model = mod_BCPE_y14,
#                                          newdata = newdata,
#                                          age_name = "age",
#                                          score_name = "y14",
#                                          datarel = ids_rel_data)

## ----newsamplenorm, echo=TRUE, include=TRUE-----------------------------------
norm_mod_BCPE_newdata[["norm_sample"]]

## ----getdata_ids_kn_data, echo=TRUE, results='hide'---------------------------
get(data("ids_kn_data"))

## ----fit truncated model, echo=TRUE, include=TRUE-----------------------------

gamlss.tr::gen.trun(par=c(0,34), family="NO", name="tr34", type="both")

mydata_NOtr34 <- shape_data(data = ids_kn_data,
                     age_name = "age_years",
                     score_name = "rawscore",
                     family = "NOtr34")
mod_NOtr34_KN <- fb_select(data = mydata_NOtr34,
                        age_name = "age_years",
                        score_name = "shaped_score",
                        family = "NOtr34")

## ----item_variables_ids_kn_data, echo=TRUE, include=TRUE----------------------
item_variables_kn <- which(substr(colnames(ids_kn_data),1,2) == "KN")

## ----selectwindowstep_ids_kn_data, echo=TRUE, include=TRUE--------------------
rel_int <- different_rel(data = ids_kn_data, 
                         item_variables = item_variables_kn, 
                         step_window = c(0.5, 1 , 2, 5, 10, 20), 
                         age_name = "age_years",
                         min_agegroup = 5,
                         max_agegroup = 20,
                         step_agegroup = c(0.5,1,1.5,2))

## ----plot_selectwindow_ids_kn_data, echo=TRUE, include=TRUE, fig.width=6, fig.height=4----
plot_drel(rel_int, ncol = 2)

## ----estrel_ids_kn_data, echo=TRUE, include=TRUE------------------------------
rel_est <- reliability_window(data = ids_kn_data,
                   age_name = "age_years",
                   item_variables = item_variables_kn,
                   window_width = 2)

## ----calculate confidence intervals, echo=TRUE, include=TRUE, eval = FALSE----
# norm_mod_BCPEtr34_KN  <- normtable_create(model = mod_NOtr34_KN,
#                                      data = mydata_NOtr34,
#                                      age_name = "age_years",
#                                      score_name = "shaped_score",
#                                      datarel = rel_est,
#                                      normtype = "IQ")

## ----calculate sample size homoscedasticity, echo=TRUE, include=TRUE, eval = FALSE----
# res1 <- sample_size_poly(
#   n_groups = 5,
#   poly_degree = 3,
#   n0 = 400
# )

## ----results sample size homoscedasticity, echo=TRUE, include=TRUE------------
res1

## ----set up sample size heteroscedasticity, echo=TRUE, include=TRUE-----------
ids_data$age_group <- cut(ids_data$age, breaks = seq(6, 18, by = 1))
v <- tapply(ids_data$y7, ids_data$age_group, var, na.rm = TRUE)

## ----calculate sample size heteroscedasticity, echo=TRUE, include=TRUE, eval = FALSE----
# res2 <- sample_size_poly(
#  n_groups = length(v),
#  poly_degree = 2,
#  n0 = 300,
#  variances = v
#  )

## ----results sample size heteroscedasticity, echo=TRUE, include=TRUE----------
res2

